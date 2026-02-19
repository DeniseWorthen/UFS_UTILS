!> @file
!! @brief Write a SCRIP format file
!! @author Denise.Worthen@noaa.gov
!!
!> This module writes a SCRIP format file
!! @author Denise.Worthen@noaa.gov

module scripgrid

  use gengrid_kinds, only: dbl_kind,int_kind,CM
  use grdvars,       only: nv
  use charstrings,   only: logmsg
  use vartypedefs,   only: maxvars, scripvars, scripvars_typedefine
  use netcdf

  implicit none
  private

  public write_staggers

contains
  !> Reshape center and corner grid points for a given stagger location and write a SCRIP file
  !! @param[in]  fname             the file name to write
  !! @param[out] lon,lat           2D center lon,lat for a given stagger
  !! @param[out] lonvert, latvert  3D corner (vertices) lon and lat for a given stagger
  !! @param[in]  imask (optional)  the land mask values
  subroutine write_staggers(fname,lon,lat,lonvert,latvert,imask)

    character(len=*), intent(in)           :: fname
    real(dbl_kind)  , intent(in)           :: lon(:,:), lat(:,:), lonvert(:,:,:), latvert(:,:,:)
    integer(int_kind),intent(in), optional :: imask(:,:)

    real(dbl_kind),    allocatable, dimension(:)   :: cnlons, cnlats
    real(dbl_kind),    allocatable, dimension(:,:) :: crlons, crlats
    integer(int_kind), allocatable, dimension(:)   :: cnmask

    integer :: ib,ie,jb,je
    integer :: idim, jdim

    ib = lbound(lon,1); ie = ubound(lon,1)
    jb = lbound(lon,2); je = ubound(lon,2)
    idim = (ie - ib) + 1
    jdim = (je - jb) + 1

    allocate(cnlons(idim*jdim), source = 0.0_dbl_kind)
    allocate(cnlats(idim*jdim), source = 0.0_dbl_kind)
    allocate(cnmask(idim*jdim), source = 1_int_kind)
    allocate(crlons(nv,idim*jdim), source = 0.0_dbl_kind)
    allocate(crlats(nv,idim*jdim), source = 0.0_dbl_kind)

    if(present(imask))then
       cnmask = reshape(imask(ib:ie, jb:je), (/idim*jdim/))
    end if

    call get_staggers((/ib,ie/),(/ib,ie/),lon,lat,lonvert,latvert,cnlons,cnlats,crlons,crlats)
    logmsg = 'creating SCRIP file '//trim(fname)
    print '(a)',trim(logmsg)
    call write_scripgrid(trim(fname),(/ib,ie/),(/ib,ie/),cnlons,cnlats,crlons,crlats,cnmask)

    deallocate(cnlons, cnlats, crlons, crlats, cnmask)

  end subroutine write_staggers
  !> Get center and corner grid points for a given stagger location
  !!
  !! @param[in]  iind                 the start/end index in the i-dimension
  !! @param[in]  jind                 the start/end index in the j-dimension
  !! @param[in]  lon, lat             2D lat,lon centers for the stagger
  !! @param[in]  lonvert, latvert     3D lat,lon vertices  for the stagger
  !! @param[out] cnlons, cnlats       1D center lons,lats
  !! @param[out] crlons, crlats       2D corner lons/lats
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine get_staggers(iind, jind, lon, lat, lonvert, latvert, cnlons, cnlats, crlons, crlats)
    integer,          intent(in)  :: iind(:), jind(:)
    real(dbl_kind),   intent(in)  :: lon(:,:), lat(:,:)
    real(dbl_kind),   intent(in)  :: lonvert(:,:,:), latvert(:,:,:)
    real(dbl_kind),   intent(out) :: cnlons(:)
    real(dbl_kind),   intent(out) :: cnlats(:)
    real(dbl_kind),   intent(out) :: crlons(:,:)
    real(dbl_kind),   intent(out) :: crlats(:,:)

    integer :: idim, jdim, n
    integer :: ib, ie, jb, je
    real(dbl_kind), allocatable :: tmp(:,:)

    ib = iind(1); ie = iind(2)
    jb = jind(1); je = jind(2)
    idim = ie - ib + 1
    jdim = je - jb + 1
    allocate(tmp(idim, jdim))

    cnlons = reshape(lon(ib:ie, jb:je), (/idim*jdim/))
    cnlats = reshape(lat(ib:ie, jb:je), (/idim*jdim/))
    do n = 1, nv
       tmp(:,:) = lonvert(ib:ie, jb:je, n)
       crlons(n,:) = reshape(tmp, (/idim*jdim/))
       tmp(:,:) = latvert(ib:ie, jb:je, n)
       crlats(n,:) = reshape(tmp, (/idim*jdim/))
    end do
    deallocate(tmp)
  end subroutine get_staggers
  !> Write a SCRIP grid file
  !!
  !! @param[in]  fname             the file name to write
  !! @param[in]  iind              the start/end index in the i-dimension
  !! @param[in]  jind              the start/end index in the j-dimension
  !! @param[out] cnlons, cnlats    1D center lons,lats
  !! @param[out] crlons, crlats    2D corner lons/lats
  !! @param[in]  imask (optional)  the land mask values
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine write_scripgrid(fname, iind, jind, cnlons, cnlats, crlons, crlats, cnmask)
    character(len=*),  intent(in) :: fname
    integer,           intent(in) :: iind(:), jind(:)
    real(dbl_kind),    intent(in) :: cnlons(:)
    real(dbl_kind),    intent(in) :: cnlats(:)
    real(dbl_kind),    intent(in) :: crlons(:,:)
    real(dbl_kind),    intent(in) :: crlats(:,:)
    integer(int_kind), intent(in) :: cnmask(:)

    integer, parameter :: grid_rank = 2

    integer :: ii, n, id, rc, ncid, dim2(2), dim1(1)
    integer :: idimid, jdimid, kdimid
    integer :: ib, ie, jb, je
    integer :: idim, jdim

    integer, dimension(grid_rank) :: gdims
    character(len=2)  :: vtype
    character(len=CM) :: vname
    character(len=CM) :: vunit

    ib = iind(1); ie = iind(2)
    jb = jind(1); je = jind(2)
    idim = ie - ib + 1
    jdim = je - jb + 1

    gdims(:) = (/idim, jdim/)

    !---------------------------------------------------------------------
    ! create the netcdf file
    !---------------------------------------------------------------------

    ! define the output variables and file name
    call scripvars_typedefine
    ! create the file
    ! 64_bit offset reqd for 008 grid
    ! produces b4b results for smaller grids
    rc = nf90_create(trim(fname), nf90_64bit_offset, ncid)
    !logmsg = '==> writing SCRIP grid to '//trim(fname)
    !print '(a)',trim(logmsg)
    if(rc .ne. 0)print '(a)', 'nf90_create = '//trim(nf90_strerror(rc))

    rc = nf90_def_dim(ncid, 'grid_size', idim*jdim, idimid)
    rc = nf90_def_dim(ncid, 'grid_corners',     nv, jdimid)
    rc = nf90_def_dim(ncid, 'grid_rank', grid_rank, kdimid)

    !grid_dims
    dim1(:) = (/kdimid/)
    rc = nf90_def_var(ncid, 'grid_dims', nf90_int, dim1, id)
    ! mask
    dim1(:) = (/idimid/)
    rc = nf90_def_var(ncid, 'grid_imask', nf90_int, dim1, id)
    rc = nf90_put_att(ncid, id,     'units',      'unitless')

    ! centers
    do ii = 1,2
       vname = trim(scripvars(ii)%var_name)
       vunit = trim(scripvars(ii)%unit_name)
       vtype = trim(scripvars(ii)%var_type)
       dim1(:) =  (/idimid/)
       if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim1, id)
       if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim1, id)
       if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim1, id)
       rc = nf90_put_att(ncid, id,     'units', vunit)
    enddo

    ! corners
    do ii = 3,4
       vname = trim(scripvars(ii)%var_name)
       vunit = trim(scripvars(ii)%unit_name)
       vtype = trim(scripvars(ii)%var_type)
       dim2(:) =  (/jdimid,idimid/)
       if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
       if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim2, id)
       if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim2, id)
       rc = nf90_put_att(ncid, id,     'units', vunit)
    enddo
    rc = nf90_enddef(ncid)

    rc = nf90_inq_varid(ncid,  'grid_dims',        id)
    rc = nf90_put_var(ncid,             id,     gdims)
    rc = nf90_inq_varid(ncid, 'grid_imask',        id)
    rc = nf90_put_var(ncid,             id,    cnmask)

    rc = nf90_inq_varid(ncid,  'grid_center_lon',        id)
    rc = nf90_put_var(ncid,                   id,    cnlons)
    rc = nf90_inq_varid(ncid,  'grid_center_lat',        id)
    rc = nf90_put_var(ncid,                   id,    cnlats)

    rc = nf90_inq_varid(ncid,  'grid_corner_lon',        id)
    rc = nf90_put_var(ncid,                   id,    crlons)
    rc = nf90_inq_varid(ncid,  'grid_corner_lat',        id)
    rc = nf90_put_var(ncid,                   id,    crlats)

    rc = nf90_close(ncid)

  end subroutine write_scripgrid
end module scripgrid
