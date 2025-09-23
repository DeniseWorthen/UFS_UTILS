module weights4rhs_mod

  use ESMF

  implicit none

  private
  public :: weights4rhs
  public :: addmask2grid

  character(len=*) , parameter :: u_FILE_u = __FILE__

  ! from ATM to ocn/ice and wav
  integer, parameter :: na2omaps = 4
  character(len=12), dimension(na2omaps) :: a2omaps = (/ &
       'bilnr       ',&
       'consf       ',&
       'consf_uv3d  ',&
       'patch_uv3d  '/)

  integer, parameter :: na2wmaps = 1
  character(len=12), dimension(na2wmaps) :: a2wmaps = (/ &
       'bilnr       '/)

  ! from OCN/ICE to atm and wav
  integer, parameter :: no2amaps = 2
  character(len=12), dimension(no2amaps) :: o2amaps = (/ &
       'consf       ', &
       'consd       '/)

  integer, parameter :: no2wmaps = 1
  character(len=12), dimension(no2wmaps) :: o2wmaps = (/ &
       'bilnr_nstod '/)

  ! from WAV to atm and ocn/ice
  integer, parameter :: nw2amaps = 1
  character(len=12), dimension(nw2amaps) :: w2amaps = (/ &
       'bilnr_nstod '/)

  integer, parameter :: nw2omaps = 1
  character(len=12), dimension(nw2omaps) :: w2omaps = (/ &
       'bilnr_nstod '/)
contains

  subroutine weights4rhs(maintask, fdir, meshatm)

    logical, intent(in) :: maintask
    character(len=*), intent(in) :: fdir
    type(ESMF_Mesh),  intent(inout) :: meshatm

    ! local variables
    type(ESMF_Mesh)    :: meshwav, meshocn
    type(ESMF_PoleMethod_Flag) :: polemethod
    character(len=120) :: bldata, fwavmesh, focnmesh, fwgt
    character(len=120) :: ftag, atmres, wavres, ocnres, maptype
    integer :: n,nn,rc

    ! hard code locations of needed ocnmesh and wavmesh files on ursa
    atmres = 'C96'
    ocnres = 'mx100'
    wavres = 'global_270k'
    bldata = '/scratch4/NAGAPE/epic/role-epic/UFS-WM_RT/NEMSfv3gfs/input-data-20250507/'
    focnmesh = '/scratch3/NCEPDEV/global/role.glopara/fix/cice/20240416/100/mesh.'//trim(ocnres)//'.nc'
    fwavmesh = trim(bldata)//'WW3_input_data_20250807/mesh.'//trim(wavres)//'.nc'

    ! ocn/ice
    if (maintask) print '(a)', 'creating mesh from '//trim(focnmesh)
    meshocn = ESMF_MeshCreate(filename=trim(focnmesh), fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! wave
    if (maintask) print '(a)', 'creating mesh from '//trim(fwavmesh)
    meshwav = ESMF_MeshCreate(filename=trim(fwavmesh), fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! src:dst

    !a->o
    ftag = trim(atmres)//'.to.'//trim(ocnres)
    do nn = 1,na2omaps
       maptype = trim(a2omaps(nn))
       fwgt = trim(fdir)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
       if (maintask) print '(a)','XXX '//trim(fwgt)
       !call create_weights(meshatm, meshocn, masksrc=ispval, maskdst=0, method, polemethod=ESMF_POLEMETHOD_ALLAVG, fwgt)
    end do

    !a->w
    ftag = trim(atmres)//'.to.'//trim(wavres)
    do nn = 1,na2wmaps
       maptype = trim(a2wmaps(nn))
       fwgt = trim(fdir)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
       if (maintask) print '(a)','XXX '//trim(fwgt)
       !call create_weights(meshat, meshwav, masksrc=ispval, maskdst=0, method, polemethod=ESMF_POLEMETHOD_NONE, fwgt)
    end do

    !o->a
    ftag = trim(ocnres)//'.to.'//trim(atmres)
    do nn = 1,no2amaps
       maptype = trim(o2amaps(nn))
       fwgt = trim(fdir)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
       if (maintask) print '(a)','XXX '//trim(fwgt)
       !call create_weights(meshocn, meshatm, masksrc=0, maskdst=1, method, polemethod=ESMF_POLEMETHOD_ALLAVG, fwgt)
    end do

    !o->w
    ftag = trim(ocnres)//'.to.'//trim(wavres)
    do nn = 1,no2wmaps
       maptype = trim(o2wmaps(nn))
       fwgt = trim(fdir)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
       if (maintask) print '(a)','XXX '//trim(fwgt)
       !call create_weights(meshocn, meshwav, masksrc=0, maskdst=0, method, polemethod=ESMF_POLEMETHOD_NONE, fwgt)
    end do

    !w->a
    ftag = trim(wavres)//'.to.'//trim(atmres)
    do nn = 1,nw2amaps
       maptype = trim(w2amaps(nn))
       fwgt = trim(fdir)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
       if (maintask) print '(a)','XXX '//trim(fwgt)
       !call create_weights(meshwav, meshatm, masksrc=0, maskdst=1, method, polemethod=ESMF_POLEMETHOD_NONE, fwgt)
    end do

    !w->o
    ftag = trim(wavres)//'.to.'//trim(ocnres)
    do nn = 1,no2wmaps
       maptype = trim(w2omaps(nn))
       fwgt = trim(fdir)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
       if (maintask) print '(a)','XXX '//trim(fwgt)
       !call create_weights(meshwav, meshocn, masksrc=0, maskdst=0, method, polemethod=ESMF_POLEMETHOD_NONE, fwgt)
    end do

  end subroutine weights4rhs

  ! subroutine create_weights(mshsrc, mshdst, masksrc, maskdst, method, polemethod, fwgt)

  !   type(ESMF_MESH), intent(inout) :: meshsrc, mesdst
  !   type(ESMF_PoleMethod_Flag), intent(inout) :: polemethod
  !   integer, intent(in) :: masksrc,maskdst

  !   !local variables
  !   type(ESMF_Field) :: fldsrc, fldst

  !   fldsrc = ESMF_FieldCreate(mshsrc, ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
  !   if (ChkErr(rc,__LINE__,u_FILE_u)) return
  !   flddst = ESMF_FieldCreate(mshdst, ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
  !   if (ChkErr(rc,__LINE__,u_FILE_u)) return


    subroutine addmask2grid(fname, fldname, atmgrid)

    character(len=*), intent(in)    :: fname
    character(len=*), intent(in)    :: fldname
    type(ESMF_Grid),  intent(inout) :: atmgrid

    ! local variable
    type(ESMF_Field)                    :: gridfld
    type(ESMF_Field)                    :: maskfld
    real(kind=ESMF_KIND_R8), pointer    :: ptr2dr8(:,:)
    integer(kind=ESMF_KIND_I4), pointer :: maskptr(:,:)
    type(ESMF_ArraySpec)                :: arraySpec
    integer                             :: i,j,rc
    ! from ocean_merge
    real(kind=ESMF_KIND_R8)             :: tmpland
    real(kind=ESMF_KIND_R8), parameter  :: min_land = 1.0e-4

    !---------------------------------------------------------------------
    ! obtain land_frac from tile file to create the mask
    !---------------------------------------------------------------------

    call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    gridfld = ESMF_FieldCreate(atmgrid, arraySpec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(gridfld, farrayPtr=ptr2dr8, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldRead(gridfld, filename=trim(fname), variableName=trim(fldname), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    maskfld = ESMF_FieldCreate(atmgrid, arraySpec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(maskfld, farrayPtr=maskptr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------------------------------------
    ! add the mask to the grid
    !---------------------------------------------------------------------

    ! add the mask to the grid
    call ESMF_GridAddItem(atmgrid, itemflag=ESMF_GRIDITEM_MASK, itemTypeKind=ESMF_TYPEKIND_I4, &
         staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_GridGetItem(atmgrid, itemflag=ESMF_GRIDITEM_MASK, staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=maskPtr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! replicate ocean_merge; the land_frac variable from the file is the mapped ocean fraction
    maskptr = 0
    do j = lbound(maskptr,2),ubound(maskptr,2)
       do i = lbound(maskptr,1),ubound(maskptr,1)
          tmpland = 1.0 - ptr2dr8(i,j)
          if (tmpland <       min_land) maskptr(i,j) = 0
          if (tmpland > 1.0 - min_land) maskptr(i,j) = 1
       end do
    end do
  end subroutine addmask2grid

  !subroutine checkmesh(mesh)
  !  type(ESMF_Mesh),  intent(inout) :: mesh
  !end subroutine checkmesh

  logical function ChkErr(rc, line, file)

    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    integer :: lrc

    ChkErr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
       ChkErr = .true.
    endif
  end function ChkErr

end module weights4rhs_mod
