module regional

  use grdvars       , only: pi, deg2rad, rearth
  use gengrid_kinds , only: dbl_kind, CL
  use gengrid_utils , only: calc_dist
  use charstrings   , only: dirsrc, dirout, maskfile, topofile

  implicit none

contains
  subroutine extract_regional_grid(x,y,maxlat,rgrid,ib,ie,jb,je)

    real(dbl_kind),           intent(in)  :: x(:,:)
    real(dbl_kind),           intent(in)  :: y(:,:)
    real(dbl_kind),           intent(in)  :: maxlat
    type(regional_grid_type), intent(in)  :: rgrid
    integer,                  intent(out) :: ib,ie,jb,je

    integer :: i,j,ifold
    integer :: iounit
    integer :: ll(2), lr(2), ul(2), ur(2)
    integer, dimension(4) :: iloc,jloc
    real(dbl_kind) :: mindist(4), dist
    character(len=CL) :: ncks_cmd, index_string

    mindist(:) = huge(1.0_dbl_kind)
    iloc = -1; jloc = -1

    ll = findindex(y,x,rgrid%latbeg, rgrid%lonbeg)
    lr = findindex(y,x,rgrid%latbeg, rgrid%lonbeg + rgrid%lon_extent)

    ! do j = 1, size(x,2)
    !     do i = 1, size(x,1)
    !        dist = calc_dist(y(i,j), x(i,j), rgrid%latbeg, rgrid%lonbeg)
    !        if (dist < mindist(1)) then
    !           mindist(1) = dist
    !           iloc(1) = i; jloc(1) = j
    !        end if

    !        dist = calc_dist(y(i,j), x(i,j), rgrid%latbeg, rgrid%lonbeg + rgrid%lon_extent)
    !        if (dist < mindist(2)) then
    !           mindist(2) = dist
    !           iloc(2) = i; jloc(2) = j
    !        end if
    !     end do
    !  end do

     !iloc(3) = iloc(2)
     !iloc(4) = iloc(1)
     ifold = findloc(y(1:size(y,1)/2, size(y,2)), maxlat, dim=1)
     print *,'XXX sg i pole ',ifold

     !ul = findcorner(y(ifold,ll(1):), x(ifold, j), (rgrid%latbeg + rgrid%lat_extent), x(ifold, jloc(1)))
     if (ifold > 0) then
        allocate(y1(:),source=y(ifold,:))
        allocate(x1(:),source=x(ifold,:))

        jb = ll(2)
        je = findindex(y1,x1,rgrid%latbeg + rgrid%lat_extent,x1(jb))
        ul = (/ll(1),je/)
        ur = (/lr(1),je/)
     else
        !print *, "Critical Error: Global fold index (ifold) not found."
     end if

     ! if (ifold > 0) then
     !    do j = jloc(1), size(y,2)

     !       dist = calc_dist(y(ifold, j), x(ifold, j), (rgrid%latbeg + rgrid%lat_extent), x(ifold, jloc(1)))

     !       if (dist < mindist(3)) then
     !          mindist(3) = dist
     !          jloc(3) = j
     !          jloc(4) = j
     !       end if
     !    end do
     ! else
     !    !print *, "Critical Error: Global fold index (ifold) not found."
     ! end if

     ! iloc,jloc are corners; want this to give the LL corner of the sub-domain
     iloc = iloc + 1
     jloc = jloc + 1
     print *,'XXX ',iloc
     print *,'XXX ',jloc
     do i = 1,4
        print *,'XXX sg ',i,x(iloc(i),jloc(i)), y(iloc(i),jloc(i))
     end do

     ! ncks command script for generating regional grid domain
     open(newunit=iounit,file='./create_regional_grid.sh')

     ! index range for regional super grid
     ib = iloc(1); jb = jloc(1)
     ie = iloc(2); je = jloc(4)

     write(index_string,'(4(a,i0,a,i0))') &
         ' -d  nx,',ib,',',ie,    &
         ' -d  ny,',jb,',',je,    &
         ' -d nxp,',ib,',',ib+1,  &
         ' -d nyp,',jb,',',jb+1
     print '(a)','XXX '//trim(index_string)
     write(iounit,'(a)')'ncks -O -F '//trim(index_string)//'  '//trim(dirsrc)//'ocean_hgrid.nc ocean_hgrid_regional.nc'

     ! index range for regional grid on reduced global grid
     ib = iloc(1)/2; jb = jloc(1)/2
     ie = iloc(2)/2; je = jloc(4)/2

     write(index_string,'(2(a,i0,a,i0))') &
          ' -d  nx,',ib,',',ie, ' -d  ny,',jb,',',je
     print '(a)','XXX '//trim(index_string)
     write(iounit,'(a)')'ncks -O -F '//trim(index_string)//'  '//trim(dirsrc)//trim(topofile)//' ocean_topog_regional.nc'
     write(iounit,'(a)')'ncks -O -F '//trim(index_string)//'  '//trim(dirsrc)//trim(maskfile)//' ocean_mask_regional.nc'
     close(iounit)

   end subroutine extract_regional_grid
 end module regional
