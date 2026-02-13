module regional

  !maybe don't pass fname..can create from charstrings values
contains
  subroutine extract_regional_grid(x,y,fname)

    real(dbl_kind), intent(in) :: x(:,:)
    real(dbl_kind), intent(in) :: y(:,:)
    character(len=*), intent(in) :: fname

    character(len=CL) :: ncks_cmd, index_string
    integer :: ifold
    integer :: iounit
    integer, dimension(4) :: iloc,jloc

    mindist(:) = huge(1.0_dbl_kind)
    iloc = -1; jloc = -1

    do j = 1, size(x,2)
        do i = 1, size(x,1)
           val = calc_dist(y(i,j), x(i,j), regional_latbeg, regional_lonbeg)
           if (val < mindist(1)) then
              mindist(1) = val
              iloc(1) = i; jloc(1) = j
           end if

           val = calc_dist(y(i,j), x(i,j), regional_latbeg, regional_lonbeg + regional_lon_extent)
           if (val < mindist(2)) then
              mindist(2) = val
              iloc(2) = i; jloc(2) = j
           end if
        end do
     end do

     iloc(3) = iloc(2)
     iloc(4) = iloc(1)
     ifold = findloc(y(1:size(y,1)/2, ny), sg_maxlat, dim=1)
     print *,'XXX sg i pole ',ifold

     if (ifold > 0) then
        do j = jloc(1), size(y,2)
           val = calc_dist(y(ifold, j), x(ifold, j), (regional_latbeg + regional_lat_extent), x(ifold, jloc(1)))

           if (val < mindist(3)) then
              mindist(3) = val
              jloc(3) = j
              jloc(4) = j
           end if
        end do
     else
        !print *, "Critical Error: Global fold index (ifold) not found."
     end if

     ! iloc,jloc are corners; want this to give the LL corner of the sub-domain
     iloc = iloc + 1
     jloc = jloc + 1
     print *,'XXX ',iloc
     print *,'XXX ',jloc
     do i = 1,4
        print *,'XXX sg ',i,x(iloc(i),jloc(i)), y(iloc(i),jloc(i))
     end do
     open(newunit=iocmd,file='./create_regional_grid.sh')

     write(index_string,'(4(a,i0,a,i0))') &
         ' -d  nx,',iloc(1),',',iloc(2),    &
         ' -d  ny,',jloc(1),',',jloc(4),    &
         ' -d nxp,',iloc(1),',',iloc(2)+1,  &
         ' -d nyp,',jloc(1),',',jloc(4)+1
     print '(a)','XXX '//trim(index_string)
     write(iocmd,'(a)')'ncks -O -F '//trim(index_string)//'  '//trim(dirsrc)//'/'//'ocean_hgrid.nc ocean_hgrid_regional.nc'

     write(index_string,'(2(a,i0,a,i0))') &
          ' -d  nx,',iloc(1)/2,',',iloc(2)/2,   &
          ' -d  ny,',jloc(1)/2,',',jloc(4)/2
     write(iocmd,'(a)')'ncks -O -F '//trim(index_string)//'  '//trim(dirsrc)//'/'//'ocean_topog.nc ocean_topog_regional.nc'
     write(iocmd,'(a)')'ncks -O -F '//trim(index_string)//'  '//trim(dirsrc)//'/'//'ocean_mask.nc ocean_mask_regional.nc'
     close(iocmd)

     call write_scripgrid(iloc(1)/2,iloc(2)/2,jloc(1)/2,jloc(4)/2,trim(fdst),trim(cstagger),imask=int(wet4))

   end subroutine extract_regional_grid
 end module regional
