!> Unit test for reshape_staggers routine
!! This test checks the reshaping of staggered grid points.
program test_reshape_staggers
  use gengrid_kinds, only: dbl_kind, int_kind
  use grdvars, only: nv
  use scripgrid
  implicit none

  integer, parameter :: idim = 4, jdim = 5

  integer           :: iind(2), jind(2)
  ! test data
  real(dbl_kind)    :: lon(idim, jdim), lat(idim, jdim)
  integer(int_kind) :: mask(idim, jdim)
  real(dbl_kind)    :: lonvert(idim, jdim, nv), latvert(idim, jdim, nv)
  ! result data
  real(dbl_kind)    :: cnlons(idim*jdim), cnlats(idim*jdim)
  integer(int_kind) :: cnmask(idim*jdim)
  real(dbl_kind)    :: crlons(nv, idim*jdim), crlats(nv, idim*jdim)

  integer :: i, j, n, l
  integer :: idx, jdx, idx1

  ! Initialize test data
  do i = 1, idim
    do j = 1, jdim
      lon(i,j) = 10.0_dbl_kind * i + j
      lat(i,j) = 20.0_dbl_kind * i + j
      mask(i,j) = 1_int_kind
      do n = 1, nv
        lonvert(i,j,n) = lon(i,j) + n
        latvert(i,j,n) = lat(i,j) + n
      end do
    end do
  end do

  do l = 1,2
     if (l .eq. 1) then
        ! Test full range
        iind = (/1, idim/)
        jind = (/1, jdim/)
     else
        iind = (/2,3/)
        jind = (/3,5/)
     end if
     call reshape_staggers(iind, jind, lon, lat, mask, lonvert, latvert, cnlons, cnlats, cnmask, crlons, crlats)

     ! Simple checks for full range
     if (cnlons(1) /= lon(iind(1),jind(1)) .or. cnlons(idim*jdim) /= lon(iind(2),jind(2))) then
        print *, 'Test failed: cnlons(1) does not match lon(1,1) , loop = ',l
        stop 1
     end if
     if (cnlats(1) /= lat(iind(1),jind(1)) .or. cnlats(idim*jdim) /= lat(iind(2),jind(2))) then
        print *, 'Test failed: cnlats(1) does not match lat(1,1)'
        stop 1
     end if
     if (cnmask(1) /= mask(iind(1),jind(1)) .or. cnmask(idim*jdim) /= mask(iind(2),jind(2))) then
        print *, 'Test failed: cnmask(1) does not match mask(1,1)'
        stop 1
     end if

     idx = 3; jdx = 2
     idx1 = (jdx - 1) * idim + idx

     ! Single point
     if (cnlons(idx1) /= lon(idx,jdx)) then
        print *, 'Test failed: cnlons(1) does not match lon(1,1)'
        stop 1
     end if
     if (cnlats(idx1) /= lat(idx,jdx)) then
        print *, 'Test failed: cnlats(1) does not match lat(1,1)'
        stop 1
     end if
     if (cnmask(idx1) /= mask(idx,jdx)) then
        print *, 'Test failed: cnmask(1) does not match mask(1,1)'
        stop 1
     end if
  end do

!   !target_idx = (imid - ib + 1) + (jmid - jb) * (ie - ib + 1)

! ! Ensure the entire vertical profile at this lat/lon moved correctly
! !if (any(x2(:, target_idx) /= x(imid, jmid, :))) then
! ! error stop "Reshape Error: Vertical column scrambled"
! !end if

!   ! Test single row (iind = (/2,2/))
!   iind = (/2,2/)
!   jind = (/1, jdim/)
!   call reshape_staggers(iind, jind, lon, lat, mask, lonvert, latvert, cnlons, cnlats, cnmask, crlons, crlats)

!   ! Check for single row
!   if (cnlons(1) /= lon(2,1)) then
!     print *, 'Test failed: cnlons(1) does not match lon(2,1) for iind=2'
!     stop 2
!   end if
!   if (cnlats(1) /= lat(2,1)) then
!     print *, 'Test failed: cnlats(1) does not match lat(2,1) for iind=2'
!     stop 2
!   end if
!   if (cnmask(1) /= mask(2,1)) then
!     print *, 'Test failed: cnmask(1) does not match mask(2,1) for iind=2'
!     stop 2
!   end if

  print *, 'reshape_staggers unit tests passed.'
end program test_reshape_staggers
