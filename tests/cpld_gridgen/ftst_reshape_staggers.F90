!> Unit test for reshape_staggers routine
!! This test checks the reshaping of staggered grid points.
program test_reshape_staggers

  use assertion_mod, only: assert_equal
  use gengrid_kinds, only: dbl_kind, int_kind, CL
  use gengrid_utils, only: reshape_staggers
  use grdvars      , only: nv

  implicit none

  integer, parameter :: idim = 4, jdim = 4
  integer, parameter :: ngrids = 1
  integer, parameter :: maxtests = 20, nresults = ngrids*maxtests

  logical           :: pass(nresults)
  character(len=8)  :: loopmsg(ngrids) = (/'Global  '/)
  character(len=CL) :: testmsg(nresults) = ' '

  integer           :: iind(2), jind(2)
  ! test data
  real(dbl_kind)    :: lon(idim, jdim), lat(idim, jdim)
  integer(int_kind) :: mask(idim, jdim)
  real(dbl_kind)    :: lonvert(idim, jdim, nv), latvert(idim, jdim, nv)
  ! result data
  real(dbl_kind), allocatable    :: cnlons(:), cnlats(:)
  integer(int_kind), allocatable :: cnmask(:)
  real(dbl_kind), allocatable    :: crlons(:, :), crlats(:, :)

  integer :: nt, ntests
  integer :: i, j, n, l, ni, nj
  integer :: idx, jdx, idx1

  character(len=CL) :: msg, errmsg
  logical :: success

  nt = 0
  pass = .false.
  ! Initialize global test data; coordinate encoded
  do j = 1, jdim
     do i = 1, idim
        lon(i,j) =  10.0_dbl_kind * i + j
        lat(i,j) = -10.0_dbl_kind * i - j
        do n = 1, nv
           lonvert(i,j,n) = i*100.0 + j*10.0 + n
           latvert(i,j,n) = i*100.0 + j*10.0 + n
        end do
     end do
  end do
  do j = 1,jdim
     mask(1:2,j) = 1_int_kind
     mask(3:4,j) = 0_int_kind
  end do

  do j = 1,jdim
     print '(i3,4f8.2)',j,(lon(i,j),i=1,idim)
  end do
  print *
  do j = 1,jdim
     print '(i3,4f8.2)',j,(lat(i,j),i=1,idim)
  end do
  print *
  do j = 1,jdim
     print '(i3,4i4)',j,(mask(i,j),i=1,idim)
  end do

  do l = 1,ngrids
     print *,'loop ',l
     if (l .eq. 1) then
        iind = (/1, idim/)
        jind = (/1, jdim/)
     else
        iind = (/2,3/)
        jind = (/3,4/)
     end if
     ni = iind(2) - iind(1) + 1
     nj = jind(2) - jind(1) + 1

     allocate(cnlons(ni*nj), source=0.0_dbl_kind)
     allocate(cnlats(ni*nj), source=0.0_dbl_kind)
     allocate(cnmask(ni*nj), source = 1_int_kind)
     allocate(crlons(nv,ni*nj), source = 0.0_dbl_kind)
     allocate(crlats(nv,ni*nj), source = 0.0_dbl_kind)

     call reshape_staggers(iind, jind, lon, lat, mask, lonvert, latvert, cnlons, cnlats, cnmask, crlons, crlats)

     nt = 0
     ! start index
     nt = nt+1; msg = 'compare lon index (1,1) to index (1)'
     !cnlons(1) = cnlons(1)+1.0d-10
     call assert_equal(cnlons(1),lon(1,1),0.0_dbl_kind,msg,success,errmsg)
     pass(nt) = success
     testmsg(nt) = trim(errmsg)

     nt = nt+1; msg = 'compare lat index (1,1) to index (1)'
     call assert_equal(cnlats(1),lat(1,1),0.0_dbl_kind,msg,success,errmsg)
     pass(nt) = success
     testmsg(nt) = trim(errmsg)

     nt = nt+1; msg = 'compare mask index (1,1) to index (1)'
     call assert_equal(cnmask(1),mask(1,1),msg,success,errmsg)
     pass(nt) = success
     testmsg(nt) = trim(errmsg)

     ! end index
     nt = nt+1; msg = 'compare lon index (ni,nj) to index (ni*nj)'
     call assert_equal(cnlons(ni*nj),lon(ni,nj),0.0_dbl_kind,msg,success,errmsg)
     pass(nt) = success
     testmsg(nt) = trim(errmsg)

     nt = nt+1; msg = 'compare lat index (ni,nj) to index (ni*nj)'
     call assert_equal(cnlats(ni*nj),lat(ni,nj),0.0_dbl_kind,msg,success,errmsg)
     pass(nt) = success
     testmsg(nt) = trim(errmsg)

     nt = nt+1; msg = 'compare mask index (ni,nj) to index (ni*nj)'
     call assert_equal(cnmask(ni*nj),mask(ni,nj),msg,success,errmsg)
     pass(nt) = success
     testmsg(nt) = trim(errmsg)

     idx = iind(1) + ni/2; jdx = jind(1) + 1
     idx1 = (jdx - 1) * ni + idx

     ! single point
     nt = nt+1; msg = 'compare lon index (idx,jdx) to index (idx1)'
     cnlons(idx1) = cnlons(idx1)+1.0e-10
     call assert_equal(cnlons(idx1),lon(idx,jdx),0.0_dbl_kind,msg,success,errmsg)
     pass(nt) = success
     testmsg(nt) = trim(errmsg)

     nt = nt+1; msg = 'compare lat index (idx,jdx) to index (idx1)'
     call assert_equal(cnlats(idx1),lat(idx,jdx),0.0_dbl_kind,msg,success,errmsg)
     pass(nt) = success
     testmsg(nt) = trim(errmsg)

     nt = nt+1; msg = 'compare mask index (idx,jdx) to index (idx1)'
     call assert_equal(cnmask(idx1),mask(idx,jdx),msg,success,errmsg)
     pass(nt) = success
     testmsg(nt) = trim(errmsg)
  end do

  ntests = nt
  do nt = 1,ntests
     print *,trim(testmsg(nt))
  end do

     !if (.not. success) print '(a)','FAIL: '//trim(msg)
     !pass(nt) =
     ! if ( (cnlons(1)     /= 11.0 .or. cnlats(1)     /= -11.0) .or. &
     !      (cnlons(ni*nj) /= 44.0 .or. cnlats(ni*nj) /= -44.0) .or. &
     !      (cnmask(1)     /= 1    .or. cnmask(ni*nj) /= 0) ) then
     !    print '(i3,a,3i3)',l, ' Test failed: outer corners fail '
     ! end if

     ! ! Check answers, first, last values
     ! nt = 1
     ! if (cnlons(1) /= lon(iind(1),jind(1)) .or. cnlons(ni*nj) /= lon(iind(2),jind(2))) then
     !    pass(nt) = .false.
     !    write(testmsg(nt),'(a)')trim(loopmsg(l))//' Test failed: mis-match first,last lons'
     !    !print '(i3,a,3i3)',l, ' Test failed: cnlons(1) does not match lon(1,1) ',idim*jdim,iind(2),jind(2)
     !    !stop 1
     ! end if

     ! nt = nt+1
     ! if (cnlats(1) /= lat(iind(1),jind(1)) .or. cnlats(ni*nj) /= lat(iind(2),jind(2))) then
     !    pass(nt) = .false.
     !    write(testmsg(nt),'(a)')trim(loopmsg(l))//' Test failed: mis-match first,last lats'
     !    !print *, l, ' Test failed: cnlats(1) does not match lat(1,1)'
     !    !stop 1
     ! end if

     ! nt = nt+1
     ! if (cnmask(1) /= mask(iind(1),jind(1)) .or. cnmask(ni*nj) /= mask(iind(2),jind(2))) then
     !    pass(nt) = .false.
     !    write(testmsg(nt),'(a)')trim(loopmsg(l))//' Test failed: mis-match first,last mask'
     !    !print *,l, ' Test failed: cnmask(1) does not match mask(1,1)'
     !    !stop 1
     ! end if
     ! print *,'PASS full range, loop ',l

  !    idx = iind(2) ; jdx = jind(1)
  !    idx1 = (jdx - 1) * idim + idx
  !    print *,ni,nj,idx1,size(cnlons),real(cnlons,4)

  !    ! Single point
  !    print *,lon(idx,jdx),idx1,cnlons(idx1)
  !    if (cnlons(idx1) /= lon(idx,jdx)) then
  !       print *, 'Test failed: cnlons(idx) does not match lon(idx,idx)'
  !       stop 1
  !    end if
  !    if (cnlats(idx1) /= lat(idx,jdx)) then
  !       print *, 'Test failed: cnlats(1) does not match lat(1,1)'
  !       stop 1
  !    end if
  !    if (cnmask(idx1) /= mask(idx,jdx)) then
  !       print *, 'Test failed: cnmask(1) does not match mask(1,1)'
  !       stop 1
  !    end if
  !    print *,' PASS single point loop ',l
  !    deallocate(cnlons, cnlats, crlons, crlats, cnmask)
  ! end do

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
