! Unit test for cpld_gridgen routine "find_angq".
!
! Reads the anglet on last row of MOM6
! rotation angle on corner points
!
! Author Denise Worthen 2/08/2022

  program ftst_find_angq

  use grdvars,       only : ni,nj
  use grdvars,       only : angle, anglet
  use angles,        only : find_angq

  implicit none

  integer, parameter :: nidat = 72
  integer :: i,j,i1, i2

  logical :: mastertask = .false.
  logical :: debug = .false.

  real(kind=8) :: latBu(nidat) = (/ &
       67.0337, 68.9241, 70.6914, 72.3525, 73.9220, &
       75.4121, 76.8336, 78.1957, 79.5066, 80.7736, &
       82.0031, 83.2009, 84.3725, 85.5228, 86.6564, &
       87.7777, 88.8909, 90.0000, 88.8909, 87.7777, &
       86.6564, 85.5228, 84.3725, 83.2009, 82.0031, &
       80.7736, 79.5066, 78.1957, 76.8336, 75.4121, &
       73.9220, 72.3525, 70.6914, 68.9241, 67.0337, &
       65.0000, 67.0337, 68.9241, 70.6914, 72.3525, &
       73.9220, 75.4121, 76.8336, 78.1957, 79.5066, &
       80.7736, 82.0031, 83.2009, 84.3725, 85.5228, &
       86.6564, 87.7777, 88.8909, 90.0000, 88.8909, &
       87.7777, 86.6564, 85.5228, 84.3725, 83.2009, &
       82.0031, 80.7736, 79.5066, 78.1957, 76.8336, &
       75.4121, 73.9220, 72.3525, 70.6914, 68.9241, &
       67.0337, 65.0000/)

  real(kind=8) :: anglet(nidat) = (/ &
       -1.3830, -1.3768, -1.3731, -1.3684, -1.3622, &
       -1.3542, -1.3440, -1.3312, -1.3151, -1.2951, &
       -1.2698, -1.2377, -1.1966, -1.1435, -1.0742, &
       -0.9844, -0.8712,  0.0705,  0.7371,  0.8712, &
        0.9844,  1.0742,  1.1435,  1.1966,  1.2377, &
        1.2698,  1.2951,  1.3151,  1.3312,  1.3440, &
        1.3542,  1.3622,  1.3684,  1.3731,  1.3768, &
        1.3830, -1.3830, -1.3768, -1.3731, -1.3684, &
       -1.3622, -1.3542, -1.3440, -1.3312, -1.3151, &
       -1.2951, -1.2698, -1.2377, -1.1966, -1.1435, &
       -1.0742, -0.9844, -0.8712, -0.7371, -0.0705, &
        0.8712,  0.9844,  1.0742,  1.1435,  1.1966, &
        1.2377,  1.2698,  1.2951,  1.3151,  1.3312, &
        1.3440,  1.3542,  1.3622,  1.3684,  1.3731, &
        1.3768,  1.3830/)

  ! pole locations on SG
  integer :: ipolesg(2)
  ! unit test values
  real(kind=8) :: puny = 1.0e-12
  real(kind=8) :: delta(15)
  real(kind=8) :: sumdelta

  print *,"Starting test of cpld_gridgen routine find_angq"

  ! 1deg MOM6 dimensions
  ni = 360
  nj = 320

  !open the supergrid file and read the x,y coords
  rc = nf90_open('./data/ocean_hgrid.nc', nf90_nowrite, ncid)
  rc = nf90_inq_varid(ncid, 'x', id)  !lon
  rc = nf90_get_var(ncid,    id,  x)

  rc = nf90_inq_varid(ncid, 'y', id)  !lat
  rc = nf90_get_var(ncid,    id,  y)
  rc = nf90_close(ncid)

  ! max lat on supergrid
  sg_maxlat = maxval(y)

  !pole index on supergrid
  ipolesg = -1
      j = ny
  do i = 1,nx/2
   if(y(i,j) .eq. sg_maxlat)ipolesg(1) = i
  enddo
  do i = nx/2+1,nx
   if(y(i,j) .eq. sg_maxlat)ipolesg(2) = i
  enddo

  ! test angleq calculation
  call find_angq

  ! required for checking longitudes across seam
  where(xsgp1 .lt. 0.0)xsgp1 = xsgp1 + 360.0

  j = ny+1
  i1 = ipolesg(1); i2 = ipolesg(2)-(ipolesg(1)-i1)
  delta = 0.0
  ! check lons match across seam
  delta( 1) = xsgp1(i1-2,j)-xsgp1(i2+2,j)
  delta( 2) = xsgp1(i1-1,j)-xsgp1(i2+1,j)
  delta( 3) = xsgp1(i1,  j)-xsgp1(i2,  j)
  delta( 4) = xsgp1(i1+1,j)-xsgp1(i2-1,j)
  delta( 5) = xsgp1(i1+2,j)-xsgp1(i2-2,j)
  ! check lats match across seam
  delta( 6) = ysgp1(i1-2,j)-ysgp1(i2+2,j)
  delta( 7) = ysgp1(i1-1,j)-ysgp1(i2+1,j)
  delta( 8) = ysgp1(i1,  j)-ysgp1(i2,  j)
  delta( 9) = ysgp1(i1+1,j)-ysgp1(i2-1,j)
  delta(10) = ysgp1(i1+2,j)-ysgp1(i2-2,j)
  ! check angq match across seam
  j = ny
  delta(11)=angq(i1-2,j)-angq(i2-2,j)
  delta(12)=angq(i1-1,j)-angq(i2-1,j)
  delta(13)=angq(i1,  j)-angq(i2,  j)
  delta(14)=angq(i1+1,j)-angq(i2+1,j)
  delta(15)=angq(i1+2,j)-angq(i2+2,j)

  sumdelta = 0.0
  sumdelta = sum(delta)

  if (sumdelta >= puny) then
    print *,'OK'
    print *,'SUCCESS!'
    deallocate(x,y,xsgp1,ysgp1,angq)
  else
    print *,'ftst_find_angq failed'
    stop 1
  endif

  end program ftst_find_angq
