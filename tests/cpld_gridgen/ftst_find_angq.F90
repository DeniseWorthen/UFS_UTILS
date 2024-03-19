! Unit test for cpld_gridgen routine "find_angq".
!
! Reads the anglet on last row of MOM6
! rotation angle on corner points
!
! Author Denise Worthen 2/08/2022

  program ftst_find_angq

    use gengrid_kinds, only : dbl_kind, int_kind
    use angles       , only : find_angq, find_ang, find_angchk

    implicit none

    integer, parameter :: ni = 5, nj = 4, npoles=2
    real(kind=8), dimension(ni,nj,2) :: anglet, angle, angchk
    real(kind=8), dimension(ni,2)    :: xangCt

    integer :: i,j,i1, i2

    integer :: ipole(npoles)

    real(dbl_kind) :: l_lonct(ni,nj) = (/                                        &
         -252.74561,    -239.00064,    -220.46665,    -199.53335,    -180.99936, &
         -262.76769,    -248.28139,    -224.73626,    -195.26374,    -171.71861, &
         -275.86250,    -263.23478,    -234.03925,    -185.96075,    -156.76522, &
         -291.65329,    -286.25383,    -263.72015,    -156.27985,    -133.74617/)

    real(dbl_kind) :: l_lonbu(ni,nj) = (/                                        &
         -251.01234,    -233.49458,    -210.00000,    -186.50542,    -168.98766, &
         -262.99894,    -243.55611,    -210.00000,    -176.44389,    -157.00106, &
         -279.68531,    -263.47519,    -210.00000,    -156.52481,    -140.31469, &
         -300.00000,    -300.00000,    -210.00000,    -120.00000,    -120.00000/)

    real(dbl_kind) :: l_latbu(ni,nj) = (/                                        &
         89.16921,      89.31628,      89.37292,      89.31628,      89.16921,   &
         89.31750,      89.50699,      89.58912,      89.50699,      89.31750,   &
         89.41886,      89.66093,      89.79818,      89.66093,      89.41886,   &
         89.45503,      89.72754,      90.00000,      89.72754,      89.45503/)

    real(dbl_kind) :: l_anglet(ni,nj) = (/                                       &
         -0.73967,      -0.49452,      -0.17430,       0.17430,       0.49452,   &
         -0.91660,      -0.65200,      -0.23454,       0.23454,       0.65200,   &
         -1.14936,      -0.91921,      -0.33304,       0.33304,       0.91921,   &
         -1.42566,      -1.33669,      -0.44988,       0.44988,       1.33669/)

    real(dbl_kind) :: l_angle(ni,nj) = (/                                        &
         0.70064,       0.38859,       0.00000,      -0.38859,      -0.70064,    &
         0.90943,       0.53371,       0.00000,      -0.53371,      -0.90943,    &
         1.20822,       0.75580,       0.00000,      -0.75580,      -1.20822,    &
         1.38117,       0.89328,       0.00000,      -0.89328,      -1.38117/)

    real(dbl_kind) :: l_angchk(ni,nj) = (/                                       &
         0.73204,       0.48692,       0.17198,      -0.17198,      -0.48692,    &
         0.90492,       0.63285,       0.23027,      -0.23027,      -0.63285,    &
         1.13597,       0.85126,       0.32134,      -0.32134,      -0.85126,    &
         1.34026,       1.05945,       0.41175,      -0.41175,      -1.05945/)


    real(dbl_kind) :: r_lonct(ni,nj) = (/                                        &
         -72.74561,     -59.00064,     -40.46665,     -19.53335,      -0.99936,  &
         -82.76769,     -68.28139,     -44.73626,     -15.26374,       8.28139,  &
         -95.86250,     -83.23478,     -54.03925,      -5.96075,      23.23478,  &
        -111.65329,    -106.25383,     -83.72015,      23.72015,      46.25383/)

    real(dbl_kind) :: r_lonbu(ni,nj) = (/                                        &
         -71.01234,     -53.49458,     -30.00000,      -6.50542,      11.01234,  &
         -82.99894,     -63.55611,     -30.00000,       3.55611,      22.99894,  &
         -99.68531,     -83.47519,     -30.00000,      23.47519,      39.68531,  &
        -120.00000,    -120.00000,     -30.00000,      60.00000,      60.00000/)

    real(dbl_kind) :: r_latbu(ni,nj) = (/                                        &
         89.16921,      89.31628,      89.37292,      89.31628,      89.16921,   &
         89.31750,      89.50699,      89.58912,      89.50699,      89.31750,   &
         89.41886,      89.66093,      89.79818,      89.66093,      89.41886,   &
         89.45503,      89.72754,      90.00000,      89.72754,      89.45503/)

    real(dbl_kind) :: r_anglet(ni,nj) = (/                                       &
         -0.73967,      -0.49452,      -0.17430,       0.17430,       0.49452,   &
         -0.91660,      -0.65200,      -0.23454,       0.23454,       0.65200,   &
         -1.14936,      -0.91921,      -0.33304,       0.33304,       0.91921,   &
         -1.42566,      -1.33669,      -0.44988,       0.44988,       1.33669/)

    real(dbl_kind) :: r_angle(ni,nj) = (/                                        &
         0.70064,       0.38859,       0.00000,      -0.38859,      -0.70064,    &
         0.90943,       0.53371,       0.00000,      -0.53371,      -0.90943,    &
         1.20822,       0.75580,       0.00000,      -0.75580,      -1.20822,    &
         1.38117,       0.89328,       0.00000,      -0.89328,      -1.38117/)

    real(dbl_kind) :: r_angchk(ni,nj) = (/                                       &
         0.73204,       0.48692,       0.17198,      -0.17198,      -0.48692,    &
         0.90492,       0.63285,       0.23027,      -0.23027,      -0.63285,    &
         1.13597,       0.85126,       0.32134,      -0.32134,      -0.85126,    &
         1.34026,       1.05945,       0.41175,      -0.41175,      -1.05945/)

  print *,"Starting test of cpld_gridgen routine find_angq"
  anglet = 0.0
  angle  = 0.0
  angchk = 0.0

  j = nj
  do i = 1,ni
     if(l_latBu(i,j) .eq. 90.00)ipole(1) = i
     if(r_latBu(i,j) .eq. 90.00)ipole(2) = i
     print *,i,l_latbu(i,j),r_latbu(i,j)
  enddo
  print *,ipole(:)

  call find_ang(ni,nj,l_lonBu,l_latBu,l_lonCt,anglet(:,:,1))
  call find_ang(ni,nj,r_lonBu,r_latBu,r_lonCt,anglet(:,:,2))

  print *,'anglet left'
  do j = 1,nj
     print '(5f15.6)',(anglet(i,j,1), i = 1,ni)
  end do
  print *,'anglet right'
  do j = 1,nj
     print '(5f15.6)',(anglet(i,j,2), i = 1,ni)
  end do

  xangCt = 0.0
  do i = 1,ni
     i2 = ipole(2)+(ipole(1)-i)+1
     !print *,i,i2,anglet(i2,nj,1)
     xangCt(i,1) = -anglet(i2,nj,1)       ! angle changes sign across seam
     xangCt(i,2) = -anglet(i2,nj,2)       ! angle changes sign across seam
  end do
  print *
  !do i = 1,ni
  !   print *,i,i2
  !   i2 = ni/2+(ipole(2)-i)+1
  !   xangCt(i,2) = -anglet(i2,nj,2)       ! angle changes sign across seam
  !end do
  do i = 1,ni
  !   print *,i,xangCt(i,1),xangCt(i,2)
  !   print *,i,anglet(i,nj,1),xangCt(i,1)
  end do

  call find_angq(ni,nj,xangCt(:,1),anglet(:,:,1),angle(:,:,1))
  call find_angq(ni,nj,xangCt(:,2),anglet(:,:,2),angle(:,:,2))
  angle(:,:,:) = -angle(:,:,:)

  print *,'angle left'
  do j = 1,nj
     print '(5f15.6)',(angle(i,j,1), i = 1,ni)
  end do
  print *,'angle right'
  do j = 1,nj
     print '(5f15.6)',(angle(i,j,2), i = 1,ni)
  end do

  call find_angchk(ni,nj,angle(:,:,1),angchk(:,:,1))
  call find_angchk(ni,nj,angle(:,:,2),angchk(:,:,2))

  print *,'angchk left'
  do j = 1,nj
     print '(5f15.6)',(angchk(i,j,1), i = 1,ni)
  end do
  print *,'angchk right'
  do j = 1,nj
     print '(5f15.6)',(angchk(i,j,2), i = 1,ni)
  end do
  end program ftst_find_angq
