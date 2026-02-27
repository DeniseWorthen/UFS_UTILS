!> @file
!! @brief Define and allocate required grid variables
!! @author Denise.Worthen@noaa.gov
!!
!> This module contains the grid variables
!! @author Denise.Worthen@noaa.gov

module grdvars
  ! Derived types for grid locations
  type :: GridLoc2D
    real(dbl_kind), allocatable :: lat(:,:)
    real(dbl_kind), allocatable :: lon(:,:)
    real(dbl_kind), allocatable :: lat_vert(:,:,:)
    real(dbl_kind), allocatable :: lon_vert(:,:,:)
    real(dbl_kind), allocatable :: xlat(:)
    real(dbl_kind), allocatable :: xlon(:)
  end type GridLoc2D

  ! Instances for each grid location
  type(GridLoc2D) :: Ct, Cu, Cv, Bu

  use gengrid_kinds, only : dbl_kind, real_kind, int_kind

  implicit none

  real(kind=dbl_kind), parameter ::      pi = 3.14159265358979323846_dbl_kind  !< the value of PI
  real(kind=dbl_kind), parameter :: deg2rad = pi/180.0_dbl_kind                !< degree to radian conversion
  real(kind=dbl_kind), parameter ::  rearth = 6371.0_dbl_kind                  !< earth radius (km)

  integer :: ni                                                    !< i-dimension of output grid
  integer :: nj                                                    !< j-dimension of output grid
  integer :: npx                                                   !< i or j-dimension of fv3 tile

  integer :: nx                                                    !< i-dimension of MOM6 supergrid
  integer :: ny                                                    !< j-dimension of MOM6 supergrid

  logical :: editmask                                              !< flag indicating whether the MOM6 land mask
                                                                   !! should be edited. Default is false.
  logical :: debug                                                 !< flag indicating whether grid information
                                                                   !! should be printed for debugging purposes
                                                                   !! Default is false.
  logical :: do_postwgts                                           !< flag indicating whether then ESMF weights to
                                                                   !! regrid from the tripole grid to a rectilinear
                                                                   !! grid should be generated. Default is false.
  logical :: do_regional = .false.                                 !< flag indicating whether a regional cutout grid
                                                                   !! should be created. Default is false.

  logical :: roottask                                              !< flag indicating whether this is the roottask

  integer, parameter :: nv = 4.                                    !< the number of vertices for each stagger location
  integer, parameter :: ncoord = 2*4.                              !< the number of coord pairs (lat,lon) for each of
                                                                   !! 4 stagger locations
  integer, parameter :: nverts = 2*4.                              !< the number of coord pairs (lat,lon) for the
                                                                   !! vertices of each stagger location
  integer, parameter ::  nvars = ncoord + nverts                   !< the total number of cooridinate variables


  real(dbl_kind)     :: sg_maxlat                                  !< the maximum latitute present in the supergrid
                                                                   !! file
  integer(int_kind)  :: ipole(2)                                   !< the i-index for both pole locations
                                                                   !! along the top-most row

  integer, parameter, dimension(nv) :: iVertCt = (/0, -1, -1,  0/) !< The i-offsets of the Bu grid at each Ct(i,j)
                                                                   !! which determine the 4 vertices of each Ct grid
                                                                   !! grid point in i
  integer, parameter, dimension(nv) :: jVertCt = (/0,  0, -1, -1/) !< The j-offsets of the Bu grid at each Ct(i,j)
                                                                   !! which determine the 4 vertices of each Ct
                                                                   !! grid point in j
  integer, dimension(nv) :: iVertCv                                !< The i-offsets of the Cu grid at each Cv(i,j)
                                                                   !! which determine the 4 vertices of each Cv
                                                                   !! grid point in i
  integer, dimension(nv) :: jVertCv                                !< The j-offsets of the Cu grid at each Cv(i,j)
                                                                   !! which determine the 4 vertices of each Cv
                                                                   !! grid point in j
  integer, dimension(nv) :: iVertCu                                !< The i-offsets of the Cv grid at each Cu(i,j)
                                                                   !! which determine the 4 vertices of each Cu
                                                                   !! grid point in i
  integer, dimension(nv) :: jVertCu                                !< The j-offsets of the Cv grid at each Cu(i,j)
                                                                   !! which determine the 4 vertices of each Cu
                                                                   !! grid point in j
  integer, dimension(nv) :: iVertBu                                !< The i-offsets of the Ct grid at each Bu(i,j)
                                                                   !! which determine the 4 vertices of each Bu
                                                                   !! grid point in i
  integer, dimension(nv) :: jVertBu                                !< The j-offsets of the Ct grid at each Bu(i,j)
                                                                   !! which determine the 4 vertices of each Bu
                                                                   !! grid point in j
  ! Super-grid source grid variables
  real(dbl_kind), allocatable, dimension(:,:)   :: x               !< The longitudes of the MOM6 supergrid
  real(dbl_kind), allocatable, dimension(:,:)   :: y               !< The latitudes of the MOM6 supergrid
  real(dbl_kind), allocatable, dimension(:,:)   :: dx              !< The grid cell width in meters of the supergrid
                                                                   !! in the x-direction (i-dimension)
  real(dbl_kind), allocatable, dimension(:,:)   :: dy              !< The grid cell width in meters of the supergrid
                                                                   !! in the y-direction (j-dimension)

  ! Output grid variables
  real(dbl_kind), allocatable, dimension(:,:) :: latCt             !< The latitude of the center (tracer) grid points
                                                                   !! on the C-grid
  real(dbl_kind), allocatable, dimension(:,:) :: lonCt             !< The longitude of the center (tracer) grid
                                                                   !! points on the C-grid
  ! See derived types above for grid variables
                                                                   !! opposite side of the tripole seam

  real(dbl_kind), allocatable, dimension(:) :: xlatBu              !< The latitude of the Bu grid points at the
                                                                   !! grid bottom
  real(dbl_kind), allocatable, dimension(:) :: xlonBu              !< The longitude of the Bu grid points at the
                                                                   !! grid bottom
  real(dbl_kind), allocatable, dimension(:) :: xlatCv              !< The latitude of the Cv grid points at the
                                                                   !! grid bottom
  real(dbl_kind), allocatable, dimension(:) :: xlonCv              !< The longitude of the Cv grid points  at the
                                                                   !! grid bottom
  ! MOM6 fix fields
  real(real_kind), allocatable, dimension(:,:) :: wet4             !< The ocean mask from a MOM6 mask file, stored as
                                                                   !! real*4 (nd)
  real(dbl_kind),  allocatable, dimension(:,:) :: wet8             !< The ocean mask from a MOM6 mask file, stored as
                                                                   !! real*8 (nd)

  real(real_kind), allocatable, dimension(:,:) :: dp4              !< The ocean depth from a MOM6 topog file, stored
                                                                   !! as real*4 (m)
  real(dbl_kind),  allocatable, dimension(:,:) :: dp8              !< The ocean depth from a MOM6 topog file, stored
                                                                   !! as real*8 (m)

  ! CICE6 fields
  real(dbl_kind), allocatable, dimension(:,:) :: ulon              !< The longitude points (on the Bu grid) for CICE6
                                                                   !! (radians)
  real(dbl_kind), allocatable, dimension(:,:) :: ulat              !< The latitude points (on the Bu grid) for CICE6
                                                                   !! (radians)
  real(dbl_kind), allocatable, dimension(:,:) ::  htn              !< The grid cell width in centimeters of the CICE6
                                                                   !! grid in the x-direction (i-dimension)
  real(dbl_kind), allocatable, dimension(:,:) ::  hte              !< The grid cell width in centimeters of the CICE6
                                                                   !! grid in the y-direction (j-dimension)

  real(kind=real_kind), parameter :: minimum_depth = 9.5           !< The minimum depth for MOM6
  real(kind=real_kind), parameter :: maximum_depth = 6500.0        !< The maximum depth for MOM6
  real(kind=real_kind), parameter :: masking_depth = 0.0           !< The masking depth for MOM6. Depths shallower than
                                                                   !! minimum_depth but deeper than masking_depth are
                                                                   !! rounded to minimum_depth
  real(kind=real_kind), parameter :: maximum_lat = 88.0            !< The maximum latitude for water points for WW3

  ! ATM resolutions
  integer, parameter :: maxatmres = 10                             !< The maximum number of possible ATM resolutions
  integer, allocatable, dimension(:) :: catm                       !< The ATM resolutions for mapped ocean masks

  ! Regional grid options
  real(dbl_kind) :: regional_lonbeg = 0.0                          !< longitude origin for regional grid (degrees)
  real(dbl_kind) :: regional_latbeg = 0.0                          !< latitude origin for regional grid (degrees)
  real(dbl_kind) :: regional_lon_extent = 0.0                      !< longitude extent for regional grid (degrees)
  real(dbl_kind) :: regional_lat_extent = 0.0                      !< latitude extent for regional grid (degrees)
contains
  !> Allocate grid variables
  !!
  !! @author Denise Worthen

  subroutine allocate_all

    allocate( x(0:nx,0:ny),  y(0:nx,0:ny) )
    allocate(  dx(nx,0:ny), dy(0:nx,ny) )

    allocate( Ct%lat(ni,nj), Ct%lon(ni,nj) )
    allocate( Ct%lat_vert(ni,nj,nv), Ct%lon_vert(ni,nj,nv) )
    allocate( Ct%xlat(ni), Ct%xlon(ni) )
    allocate( Cu%lat(ni,nj), Cu%lon(ni,nj) )
    allocate( Cu%lat_vert(ni,nj,nv), Cu%lon_vert(ni,nj,nv) )
    allocate( Cu%xlat(ni), Cu%xlon(ni) )
    allocate( Cv%lat(ni,nj), Cv%lon(ni,nj) )
    allocate( Cv%lat_vert(ni,nj,nv), Cv%lon_vert(ni,nj,nv) )
    allocate( Cv%xlat(ni), Cv%xlon(ni) )
    allocate( Bu%lat(ni,nj), Bu%lon(ni,nj) )
    allocate( Bu%lat_vert(ni,nj,nv), Bu%lon_vert(ni,nj,nv) )
    allocate( Bu%xlat(ni), Bu%xlon(ni) )
    allocate( xangCt(ni) )

    allocate( wet4(ni,nj) )
    allocate( wet8(ni,nj) )

    allocate(  dp4(ni,nj) )
    allocate(  dp8(ni,nj) )

    allocate( ulon(ni,nj), ulat(ni,nj) )
    allocate(  htn(ni,nj),  hte(ni,nj) )

  end subroutine allocate_all
end module grdvars
