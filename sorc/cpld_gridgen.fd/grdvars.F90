!> @file
!! @brief Define and allocate required grid variables
!! @author Denise.Worthen@noaa.gov
!!
!> This module contains the grid variables
!! @author Denise.Worthen@noaa.gov

module grdvars

  use gengrid_kinds, only : dbl_kind, real_kind, int_kind

  implicit none

  type :: grid
    real(dbl_kind), allocatable :: lat(:,:)
    real(dbl_kind), allocatable :: lon(:,:)
    real(dbl_kind), allocatable :: latvert(:,:,:)
    real(dbl_kind), allocatable :: lonvert(:,:,:)
    real(dbl_kind), allocatable :: xlat(:)
    real(dbl_kind), allocatable :: xlon(:)
    integer, allocatable        :: iVert(:)
    integer, allocatable        :: jVert(:)
  end type grid
  type(grid) :: Ct, Cu, Cv, Bu

  type :: tstatic
    real(dbl_kind),  allocatable, dimension(:,:) :: areaCt !< The grid areas of the Ct grid cell in m2
    real(dbl_kind),  allocatable, dimension(:,:) :: anglet !< The rotation angle on Ct points (opposite sense from angle)
    real(dbl_kind),  allocatable, dimension(:,:) :: angle  !< The rotation angle on Bu points
    real(dbl_kind),  allocatable, dimension(:,:) :: angchk !< The rotation angle on Ct points, as calculated by CICE
                                                           !! internally using angle on Bu
    real(dbl_kind),  allocatable, dimension(:) :: xangCt   !< The rotation angle on the Ct grid points on the opposite
                                                           !! side of the tripole seam
    real(real_kind), allocatable, dimension(:,:) :: wet4   !< The ocean mask from a MOM6 mask file, stored as real*4 (nd)
    real(dbl_kind),  allocatable, dimension(:,:) :: wet8   !< The ocean mask from a MOM6 mask file, stored as real*8 (nd)
    real(real_kind), allocatable, dimension(:,:) :: dp4    !< The ocean depth from a MOM6 topog file, stored as real*4 (m)
    real(dbl_kind),  allocatable, dimension(:,:) :: dp8    !< The ocean depth from a MOM6 topog file, stored as real*8 (m)
    real(dbl_kind),  allocatable, dimension(:,:) :: ulon   !< The longitude points (on the Bu grid) for CICE6
                                                           !! (radians)
    real(dbl_kind),  allocatable, dimension(:,:) :: ulat   !< The latitude points (on the Bu grid) for CICE6
                                                           !! (radians)
    real(dbl_kind),  allocatable, dimension(:,:) ::  htn   !< The grid cell width in centimeters of the CICE6
                                                           !! grid in the x-direction (i-dimension)
    real(dbl_kind),  allocatable, dimension(:,:) ::  hte   !< The grid cell width in centimeters of the CICE6
                                                           !! grid in the y-direction (j-dimension)
end type tstatic

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
  !integer, dimension(nv) :: iVertCv                                !< The i-offsets of the Cu grid at each Cv(i,j)
                                                                   !! which determine the 4 vertices of each Cv
                                                                   !! grid point in i
  !integer, dimension(nv) :: jVertCv                                !< The j-offsets of the Cu grid at each Cv(i,j)
                                                                   !! which determine the 4 vertices of each Cv
                                                                   !! grid point in j
  !integer, dimension(nv) :: iVertCu                                !< The i-offsets of the Cv grid at each Cu(i,j)
                                                                   !! which determine the 4 vertices of each Cu
                                                                   !! grid point in i
  !integer, dimension(nv) :: jVertCu                                !< The j-offsets of the Cv grid at each Cu(i,j)
                                                                   !! which determine the 4 vertices of each Cu
                                                                   !! grid point in j
  !integer, dimension(nv) :: iVertBu                                !< The i-offsets of the Ct grid at each Bu(i,j)
                                                                   !! which determine the 4 vertices of each Bu
                                                                   !! grid point in i
  !integer, dimension(nv) :: jVertBu                                !< The j-offsets of the Ct grid at each Bu(i,j)
                                                                   !! which determine the 4 vertices of each Bu
                                                                   !! grid point in j
  ! Super-grid source grid variables
  real(dbl_kind), allocatable, dimension(:,:)   :: x               !< The longitudes of the MOM6 supergrid
  real(dbl_kind), allocatable, dimension(:,:)   :: y               !< The latitudes of the MOM6 supergrid
  real(dbl_kind), allocatable, dimension(:,:)   :: dx              !< The grid cell width in meters of the supergrid
                                                                   !! in the x-direction (i-dimension)
  real(dbl_kind), allocatable, dimension(:,:)   :: dy              !< The grid cell width in meters of the supergrid
                                                                   !! in the y-direction (j-dimension)

  ! ! Output grid variables
  ! real(dbl_kind), allocatable, dimension(:,:) :: areaCt            !< The grid areas of the Ct grid cell in m2
  ! real(dbl_kind), allocatable, dimension(:,:) :: anglet            !< The rotation angle on Ct points (opposite sense
  !                                                                  !! from angle)
  ! real(dbl_kind), allocatable, dimension(:,:) :: angle             !< The rotation angle on Bu points
  ! real(dbl_kind), allocatable, dimension(:,:) :: angchk            !< The rotation angle on Ct points, as calculated by
  !                                                                  !! CICE internally using angle on Bu
  ! real(dbl_kind), allocatable, dimension(:) :: xangCt              !< The rotation angle on the Ct grid points on the
  !                                                                  !! opposite side of the tripole seam

  ! ! MOM6 fix fields
  ! real(real_kind), allocatable, dimension(:,:) :: wet4             !< The ocean mask from a MOM6 mask file, stored as
  !                                                                  !! real*4 (nd)
  ! real(dbl_kind),  allocatable, dimension(:,:) :: wet8             !< The ocean mask from a MOM6 mask file, stored as
  !                                                                  !! real*8 (nd)

  ! real(real_kind), allocatable, dimension(:,:) :: dp4              !< The ocean depth from a MOM6 topog file, stored
  !                                                                  !! as real*4 (m)
  ! real(dbl_kind),  allocatable, dimension(:,:) :: dp8              !< The ocean depth from a MOM6 topog file, stored
  !                                                                  !! as real*8 (m)

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

contains
  !> Allocate grid variables
  !!
  !! @author Denise Worthen

  subroutine allocate_all

    allocate( x(0:nx,0:ny),  y(0:nx,0:ny) )
    allocate(  dx(nx,0:ny), dy(0:nx,ny) )

    allocate(Ct%lat(ni,nj), Ct%lon(ni,nj))
    allocate(Cu%lat(ni,nj), Cu%lon(ni,nj))
    allocate(Cv%lat(ni,nj), Cv%lon(ni,nj))
    allocate(Bu%lat(ni,nj), Bu%lon(ni,nj))

    allocate(Ct%iVert(nv), Ct%jVert(nv))
    allocate(Cu%iVert(nv), Cu%jVert(nv))
    allocate(Cv%iVert(nv), Cv%jVert(nv))
    allocate(Bu%iVert(nv), Bu%jVert(nv))

    allocate(tstatic%areaCt(ni,nj), tstatic%anglet(ni,nj), tstatic%angle(ni,nj), tstatic%angchk(ni,nj))

    allocate(Ct%latvert(ni,nj,nv), Ct%lonvert(ni,nj,nv))
    allocate(Cu%latvert(ni,nj,nv), Cu%lonvert(ni,nj,nv))
    allocate(Cv%latvert(ni,nj,nv), Cv%lonvert(ni,nj,nv))
    allocate(Bu%latvert(ni,nj,nv), Bu%lonvert(ni,nj,nv))

    allocate(Ct%xlon(ni), Ct%xlat(ni))
    allocate(Cu%xlon(ni), Cu%xlat(ni))
    allocate(Cv%xlon(ni), Cv%xlat(ni))
    allocate(Bu%xlon(ni), Bu%xlat(ni))
    allocate(tstatic%xangCt(ni))

    allocate(tstatic%wet4(ni,nj))
    allocate(tstatic%wet8(ni,nj))

    allocate(tstatic%dp4(ni,nj))
    allocate(tstatic%dp8(ni,nj))

    allocate( ulon(ni,nj), ulat(ni,nj) )
    allocate(  htn(ni,nj),  hte(ni,nj) )

  end subroutine allocate_all

end module grdvars
