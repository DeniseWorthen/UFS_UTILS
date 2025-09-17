program genweights

  use ESMF
  use mpi_f08

  implicit none
  type(MPI_Comm) :: mpic  ! mpi_f08
  type(ESMF_VM) :: vm

  integer, parameter :: ispval    = -987987                ! spval for RH mask values

  integer :: int_mpic
  integer :: rc
  integer :: n,nn
  integer :: localPet, nPet
  logical :: maintask
  logical :: isPresent
  ! local variables
  type(ESMF_Grid)    :: atmgrid, atmgridocnmask
  type(ESMF_Mesh)    :: meshwav, meshocn
  type(ESMF_PoleMethod_Flag) :: polemethod
  integer(kind=ESMF_KIND_I4), pointer :: maskptr(:,:)

  character(len=120) :: bldata, fv3dir, fmosaic, ftilepath, focnmask
  character(len=120) :: fsrc, fwavmesh, focnmesh, fwgt
  character(len=120) :: ftag, atmres, wavres, ocnres, maptype
  character(len=120) :: logmsg

  ! hard code locations of needed ocnmesh and wavmesh files on ursa
  atmres = 'C96'
  ocnres = 'mx100'
  wavres = 'global_270k'
  bldata = '/scratch4/NAGAPE/epic/role-epic/UFS-WM_RT/NEMSfv3gfs/input-data-20250507/'
  fv3dir = '/scratch4/NCEPDEV/nems/Denise.Worthen/utils_dw/fix/orog'
  focnmesh = '/scratch3/NCEPDEV/global/role.glopara/fix/cice/20240416/100/mesh.'//trim(ocnres)//'.nc'
  fwavmesh = trim(bldata)//'WW3_input_data_20250807/mesh.'//trim(wavres)//'.nc'

  !
  call ESMF_Initialize()
  call ESMF_VMGetGlobal(vm)
  call ESMF_VMGet(vm, localPet=localPet, peCount=nPet, mpiCommunicator=int_mpic, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  mpic%mpi_val = int_mpic
  maintask = .false.
  if (localPet == 0) maintask=.true.
  if (maintask) then
     if (mod(nPet,6) /= 0) then
        print '(a)', 'nPets not a multiple of 6; Aborting '
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
     else
        print '(a,i4,a)','Running on = ',npet,' tasks'
     end if
  end if

  ! do this is simple module procedure
  !call addmask2grid(

  ! Create an atm Grid, add a mask and set it to ispval
  fmosaic = trim(fv3dir)//'/'//trim(atmres)//'/'//trim(atmres)//'_mosaic.nc'
  ftilepath = trim(fv3dir)//'/'//trim(atmres)//'/'
  logmsg = 'creating AtmGrid from '//trim(fmosaic)
  if (maintask) print '(a)',trim(logmsg)

  atmgrid = ESMF_GridCreateMosaic(filename=trim(fmosaic), tileFilePath=trim(ftilepath), &
       staggerLocList = (/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !call addmask2grid(atmgrid, ispval)
  ! call ESMF_GridGetItem(atmgrid, staggerloc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, &
  !      isPresent=isPresent, rc=rc)
  ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! if (.not. isPresent) then
  !    print *,'mask is not present'
  !    ! add the mask item to the grid
  !    call ESMF_GridAddItem(atmgrid, itemflag=ESMF_GRIDITEM_MASK, itemTypeKind=ESMF_TYPEKIND_I4, &
  !         staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
  !    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !    maskPtr = ispval
  !    call ESMF_GridGetItem(atmgrid, itemflag=ESMF_GRIDITEM_MASK, staggerloc=ESMF_STAGGERLOC_CENTER, &
  !         farrayPtr=maskPtr, rc=rc)
  !    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! end if

  ! Create an atm Grid, add a mask and set it to the mapped ocean mask value
  atmgridocnmask = ESMF_GridCreateMosaic(filename=trim(fmosaic), tileFilePath=trim(ftilepath), &
       staggerLocList = (/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fsrc = trim(fv3dir)//'/'//trim(atmres)//'/ocean_mask/'//trim(ocnres(3:5))//'/'// &
       trim(atmres)//'.'//trim(ocnres)//'.tile*.nc'
  print '(a)',trim(fsrc)
  !call addmask2grid(atmgridocnmask, trim(fsrc))

  ! call ESMF_GridGetItem(atmgridocnmask, staggerloc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, &
  !      isPresent=isPresent, rc=rc)
  ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! if (.not. isPresent) then
  !    !add mappedmask
  ! end if


end program genweights
