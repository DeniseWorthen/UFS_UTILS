program genweights

  use ESMF
  use mpi_f08

  implicit none
  type(MPI_Comm) :: mpic  ! mpi_f08
  type(ESMF_VM) :: vm

  integer :: localPet, nPet
  logical :: maintask

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


end program genweights
