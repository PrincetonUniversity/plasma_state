program cstate

  ! compare plasma states
  ! all action in cstate_lib

  integer :: luntty

  call uinita(0)       ! UREAD init

  luntty = lnout(0)

  call initvis("CSTATE")

  call cstate_main(1)  ! acquire list of states; call code to read & compare...

  call uexita          ! UREAD exit
  stop

end program cstate
