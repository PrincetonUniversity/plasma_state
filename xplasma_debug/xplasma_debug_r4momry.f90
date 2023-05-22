subroutine xplasma_debug_r4momry(zxi,zth,ijac,zR,zZ,zjac)

  use xplasma_definitions
  use xpl_dbg_module
  implicit NONE

  !  interface for 2d equilibrium data -- arguments like the old
  !  TRANSP momry routine -- note REAL not REAL*8 arguments!

  !  this is used for debug plot purposes only...

  ! input:
  real zxi                          ! radial mag. coordinate
  real zth                          ! poloidal angle coordinate
  integer ijac                      ! =1 if 2x2 Jacobian is desired

  ! output:
  real zR
  real zZ
  real zjac(2,2)

  !---------------------------------

  real*8 zR8(1),zZ8(1),zRdx8(1),zZdx8(1),zRdth8(1),zZdth8(1),zx8(1),zth8(1)
  real*8 zRZ8(1,2)

  integer ierr,id_R,id_Z,id2(2)

  !---------------------------------

  zR=0.0
  zZ=0.0
  zjac=0.0

  zx8=zxi
  zth8=zth

  if(ijac.eq.0) then

     call xplasma_common_ids(sp,ierr, id_R=id_R,id_Z=id_Z)
     if(min(id_R,id_Z).eq.0) return
     if(ierr.ne.0) return

     id2(1)=id_R; id2(2)=id_Z

     call xplasma_RZeval_2d(sp,id2, &
          xplasma_theta_coord,zth8,xplasma_rho_coord,zx8, &
          zRZ8,ierr)

     zR=zRZ8(1,1)
     zZ=zRZ8(1,2)

  else

     call xplasma_rzjac(sp,zx8,zth8,ierr, &
          r=zR8,z=zZ8, &
          drdrho=zRdx8,dzdrho=zZdx8,drdtheta=zRdth8,dzdtheta=zZdth8)

     zR=zR8(1)
     zZ=zZ8(1)

     zjac(1,1)=zRdx8(1)
     zjac(2,1)=zRdth8(1)

     zjac(1,2)=zZdx8(1)
     zjac(2,2)=zZdth8(1)

  endif

end subroutine xplasma_debug_r4momry
