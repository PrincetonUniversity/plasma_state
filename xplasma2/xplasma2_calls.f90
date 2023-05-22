module xplasma2_calls
  interface eqi_qpsi_regen
     subroutine eqi_qpsi_regen(gval,inum,rs,zs,qval)
       implicit NONE
       real*8, intent(in) :: gval    ! "g", (R*B_phi), Tesla*m
       integer, intent(in) :: inum   ! #pts on surface
       real*8, intent(in) :: rs(inum),zs(inum)  ! closed flux surface contour
       !                     with rs(1)=rs(inum) and zs(1)=zs(inum)
       real*8, intent(out) :: qval   ! "q" value estimatedend subroutine eqi_qpsi_regen
     end subroutine eqi_qpsi_regen
  end interface eqi_qpsi_regen
  
  interface eqi_geq_findrzs
     subroutine eqi_geq_findrzs(ns,lmask,x,dif,idum,psi,idum2,rzvec,id2)
       implicit NONE
       integer, parameter :: r8=selected_real_kind(12,100)
       integer, intent(in) :: ns        ! no. of segments
       logical, intent(in) :: lmask(ns) ! mask segments where target psi was reached
       
       real(r8), intent(in) :: x(ns)    ! parameter, 0->(r0,z0), 1->(r/zseek*(:))
       
       real(r8), intent(out) :: dif(ns) ! psi difference at surfaces
       
       integer, intent(in) :: idum,idum2,id2
       real(r8), intent(in) :: psi(ns)  ! target psi values
       
       real(r8), intent(out) :: rzvec(ns,id2)  ! (R,Z) valuesend subroutine eqi_geq_findrzs
     end subroutine eqi_geq_findrzs
  end interface eqi_geq_findrzs
  
  interface eqi_geq_intrp
     subroutine eqi_geq_intrp(psi_geq, f_geq, ngeq, psi, ns, f, ier)
       implicit none
       integer, parameter :: r8=selected_real_kind(12,100)
       integer, intent(in) :: ngeq        ! no. of pts in G-EQDSK 1d profiles
       real(r8), intent(in) :: psi_geq(ngeq)  ! G-EQDSK psi grid (strict ascending)
       real(r8), intent(in) :: f_geq(ngeq)    ! G-EQDSK f(psi) profile
       
       integer, intent(in) :: ns          ! no. of psi-surfaces wanted
       real(r8), intent(in) :: psi(ns)    ! psi values at psi-surfaces
       
       real(r8), intent(out) :: f(ns)     ! profile values at psi-surfaces
       
       integer, intent(out) :: ier        ! completion code:  0 = OK
     end subroutine eqi_geq_intrp
  end interface eqi_geq_intrp
  interface eqi_find_curv
     subroutine eqi_find_curv(nsrch, lmask, psivec, result, id1, zinput, id2i, &
          zoutput, id2o)
       implicit NONE
       integer, parameter :: r8=selected_real_kind(12,100)
       integer, intent(in) :: nsrch   ! number of searches, must = 1
       logical, intent(in) :: lmask(nsrch) ! mask searches where soln is known
       
       real*8, intent(in) :: psivec(nsrch)    ! psi location to try next
       
       real*8, intent(out) :: result(nsrch)   ! curvature difference at surfaces
       
       integer, intent(in) :: id1,id2i,id2o   ! auxilliary array dimensions
       
       real*8, intent(in) :: zinput(id1,id2i) ! target curvature value here
       real*8, intent(out) :: zoutput(id1,id2o)  ! output: 0=OK, 1= arg error
     end subroutine eqi_find_curv
  end interface eqi_find_curv
  interface eqi_find_curvature
     subroutine eqi_find_curvature(ns,psi,xmin,psi_range,rzvec,curvature,mincurv)
       implicit NONE
       
       integer, intent(in) :: ns  ! number of rays; should be equal to nthad
       
       real*8, intent(in) :: psi  ! target psi value 
       
       real*8, intent(in) :: xmin ! minimum normalized distance from (r0,z0) at
       !  which to start search (in range [0:1]; could be 0; less than 1).
       !  0 corresponds to (r0,z0); 1 corresponds to r/zadapt.
       
       real*8, intent(in) :: psi_range  ! Psi(max)-Psi(min) in domain sought over
       real*8, intent(out) :: rzvec(ns,2)   ! (R,Z) values of psi surface found
       real*8, intent(out) :: curvature(ns) ! curvature along surface
       real*8, intent(out) :: mincurv       ! extrapolated minimum curvature
     end subroutine eqi_find_curvature
  end interface eqi_find_curvature
  interface eqi_contour
     subroutine eqi_contour(xp, yp, cbdy, errscale, cbdy_got, psiav, istat, ier)
       implicit none
       integer, parameter :: r8 = selected_real_kind(12,100)
       real(r8), intent(in) :: xp, yp ! starting point
       real(r8), intent(in) :: cbdy   ! min. curvature parameter
       real(r8), intent(in) :: errscale ! accuracy test scale
       real(r8), intent(out) :: cbdy_got ! min. curvature observed
       real(r8), intent(out) :: psiav ! avg Psi value along contour
       integer, intent(out) :: istat
       !                        =0 -- closed surface, curvature OK
       !                              (psiav,cbdy_got) computed
       !                        =1 -- closed surface, curvature too sharp
       !                              (psiav,cbdy_got) computed
       !                        =2 -- open surface or grad(Psi)=0 encountered
       !                              (psiav computed; cbdy_got=0)
       !                        =3 -- some other error
       !                              (psiav=0; cbdy_got=0)
       integer, intent(out) :: ier
     end subroutine eqi_contour
  end interface eqi_contour
  interface eqi_geq_psfind
     subroutine eqi_geq_psfind(psi_geq, q_geq, ngeq, xi, ns, fbdy, &
          & psi, phi, q, ier)
       implicit none
       integer, parameter :: r8=selected_real_kind(12,100)
       
       ! ------------------ arguments ---------------------------------
       
       integer, intent(in) :: ngeq        ! no. of pts in G-EQDSK 1d profiles
       real(r8), intent(in) :: psi_geq(ngeq)  ! G-EQDSK psi grid (strict ascending)
       real(r8), intent(in) :: q_geq(ngeq)    ! G-EQDSK q(psi)
       
       integer, intent(in) :: ns          ! no. of surfaces wanted
       real(r8), intent(in) :: xi(ns)     ! spacing of surfaces in sqrt(tor.flux)
       ! strict ascending, xi(1)=0, xi(ns)=1
       
       real(r8), intent(in) :: fbdy       ! back-off parameter for outermost surface
       
                                     ! on output psi(1)=0, psi(ns)=
       !   fbdy*(psi_geq(ngeq)-psi_geq(1))
       
       real(r8), intent(out) :: psi(ns)   ! the selected psi surfaces
       real(r8), intent(out) :: phi(ns)   ! the enclosed toroidal flux (Wb)
       !   satisfying xi(j)=sqrt(phi(j)/phi(ns))
       real(r8), intent(out) :: q(ns)     ! the q profile at {psi(1),...,psi(ns)}
       
       integer, intent(out) :: ier        ! completion code, 0=OK
       
       
     end subroutine eqi_geq_psfind
  end interface eqi_geq_psfind
end module xplasma2_calls
