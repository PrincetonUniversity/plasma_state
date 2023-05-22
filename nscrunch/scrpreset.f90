SUBROUTINE scrpreset(jmax,ns,imom)
  use ps_physconst_mod, only: zero
  use mintrp
  use scrunch_inc1
  implicit none
  integer, intent(in) :: jmax, ns, imom
  ntheta = jmax
  mpol = imom+1
  mnd=mpol*(nphi+1)
  mpol2=2*mpol
  mpol3=3*mpol
  mpol4=4*mpol
  n2=mpol4+ntheta
  nu      = jmax
  ntheta0 = jmax
  ncmax   = ns
  moption = 3
  numc = ns
  facmid = zero  ! magic numbers
  facedg = zero  ! don't change
  facdx0 = zero  ! (defaults set in scriscrunch).
  return
END SUBROUTINE scrpreset


subroutine scrpreset_wr(icon)

  ! write on LUN icon the info needed to call scrpreset

  use mintrp
  use scrunch_inc1
  implicit none
  integer, intent(in) :: icon

  write(icon) ncmax,nu,mpol
  return
end subroutine scrpreset_wr
