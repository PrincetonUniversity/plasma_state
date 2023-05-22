subroutine scrmemory
  use mintrp
  use scrunch_inc1
  use scrunch_inc2
  !...begin heap allocation
  implicit none

  integer :: ialloc

  ialloc = 0
  allocate(rin(nu,ncmax), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating rin(nu,ncmax)')
  end if
  allocate(zin(nu,ncmax), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating zin(nu,ncmax)')
  end if
  allocate(xvec(n2), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating xvec(n2)')
  end if
  allocate(gvec(n2), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating gvec(n2)')
  end if
  allocate(xdot(n2), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating xdot(n2)')
  end if
  allocate(xstore(n2), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating xstore(n2)')
  end if
  allocate(m1(mnd), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating m1(mnd)')
  end if
  allocate(n1(mnd), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating n1(mnd)')
  end if
  allocate(rmomb(mpol,2), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating rmomb(mpol,2)')
  end if
  allocate(zmomb(mpol,2), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating zmomb(mpol,2)')
  end if
  allocate(mm(mpol), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating mm(mpol)')
  end if
  allocate(dm1(mpol), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating dm1(mpol)')
  end if
  allocate(faccon(mpol), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating faccon(mpol)')
  end if
  allocate(xmpq(mpol,4), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating xmpq(mpol,4)')
  end if
  allocate(result(nphi2,mpol,4), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating result(nphi2,mpol,4)')
  end if
  allocate(rin3d(ntheta,nphi), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating rin3d(ntheta,nphi)')
  end if
  allocate(zin3d(ntheta,nphi), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating zin3d(ntheta,nphi)')
  end if
  allocate(angle(ntheta,nphi), STAT=ialloc)
  if(ialloc.ne.0) then
    call scraborter('ERROR allocating angle(ntheta,nphi)')
  end if
  !...end heap allocation
  return
end subroutine scrmemory
