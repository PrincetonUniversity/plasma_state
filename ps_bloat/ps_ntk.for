      subroutine ps_ntk(intk)
c
c  return the R,Z sequence grid size
c
      INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)
      include 'ps_tkbparm.inc'
c
      intk=ntk
      return
      end
