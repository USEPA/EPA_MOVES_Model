C**** FNDHPC
c
      integer*4 function fndhpc( hp )
c
c-----------------------------------------------------------------------
c
c     This function finds the index into hpclev of the specified HP.
c
c     Return value:
c           0 = no match found
c         > 0 = the index of the matching HP category
c
c     Argument declaration
c       Inputs:
c             hp  R  HP category to find
c
c-----------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------
c
c      05/19/05  -cimulus-  original development
c      07/20/05  -cimulus- floating-point comparison for equality okay;
c                          hp values are read from file and must be 0,
c                          9999, or one of the values in hpclev, which
c                          are hard-coded, rather than being calculated
c                          at runtime
c
c-----------------------------------------------------------------------
c  Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdeqp.inc'
c
c-----------------------------------------------------------------------
c  Argument declarations:
c-----------------------------------------------------------------------
c
      real*4 hp
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c  Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c   --- assume no match ---
c
      fndhpc = 0
c
c   --- find the matching HP category ---
c
      do i = 1, MXHPC
          if( hp .EQ. hpclev(i) ) then ! floating-point comparison for equality okay; hp values are read from file and must be 0, 9999, or one of the values in hpclev, which are hard-coded, rather than being calculated at runtime
              fndhpc = i
              exit
          endif
      end do
c
c   --- return ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c  Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
