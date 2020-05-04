C**** VLDRTRFTHP
c
      logical*4 function vldrtrfthp( hp )
c
c-----------------------------------------------------------------------
c
c     Validates the HP to ensure it is valid for a retrofit
c     specification.  The HP is only valid if it found in the hpclev
c     array, if it is 0 (required for minimum HP when range is 0 to 1),
c     or if it is 9999 (required for maximum HP when range is > 3000).
c
c     Return value:
c         .TRUE.   =  HP is valid
c         .FALSE.  =  HP is not valid
c
c     Argument declaration
c       Inputs:
c             hp  R  HP to validate
c
c-----------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------
c
c      05/19/05  -cimulus-  original development
c      07/20/05  -cimulus-  floating-point comparison for equality okay;
c                           hp values are read from file and must be 0,
c                           9999, or one of the values in hpclev, which
c                           are hard-coded, rather than being calculated
c                           at runtime
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
c   --- assume not valid
c
      vldrtrfthp = .FALSE.
c
c   --- if the HP is found in the array of HP levels,
c       0, or 9999, return true ---
c
      if( hp .EQ. 0. .OR. hp .EQ. 9999. ) then ! floating-point comparison for equality okay; hp values are read from file and must be 0, 9999, or one of the values in hpclev, which are hard-coded, rather than being calculated at runtime
          vldrtrfthp = .TRUE.
      else
          do i = 1, MXHPC
              if( hp .EQ. hpclev(i) ) then ! floating-point comparison for equality okay; hp values are read from file and must be 0, 9999, or one of the values in hpclev, which are hard-coded, rather than being calculated at runtime
                  vldrtrfthp = .TRUE.
                  exit
              endif
          end do
      endif
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
