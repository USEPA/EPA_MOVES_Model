C**** VLDRTRFTSCC
c
      logical*4 function vldrtrftscc( scc )
c
c-----------------------------------------------------------------------
c
c     Validates the SCC to ensure it is valid for a retrofit
c     specification.  The SCC is only valid if it is RTRFTSCCALL, or
c     matches something in the eqpcod array (whether exactly or as
c     a global).
c
c     Return value:
c         .TRUE.   =  SCC is valid
c         .FALSE.  =  SCC is not valid
c
c     Argument declaration
c       Inputs:
c             scc   C  SCC to validate
c
c-----------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------
c
c      05/19/05  -cimulus-  original development
c
c-----------------------------------------------------------------------
c  Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdrtrft.inc'
c
c-----------------------------------------------------------------------
c  Argument declarations:
c-----------------------------------------------------------------------
c
      character*10 scc
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c   chkasc  L   returns true if SCC code is needed for current run
c
      logical*4 chkasc
c
c-----------------------------------------------------------------------
c  Local variables:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c   --- assume not valid
c
      vldrtrftscc = .FALSE.
c
c   --- if the SCC is RTRFTSCCALL or matches an SCC in eqpcod (whether a
c       specific or a global match), return true ---
c
      if( scc .EQ. RTRFTSCCALL .OR. chkasc(scc, .FALSE.) ) then
          vldrtrftscc = .TRUE.
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
