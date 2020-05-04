C**** VLDRTRFTTCHTYP
c
      logical*4 function vldrtrfttchtyp( techtype )
c
c-----------------------------------------------------------------------
c
c     Validates the tech type to ensure it is valid for a retrofit
c     specification.  The tech type is only valid if it is
c     RTRFTTCHTYPALL, or found in the tecdet array for THC exhaust,
c     which will always be loaded by NONROAD.
c
c     Return value:
c         .TRUE.   =  tech type is valid
c         .FALSE.  =  tech type is not valid
c
c     Argument declaration
c       Inputs:
c             techtype  C  tech type to validate
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
      character*10 techtype
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c   fnddet  I   returns the index in the deterioration factor arrays
c
      integer*4 fnddet
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
      vldrtrfttchtyp = .FALSE.
c
c   --- if the tech type is RTRFTTCHTYPALL or matches a tech type in
c       tecdet for THC exhaust, return true ---
c
      if( techtype .EQ. RTRFTTCHTYPALL
     &        .OR. fnddet(IDXTHC, techtype) .GT. 0 ) then
          vldrtrfttchtyp = .TRUE.
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
