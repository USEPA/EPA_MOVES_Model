C**** RTRFTENGOVRLP
c
      logical*4 function rtrftengovrlp( a, b )
c
c-----------------------------------------------------------------------
c
c     Determines whether or not the sets of engines for two retrofit
c     records overlap.
c
c     Return value:
c         .TRUE.   =  the two sets of engines overlap
c         .FALSE.  =  the two sets of engines do not overlap
c
c     Argument declaration
c       Inputs:
c             a  I  index of first retrofit record to compare
c             b  I  index of second retrofit record to compare
c
c-----------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------
c
c      05/25/05  -cimulus-  original development
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
      integer*4 a
      integer*4 b
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
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c   --- assume sets of engines do not overlap ---
c
      rtrftengovrlp = .FALSE.
c
c   --- if the SCC ranges do not overlap, return false ---
c
      if( rtrftscc(a) .NE. RTRFTSCCALL .AND.
     &        rtrftscc(b) .NE. RTRFTSCCALL .AND.
     &        rtrftscc(a) .NE. rtrftscc(b) ) then ! if neither is all-inclusive and they are not equal
c
c       --- ensure that neither SCC is a global
c           that contains the other ---
c
          if( ( rtrftscc(a)(5:10) .EQ. '000000' .OR.
     &            rtrftscc(b)(5:10) .EQ. '000000' ) ) then ! if either is a 4-digit global
              if( rtrftscc(a)(1:4) .NE. rtrftscc(b)(1:4) ) then
                  goto 9999 ! engines do not overlap; return false
              endif
          else if( ( rtrftscc(a)(8:10) .EQ. '000' .OR.
     &            rtrftscc(b)(8:10) .EQ. '000' ) ) then ! else if either is a 7-digit global
              if( rtrftscc(a)(1:7) .NE. rtrftscc(b)(1:7) ) then
                  goto 9999 ! engines do not overlap; return false
              endif
          else ! else neither is a global SCC
              goto 9999 ! engines do not overlap; return false
          endif
      endif
c
c   --- if the tech type ranges do not overlap, return false ---
c
      if( rtrfttechtype(a) .NE. RTRFTTCHTYPALL .AND.
     &        rtrfttechtype(b) .NE. RTRFTTCHTYPALL .AND.
     &        rtrfttechtype(a) .NE. rtrfttechtype(b) ) then ! if neither is all-inclusive and they are not equal
          goto 9999 ! engines do not overlap; return false
      endif
c
c   --- if the HP ranges do not overlap, return false ---
c
      if( rtrfthpmx(a) .LE. rtrfthpmn(b) .OR.
     &        rtrfthpmx(b) .LE. rtrfthpmn(a) ) then ! use <= instead of < because minimum HP values are non-inclusive
          goto 9999 ! engines do not overlap; return false
      endif
c
c   --- if this point is reached, the engines must
c       overlap, so return true ---
c
      rtrftengovrlp = .TRUE.
      goto 9999
c
c-----------------------------------------------------------------------
c  Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
