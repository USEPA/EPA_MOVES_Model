C**** STRLEN version 1.0 strlen.f 1.5 5/20/92
c
      function strlen(
     &                 string )
c
      IMPLICIT NONE
c
      integer*4 strlen
c
c-----------------------------------------------------------------------
c
c   Description:
c
c     This routine returns the actual length of a string, i.e. with no
c     trailing blanks.
c
c   Arguments:
c
c     Inputs:
c       string   C   string for determining length
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     11/10/91  -gmw-  Original development
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c       BLANK    C  blank character
c
      character*1 BLANK
      parameter( BLANK=' ' )
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 idclar
      integer*4 i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize length to zero ----
c
      idclar = len( string )
      strlen = 0
c
c   ---- check from end of string to beginning for non-blank ----
c
      do 10 i=idclar,1,-1
         if( string(i:i) .NE. BLANK ) then
c
c   ---- non-blank character found, return length ----
c
             strlen = i
             goto 9999
         endif
   10 continue
c
c   ---- string must be all blank, return the zero length ----
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
