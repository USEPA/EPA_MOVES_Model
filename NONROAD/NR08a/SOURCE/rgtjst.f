C**** RGTJST 
c
      subroutine rgtjst( string )
c
c-----------------------------------------------------------------------
c
c   Description:
c
c     This routine right justifies a string, .ie. removes the trailing
c     blanks.
c
c   Arguments:
c
c     Inputs/Outputs: (the string argument serves as both input and
c                      output)
c       string   C   string to left justify
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
      IMPLICIT NONE

      character*(*) string
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
c   strlen  I   returns the actual length of a string
c
      integer*4 strlen
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c       BLANK    C  blank character
c
      character*1 BLANK
      parameter(BLANK=' ')
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   ibeg  I   last non-blank character of string
c
      integer*4 iend, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- already left justified so just return ----
c
      if( string(LEN(string):LEN(string)) .NE. BLANK ) goto 9999
c
c   ---- find last non-blank character ---
c
      iend = strlen ( string )
      if( iend .LT. 1 ) goto 9999
c
c   ---- move all characters to end ---
c
      do 10 i=LEN(string),LEN(string)-iend+1,-1
         string(i:i) = string(i-LEN(string)+iend:i-LEN(string)+iend)
   10 continue
c
c   ---- blank out leftmost characters ---
c
      do 20 i=1,LEN(string)-iend
         string(i:i) = BLANK
   20 continue
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
