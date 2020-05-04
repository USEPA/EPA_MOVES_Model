C**** LFTJST
c
      subroutine lftjst( string )
c
c-----------------------------------------------------------------------
c
c   Description:
c
c     This routine left justifies a string, .ie. it removes the leading
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
c   Local parameters:
c-----------------------------------------------------------------------
c
c       BLANK    C  blank character
c
      character*1 BLANK
c
      parameter(BLANK=' ')
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   ibeg  I  first non-blank character of string
c
      integer*4 ibeg, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- already left justified so just return ----
c
      if( string(1:1) .NE. BLANK ) goto 9999
c
c   ---- check from beninng of string to end for non-blank ----
c
      do 10 i=1,LEN(string)
         if( string(i:i) .NE. BLANK ) then
c
c   ---- non-blank character found, return length ----
c
             ibeg = i
             goto 111
         endif
   10 continue
c
c   ---- string must be all blank, take no action and return ----
c
      goto 9999
c
c   ---- shift all characters to the beginning of string ---
c
  111 continue
      do 20 i=1,LEN(string)-ibeg+1
         string(i:i) = string(i+ibeg-1:i+ibeg-1)
         string(i+ibeg-1:i+ibeg-1) = BLANK
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
