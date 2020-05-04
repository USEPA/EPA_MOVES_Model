C**** FNDCHR 
c
      function fndchr(
     &                string, ilen, array, nchr )
C
      IMPLICIT NONE
C
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c
c     This routine searches through an array of character strings for a
c     string matching the input string.  The return value is the index
c     of the string in the array.  A return value of zero means no
c     match was made.
c
c   Arguments:
c
c     Inputs:
c       string   C   string to seach for
c       ilen     I   declared length of strings
c       array    C   array of strings to search
c       nchr     I   number of entries in the array
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
      integer*4     ilen
      integer*4     nchr
      character*(*) array(nchr)
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4   i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      fndchr = 0
c
c   ---- loop through the array ---
c
      do 10 i=1,nchr
c
c   ---- if match, set return value and return right away ---
c
         if( string(1:ilen) .EQ. array(i)(1:ilen) ) then
             fndchr = i
             goto 9999
         endif
   10 continue
c
c   ---- no match, return zero ----
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
