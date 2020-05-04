C**** FNDASC
c
      function fndasc(
     &                ascin, array, ncount )
C
      IMPLICIT NONE
C
      integer*4 fndasc
c
c-----------------------------------------------------------------------
c
c     This routine searches through an array of SCC codes and searches
c     for the best match of the SCC code in the argument list.
c     The best match is based on the SCC code heierchy.  For example,
c     the best match for the code 2265001010 are as follows:
c           2265001010
c           2265001000
c           2265000000
c
c   Arguments:
c
c     Inputs:
c       ascin    C   SCC code to seach for
c       array    C   array of codes to search
c       ncount   I   number of entries in the array
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     11/10/95  -gmw-  Original development
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*10 ascin
      integer*4    ncount
      character*10 array(ncount)
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      character*10 ascglb(2)
      integer*4    i, j, idxglb(2)
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize the found-match arrays ---
c
      fndasc = 0
      ascglb(1) = ascin(1:4)//'000000'
      ascglb(2) = ascin(1:7)//'000'
      idxglb(1) = -9
      idxglb(2) = -9 
c
c   ---- loop through the array ---
c
      do 10 i=1,ncount
c
c   ---- if exact match, set return value and return right away ---
c
         if( ascin .EQ. array(i) ) then
             fndasc = i
             goto 9999
         endif
c
c   --- check the global matches and set the local index array ---
c
         do 20 j=1,2
            if( array(i) .EQ. ascglb(j) ) idxglb(j) = i
   20    continue
   10 continue
c
c   ---- find the best match, loop backwards through 
c        found-match array ---
c
      do 30 j=2,1,-1
         if( idxglb(j) .GT. 0 ) then
            fndasc = idxglb(j)
            goto 9999
         endif
  30  continue
c
c   --- no match, will return zero ---
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
