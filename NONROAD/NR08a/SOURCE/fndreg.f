C**** FNDREG 
c
      function fndreg( fipin )
C
      IMPLICIT NONE
C
      integer*4 fndreg
c
c-----------------------------------------------------------------------
c
c     This routine searches through an array of region definitions and
c     searches for the best match of FIPS code.
c   Return value:
c     0    = no match found
c     > 0  = index of best match in arrays
c   Arguments:
c     Inputs:
c       fipin    C   FIPS code to seach for
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     05/10/97  -gwilson-  Original development
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'nonrdprm.inc'
      include 'nonrdreg.inc'
      include 'nonrdtpl.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*5 fipin
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 idxus, idxst, i, j
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      fndreg = 0
      idxus = 0
      idxst = 0
c
c   ---- loop through array of region codes ---
c
      do 10 i=1,nregcd
c
c   ---- loop through array of FIPS codes in this region ---
c
         do 20 j=1,isttcnt(i)
c
c   ---- if exact match, set return value and return right away ---
c
            if( fipin .EQ. rgstt(i,j) ) then
               fndreg = i
               goto 9999
            endif
c
c   --- look for match of state code ---
c
            if( rgstt(i,j)(3:5) .EQ. '000' .AND. 
     &                 fipin(1:2) .EQ. rgstt(i,j)(1:2) ) idxst = i
c
c   --- if region FIPS code is national it matches everything ---
c
            if( rgstt(i,j) .EQ. '00000' ) idxus = i
   20    continue
   10 continue
c
c   ---- check for global matches ---
c
      if( idxst .GT. 0 ) then
         fndreg = idxst 
         goto 9999
      else if( idxus .GT. 0 ) then
         fndreg = idxus
         goto 9999
      endif
c
c   --- no match, return value is zero ----
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
