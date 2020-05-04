c**** FNDACT
c
      function fndact( asccin, fipin, hpin )
C
      IMPLICIT NONE
C
      integer*4 fndact
c
c-----------------------------------------------------------------------
c
c     This routine searches through the array of equipment types read
c     from the activity file and looks for the best match of the 
c     identification data in the argument list.
c     The best match is defined as:
c                SCC        Region
c                ------     ------
c                Exact      Exact
c                Exact      Global
c                Global     Exact
c                Global     Global
c
c    The average HP must be in the HP range of the matched record, but
c    there is no check for best match on HP range.
c
c   Arguments:
c     Inputs:
c       asccin   C   SCC code to match
c       fipin    C   FIPS code of the county
c       hpin     I   HP category
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     01/10/94  --gmw--  Original development
c     07/23/96  --jlf--  added option of technology code = ALL in
c                        the activity file
c     06/10/97  --gwilson-- Changed literal ALL to be a parameter
c     05/20/04  --dfk--  added switch to search for exhaust or evap tech type
c     07/11/05  --cimulus--  evaporative no longer compares technology
c                            type to find activity data, so force match
c                            by assuming ALL when iexev is evap
c     07/13/05  --cimulus--  exhaust no longer compares technology
c                            type to find activity data, so force match
c                            by assuming ALL when iexev is exhaust
c     07/15/05  --cimulus--  removed tech type searching/matching
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'nonrdprm.inc'
      include 'nonrdeqp.inc'
      include 'nonrdact.inc'
      include 'nonrdreg.inc'
      include 'nonrdtpl.inc'
c
c-----------------------------------------------------------------------
c   External funnctions:
c-----------------------------------------------------------------------
c
c   fndreg   I    returns the index of a region county for a county
c
      integer*4 fndreg
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*10 asccin
      character*5  fipin
      real*4       hpin
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c   MXGLB  I  maximum number of global checks
c
      integer*4 MXGLB
c
      parameter( MXGLB = 3 )
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      character*10 ascglb(MXGLB)
      character*5  rgncd
      integer*4    iexact, iexreg, idxglb(MXGLB), idxrgb(MXGLB)
      integer*4    idxreg, i, j
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c  --- initialize the global SCC codes ---
c
      fndact = 0
      do 10 i=1,MXGLB
         ascglb(i) = '0000000000'
         idxglb(i) = -9
         idxrgb(i) = -9
   10 continue
      iexreg = 0
      iexact = 0
      ascglb(2)(1:4) = asccin(1:4)
      ascglb(3)(1:7) = asccin(1:7)
c
c   --- get the index of the region code ---
c
      idxreg = fndreg( fipin ) 
      if( idxreg .GT. 0 ) rgncd = rgdfcd(idxreg)
c
c   ---- loop through the array of codes ---
c
      do 40 i=1,nactrc
c
c   ---- check if the HP category is within the range ----
c
         if( hpin .GE. hpcact(1,i) .AND. hpin .LE. hpcact(2,i) ) then
c
c   ---- if exact match of SCC, set the match on region ---
c
             if( asccin .EQ. ascact(i) ) then
                 if( subact(i) .EQ. '     ' ) then
                   iexact = i
                 else if( subact(i) .EQ. rgncd ) then
                   iexreg = i
                 endif
             else
c
c   ---- loop over global SCC codes, setting indexes for match on region ---
c
                 do 50 j=1,MXGLB
                     if( ascglb(j) .EQ. ascact(i) ) then
                        if( subact(i) .EQ. '     ' ) then
                           idxglb(j) = i
                        else if( subact(i) .EQ. rgncd ) then
                           idxrgb(j) = i
                        endif
                     endif
   50            continue
              endif
          endif
c
c   ---- next seasonality code ---
c
   40 continue
c
c   --- check for exact matches of SCC code, an exact region match
c       is better than a global region match ---
c
      if( iexreg .GT. 0 ) then
         fndact = iexreg 
         goto 9999
      else if( iexact .GT. 0 ) then
         fndact = iexact
         goto 9999
      endif
c
c   --- loop over global indexes, backwards for best match,
c       an exact region match is better than a global region match ---
c
      do 60 i=MXGLB,1,-1
          if( idxrgb(i) .GT. 0 ) then
               fndact = idxrgb(i)
               goto 9999
          else if( idxglb(i) .GT. 0 ) then
               fndact = idxglb(i)
               goto 9999
           endif
   60 continue
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
