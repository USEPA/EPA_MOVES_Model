C**** FNDTPM
c
      subroutine fndtpm( asccin, fipin, idxtpm, idxtpl )
c
c-----------------------------------------------------------------------
c
c     This routine searches through the arrays of monthly
c     and daily seasonal data and looks for the best matches on
c     each.
c
c   Arguments:
c     Inputs:
c       asccin   C   SCC code to match
c       fipin    C   FIPS code
c     Outputs:
c       idxtpm   I   index to Monthly profile code
c       idxtpl   I   index to Daily profile code
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     12/07/95  -djk-  Original development
c     07/20/96  -jlf-  Added lookup of daily & hourly codes
c     05/20/97  -gwilson- Modified to use region codes
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdtpl.inc'
      include 'nonrdreg.inc'
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
c    fndreg   I   returns the index of a region for a county
c
      integer*4 fndreg
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*10 asccin
      character*5  fipin
      integer*4    idxtpm
      integer*4    idxtpl
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c  MXGLB  I  maximum number of global matches
c
      integer*4 MXGLB
      parameter( MXGLB = 3 )
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      character*10 ascglb(MXGLB)
      character*5  sregin
      integer*4    idxglb(MXGLB), idxsrg(2), idxreg, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c  --- initialize the global codes ---
c
      idxtpm = -9
      idxtpl = -9
      do 10 i = 1, MXGLB
         ascglb(i) = '0000000000'
 10   continue
      ascglb(1)(1:4) = asccin(1:4)
      ascglb(2)(1:7) = asccin(1:7)
      ascglb(3) = asccin
c
c  --- reinitialize the pointers ---
c
      idxglb(1) = -9
      idxglb(2) = -9
      idxglb(3) = -9
      idxsrg(2) = -9
      idxsrg(1) = -9
c
c   --- get the subregion containing this county ---
c
      idxreg = fndreg( fipin )
      if( idxreg .GT. 0 ) sregin = rgdfcd(idxreg)
c
c   --- loop through the array of codes ---
c
      do 30 i=1,ntplmn
c
c   --- check if record matches SCC code  ---
c
         if( asccin .EQ. asctpm(i) ) then
c
c   --- check if subregion matches, if so then exact match so just return ---
c
            if( sregin .EQ. sbrtpm(i) ) then
               idxtpm = i
               goto 111
            endif 
c
c   --- check if the subregion is global  ---
c
            if( sbrtpm(i) .EQ. '     ' ) idxglb(3) = i
         endif
c
c   --- check if the code matches global 2 ---
c
         if( ascglb(2) .EQ. asctpm(i) ) then
c
c   --- check if the region code matches or is global
c
            if( sregin .EQ. sbrtpm(i) ) then
               idxsrg(2) = i
            else if( sbrtpm(i) .EQ. '     ' ) then
               idxglb(2) = i
            endif
         endif
c
c   --- check if the code matches global 3 ---
c
         if( ascglb(1) .EQ. asctpm(i) ) then
c
c   --- check if the region code matches or is global
c
            if( sregin .EQ. sbrtpm(i) ) then
               idxsrg(1) = i
            else if( sbrtpm(i) .EQ. '     ' ) then
               idxglb(1) = i
            endif
         endif
c
c   --- next seasonality code ---
c
   30 continue
c
c   --- select best match and do day-of-week next ---
c
      if( idxglb(3) .GT. 0 ) then
         idxtpm = idxglb(3)
      else if( idxsrg(2) .GT. 0 ) then
         idxtpm = idxsrg(2)
      else if( idxglb(2) .GT. 0 ) then
         idxtpm = idxglb(2)
      else if( idxsrg(1) .GT. 0 ) then
         idxtpm = idxsrg(1)
      else if( idxglb(1) .GT. 0 ) then
         idxtpm = idxglb(1)
      endif
c
c   ---- find days now and reinitialize some indices ----
c
  111 continue
      idxglb(1) = -9
      idxglb(2) = -9
      idxglb(3) = -9
      idxsrg(2) = -9
      idxsrg(1) = -9
c
c   --- loop through the array of daily codes ---
c
      do 40 i=1,ntplcd
c
c   --- check if record matches SCC code  ---
c
         if( asccin .EQ. asctpl(i) ) then
c
c   --- check if subregion matches ---
c
            if( sregin .EQ. sbrtpl(i) ) then
c
c   --- match is exact, return ---
c
               idxtpl = i
               goto 9999
            endif
c
c   --- check if the subregion is global  ---
c
            if( sbrtpl(i) .EQ. '     ' ) idxglb(3) = i
         endif
c
c   --- check if the code matches global 2 ---
c
         if( ascglb(2) .EQ. asctpl(i) ) then
c
c   --- check if the region code matches or is global
c
            if( sregin .EQ. sbrtpl(i) ) then
               idxsrg(2) = i
            else if( sbrtpl(i) .EQ. '     ' ) then
               idxglb(2) = i
            endif
         endif
c
c   --- check if the code matches global 3 ---
c
         if( ascglb(1) .EQ. asctpl(i) ) then
c
c   --- check if the region code matches or is global
c
            if( sregin .EQ. sbrtpl(i) ) then
               idxsrg(1) = i
            else if( sbrtpl(i) .EQ. '     ' ) then
               idxglb(1) = i
            endif
         endif
c
c   --- next seasonality code ---
c
   40 continue
c
c   --- select best match and return --
c
      if( idxglb(3) .GT. 0 ) then
         idxtpl = idxglb(3)
      else if( idxsrg(2) .GT. 0 ) then
         idxtpl = idxsrg(2)
      else if( idxglb(2) .GT. 0 ) then
         idxtpl = idxglb(2)
      else if( idxsrg(1) .GT. 0 ) then
         idxtpl = idxsrg(1)
      else if( idxglb(1) .GT. 0 ) then
         idxtpl = idxglb(1)
      endif
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
