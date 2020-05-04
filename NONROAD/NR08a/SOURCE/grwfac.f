C**** GRWFAC  
c
      subroutine grwfac( ierr, factor, ibasyr, igyear, infips, indcod ) 
c
c-----------------------------------------------------------------------------
c
c   subroutine to compute the growth factor from the current indicator 
c   data
c   
c     Argument declaration
c       Outputs:
c             ierr    I  error indicator flag
c             factor  R  the growth factor based on the current indicator
c       Inputs:
c             ibasyr  I  the base population year
c             igyear  I  the growth year 
c             infips  I  FIPS code
c             indcod  C  indicator code
c
c-----------------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------------
c
c        12-05-95  -djk-  Original Development
c        07-16-96  -gwl-  Rewritten
c        06/09/97  -gwilson- Now writes error if no data available.
c        09/19/01  -charvey- For future year interpolate between two 
c                    closest yrs of input vs using later year index.
c        01/24/02  -charvey- If base pop year in pop file is not a year
c                    in the growth file, interpolate to get base index.
c        03/14/02 12 charvey: Properly backcast & handle negative growth.
c        07/23/04  charvey: corrections to negative growth handling special cases.
c        04/25/05  -cimulus- Redeveloped extrapolation/interpolation of
c                            base-year and growth-year indicators
c        05/04/05  -cimulus- Changed ipopyr to ibasyr
c        05/05/05  -cimulus- Removed error message about 0
c                            interpolated-base-year indicator, and added
c                            special case handling for base-year
c                            indicator of zero
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdgrw.inc'
      include 'nonrdreg.inc'
c
c------------------------------------------------------------------------
c  Argument declarations:
c------------------------------------------------------------------------
c
      integer*4    ierr
      integer*4    ibasyr
      integer*4    igyear
      real*4       factor
      character*5  infips
      character*4  indcod
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c
c------------------------------------------------------------------------
c  Local variables:
c------------------------------------------------------------------------
c
      character*5 stfips
      integer*4   i, ibeg, iend
      real*4      baseyearind ! growth indicator for base-population year, interpolated or extrapolated as necessary
      real*4      growthyearind ! growth indicator for growth year, interpolated or extrapolated as necessary
      real*4      indchange ! temporary storage of year-to-year change in indicator value
      real*4      lowerboundindchange ! year-to-year change in indicator value at the lower end of the range of indicators
      real*4      upperboundindchange ! year-to-year change in indicator value at the upper end of the range of indicators
      real*4      tmpbaseyearind
c
c------------------------------------------------------------------------
c  Entry point:
c------------------------------------------------------------------------
c     
c --- default growth is no growth ---
c
      ierr = IFAIL
      factor = 0.0
c
c --- if episode year equals base year, then no growth ---
c
CCC 
CCC  but still need to calc growth to get proper adjusted age dist?
CCC
      if( igyear .EQ. ibasyr ) then
cc
cc        write(IOWMSG,'(//1X,A,A4,2X,2I4)',ERR=9999)
cc     &    'indcod,ibasyr = igyear: ',indcod,ibasyr,igyear
cc
        ierr = ISUCES
        goto 9999      
      endif
CCC
c
c --- zero indicator year indecies ---
c
      ibeg = 0
      iend = 0
c                             
c --- loop over the indicator fips searching for national, state,
c     or county match (note--fipgrw is sorted by fips code) ---
c
      stfips = infips(1:2)//'000'
c                                        
      do 10 i=1,nrcgrw
         if( fipgrw(i) .EQ. '00000') iend = i
         if( fipgrw(i) .EQ. stfips) iend = i
         if( fipgrw(i) .EQ. infips) iend = i
   10 continue      
c         
c --- if there is no match, use default growth ---
c
      if( iend .EQ. 0 ) goto 7000
c  
c --- backup to the first occurrance ---
c
      ibeg = 1
      do 20 i=iend-1,1,-1
         if( fipgrw(i) .NE. fipgrw(iend) ) then
            ibeg = i + 1 
            goto 111
         endif
   20 continue
c
c --- if ibeg equals iend then only one indicator,
c     we need two years for equation, so write error and return ---
c
  111 continue
cc
cc      write(IOWMSG,'(//1X,A,A4,2X,2I4)',ERR=9999)
cc     &  'indcod,ibeg,iend: ',indcod,ibeg,iend
cc
      if( ibeg .EQ. iend .OR. iyrgrw(ibeg) .EQ. iyrgrw(iend) ) goto 7000
c
c --- calculate lower boundary year-to-year indicator
c     change (forward in time), if necessary ---
      if( ibasyr .LT. iyrgrw(ibeg) .OR. igyear .LT. iyrgrw(ibeg) ) then
          lowerboundindchange = (valgrw(ibeg + 1) - valgrw(ibeg)) /
     &            (iyrgrw(ibeg + 1) - iyrgrw(ibeg)) ! growing forward, so subtract lower year from higher year
      endif
c
c --- calculate upper boundary year-to-year indicator
c     change (forward in time), if necessary ---
      if( ibasyr .GT. iyrgrw(iend) .OR. igyear .GT. iyrgrw(iend) ) then
          upperboundindchange = (valgrw(iend) - valgrw(iend - 1)) /
     &            (iyrgrw(iend) - iyrgrw(iend - 1)) ! growing forward, so subtract lower year from higher year
      endif
c
c --- calculate growth indicator for the base-population year ---
      if( ibasyr .LT. iyrgrw(ibeg) ) then
c     --- extrapolate bacwards based on the year-to-year
c         indicator change at the lower end of the range ---
          baseyearind = max(0., valgrw(ibeg) +
     &            (lowerboundindchange * (ibasyr - iyrgrw(ibeg)))) ! growing backwards, so subtract higher year from lower year
      else if( ibasyr .GT. iyrgrw(iend) ) then
c     --- extrapolate forward based on the year-to-year
c         indicator change at the upper end of the range ---
          baseyearind = max(0., valgrw(iend) +
     &            (upperboundindchange * (ibasyr - iyrgrw(iend)))) ! growing forward, so subtract lower year from higher year
      else if( ibasyr .EQ. iyrgrw(iend) ) then
c     --- matched last growth-data year ---
          baseyearind = valgrw(iend)
      else
          do i=ibeg,iend-1
              if( ibasyr .EQ. iyrgrw(i) ) then
c             --- matched current growth-data year ---
                  baseyearind = valgrw(i)
                  exit
              else if( ibasyr .LT. iyrgrw(i + 1) ) then
c             --- in between growth-data years, so interpolate based on
c                 year-to-year indicator change between the two years ---
                  indchange = (valgrw(i + 1) - valgrw(i)) /
     &                    (iyrgrw(i + 1) - iyrgrw(i)) ! growing forward, so subtract lower year from higher year
                  baseyearind = valgrw(i) + (indchange *
     &                    (ibasyr - iyrgrw(i))) ! growing forward, so subtract lower year from higher year
                  exit
              endif
          end do
      endif
c
c --- calculate growth indicator for the growth year ---
      if( igyear .LT. iyrgrw(ibeg) ) then
c     --- extrapolate bacwards based on the year-to-year
c         indicator change at the lower end of the range ---
          growthyearind = max(0., valgrw(ibeg) +
     &            (lowerboundindchange * (igyear - iyrgrw(ibeg)))) ! growing backwards, so subtract higher year from lower year
      else if( igyear .GT. iyrgrw(iend) ) then
c     --- extrapolate forward based on the year-to-year
c         indicator change at the upper end of the range ---
          growthyearind = max(0., valgrw(iend) +
     &            (upperboundindchange * (igyear - iyrgrw(iend)))) ! growing forward, so subtract lower year from higher year
      else if( igyear .EQ. iyrgrw(iend) ) then
c     --- matched last growth-data year ---
          growthyearind = valgrw(iend)
      else
          do i=ibeg,iend-1
              if( igyear .EQ. iyrgrw(i) ) then
c             --- matched current growth-data year ---
                  growthyearind = valgrw(i)
                  exit
              else if( igyear .LT. iyrgrw(i + 1) ) then
c             --- in between growth-data years, so interpolate based on
c                 year-to-year indicator change between the two years ---
                  indchange = (valgrw(i + 1) - valgrw(i)) /
     &                    (iyrgrw(i + 1) - iyrgrw(i)) ! growing forward, so subtract lower year from higher year
                  growthyearind = valgrw(i) + (indchange *
     &                    (igyear - iyrgrw(i))) ! growing forward, so subtract lower year from higher year
                  exit
              endif
          end do
      endif
c
c --- if both indicators are zero, return default growth factor of zero ---
      if( baseyearind .EQ. 0. .AND. growthyearind .EQ. 0.) then
          ierr = ISUCES
          goto 9999
      endif
c
c --- cannot calculate growth factor if base-year indicator is zero ---
      if( baseyearind .EQ. 0. ) then
c     --- adjust base-year indicator to the minimum allowed
c         to avoid divide by zero ---
          tmpbaseyearind = max(MINGRWIND, baseyearind)
          write(IOWMSG,9002,ERR=9999) 'WARNING:  Adjusting ',
     &            'base-year growth indicator to avoid divide by zero',
     &            'Base Yr','Base Yr Ind','Adj Base Yr Ind',
     &            'Growth Yr','Growth Yr Ind',
     &            ibasyr,baseyearind,tmpbaseyearind,
     &            igyear,growthyearind
          nwarn = nwarn + 1
      endif
c
c --- calculate the growth factor ---
      factor = (growthyearind - baseyearind) /
     &        (baseyearind * (igyear - ibasyr))
c
c  --- set error code to sucess and return ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR:  Could not find any ',
     &                                        'valid growth data for '
      write(IOWSTD,9000,ERR=9999) 'County','Year','Indicator Code',
     &                                            infips, igyear, indcod
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR:  Could not find any ',
     &                                        'valid growth data for '
      write(IOWMSG,9000,ERR=9999) 'County','Year','Indicator Code',
     &                                            infips, igyear, indcod
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(10X,A6,3X,A4,3X,A,/,10X,A5,4X,I4,10X,A4)
 9001 format(10X,A6,3X,A9,3X,A,/,10X,A5,4X,I4,15X,A4)
 9002 format(/,1X,2A,/,T11,A,T23,A,T39,A,T59,A,T73,A,
     &        /,T11,I4,T19,F15.6,T39,F15.6,T59,I4,T71,F15.6)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999  continue
       return
       end
