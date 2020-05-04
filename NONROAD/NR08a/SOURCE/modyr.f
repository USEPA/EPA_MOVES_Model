C**** MODYR
c
      subroutine modyr(yryrfrcscrp, modfrc, stradj, actadj, nyrlif,
     &                 detage, uselif, disin, strhrs, acthrs, idxunt,
     &                 eload, agecod, popgrwfac)
c
c-----------------------------------------------------------------------
c
c    This routine calculates the fraction of population still in 
c    service of a certain equipment type.  It uses the hours of service
c    per year and the expected hours of life together with the scrappage
c    curve data.  If the distribution code is not the default code the
c    adjustment factors by year are retrieved and applied.  Otherwise
c    just the annual activity is used for all years.    
c
c    Argument declaration.
c     Outputs:
c       yryrfrcscrp  R  year-to-year faction scrapped by age
c       modfrc       R  array of population fractions for each model year
c       stradj       R  array of new adjusted starts hours for each year
c       actadj       R  array of new adjusted activity hours for each year
c       nyrlif       I  number of years in the lifetime 
c       detage       R  age of the engine, used for deterioration calculation
c     Inputs:
c       uselif       R  expected lifespan of equipment in hours
c                       this is currently read from the population file in years
c                       and multiplied by annual use hours here to get total hours
c       disin        C  code that tells which scrappage curve to use
c       strhrs       R  number of starts from activity file
c       acthrs       R  number of hours of use from activity file
c       idxunt       R  index units of activity data
c       eload        R  equipment load factor
c       agecod       C  code that indicates which age vs activity curve to apply
c       popgrwfac    R  population growth factor
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      06/16/93  --rel--  original development
c      12/18/95  --djk--  modified for EPA offroad
c      07/15/96  --asr--  modified for mid-year evaluation
c      07/21/96  --jlf--  modified to pass back useful life consumed for
c                         deterioration calculation
c      06/10/97  --gwilson-- added switch to do end of year fractions
c                            when doing annual, winter season, or
c                            a late year month
c      11/30/99  -mjimenez- added age vs activity adjustment
c      08/30/01  -rgiannelli- static age dist with backcast growth in it.
c      08/31/01  -charvey- removes all partial year scrap & det. code.
c      04/06/05  -cimulus- replaced modscp output parameter with
c                          yryrfrcscrp; added popgrwfac input parameter;
c                          removed search-for-scrappage-curve handling
c                          and calculation of model-year fractions and
c                          model-year scrappage, which are done in
c                          scrptime now
c      06/01/05  -cimulus- moved activity-adjustment calculation to
c                          after the call to scrptime so that it uses
c                          the nyrlif value calculated by scrptime
c      07/20/05  -cimulus- removed unused variables
c
c-----------------------------------------------------------------------
c    Include files
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

       include 'nonrdprm.inc'
       include 'nonrdio.inc'
       include 'nonrdefc.inc'
       include 'nonrdeqp.inc'
       include 'nonrdact.inc'
       include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real*4       yryrfrcscrp(MXAGYR)
      real*4       modfrc(MXAGYR)
      real*4       stradj(MXAGYR)
      real*4       actadj(MXAGYR)
      real*4       detage(MXAGYR)
      integer*4    nyrlif
      real*4       uselif
      character*10 disin
      real*4       acthrs
      real*4       strhrs
      integer*4    idxunt
      real*4       eload
      character*10 agecod
      real*4       popgrwfac
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   fndchr   I   returns index of a string in array of strings
c
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i, j, idxage
      real*4    adjfac(MXAGYR), accum, acttmp
      integer*4 ierr
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- initiialize the adjustment factors ----
c
      do 10 i=1,MXAGYR
          adjfac(i) = 1.0
          modfrc(i) = 0.0
   10 continue
c
c  --- search for the alternate activity curve to apply ---
c
      idxage = 0
      if ( agecod .NE. 'DEFAULT' )  then
         idxage = fndchr ( agecod, 10, agenam, nagenm )
         if ( idxage .LE. 0 )  then
            write(IOWMSG,'(/,1X,4A)',ERR=9999) 'WARNING:  Cannot ',
     &                  'find /AGE ADJUSTMENT/ curve ', agecod,
     &                  '   Using DEFAULT, no adjustment.'
            nwarn = nwarn + 1
         endif
      endif
c
c   --- set the local variable to be the annual units ---
c
      if( idxunt .EQ. IDXHRY ) then
         acttmp = acthrs
      else if( idxunt .EQ. IDXHRD ) then
         acttmp = acthrs * 365
      else if( idxunt .EQ. IDXGLY .AND. uselif .GT. 0. ) then
         acttmp = 1.0 / (2*uselif)
      else if( idxunt .EQ. IDXGLD .AND. uselif .GT. 0. ) then
         acttmp = 1.0 / (2*uselif)
      endif
c
c   --- calculate life in years, year-to-year scrappage percentages,
c       and initial model-year fractions (age distribution)
c         NOTE:  Uses a single activity in hours per year value (acttmp),
c         since the alternate-activity-curve functionality is not actually
c         being used.  See note in scrptime about required changes to
c         integrate alternate activity curve ---
c
      call scrptime( ierr, yryrfrcscrp, nyrlif, modfrc, uselif,
     &        eload, acttmp, disin, popgrwfac )
c
c   --- calculate the activity adjustment
c       and start adjustment by age ----
c         NOTE:  The scrptime subroutine uses a single activity in hours
c         per year value (acttmp), since the alternate-activity-curve
c         functionality is not actually being used.  If alternate
c         activity curves are ever to be used, the below calculation
c         will have to be integrated into scrptime ---
c
      accum = 0
      if( uselif .LE. 0 ) uselif = 1.0
c      do 40 i=1,MXAGYR
      do 40 i=1,nyrlif
c         if( accum .LT. 2*uselif ) then
c             nyrlif = i
             if ( idxage .LE. 0 )  then
                actadj(i) = acttmp
             else
                if ( accum/uselif .GE. 2 )  then
                   actadj(i) = 0.0
                else if ( accum/uselif .LE. agebin(1) )  then
                   actadj(i) = agepct(idxage,1)/100.0 * acttmp
                else
                   do 35 j = 1, MXUSE
                      if ( accum/uselif .GT. agebin(j) .AND. 
     &                     accum/uselif .LE. agebin(j+1) )  then
                         actadj(i) = agepct(idxage,j)/100.0 * acttmp
                         goto 39
                      endif
   35              continue
   39              continue
                endif
             endif
             stradj(i) = adjfac(i) * strhrs
             accum = accum + actadj(i) * eload
c         endif
   40 continue
c
c   ---- save the age of the engine that is used for
c        deterioration calculation ---
c
      accum = 0. 
      do 50 i=1,nyrlif
        accum = accum + actadj(i) * eload
        if( accum .GT. 0 ) then
           detage(i) = accum / uselif
        else
           detage(i) = 0.0
        endif
  50  continue
c
c   --- return to calling routine ----
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
