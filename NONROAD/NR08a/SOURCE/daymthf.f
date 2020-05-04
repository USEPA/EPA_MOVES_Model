c**** DAYMTHF
c
      subroutine daymthf(asccod, fipin, daymthfac, mthf, dayf, ndays)
c
c-----------------------------------------------------------------------
c
c    Calculates monthly activity factor for every day of year for a 
c    specified engine type. 
c    The return value is a single factor to convert the annual emissions
c    estimate the to period requested by the user.
c
c   Arguments:
c     Outputs:
c       daymthfac R  array of calculated adjustment factors
c       mthf      R  month(s) vs annual adjustment factor
c       ndays     I  number of days spanned by interval (used for 
c                    diurnal & permeation emission caclulations)
c     Inputs:
c       nday    I  which day of year to look at 
c       asccod  C  SCC code to match
c       fipin   C  FIPs code to match
c
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      02/10/06 --epa-- lifted from dailyf & dayloop, and simplified to
c                       just do monthly factors for every day of year
c      04/19/06 --epa-- incorp dailyf() and move outside of emsadj().
c                       Fixes all SI Daily Temp/RVP month/season/year.
c      05/09/06 --epa-- Fixes dsl PM/sulfur calc for Daily & TypDay runs.
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdreg.inc'
      include 'nonrdusr.inc'
      include 'nonrdefc.inc'
      include 'nonrdeqp.inc'
      include 'nonrdtpl.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      character*10 asccod
      character*5  fipin
      real*4       daymthfac(MXDAYS), mthf, dayf
      integer*4    ndays
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------

      integer*4 idxtpd, idxtpm, modays(12)
cc              mostrt(13)
      integer*4 i, jday, jbeg, jend
      real*4    mthin(MXMTH), nmths, dayin(MXDAY)
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data modays /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
cc      data mostrt /1,32,60,91,121,152,182,213,244,274,305,335,366/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- initialize array ---
c
      do 10 i=1,MXDAYS
        daymthfac(i) = 1.
   10 continue
c
cc  if not using daily temps/RVPs for multiple months, return
c
cc      if(.NOT.ldayfl .OR. (iprtyp .EQ. IDXMTH) ) goto 9999
c
c   --- call routine to get the monthly and daily seasonality data ---
c
cc    dailyf = -9.
      call fndtpm( asccod, fipin, idxtpm, idxtpd )
c
c  --- if match found for day-of-week, load the values ---
c
      if( idxtpd .GT. 0 ) then
         do 15 i=1,MXDAY
            dayin(i) = dayfac(i,idxtpd)
   15    continue
c
c  --- otherwise load the defaults ---
c
      else
         do 20 i=1,MXDAY
            dayin(i) = defday(i)
   20    continue
      endif
c
c  --- if match found for month of year, load the values ---
c
      if( idxtpm .GT. 0 ) then
         do 30 i=1,MXMTH
            mthin(i) = mthfac(i,idxtpm)
   30    continue
c
c  --- otherwise load the defaults ---
c
      else
         do 40 i=1,MXMTH
            mthin(i) = defmth(i)
   40    continue
      endif
c
c  --- set number of months ---
c
      if( iprtyp .EQ. IDXANN ) then
         nmths = 12.
      else if( iprtyp .EQ. IDXSES ) then
         nmths = 3.
      else 
         nmths = 1.
      endif
c
c  --- loop over months & days and assign daily monthly factors ---
c  --- assigning same value to every day within a month ---
c
      mthf = 0.
      ndays = 0
      jbeg = 1
      jend = 0
      do 50 i=1,MXMTH
cc
ctst1       if( iprtyp .NE. IDXANN ) then
ctst1         mthin(i) = mthin(i) * REAL(MXDAYS) / REAL(modays(i) * 12)
ctst1       endif
cc
         if( lmonth(i) ) then
            mthf = mthf + mthin(i) 
            ndays = ndays + modays(i)
         endif
         if(ldayfl) then
           jend = jbeg + modays(i) - 1
ctstd           if(iprtyp .NE. IDXANN) then
ctstd              mthin(i) = mthin(i) * REAL(MXDAYS) / REAL(modays(i) * 12)
ctstd           endif
           do 60 jday = jbeg,jend
              daymthfac(jday) = mthin(i) / REAL(modays(i))
ctst              daymthfac(jday) = mthin(i) * nmths
cc            daymthfac(jday) = mthin(i) * 12.
   60 continue
cdiag           write(IOWMSG,'(A, 1X, A, 1X, 4I4, 2F9.6)') 
cdiag     &       fipin, asccod, i, jbeg, jend, jday, mthin(i), 
cdiag     &       daymthfac(jday-1)
           jbeg = jend + 1
         endif
   50 continue
c
c  --- if period is not Typical Day, day-of-week adjustment is 1.0 ---
c
      if( ismtyp .EQ. IDXTOT ) then
          dayf = 1.0
      else
c
c  --- if the period is daily, multiply by 7 to get a weeks worth and 
c      then apply the day of week factor ---
c
cc        ndays = 1
          dayf = 0.
          if( ldays(IDXWKD) ) then 
             dayf = dayin(IDXWKD)
          else
             dayf = dayin(IDXWKE)
          endif
          dayf = 7 * dayf
      endif
c
cdiag      write(IOWMSG,'(A, 1X, A, 1X, I5, 3F9.6, I5)') 
cdiag     &   fipin, asccod, idxtpm, daymthfac(182), mthf, dayf, ndays
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
