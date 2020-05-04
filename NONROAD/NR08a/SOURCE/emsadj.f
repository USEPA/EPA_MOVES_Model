C**** EMSADJ
c
      subroutine emsadj(adjfac,asccod,code, daymthfac)
c
c-----------------------------------------------------------------------
c  
c    This routine calculates the various emission adjustment factors.
c    All of the adjustments are incorporated into a single array of
c    factors, indexed by species.
c
c     Argument description:
c       Outputs:
c          adjfac  R   array of calculated adjustment factors
c          adjtime R   adjustment for time period
c       Inputs:
c          asccod  C   SCC code of equipment
c          code    C   5-digit fip code
c          daymthfac  R  array of daily monthly adjustments
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      06/10/97  --gmw--  original development
c      11/10/99  --mmj--  modified oxygenate correction per NR-003
c                         tech report
c      05/27/04  --dfk--  added temperature corrections for tank and hose 
c                         permeation
c      06/04/04  --dfk--  removed corrections for diurnal (they are
c                         performed by updated Wade equation routine)
c      06/15/04  --dfk--  added optional daily temperature corrections
c      09/22/04  --dfk--  added optional diesel sulfur corrections
C      11/15/04  --dfk--  added temp corrections for rec-marine 3-hoses
C      02/25/05  --cimulus--  changed temperature correction for
c                             tank permeation (now 50% lower with
c                             each 10C (18F) reduction from 85F)
c      03/03/05  --dfk--  separated the time period adjustment from the
c                         emission adjustment due to the way diesel SOX
c                         correction is handled in clcems. Fixed the
c                         time adjustment correction for Winter.
c      07/20/05  --cimulus--  commented variables/functions that were
c                             only used in commented code
c      07/20/05  --cimulus--  commented variables that were set but
c                             only used in commented code
c      11/18/05 --epa-- change 4-stk oxy adjustment for CO from 6.3%
c                       to 6.2% per percent oxygen, per peer review.
c      02/10/06 --epa-- add daymthfac adjustment for exhaust
c                       and all per-activity (not per-day) pollutants.
c      04/03/06 --epa-- add 2280002xxx as dsl marine sulfur adj SCC.
c      04/21/06 --epa-- Fixes all SI Daily Temp/RVP month/season/year. 
c      05/09/06 --epa-- Fixes dsl PM/sulfur calc for Daily & TypDay runs.
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdusr.inc'
      include 'nonrdefc.inc'
      include 'nonrdeqp.inc'
      include 'nonrdio.inc'
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   caludi   R   returns the diurnal "uncontrolled" emission rate
c
c      real*4 caludi
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real*4       adjfac(MXPOL,MXDAYS), daymthfac(MXDAYS)
cc    real*4       adjtime
      character*10 asccod
      character*5  code
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 idxyr, iseas, i
      real*4    temfac, acoeff, soxcor, oxyadj
      integer*4 jday,jbday,jeday,jbskip,jeskip,ireg
      real*4    tamb
      logical   lskip
cold      real*4    stdval, epsval
cold      logical   tmax,tmin
cold      integer*4  jndays
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
cc   --- set looping indexes
c
      call dayloop(jbday,jeday,jbskip,jeskip,lskip)
c
c  set number of days in period
c
cc      if(lskip) then
cc        jndays=31+28+31 ! January+February+December (no leap years)
cc      else
cc        jndays=jeday-jbday+1
cc      end if
c
c  set the time period adjustment factor
c
cc      adjtime=1.0/float(jndays)
c
c  loop over selected period
c
      do jday=jbday,jeday
c
c  if winter and using daily values, skip 1 March to 30 November, inclusive
c
      if(lskip.and.jday.ge.jbskip.and.jday.le.jeskip) cycle
c
c  set temperatures
c
      if(ldayfl) then
         read(code(1:2),*) ireg
cold         tmax=daytmp(jday,1,ireg)
cold         tmin=daytmp(jday,2,ireg)
         tamb=daytmp(jday,3,ireg)
      else
cold         tmax=tempmx
cold         tmin=tempmn
         tamb=amtemp
      end if
c      
c   --- initialize the emission factors to no adjustment ---
c
      do 10 i=1,MXPOL
         adjfac(i,jday) = 1.0  
   10 continue 
c
c   --- assign monthly activity adjustments to days for activity ---
c   --- based EFs (not Disp or Spill) if doing a daily TempRVP run ---
c
      if(ldayfl) then
         do 15 i=1,MXPOL
            if( (i .GE. IDXDIU .AND. i .LE. IDXVNT)
     &        .OR. i .EQ. IDXSPL) cycle
            adjfac(i,jday) = daymthfac(jday)
   15    continue 
      end if
c
c   --- temperature corrections for diurnal emissions ---
c
cold      if( ifuel .EQ. IDXGS2 .OR. ifuel .EQ. IDXGS4 ) then
cold        stdval = caludi( 9.0, 60.0, 84.0, 40.0 ) 
cold        epsval = caludi( fulrvp, tmin, tmax, 40.0 ) 
cold        if( stdval .NE. 0. ) adjfac(IDXDIU) = 
cold     &                                adjfac(IDXDIU) * epsval/stdval
cold      endif
c
c   --- temperature corrections for exhaust emissions ---
c       (NOTE: 2-stroke is included in case data becomes available,
c       we need only change the coefficients ) ----
c
c   --- Gasoline 4-stroke ----
c
      if( ifuel .EQ. IDXGS4 ) then
         if( tamb .LE. 75.0 ) then
             acoeff = -0.00240
             temfac = EXP( acoeff * (tamb - 75.0) )
             adjfac(IDXTHC,jday) = adjfac(IDXTHC,jday) * temfac
c
             acoeff = 0.0015784
             temfac = EXP( acoeff * (tamb - 75.0) )
             adjfac(IDXCO,jday) = adjfac(IDXCO,jday) * temfac
c
             acoeff = -0.00892
             temfac = EXP( acoeff * (tamb - 75.0) )
             adjfac(IDXNOX,jday) = adjfac(IDXNOX,jday) * temfac
         else
             acoeff = 0.00132
             temfac = EXP( acoeff * (tamb - 75.0) )
             adjfac(IDXTHC,jday) = adjfac(IDXTHC,jday) * temfac
c
             acoeff = 0.00375
             temfac = EXP( acoeff * (tamb - 75.0) )
             adjfac(IDXCO,jday) = adjfac(IDXCO,jday) * temfac
c
             acoeff = -0.00873
             temfac = EXP( acoeff * (tamb - 75.0) )
             adjfac(IDXNOX,jday) = adjfac(IDXNOX,jday) * temfac
         endif
      endif
c
c   --- Gasoline 2-stroke ----
c
      if( ifuel .EQ. IDXGS2 ) then
         if( tamb .GE. 75.0 ) then
             acoeff = 0.0
             temfac = EXP( acoeff * (tamb - 75.0) )
             adjfac(IDXTHC,jday) = adjfac(IDXTHC,jday) * temfac
c
             acoeff = 0.0
             temfac = EXP( acoeff * (tamb - 75.0) )
             adjfac(IDXCO,jday) = adjfac(IDXCO,jday) * temfac
c
             acoeff = 0.0
             temfac = EXP( acoeff * (tamb - 75.0) )
             adjfac(IDXNOX,jday) = adjfac(IDXNOX,jday) * temfac
         else
             acoeff = 0.0
             temfac = EXP( acoeff * (tamb - 75.0) )
             adjfac(IDXTHC,jday) = adjfac(IDXTHC,jday) * temfac
c
             acoeff = 0.0
             temfac = EXP( acoeff * (tamb - 75.0) )
             adjfac(IDXCO,jday) = adjfac(IDXCO,jday) * temfac
         endif
      endif
c
c   --- oyxgenate correction to gasoline exhaust ---
c       (NOTE: 2-stroke is included in case data becomes, we need 
c       only change the coefficients ) ----
c
c   --- Gasoline 4-stroke ----
c
      if( .NOT. lrfg .AND. ifuel .EQ. IDXGS4 ) then
          acoeff = 0.045
          oxyadj = 1.0 - ( acoeff * oxypct )
          adjfac(IDXTHC,jday) = adjfac(IDXTHC,jday) * oxyadj
c
          acoeff = 0.062
          oxyadj = 1.0 - ( acoeff * oxypct )
          adjfac(IDXCO,jday) = adjfac(IDXCO,jday) * oxyadj
c
          acoeff = -0.115
          oxyadj = 1.0 - ( acoeff * oxypct )
          adjfac(IDXNOX,jday) = adjfac(IDXNOX,jday) * oxyadj
      endif
c
c   --- Gasoline 2-stroke ----
c
      if( .NOT. lrfg .AND. ifuel .EQ. IDXGS2 ) then
          acoeff = 0.006
          oxyadj = 1.0 - ( acoeff * oxypct )
          adjfac(IDXTHC,jday) = adjfac(IDXTHC,jday) * oxyadj
c
          acoeff = 0.065
          oxyadj = 1.0 - ( acoeff * oxypct )
          adjfac(IDXCO,jday) = adjfac(IDXCO,jday) * oxyadj
c
          acoeff = -0.186
          oxyadj = 1.0 - ( acoeff * oxypct )
          adjfac(IDXNOX,jday) = adjfac(IDXNOX,jday) * oxyadj
      endif
c
c   --- correction for sulfur content on SOx ---
c
      if( .NOT. lrfg .AND. ifuel .GT. 0 .AND. ifuel .LE. IDXCNG ) then
          soxcor = soxful(ifuel) / soxbas(ifuel)
cc
cc  --- alternative diesel sulfur content for rec marine --- 
cc
          if(asccod(1:7) .EQ. '2282020' .OR. 
     &       asccod(1:7) .EQ. '2280002') then
               soxcor = soxdsm / soxbas(ifuel)
          endif
cc
          adjfac(IDXSOX,jday) = adjfac(IDXSOX,jday) * soxcor
      endif
c
c   --- correction for altitude ---
c
      if( lhigh .AND. ifuel .GT. 0 .AND. ifuel .LE. IDXCNG ) then
          do 20 i=IDXTHC,IDXSOX
             adjfac(i,jday) = adjfac(i,jday) * altfac(ifuel)
   20     continue
      endif
c
c   --- correction for RFG ---
c
      iseas = 0
      idxyr = 0
      if( lrfg .AND. imonth .GT. 0 ) iseas = idseas(imonth)
      if( iseas .EQ. IDXWTR .OR. iseas .EQ. IDXSUM ) then
         do 30 i=1,NRFGBIN
             if( iepyr .GE. iyrbin(iseas,i,1) .AND. 
     &                       iepyr .LE. iyrbin(iseas,i,2) ) idxyr = i
   30    continue
c
         if( ifuel .EQ. IDXGS2 .AND. idxyr .NE. 0 ) then
            adjfac(IDXTHC,jday) = adjfac(IDXTHC,jday)
     &                          * rfggs2(iseas,idxyr,IDXTHC) 
            adjfac(IDXCO,jday)  = adjfac(IDXCO,jday)
     &                          * rfggs2(iseas,idxyr,IDXCO) 
            adjfac(IDXNOX,jday) = adjfac(IDXNOX,jday)
     &                          * rfggs2(iseas,idxyr,IDXNOX) 
            adjfac(IDXSOX,jday) = adjfac(IDXSOX,jday)
     &                          * rfggs2(iseas,idxyr,IDXSOX) 
            adjfac(IDXPM,jday)  = adjfac(IDXPM,jday)
     &                          * rfggs2(iseas,idxyr,IDXPM) 
         endif
         if( ifuel .EQ. IDXGS4 .AND. idxyr .NE. 0 ) then
            adjfac(IDXTHC,jday) = adjfac(IDXTHC,jday)
     &                          * rfggs4(iseas,idxyr,IDXTHC) 
            adjfac(IDXCO,jday)  = adjfac(IDXCO,jday) 
     &                          * rfggs4(iseas,idxyr,IDXCO) 
            adjfac(IDXNOX,jday) = adjfac(IDXNOX,jday)
     &                          * rfggs4(iseas,idxyr,IDXNOX) 
            adjfac(IDXSOX,jday) = adjfac(IDXSOX,jday)
     &                          * rfggs4(iseas,idxyr,IDXSOX) 
            adjfac(IDXPM,jday)  = adjfac(IDXPM,jday)
     &                          * rfggs4(iseas,idxyr,IDXPM) 
         endif
      endif
c
c   --- temperature corrections for tank and hose permeations
c   --- (temperature must be in degrees F)
c
      adjfac(IDXTKP,jday) = adjfac(IDXTKP,jday)
     &                    * 3.788519E-2*exp(3.850818E-2*tamb)
      adjfac(IDXHOS,jday) = adjfac(IDXHOS,jday) 
     &                    * 6.013899e-2*exp(3.850818e-2*tamb)
      adjfac(IDXNCK,jday) = adjfac(IDXHOS,jday)
      adjfac(IDXSR ,jday) = adjfac(IDXHOS,jday)
      adjfac(IDXVNT,jday) = adjfac(IDXHOS,jday)
c
      end do ! iday
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end


