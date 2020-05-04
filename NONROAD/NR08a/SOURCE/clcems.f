c**** CLCEMS
c
      subroutine clcems(ierr, emsday, emsbmy,
     &             idxyr, idxtec, idxtch, dage, detcap, adetcf, bdetcf,
     &             idxunt, tfrac, hpval, denful, bsfval, iact, adjems,
     &             adjtime, emsfac, tpltmp, sadj, pop, mfrac, ndays, 
     &             afac )
c
c-----------------------------------------------------------------------
c  
c   This routine calculates emissions
c      Argument descriptions:
c        Outputs:
c          ierr    I   error flag
c          emsday  R   calculated emissions
c          emsbmy  R   calculated emissions by-model-year
c        Inputs:
c          idxyr   I   year index
c          idxtec  I   technology type index
c          idxtch  I   number of technology types 
c          dage    R   age of deterioration
c          detcap  R   cap on age of equip for deterioration calc
c          adetcf  R   A coefficient of deterioration factor equation
c          bdetcf  R   B coefficient of deterioration factor equation
c          idxunt  I   index of unit conversion
c          tfrac   R   technology type fraction
c          hpval   R   horsepower
c          denful  R   fuel density
c          bsfval  R   bsfc value
c          iact    I   activity index
c          adjems  R   adjustment factors
c          adjtime R   time period adjustment factor
c          emsfac  R   emission factors
c          sadj    R   starts hour
c          tpltmp  R   temporal adjustment factor
c          pop     R   population
c          mfrac   R   model year fraction
c          ndays   I   number of period days
c          afac    R   activity adjustment
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      10/8/98  --mmj-- taken from the process routines 'prcxxx'
c      02/10/00 mjimenez added error to diurnal calc if no spillage file
c      01/19/01 charvey adds code for T3 & T4 sulfur PM adjustment.
c      06/11/01 -3 charvey mods vapor disp equation from NEVES to ORVR.
c      06/14/01 -4 charvey adds carbon fractions by fuel type for CO2.
c      06/25/01 -5 charvey fixes carbon fract: IDXGS2/4, not IDXGAS.
c      08/03/01 -6 charvey tries T5 sulf PM adj using ".LT. T4" code
c      08/31/01 -7 charvey/bgiannelli remove unused modscp declaration.
c      09/28/01 -11 charvey fixes SOx calc for LPG & CNG. 
c      10/30/01 -12 charvey allow CI base sulfur input in OPT file packet.
c      02/26/02 -13 charvey: T4 & T4N sulfate conversion 30% from 2.2% for SOX & PM.
c      03/14/02 -14 charvey: fix stage II to only apply to pump refueling.
c      05/27/04 -dfk- changed to only calculate exhaust and start emissions
c      06/15/04 -dfk- added optional daily temperature/RVP corrections
c      09/21/04 -dfk- changed emsbmy to sum over all days
c      09/22/04 -dfk- added rec marine sulfur code from NR04n2.
c      01/11/05 -dfk- added code to process months, seasons, or annual periods
c      03/03/05 -dfk- separated the time period adjustment from the
c                     emission adjustment due to the way diesel SOX
c                     is handled in clcems.
c      05/23/05 -cimulus- apply the retrofit reduction, if any
c      06/22/05 -EPA- fix detcap handling for other than 1.0 medlife.
c      07/19/05 -cimulus- removed unused variables/functions
c      07/20/05 -cimulus- floating-point comparison for equality okay;
c                         sulbas values are hard-coded or read from
c                         file, rather than being calculated at runtime
c      07/29/05 -cimulus- removed unused arguments refmod and fulcsm
c      02/12/06 -epa- set tpltmp2 to 1 if Daily Temp/RVP, handled by daymthf
c                     This gives properly monthly-activity weighted ANNUAL or 
c                     MONTHLY total values, but NOT seasonal or typical day.
c      04/19/06 --epa-- Fixes all SI Daily Temp/RVP month/season/year. 
c      05/09/06 --epa-- Fixes dsl PM/sulfur calc for Daily & TypDay runs.
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdefc.inc'
      include 'nonrdeqp.inc'
      include 'nonrdact.inc'
      include 'nonrdusr.inc'
      include 'nonrdrtrft.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real*4      emsday(MXPOL)
      real*4      emsbmy(MXPOL)
c
      integer*4   ierr
      integer*4   idxyr
      integer*4   idxtec
      integer*4   idxtch
      integer*4   idxunt(MXPOL,MXTECH)
      integer*4   iact
      integer*4   ndays
      real*4      dage
      real*4      detcap(MXPOL,MXTECH)
      real*4      adetcf(MXPOL,MXTECH)
      real*4      bdetcf(MXPOL,MXTECH)
      real*4      tfrac
      real*4      hpval
      real*4      denful
      real*4      bsfval
      real*4      adjems(MXPOL,MXDAYS), adjtime
      real*4      emsfac(MXAGYR,MXPOL,MXTECH)
      real*4      sadj
      real*4      tpltmp
      real*4      pop
      real*4      mfrac
      real*4      afac
      real*4      soxcnv
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   unitcf   R   returns unit conversion factor
c   fndchr   I   returns the index of string in array of strings  
c
      real*4    unitcf
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4    idxspc, idxalt
      real*4       cvtbck, sulbas
      real*4       detrat, emstmp,  emiss, cvttmp, emsthc
      real*4       cfrac, temiss
      integer*4    jday,jbday,jeday,jbskip,jeskip
      logical      lskip
      real*4       tpltmp2
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- Do not modify tpltmp, which gets used later in PRC* ---
c
       tpltmp2 = tpltmp
c
c   --- set looping indexes
c
      call dayloop(jbday,jeday,jbskip,jeskip,lskip)
cc
cc    if(ldayfl .AND. iprtyp .NE. IDXMTH) tpltmp2 = 1.
cc
c  loop over selected period
c
      do jday=jbday,jeday
c
c  if winter and using daily values, skip 1 March to 30 November, inclusive
c
      if(lskip.and.jday.ge.jbskip.and.jday.le.jeskip) cycle
c
c --- calculate emissions for each exhaust or start pollutant ---
c
       do 90 idxspc=1,MXPOL
c
       if(idxspc.ge.IDXDIU.and.idxspc.le.IDXRLS) cycle
c
c   --- skip if emission factor file was not supplied ---
c
       if( .NOT. lfacfl(idxspc) .AND. .NOT. 
     &          (idxspc .EQ. IDXCO2 .OR. idxspc .EQ. IDXSOX .OR.     
     &                                 idxspc .EQ. IDXDIS) ) cycle
c
c  --- calculate the detrioration rate from values retrieved earlier ----
c
          if( dage .LE. detcap(idxspc,idxtec) ) then
              detrat = 1. + adetcf(idxspc,idxtec) * 
     &                         dage**bdetcf(idxspc,idxtec)
          else
              detrat = 1. + adetcf(idxspc,idxtec) *
     &                     detcap(idxspc,idxtec)**bdetcf(idxspc,idxtec)
          endif
c
c  --- call routine to get the units converion for this species ---
c
          cvttmp = unitcf( idxunt(idxspc,idxtec),hpval,iact,
     &                                    denful,bsfval )
          emstmp = emsfac(idxyr,idxspc,idxtec) * cvttmp * detrat * 
cc     &                 adjems(idxspc,jday)
     &                 adjems(idxspc,jday) * adjtime
CTEST      if(idxspc.eq.IDXTHC.OR.idxspc.eq.IDXNOX)
CTEST     &write(99,99) idxyr,idxspc,idxtec,adetcf(idxspc,idxtec),
CTEST     &bdetcf(idxspc,idxtec),dage,detrat,emstmp,
CTEST     &emsfac(idxyr,idxspc,idxtec),cvttmp,adjems(idxspc,jday) * adjtime
CTEST  99  format(3i3,8es16.6)
c
c   --- save the THC exhaust emission factor for use later ---
c
          if( idxspc .EQ. IDXTHC ) emsthc = 
     &                emsfac(idxyr,idxspc,idxtec) * cvttmp * detrat
c
c   --- calculate the SOx emissions from the HC emission factor ---
c
          if( idxspc .EQ. IDXSOX .AND. tfrac .GT. 0. ) then
              cvtbck = 1.0 / (hpval * faclod(iact) )
cc
              soxcnv = soxfrc(ifuel)
c
c  --- look for alternate sulfur conversion factor by tech type ---
c
              idxalt = fndchr( 
     &                      tectyp(idxtch,idxtec), 10, sultec, numalt )
              if( idxalt .GT. 0) then
                if( sulcnv(idxalt) .GE. 0.) soxcnv = sulcnv(idxalt)
              end if
c
              emsfac(idxyr,idxspc,idxtec) = hpval * faclod(iact) * 
     &           ( bsfval * 453.6 * (1.0 - soxcnv)
     &             - emsthc*cvtbck ) * 0.01*soxbas(ifuel) * 2.0
              emstmp = emsfac(idxyr,idxspc,idxtec) * 
cc     &                 adjems(idxspc,jday)
     &                 adjems(idxspc,jday) * adjtime
          endif
c
c  --- calculate the Crankcase emission factor from HC emission factor ----
c
          if( idxspc .EQ. IDXCRA .AND. tfrac .GT. 0 
     &                  .AND. emsfac(idxyr,IDXTHC,idxtec) .GT. 0.0 
     &                      .AND. emsfac(idxyr,IDXCRA,idxtec) .GT. 0.0 )
     &          emstmp = emsfac(idxyr,idxspc,idxtec) * emsthc * 
cc     &                   adjems(idxspc,jday)
     &                   adjems(idxspc,jday) * adjtime
c
c  --- special case for CO2, CO2 emissions are just a function of BSFC ---
c
          if( idxspc .EQ. IDXCO2 .AND. tfrac .GT. 0 ) then
              cvtbck = 1.0 / (hpval * faclod(iact) )
              if( ifuel .EQ. IDXGS2 .OR. ifuel .EQ. IDXGS4 ) then
                cfrac = CMFGAS
              elseif( ifuel .EQ. IDXDSL ) then
                cfrac = CMFDSL
              elseif( ifuel .EQ. IDXLPG ) then
                cfrac = CMFLPG
              elseif( ifuel .EQ. IDXCNG ) then
                cfrac = CMFCNG
              endif
              emsfac(idxyr,idxspc,idxtec) =  hpval * faclod(iact) * 
     &           ( bsfval * 453.6 - emsthc*cvtbck ) * cfrac * 44./12.
             emstmp = emsfac(idxyr,idxspc,idxtec) * detrat *
cc     &                adjems(idxspc,jday)
     &                adjems(idxspc,jday) * adjtime
          endif 
c
c  --- Correction for sulfur content to PM, need to wait until
c      now because the correction factor uses BSFC which is by
c      technology type.  ---
c
          if( idxspc .EQ. IDXPM .AND. ifuel .EQ. IDXDSL) then
            sulbas = soxbas(IDXDSL)
c
c  --- Loop check if there is any alternate base sulfur level
c      for this techtype for PM calc only. ---
c
            idxalt = fndchr( tectyp(idxtch,idxtec), 10, sultec, numalt)
            if( idxalt .GT. 0 ) then
              if(sulalt(idxalt) .GE. 0. ) sulbas = sulalt(idxalt)
            end if
c
c  --- '1.0' means no PM adjustment: in-use expected to equal cert sulfur.
c       (but it is not a multiplicative factor).
c
            if( sulbas .NE. 1.0 ) then ! floating-point comparison for equality okay; sulbas values are hard-coded or read from file, rather than being calculated at runtime
c
              soxcnv = soxfrc(ifuel)
              if( idxalt .GT. 0 ) then
                if( sulcnv(idxalt) .GE. 0.) soxcnv = sulcnv(idxalt)
              end if
c
cc --- Calc sulfur-adjusted PM.  The adjems(IDXSOX) handles marine soxdsm
c
cdiag        if(hpval .LE. 16.0 .AND. idxyr .EQ. 1 .AND. idxtec .EQ. 2) then
cxdiag        if(hpval .LE. 16.0 .AND. idxyr .EQ. 1 .AND. jday .EQ. 1) then
cdiag          write(IOWMSG,'(1X,3I4,F7.2,F7.4,F10.7,F7.4,3F10.7,F7.4)')
cdiag     &    idxyr, idxtec, jday, hpval, bsfval, emstmp, sulbas, soxcnv,
cdiag     &    adjems(IDXPM, jday), adjems(IDXSOX, jday), adjtime
cdiag        endif
c
              emstmp = emstmp - bsfval * 453.6 * hpval *
     &          faclod(iact) * 7.0 * soxcnv * 0.01 * adjtime *
     &          (sulbas * adjems(IDXPM,jday) -
     &          soxbas(IDXDSL) * adjems(IDXSOX,jday))
            endif
          endif
c
c  ---- Starts depend on number of starts ---
c
          if(idxspc .GE. IDSTHC) then
             emiss = emstmp * sadj * 
     &            tpltmp2 * pop * mfrac * tchfrc(idxtch,idxtec)
c
c  ---- "normal" exhaust species ---
c
          else
             if( idxunt(idxspc,idxtec) .EQ. IDXGDY ) then
                 emiss = emstmp * ndays * tpltmp2 *
     &               pop * mfrac * tchfrc(idxtch,idxtec)
             else
                 emiss = emstmp * afac * tpltmp2 *
     &               pop * mfrac * tchfrc(idxtch,idxtec)
             endif
          endif
c
          if( emsfac(idxyr,idxspc,idxtec) .LT. 0.
     &                         .OR. emsday(idxspc) .LT. 0.0 ) then
              emsday(idxspc) = RMISS
              emsbmy(idxspc) = RMISS
          else
c
c           --- apply the retrofit reduction, if any ---
c
              if( rtrftplltntrdfrc(idxspc) .GT. 0. ) then
                  emiss = emiss * (1. - rtrftplltntrdfrc(idxspc))
              endif
c
              temiss = emiss * CVTTON
              emsday(idxspc) = emsday(idxspc) + temiss
              emsbmy(idxspc) = emsbmy(idxspc) + temiss
          endif
c
   90  continue
c
      end do  ! jday
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end



