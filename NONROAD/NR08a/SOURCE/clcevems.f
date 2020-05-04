c**** CLCEVEMS
c
      subroutine clcevems(ierr, asccod, emsday, emsbmy, idxyr, idxtec,
     &                    idxtch, dage, detcap, adetcf, bdetcf, idxunt,
     &                    tfrac, hpval, adjems, adjtime, emsfac, tpltmp,
     &                    sadj, pop, mfrac, ndays, tvol, afac, refmod, 
     &                    fulcsm, tfull, tmfrac, hoselen, hosedia, 
     &                    hmfrac, necklen, neckdia, supretlen, 
     &                    supretdia, ventlen, ventdia, hsstrt, diufrac, 
     &                    tnke10fac, hose10fac, ncke10fac, sre10fac, 
     &                    vnte10fac, code )
c
c-----------------------------------------------------------------------
c  
c   This routine calculates emissions
c      Argument descriptions:
c        Outputs:
c          ierr      I   error flag
c          emsday    R   calculated emissions
c          emsbmy    R   calculated emissions by-model-year
c        Inputs:
c          idxyr     I   year index
c          idxtec    I   technology type index
c          idxtch    I   number of technology types 
c          dage      R   age of deterioration
c          detcap    R   cap on age of equip for deterioration calc
c          adetcf    R   A coefficient of deterioration factor equation
c          bdetcf    R   B coefficient of deterioration factor equation
c          idxunt    I   index of unit conversion
c          tfrac     R   technology type fraction
c          hpval     R   horsepower
c          adjems    R   adjustment factors
c          adjtime   R   time period adjustment factor
c          emsfac    R   emission factors
c          sadj      R   starts hour
c          tpltmp    R   temporal adjustment factor
c          pop       R   population
c          mfrac     R   model year fraction
c          ndays     I   number of period days
c          tvol      R   tank volume (gallons)
c          tfull     R   fraction of tank filled with fuel
c          tmfrac    R   tank metal fraction
c          hmfrac    R   non rec marine hose metal fraction
c          hosedia   R   non rec marine hose diameter (meters)
c          hoselen   R   non rec marine hose length (meters)
c          neckdia   R   rec marine fill neck hose diameter (meters)
c          necklen   R   rec marine fill neck hose length (meters)
c          supretdia R   rec marine suppy/return hose diameter (meters)
c          supretlen R   rec marine supply/return hose length (meters)
c          ventdia   R   rec marine vent hose diameter (meters)
c          ventlen   R   rec marine vent hose length (meters)
c          afac      R   activity adjustment
c          refmod    C   refueling mode
c          fulcsm    R   fuel consumption
c          hsstrt    R   hot soak starts per hour of operation
c          diufrac   R   diurnal fractions
c          code      C   5-digit fips code
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      06/17/04  --dfk-- original development (from clcems)
c      10/13/04  --dfk-- changed hose permeation calc from multiplier to EF
c                        changed tank permeation calc from multiplier to EF
c      11/15/04  --dfk-- changed rec-marine hose permeation into 3 sources
c      12/06/04  --dfk-- changed diurnal calcs to apply Wade Eqgn 0.78 
c                        adjustment factor to all portable plastic containers
c      01/11/05  --dfk-- added code to process months, seasons, or annual 
c                        periods
c      01/13/05  --dfk-- added 40F limits to tmax/tmin for diurnal calculations
c      03/03/05  --dfk-- separated the time period adjustment from the
c                        emission adjustment due to the way diesel SOX
c                        is handled in clcems.
c      05/11/05  --epa-- remove spillage for non-gasoline fuels, allowing
c                        for easy re-inclusion if ever desired. 
c      06/22/05  --epa-- fix detcap handling for other than 1.0 medlife.
c      07/19/05  --cimulus-- removed unused variables/functions
c      07/20/05  --cimulus-- floating-point comparison for equality okay;
c                            diufrac values are read from file, rather
c                            than being calculated at runtime
c      08/17/05  --epa-- bypass diurnal calcs if tmax <= 40F. Fixes small float
c                        error that yielded negative VapGen in wadeeq.for. 
c                        add negative diurnal error check. 
c      11/16/05  --epa-- Remove incorrect cap on hot soak calc 260 hrs/yr.
c                        (260 soaks/year cap is used externally to calc the
c                        soaks/hour input)
c      02/08/06  --epa-- Remove tpltmp adjustment from Diurnal, Tank,
c                        and all Hose calcs, since it doesn't apply to
c                        emiss/day input types.
c      02/10/06 -epa- set tpltmp2 to 1 if Daily Temp/RVP, handled by daymthf
c      04/21/06  --epa-- Fixes all Daily Temp/RVP month/season/year. 
c      09/22/06  --epa-- Add calc of ethanol blend hose & tank permeation.
c      09/29/06  --epa-- Fix ethanol adj factor contrl tech type recognition.
c      11/20/06  --epa-- Fix ethanol adj factor above E20 cap from 85.0 to 0.85 
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
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real*4      emsday(MXPOL)
      real*4      emsbmy(MXPOL)
c
      character*10 asccod
      character*9  refmod
      character*5  code
      integer*4    ierr
      integer*4    idxyr
      integer*4    idxtec
      integer*4    idxtch
      integer*4    idxunt(MXPOL,MXEVTECH)
      integer*4    ndays
      real*4       dage
      real*4       detcap(MXPOL,MXEVTECH)
      real*4       adetcf(MXPOL,MXEVTECH)
      real*4       bdetcf(MXPOL,MXEVTECH)
      real*4       tfrac
      real*4       hpval
      real*4       adjems(MXPOL,MXDAYS), adjtime
      real*4       emsfac(MXAGYR,MXPOL,MXEVTECH)
      real*4       sadj
      real*4       tpltmp
      real*4       pop
      real*4       mfrac
      real*4       tvol
      real*4       tfull
      real*4       tmfrac
      real*4       hmfrac
      real*4       hosedia
      real*4       hoselen
      real*4       afac
      real*4       fulcsm
      real*4       stg2tmp
      real*4       hsstrt
      real*4       diufrac(5,MXEVTECH)
      real*4       tnke10fac, hose10fac, ncke10fac, sre10fac, vnte10fac
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   wadeeq   R   returns the wade equation diurnal emission factor 
c
      real*4    wadeeq
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4    idxspc
      real*4       tempds
      real*4       detrat, emstmp,  emiss
      real*4       sfcarea
      real*4       tankef
      real*4       hoseef
      real*4       permef
      integer*4    ib
      integer*4    ie
      real*4       tmax,tmin,tamb,tavg,dtmax,dtmin,swing,trvp,temiss
      integer*4    jday,jbday,jeday,j,ireg,jbskip,jeskip
      real*4       necklen,neckdia,supretlen,supretdia,ventlen,ventdia
      real*4       pi,diutmax,diutmin
      logical      lskip
      real*4       tpltmp2
      real*4       ethpwr, ethmktfrc, ethvfrc, temeth, e10fac
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
c   --- set pi and hose ef
c
      pi=3.141592657
      hoseef = 0.
      ethpwr = 0.40
      ethmktfrc = 0.01 * ethmkt
      ethvfrc = 0.01 * ethvpct
      e10fac = 1.0
c
c   --- set looping indexes
c
      call dayloop(jbday,jeday,jbskip,jeskip,lskip)
c
cc
cc    if(ldayfl .AND. iprtyp .NE. IDXMTH) tpltmp2 = 1.
cc
c  --- loop over selected period
c
      do jday=jbday,jeday
c
c  if winter and using daily values, skip 1 March to 30 November, inclusive
c
      if(lskip.and.jday.ge.jbskip.and.jday.le.jeskip) cycle
c
c   --- set evap loop parameters based on fuel: only gas powered equipment
c       have all evap species; however all fuels have spillage.
c       also set diurnal temperatures and RVP
c
       if(ifuel.eq.IDXGS2 .or. ifuel.eq.IDXGS4) then
         ib=IDXDIU
         ie=IDXRLS
         if(ldayfl) then
            read(code(1:2),*) ireg
            tmax=daytmp(jday,1,ireg)
            tmin=daytmp(jday,2,ireg)
            tamb=daytmp(jday,3,ireg)
            trvp=dayrvp(jday,  ireg)
         else
            tmax=tempmx
            tmin=tempmn
            tamb=amtemp
            trvp=fulrvp
         end if      
       else
         ib=IDXSPL
         ie=IDXSPL
       end if
c
c --- calculate emissions for each evap pollutant ---
c
       do 100 idxspc=ib,ie
c
c   --- skip if emission factor file was not supplied ---
c
          if( .NOT. lfacfl(idxspc) .AND. .NOT. 
     &          (idxspc .EQ. IDXCO2 .OR. idxspc .EQ. IDXSOX .OR. 
     &                                 idxspc .EQ. IDXDIS) ) cycle
c
c  --- calculate the deterioration rate from values retrieved earlier ----
c
          if( dage .LE. detcap(idxspc,idxtec) ) then
              detrat = 1. + adetcf(idxspc,idxtec) * 
     &                         dage**bdetcf(idxspc,idxtec)
          else
              detrat = 1. + adetcf(idxspc,idxtec) *
     &                     detcap(idxspc,idxtec)**bdetcf(idxspc,idxtec)
          endif
c
c  --- get the emission factor or multiplication correction factor for this species ---
c
          emstmp = emsfac(idxyr,idxspc,idxtec) 
c
c  --- spillage emissions are a function of tank volume  ---
c  --- different for container or gas pump (gas only)    ---
c
          if( idxspc .EQ. IDXSPL )  then
c
c  --- if ever want to include spillage from non-gasoline fuels, just need
c  --- to remove the following outer if/else/endif check for GS2 & GS4.
c
             if(ifuel.eq.IDXGS2 .or. ifuel.eq.IDXGS4) then
                if ( lfacfl(IDXSPL) .AND. refmod .NE. '         ' ) then
                   if( refmod .EQ. PUMP ) then
                     emiss = PMPFAC / tvol
                   elseif ( refmod .EQ. CNTR )  then
                     emiss = CNTFAC / tvol
                   endif
                else
                   emiss = 0.0
                endif
             else
                emiss = 0.0
             endif
cc             emiss = emiss * adjems(idxspc,jday)
ccx             emiss = emiss * adjems(idxspc,jday) * adjtime
             temiss = emiss * CVTTON * fulcsm
             if(ldayfl) temiss = temiss / float(ndays)
c
             emsday(idxspc) = emsday(idxspc) + temiss
             emsbmy(idxspc) = emsbmy(idxspc) + temiss
             goto 90
          endif
c
c  ---- vapor displacement emissions are a function of ambient temp and RVP ---
c  ---  different for container or gas pump (gas only)                      ---
c
          if( idxspc .EQ. IDXDIS )  then
             if ( lfacfl(IDXSPL) .AND. refmod .NE. '         ' ) then
                   if( refmod .EQ. PUMP ) then
                     tempds =  62 + 0.6 * (tamb - 62.)
                   elseif (refmod .EQ. CNTR )  then
                     tempds = tamb
                   endif
                   emiss = EXP( -1.2798 - 0.0049 * (tempds - tamb)
     &                      + 0.0203 * tempds + 0.1315 * trvp )
c
c                  emiss =  -5.909 - 0.0949 * (tamb - tempds) 
c    &                       + 0.0884 * tempds + 0.485 * trvp
                else
                   emiss = 0.0
                endif
                if( refmod .EQ. PUMP ) then
                  stg2tmp = stg2fac
                else
                  stg2tmp = 1.0
                endif
cc               emiss = emiss * adjems(idxspc,jday)
ccx               emiss = emiss * adjems(idxspc,jday) * adjtime
               temiss = emiss * CVTTON * fulcsm * stg2tmp
cc               if(ldayfl) temiss = temiss / float(ndays)
               if(ldayfl) then
                  if(iprtyp .EQ. IDXANN) then
                     temiss = temiss * adjems(idxspc,jday)
                  else
                     temiss = temiss / float(ndays)
                  endif
               endif
c
               emsday(idxspc) = emsday(idxspc) + temiss
               emsbmy(idxspc) = emsbmy(idxspc) + temiss
             goto 90
          endif
c
c  --- tank permeation emissions are a function of tank surface area and metal fraction  ---
c
          if( idxspc .EQ. IDXTKP )  then
             if ( lfacfl(IDXTKP) .AND. lfacfl(IDXSPL) ) then
                 sfcarea=0.15*sqrt((((tvol+2.0)**2)/(2.0**2))-1.)
                   emiss = emstmp * (1.0-tmfrac) * sfcarea
              else
                 emiss = 0.0
              endif
              tankef=emiss ! store uncorrected tank EF (g/day) for use with HS/RL
              temiss = emiss * CVTTON * adjems(idxspc,jday) *
     &         adjtime * detrat *
ccx     &         adjtime * ndays * detrat * tpltmp2 * 
     &         pop * mfrac * evtchfrc(idxtch,idxtec)
c
c --- Ethanol Blend Permeation Effect
c --- If E10fac input is 1.0, also assume no EtOH effect on control techtypes
c --- Limit EtOH effect to what happens at E85. 
c
            if ( ethmktfrc .GT. 0.0 .AND. ethvfrc .GT. 0.0 ) then
               e10fac = tnke10fac
               if( evtectyp(idxtch,idxtec)(3:3) .NE. '0'
     &             .AND. e10fac .NE. 1. ) e10fac = 2.0  ! floating-point comparison for equality okay; e10fac values are read from file, rather than being calculated at runtime
               if ( ethvfrc .LE. 0.2 ) then
                 temeth = temiss * (1 + (e10fac - 1) 
     &             * (10 * ethvfrc) ** ethpwr)
               else
                 temeth = temiss * (1 + (e10fac - 1) 
     &             * (10 * 0.2) ** ethpwr)
     &             * (1 - ((MIN(ethvfrc, 0.85) - 0.2)/0.8)
     &             ** (1/ethpwr))
               endif
               temiss = temiss * (1 - ethmktfrc) + temeth * ethmktfrc
            endif
c
              if(.NOT. ldayfl) temiss = temiss * ndays
c
              emsbmy(idxspc) = emsbmy(idxspc) + temiss
              emsday(idxspc) = emsday(idxspc) + temiss
cc
c      if( asccod .EQ. '2265004010' ) then
c        write(IOWMSG,'(A, 1X, F9.3, 2I5, 1X, A, 3F9.6)') 
c     &   asccod, hpval, idxtec, idxtch, evtectyp(idxtch,idxtec), 
c     &   tankef, e10fac, temiss
c      endif
cc     &   adjems(idxspc,jday), adjtime, ndays, tpltmp2
cc
            goto 90
          endif
c
c  --- hose permeation emissions are a function of hose diameter, length and metal fraction  ---
c
c  --- all non rec marine
          if( idxspc .EQ. IDXHOS )  then
           if ( lfacfl(IDXHOS) .AND. lfacfl(IDXSPL) ) then
               sfcarea=pi*hoselen*hosedia
                 emiss = emstmp * (1.0-hmfrac) * sfcarea ! non-rec-marine
            else
               emiss = 0.0
            endif
            hoseef=hoseef + emiss ! store uncorrected hose EF (g/day) for use with HS/RL
            temiss = emiss * CVTTON * detrat *
     &       adjems(idxspc,jday) * adjtime *
cc     &       adjems(idxspc,jday) * adjtime * tpltmp2 *
     &       pop * mfrac * evtchfrc(idxtch,idxtec)
c
c --- ethanol blend effect
c
            if ( ethmktfrc .GT. 0.0 .AND. ethvfrc .GT. 0.0 ) then
               e10fac = hose10fac
               if( evtectyp(idxtch,idxtec)(4:4) .NE. '0'
     &             .AND. e10fac .NE. 1. ) e10fac = 2.0  ! floating-point comparison for equality okay; e10fac values are read from file, rather than being calculated at runtime
               if ( ethvfrc .LE. 0.2 ) then
                 temeth = temiss * (1 + (e10fac - 1) 
     &             * (10 * ethvfrc) ** ethpwr)
               else
                 temeth = temiss * (1 + (e10fac - 1) 
     &             * (10 * 0.2) ** ethpwr)
     &             * (1 - ((MIN(ethvfrc, 0.85) - 0.2)/0.8)
     &             ** (1/ethpwr))
               endif
               temiss = temiss * (1 - ethmktfrc) + temeth * ethmktfrc
            endif
c
            if(.NOT. ldayfl) temiss = temiss * ndays
c
            emsbmy(idxspc) = emsbmy(idxspc) + temiss
            emsday(idxspc) = emsday(idxspc) + temiss
           goto 90
          endif
c
c  --- rec marine fill neck
          if( idxspc .EQ. IDXNCK )  then
            if ( lfacfl(IDXNCK) .AND. lfacfl(IDXSPL) ) then
               sfcarea=pi*necklen*neckdia
                 emiss = emstmp * sfcarea ! rec-marine hoses are 100% non-metal 
            else
               emiss = 0.0
            endif
            hoseef=hoseef + emiss ! store uncorrected hose EF (g/day) for use with HS/RL
            temiss = emiss * CVTTON * detrat *
     &       adjems(idxspc,jday) * adjtime *
cc     &       adjems(idxspc,jday) * adjtime * tpltmp2 *
     &       pop * mfrac * evtchfrc(idxtch,idxtec)
c
c --- ethanol blend effect
c
            if ( ethmktfrc .GT. 0.0 .AND. ethvfrc .GT. 0.0 ) then
               e10fac = ncke10fac
               if( evtectyp(idxtch,idxtec)(4:4) .NE. '0'
     &             .AND. e10fac .NE. 1. ) e10fac = 2.0  ! floating-point comparison for equality okay; e10fac values are read from file, rather than being calculated at runtime
               if ( ethvfrc .LE. 0.2 ) then
                 temeth = temiss * (1 + (e10fac - 1) 
     &             * (10 * ethvfrc) ** ethpwr)
               else
                 temeth = temiss * (1 + (e10fac - 1) 
     &             * (10 * 0.2) ** ethpwr)
     &             * (1 - ((MIN(ethvfrc, 0.85) - 0.2)/0.8)
     &             ** (1/ethpwr))
               endif
               temiss = temiss * (1 - ethmktfrc) + temeth * ethmktfrc
            endif
c
            if(.NOT. ldayfl) temiss = temiss * ndays
c
            emsbmy(idxspc) = emsbmy(idxspc) + temiss
            emsday(idxspc) = emsday(idxspc) + temiss
           goto 90
          endif
c
c  --- rec marine supply/return
          if( idxspc .EQ. IDXSR )  then
           if ( lfacfl(IDXSR) .AND. lfacfl(IDXSPL) ) then
               sfcarea=pi*supretlen*supretdia
                 emiss = emstmp * sfcarea ! rec-marine hoses are 100% non-metal 
            else
               emiss = 0.0
            endif
            hoseef=hoseef + emiss ! store uncorrected hose EF (g/day) for use with HS/RL
            temiss = emiss * CVTTON * detrat *
     &       adjems(idxspc,jday) * adjtime *
cc     &       adjems(idxspc,jday) * adjtime * tpltmp2 * 
     &       pop * mfrac * evtchfrc(idxtch,idxtec)
c
c --- ethanol blend effect
c
            if ( ethmktfrc .GT. 0.0 .AND. ethvfrc .GT. 0.0 ) then
               e10fac = sre10fac
               if( evtectyp(idxtch,idxtec)(4:4) .NE. '0'
     &             .AND. e10fac .NE. 1. ) e10fac = 2.0  ! floating-point comparison for equality okay; e10fac values are read from file, rather than being calculated at runtime
               if ( ethvfrc .LE. 0.2 ) then
                 temeth = temiss * (1 + (e10fac - 1) 
     &             * (10 * ethvfrc) ** ethpwr)
               else
                 temeth = temiss * (1 + (e10fac - 1) 
     &             * (10 * 0.2) ** ethpwr)
     &             * (1 - ((MIN(ethvfrc, 0.85) - 0.2)/0.8)
     &             ** (1/ethpwr))
               endif
               temiss = temiss * (1 - ethmktfrc) + temeth * ethmktfrc
            endif
c
            if(.NOT. ldayfl) temiss = temiss * ndays
c
            emsbmy(idxspc) = emsbmy(idxspc) + temiss
            emsday(idxspc) = emsday(idxspc) + temiss
           goto 90
          endif
c
c  --- rec marine vent
          if( idxspc .EQ. IDXVNT )  then
           if ( lfacfl(IDXVNT) .AND. lfacfl(IDXSPL) ) then
               sfcarea=pi*ventlen*ventdia
                 emiss = emstmp * sfcarea ! rec-marine hoses are 100% non-metal 
            else
               emiss = 0.0
            endif
            hoseef=hoseef + emiss ! store uncorrected hose EF (g/day) for use with HS/RL
            temiss = emiss * CVTTON * detrat *
     &       adjems(idxspc,jday) * adjtime *
cc     &       adjems(idxspc,jday) * adjtime * tpltmp2 *
     &       pop * mfrac * evtchfrc(idxtch,idxtec)
c
c --- ethanol blend effect
c
            if ( ethmktfrc .GT. 0.0 .AND. ethvfrc .GT. 0.0 ) then
               e10fac = vnte10fac
               if( evtectyp(idxtch,idxtec)(4:4) .NE. '0'
     &             .AND. e10fac .NE. 1. ) e10fac = 2.0  ! floating-point comparison for equality okay; e10fac values are read from file, rather than being calculated at runtime
               if ( ethvfrc .LE. 0.2 ) then
                 temeth = temiss * (1 + (e10fac - 1) 
     &             * (10 * ethvfrc) ** ethpwr)
               else
                 temeth = temiss * (1 + (e10fac - 1) 
     &             * (10 * 0.2) ** ethpwr)
     &             * (1 - ((MIN(ethvfrc, 0.85) - 0.2)/0.8)
     &             ** (1/ethpwr))
               endif
               temiss = temiss * (1 - ethmktfrc) + temeth * ethmktfrc
            endif
c
            if(.NOT. ldayfl) temiss = temiss * ndays
c
            emsbmy(idxspc) = emsbmy(idxspc) + temiss
            emsday(idxspc) = emsday(idxspc) + temiss
           goto 90
          endif
c
c  ---- Hot soak depends on activity and starts/hour ---
c       Remove 3-hours worth of tank and hose permeation from hot soaks
c
          if( idxspc .EQ. IDXSOK) then
            if ( lfacfl(IDXSOK) .AND. lfacfl(IDXSPL) ) then
ctd              permef = (tankef+hoseef)*3./24. ! 3 hours uncorrected permeation
              permef = 0.
             if(emstmp .ge. permef) then
ctest              write(*,*) emstmp,permef,tankef,hoseef
              emiss = emstmp - permef ! remove uncorrected permeation
              emiss = afac * hsstrt * emiss ! hrs * soaks/hr * g/event
             else
              goto 7000
             end if
            else
              emiss = 0.0
            end if
            temiss = emiss * CVTTON * detrat * tpltmp2 * 
     &       adjems(idxspc,jday) * adjtime * 
     &       pop * mfrac * evtchfrc(idxtch,idxtec)
            emsbmy(idxspc) = emsbmy(idxspc) + temiss
            emsday(idxspc) = emsday(idxspc) + temiss
            goto 90
          endif
c
c  ---- Running Loss depends on activity ---
c       Remove 40 minuntes of tank and hose permeation from running loss
c
          if( idxspc .EQ. IDXRLS) then
            if ( lfacfl(IDXRLS) ) then
ctd            permef = (tankef+hoseef)*40./(24.*60.) ! 40 minutes uncorrected permeation
              permef = 0.
            If(emstmp .ge. permef) then
ctest              write(*,*) emstmp,permef,tankef,hoseef
              emiss = emstmp - permef ! remove uncorrected permeation
              emiss = emiss * afac 
            else
              go to 7002
            end if
            else
              emiss = 0.0
            end if
ctest            WRITE(99,*) idxspc,jday,tmax,tmin,tamb,trvp,ndays,emiss,
ctest     &afac,detrat,
ctest     &tpltmp2,adjems(idxspc,jday)*adjtime,pop,mfrac,evtchfrc(idxtch,idxtec)
            temiss = emiss * CVTTON * detrat * tpltmp2 * 
     &       adjems(idxspc,jday) * adjtime * 
     &       pop * mfrac * evtchfrc(idxtch,idxtec)
            emsbmy(idxspc) = emsbmy(idxspc) + temiss
            emsday(idxspc) = emsday(idxspc) + temiss
            goto 90
          endif
c
c  ---- diurnal emissions are different ---
c        j=1-4: for all non-rec marine and rec-marine portable plastic, multiply Wade Eq by 0.78
c        j=2,4: for rec-marine tanks on trailer, reduce temperature swing by 50%
c        j=3,5: for rec-marine tanks in water, reduce temperature swing by 80%        
c       Note: the emstmp is a multiplicative correction factor, not an EF
c       For all diurnals, limit lowest temperature to 40F.
c
          if( idxspc .EQ. IDXDIU ) then
            if ( lfacfl(IDXDIU) ) then
              emiss = 0.
ccah
              if( tmax .GT. DIUMIN ) then
ccah
                diutmax=max(tmax, DIUMIN)
                diutmin=max(tmin, DIUMIN)
                tavg=(diutmax+diutmin)/2.
                swing=(diutmax-diutmin)/2.
                do j=1,5
                  if(diufrac(j,idxtec).eq.0.) cycle ! floating-point comparison for equality okay; diufrac values are read from file, rather than being calculated at runtime
                  if(j.eq.1) then
                    emiss = emiss + 
     &                   wadeeq(tfull,tvol,trvp,diutmin,diutmax) * 
     &                   0.78 * diufrac(j,idxtec) 
                  else
                    if(j.eq.2.or.j.eq.4) then
                      dtmax=tavg+swing * 0.5
                      dtmin=tavg-swing * 0.5
                    else
                      dtmax=tavg+swing * 0.2
                      dtmin=tavg-swing * 0.2
                    end if
                    emiss = emiss + wadeeq(tfull,tvol,trvp,dtmin,dtmax)
     &                        * 0.78 * diufrac(j,idxtec)
                  end if
ccah
                  if( emiss .LT. 0.0 ) then
                    goto 7004
                  end if
ccah
                  if(diufrac(j,idxtec).eq.1.) exit ! floating-point comparison for equality okay; diufrac values are read from file, rather than being calculated at runtime
                end do
                emiss = emiss * emstmp
ccah
              end if
ccah
            else
              emiss = 0.0
            end if
            temiss = emiss * CVTTON * detrat *
cc            temiss = emiss * ndays * CVTTON * detrat *
     &          adjems(idxspc,jday) * adjtime *
cc     &          adjems(idxspc,jday) * adjtime * tpltmp2 *
     &          pop * mfrac * evtchfrc(idxtch,idxtec)
ctest      WRITE(*,*) emiss,ndays,detrat,tpltmp2,adjems(idxspc,jday)*adjtime,
ctest     &pop,mfrac,evtchfrc(idxtch,idxtec)
            if(.NOT. ldayfl) temiss = temiss * ndays
c
            emsbmy(idxspc) = emsbmy(idxspc) + temiss
            emsday(idxspc) = emsday(idxspc) + temiss
          end if
c
c  ---- catch negative emissions ---
c
   90     if( emsbmy(idxspc) .LT. 0.0 ) then
            emsday(idxspc) = RMISS
            emsbmy(idxspc) = RMISS
          endif
c
  100  continue
c
      end do
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,A,1X,A,5F9.4)')
     &   'ERROR:  Negative Hot Soak EF calculated:',
     &    asccod,hpval,emstmp,permef,tankef,hoseef
      write(IOWMSG,'(/,1X,A,1X,A,5F9.4)')
     &   'ERROR:  Negative Hot Soak EF calculated:',
     &    asccod,hpval,emstmp,permef,tankef,hoseef
      ierr = ISUCES
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,A,1X,A,5F9.4)') 
     &   'ERROR:  Negative Running Loss EF calculated:',
     &    asccod,hpval,emstmp,permef,tankef,hoseef
      write(IOWMSG,'(/,1X,A,1X,A,5F9.4)') 
     &   'ERROR:  Negative Running Loss EF calculated:',
     &    asccod,hpval,emstmp,permef,tankef,hoseef
      ierr = ISUCES
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,A,1X,A,2F9.4)') 
     &   'ERROR:  Negative Diurnal Emissions calculated:',
     &    asccod,hpval,tmax,emiss
      write(IOWMSG,'(/,1X,A,1X,A,2F9.4)') 
     &   'ERROR:  Negative Diurnal Emissions calculated:',
     &    asccod,hpval,tmax,emiss
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end










