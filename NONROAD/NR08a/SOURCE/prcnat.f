c**** PRCNAT
c
      subroutine prcnat(ierr,icurec,asccod,idxsta,growth)
c
c-----------------------------------------------------------------------
c  
c   This routine does the processing for the nation to state allocation
c   of populations and the application of all of the emission factors
c   and seasonality factors.
c      Argument descriptions:
c        Outputs:
c          ierr    I   error code
c        Inputs:
c          icurec  I   record number of current record
c          asccod  C   SCC code of current record
c          idxsta  I   index of the state 
c          growth  R   growth value (-9 means not yet calulated)
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      05/20/97  --gmw-- original developnent
c      11/04/99  --mmj-- modified HP bin selection; now based on
c                        HP range midpoint
c      02/10/00 mjimenez tank volume specified in spillage file
c      07/03/01  charvey puts lpg/cng fulcsm back in. 
c      08/03/01 -3 charvey adds fuel consump to bmy output.
c      10/11/01 rgiannelli force getgrw even in base year
c      06/28/04 --dfk--  added evap processing
c      07/23/04 charvey fixes growth calc to handle different pop input years.
c      11/15/04 --dfk-- added rec-marine 3-hose calc
c      02/09/05 --dfk-- corrected problem with evap not using the right
c                       tech group, removed obsolete evap population code,
c                       optimized code
c      02/24/05 -cimulus- added activity parameter to wrtbmy calls
c      02/25/05 -cimulus- added load factor and HP average parameters
c                         to wrtdat and wrtbmy calls
c      03/03/05 --dfk-- separated the time period adjustment from the
c                       emission adjustment due to the way diesel SOX
c                       is handled in clcems.
c      03/15/05 -cimulus- replaced episode year with growth year
c                         for growth related logic
c      03/16/05 -cimulus- replaced episode year with technology year
c                         for technology related logic
c      04/06/05 -cimulus- updated scrappage methodolody:  removed modscp
c                         and modpop which are no longer used; changed
c                         the year range passed to grwfac (see comment
c                         below); tweaked the parameters to the modyr
c                         call; replaced grwclc with agedist
c      05/20/05 -cimulus- added fraction and units retrofitted
c                         parameters to wrtdat and wrtbmy calls
c      05/20/05 -cimulus- filter retrofit records, by SCC and HP, then
c                         by model year, and then by tech type
c      05/23/05 -cimulus- calculate the fraction and units retrofitted
c                         and retrofit emission effects for the
c                         by-model-year iteration
c      05/23/05 -cimulus- calculate the units and fraction retrofitted
c                         for the SCC-HP iteration
c      07/15/05 -cimulus- removed obsolete tecin and iexev parameters
c                         from fndact calls
c      07/20/05 -cimulus- floating-point comparison for equality okay;
c                         lodfac stores initial found in faclod and then
c                         is compared back against faclod, and faclod
c                         values are read from file, rather than being
c                         calculated at runtime
c      07/30/05 -cimulus- added model-year loop before state loop
c                         to initialize values that depend on model year
c                         but not on state code, improving performance
c      07/30/05 -cimulus- simplified find-activity, get-growth, and
c                         model-year-age-distribution handling now that
c                         fndact no longer depends on tech type
c      07/30/05 -cimulus- cleaned up code for consistency, in terms of
c                         whitespace and commenting
c      07/30/05 -cimulus- removed unused rfmode and fulcsm parameters
c                         from clcems call
c      07/30/05 -cimulus- removed redundant thisfc variable
c      07/30/05 -cimulus- changed size of rfmode and tank, which are
c                         evap related, from MXTECH to MXEVTECH
c      07/30/05 -cimulus- pass min(model year, tech year) instead of
c                         model year to emfclc and evemfclc, so that
c                         they limit searches by the tech year
c      07/30/05 -cimulus- corrected deterioration cap parameter passed
c                         to clcevems; pass evdetcap instead of detcap
c      04/20/06  --epa-- fixes all Daily Temp/RVP month/season/year.
c                        fully replaced dailyf with daymthf.
c      09/18/06  --epa-- add E10 hose & tank permeation adj factors
c      03/15/12  --gwilson-- Put in check to make sure tech fractions
c                            are foundfor this model year
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE
c
      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdreg.inc'
      include 'nonrdefc.inc'
      include 'nonrdeqp.inc'
      include 'nonrdalo.inc'
      include 'nonrdgrw.inc'
      include 'nonrdact.inc'
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      integer*4    icurec
      character*10 asccod
      integer*4    idxsta
      real*4       growth
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   fndasc   I   returns the best match of an SCC code in an array
c   fndact   I   returns the index of record in activity arrays
c   fndgxf   I   returns the index of the record in growth Xref arrays
c   fndrfm   I   returns the index of the refueling mode
c   fndtch   I   returns the index of the exhaust technology type
c   fndevtch I   returns the index of the evap technology type
c   dailyf   R   returns a month/day/hour adjustment factor
c
      integer*4 fndasc
      integer*4 fndact
      integer*4 fndgxf
      integer*4 fndrfm
      integer*4 fndtch
      integer*4 fndevtch
c      real*4    dailyf
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   --- general purpose ---
c
      integer*4    jerr, i, j, idxasc
      character*5  subcur
      real*4       zeroems(MXPOL)
      real*4       missems(MXPOL)
      real*4       hpmid, hplev, hpval
      real*4       denful
      logical*4    luse
      integer*4    idxact, idxgrw
      real*4       grwtmp
      integer*4    nyrlif, idxyr, iyr
      real*4       yryrfrcscrp(MXAGYR) ! year-to-year fraction scrapped by age
      real*4       modfrc(MXAGYR), actadj(MXAGYR)
      real*4       detage(MXAGYR), stradj(MXAGYR)
      real*4       adjems(MXPOL,MXDAYS), adjtime
      real*4       tplfac, tpltmp
      integer*4    ndays
      integer*4    tchmdyr ! model year used for looking up tech type related data (minimum of model year and tech year)
      real*4       emsday(MXPOL)
      real*4       emsbmy(MXPOL)
      real*4       popbmy
      real*4       actbmy
      real*4       fulbmy
      real*4       fulcsm
c
c   --- national level ---
c
      real*4       popus
c
c   --- state level ---
c
      integer*4    idx
      real*4       popsta(NSTATE), grwsta(NSTATE)
c
c   --- exhaust tech ---
c
      integer*4    idxtch(MXAGYR)
      character*10 tecnam(MXTECH)
      real*4       tecfrc(MXTECH)
      real*4       emsfac(MXAGYR,MXPOL,MXTECH)
      real*4       bsfc(MXAGYR,MXTECH)
      integer*4    idxunt(MXPOL,MXTECH)
      real*4       adetcf(MXPOL,MXTECH)
      real*4       bdetcf(MXPOL,MXTECH)
      real*4       detcap(MXPOL,MXTECH)
      real*4       poptot
      real*4       acttot
      real*4       strtot
      real*4       fracretro ! fraction retrofitted for the entire SCC-HP population
      real*4       unitsretro ! number of units retrofitted for the entire SCC-HP population
      real*4       fracretrobmy ! fraction retrofitted for a single model year's population
      real*4       unitsretrobmy ! number of units retrofitted for a single model year's population
c
c   --- evap tech ---
c
      integer*4    idxevtch(MXAGYR)
      character*10 evtecnam(MXEVTECH)
      real*4       evtecfrc(MXEVTECH)
      real*4       evemsfac(MXAGYR,MXPOL,MXEVTECH)
      integer*4    idxevunt(MXPOL,MXEVTECH)
      real*4       aevdetcf(MXPOL,MXEVTECH)
      real*4       bevdetcf(MXPOL,MXEVTECH)
      real*4       evdetcap(MXPOL,MXEVTECH)
      real*4       fulbmytot
      real*4       evpoptot
      real*4       evacttot
      real*4       evstrtot
      integer*4    idxref, idxall
      character*10 tname
      character*9  rfmode(MXAGYR,MXEVTECH)
      real*4       tank(MXAGYR,MXEVTECH)
      real*4       tfull(MXAGYR,MXEVTECH)
      real*4       tmetal(MXAGYR,MXEVTECH)
      real*4       hlen(MXAGYR,MXEVTECH)
      real*4       hdia(MXAGYR,MXEVTECH)
      real*4       hmetal(MXAGYR,MXEVTECH)
      real*4       hsstart(MXAGYR,MXEVTECH)
      real*4       nlen(MXAGYR,MXEVTECH)
      real*4       ndia(MXAGYR,MXEVTECH)
      real*4       slen(MXAGYR,MXEVTECH)
      real*4       sdia(MXAGYR,MXEVTECH)
      real*4       vlen(MXAGYR,MXEVTECH)
      real*4       vdia(MXAGYR,MXEVTECH)
      real*4       tnke10fac(MXEVTECH)
      real*4       hose10fac(MXEVTECH)
      real*4       ncke10fac(MXEVTECH)
      real*4       sre10fac(MXEVTECH)
      real*4       vnte10fac(MXEVTECH)
      real*4       diufrac(5,MXEVTECH)
      real*4       diufracbmy(MXAGYR,5,MXEVTECH)
c
      real*4       daymthfac(MXDAYS), mthf, dayf, tplful
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- initialize the error code ---
c
      ierr = IFAIL
c
c   --- initialize variables that do not change throughout
c       the life of the subroutine ---
c
      subcur = ' '
      zeroems = 0.
      missems = RMISS
c
c   --- find HP level for this HP category ---
c
      hpmid = (hprang(1,icurec)+hprang(2,icurec)) / 2.
      if( hpmid .LE. hpclev(1) ) then
         hplev = hpclev(1)
      else if( hpmid .GT. hpclev(MXHPC) ) then
         hplev = 9999
      else
         hplev = -9.
         do i = 2, MXHPC
            if( hplev .LT. 0. .AND. hpmid .LT. hpclev(i) ) then
               hplev = hpclev(i)
            endif
         end do
      endif
      hpval = avghpc(icurec)
c
c   --- if the population is zero, just write zero records and return ---
c
      popus = popeqp(icurec)
      if( popus .LE. 0. ) then
         do i = 1, NSTATE
            if( lstacd(i) ) then
               call wrtdat(jerr,statcd(i),subcur,asccod,hplev,
     &               0.,0.,0.,0.,0.,0.,0.,zeroems)
               if( jerr .NE. ISUCES ) goto 9999
            endif
         end do
         ierr = ISUCES
         goto 9999
      endif
c
c   --- initialize the population arrays to zero ---
c
      do i = 1, NSTATE
         popsta(i) = 0.
      end do
c
c   --- if this is a national record, call routine to
c       calculate the populations in each state based on
c       national to state allocation ---
c
      if( idxsta .LE. 0 ) then
c
c   --- get the allocation coefficients for this SCC code,
c       if not found, error ---
c
         idxasc = fndasc( asccod, ascalo, nalorc )
         if( idxasc .LE. 0 ) goto 7000
c
c   --- call routine to calculate the populations in each
c       state based on national to state allocation ---
c
         nnatrc = nnatrc + 1
         call alosta(jerr,popsta,grwsta,icurec,
     &         popus,idxasc,luse,growth)
         if( jerr .NE. ISUCES ) goto 9999
c
c   --- if this is a state record, just assign all population to
c       this state ---
c
      else
         popsta(idxsta) = popeqp(icurec)
         grwsta(idxsta) = 1.
         nstarc(idxsta) = nstarc(idxsta) + 1
      endif
c
c   --- call routine to find the number of exhaust tech types
c       for this piece of equipment and tech year ---
c
      if( fndtch(asccod,hpval,itchyr) .LE. 0 ) then
         write(IOWMSG,'(/,1X,3A,1X,F7.1)',ERR=9999) 'WARNING:  Could ',
     &      'not find any exhaust technology fractions for equipment: '
         write(IOWMSG,'(10X,A,10X,A)',ERR=9999) 'SCC code','Average HP'
         write(IOWMSG,'(10X,A,7X,F7.1)',ERR=9999) asccod,hpval
         write(IOWMSG,'(8X,A)',ERR=9999) 'Skipping...'
         call chkwrn(jerr,IDXWTC)
         if( jerr .NE. ISUCES ) goto 9999
         ierr = ISKIP
         goto 9999
      endif
c
c   --- call routine to find the number of evap tech types
c       for this piece of equipment and tech year ---
c
      if( fndevtch(asccod,hpval,itchyr) .LE. 0 ) then
         write(IOWMSG,'(/,1X,3A,1X,F7.1)',ERR=9999) 'WARNING:  Could ',
     &         'not find any evap technology fractions for equipment: '
         write(IOWMSG,'(10X,A,10X,A)',ERR=9999) 'SCC code','Average HP'
         write(IOWMSG,'(10X,A,7X,F7.1)',ERR=9999) asccod,hpval
         write(IOWMSG,'(8X,A)',ERR=9999) 'Skipping...'
         call chkwrn(jerr,IDXWTC)
         if( jerr .NE. ISUCES ) goto 9999
         ierr = ISKIP
         goto 9999
      endif
c
c   --- find the fuel density, used for fuel consumption calc ---
c
      denful = 1.
      if( ifuel .EQ. IDXGS2 .OR. ifuel .EQ. IDXGS4 ) then
         denful = DENGAS
      else if( ifuel .EQ. IDXCNG ) then
         denful = DENCNG
      else if( ifuel .EQ. IDXLPG ) then
         denful = DENLPG
      else if( ifuel .EQ. IDXDSL ) then
         denful = DENDSL
      endif
c
c   --- pre-calculate values that depend on model year
c       but that do not depend on state code ---
c
      do iyr = iepyr - MXAGYR + 1, iepyr
c
c   --- compute relative index -- backwards from absolute year loop ---
c
         idxyr = iepyr - iyr + 1
c
c   --- if the model year is greater than the tech year, use the tech
c       year for tech type related searches that involve model year ---
c
         tchmdyr = min(iyr, itchyr)
c
c   --- call routine to find the exhaust technology index for this piece
c       of equipment and model year (model year limited by tech year) ---
c
         idxtch(idxyr) = fndtch(asccod,hpval,tchmdyr)
c
c  --- check that there is data for this year ---
c
         if( idxtch(idxyr) .EQ. 0 ) then
            write(IOWMSG,'(/,1X,3A,1X,F7.1)',ERR=9999) 
     &           'WARNING:  Could ','not find any exhaust technology ',
     &                                       'fractions for equipment: '
            write(IOWMSG,'(10X,A,10X,A,10X,A)',ERR=9999)
     &                            'SCC code','Average HP','Year'
            write(IOWMSG,'(10X,A,7X,F7.1,14X,I4)',ERR=9999)
     &                                           asccod,hpval,tchmdyr
            write(IOWMSG,'(8X,A)',ERR=9999) 'Skipping...'
            call chkwrn(jerr,IDXWTC)
            if( jerr .NE. ISUCES ) goto 9999
            ierr = ISKIP
            goto 9999
         endif
c
         do i = 1, ntech(idxtch(idxyr))
            tecnam(i) = tectyp(idxtch(idxyr),i)
            tecfrc(i) = tchfrc(idxtch(idxyr),i)
         end do
c
c   --- call routine to calculate the exhaust emission factors for this
c       equipment type and model year ---
c
         call emfclc( jerr, emsfac, bsfc, idxunt, adetcf, bdetcf,
     &         detcap, asccod, tecnam, ntech(idxtch(idxyr)), tecfrc,
     &         tchmdyr, idxyr, icurec)
         if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to find the evap technology index for this piece
c       of equipment and model year (model year limited by tech year) ---
c
         idxevtch(idxyr) = fndevtch(asccod,hpval,tchmdyr)
c
         do i = 1, nevtech(idxevtch(idxyr))
            evtecnam(i) = evtectyp(idxevtch(idxyr),i)
            evtecfrc(i) = evtchfrc(idxevtch(idxyr),i)
c
c   --- set up refueling, hose and tank data ---
c
            if ( lfacfl(IDXSPL) ) then
               idxall = fndrfm( asccod, hpval, TECDEF )
               j=IDXSPL-IDXDIU+2-3
               tname='E'//evtecnam(i)(j:j)
               idxref = fndrfm( asccod, hpval, tname )
               if ( idxref .EQ. 0 )  idxref = idxall
               if ( idxref .GT. 0 )  then
                  rfmode(idxyr,i) = modspl(idxref)
                  if ( untspl(idxref) .EQ. GALLON )  then
                     tank(idxyr,i) = volspl(idxref)
                  elseif ( untspl(idxref) .EQ. GALHP )  then
                     tank(idxyr,i) = MIN( volspl(idxref)*hpval, TNKMAX )
                  endif
                  tfull(idxyr,i)  = tnkful(idxref)
                  tmetal(idxyr,i) = tnkmtl(idxref)
                  hlen(idxyr,i)   = hoslen(idxref)
                  hdia(idxyr,i)   = hosdia(idxref)
                  hmetal(idxyr,i) = hosmtl(idxref)
                  hsstart(idxyr,i)= hssph(idxref)
                  nlen(idxyr,i)   = ncklen(idxref)
                  ndia(idxyr,i)   = nckdia(idxref)
                  slen(idxyr,i)   = srlen(idxref)
                  sdia(idxyr,i)   = srdia(idxref)
                  vlen(idxyr,i)   = vntlen(idxref)
                  vdia(idxyr,i)   = vntdia(idxref)
                  do j = 1, 5
                     diufracbmy(idxyr,j,i)=diufrc(j,idxref)
                  end do
                  tnke10fac(i) = tnke10(idxref)
                  hose10fac(i) = hose10(idxref)
                  ncke10fac(i) = ncke10(idxref)
                  sre10fac(i)  = sre10(idxref)
                  vnte10fac(i) = vnte10(idxref)
               else
                  rfmode(idxyr,i) = '         '
                  tank(idxyr,i) = -9.
                  write(IOWMSG,'(/,1X,2A,F6.1,2(1X,A))',ERR=9999)
     &                  'WARNING:  No Spillage data found for: ',
     &                  asccod, hpval, evtecnam(i), tname
                  call chkwrn(jerr,IDXWEM)
                  if( jerr .NE. ISUCES ) goto 9999
               endif
            endif
c
c   --- next evap technology type ---
c
         end do
c
c   --- call routine to calculate the evap emission factors for this
c       equipment type and model year ---
c
         call evemfclc( jerr, evemsfac, idxevunt, aevdetcf, bevdetcf,
     &         evdetcap, asccod, evtecnam, nevtech(idxevtch(idxyr)),
     &         evtecfrc, tchmdyr, idxyr, icurec)
         if( jerr .NE. ISUCES ) goto 9999
c
c   --- next model year ---
c
      end do
c
c   --- loop over states ---
c
      do idx = 1, NSTATE
c
c  --- skip if this state is not requested, or if doing national
c      record and state has specific records skip it ---
c
         if( .NOT. lstacd(idx) ) then
            cycle
         endif
         if( idx .LE. 0 .AND. lstlev(idx) ) then
            cycle
         endif
c
c  --- if population for this state is zero, just write a zero record
c      and go to next state ---
c
         if( popsta(idx) .LE. 0 ) then
            call wrtdat(jerr,statcd(idx),subcur,asccod,hplev,
     &            0.,0.,0.,0.,0.,0.,0.,zeroems)
            if( jerr .NE. ISUCES ) goto 9999
            cycle
         endif
c
c   --- initialize daily emissions to zero ---
c
         do i = 1, MXPOL
            emsday(i) = 0.
         end do
c
c-----------------------------------------------------------------------
c    Add monthly activity weighting for daily loop:
c-----------------------------------------------------------------------
c
      call daymthf(asccod, statcd(idx), daymthfac, mthf, dayf, ndays)
c
c --- set the time period adjustment factor: Total vs Typical Day ---
c
      if( ismtyp .EQ. IDXTOT ) then
        adjtime=1.0
      else
        adjtime=1.0/float(ndays)
      endif
c
      if(ldayfl) then
        tplfac = dayf
      else
        tplfac = mthf * dayf
      endif
      tplful = mthf * dayf
c
c   --- call routine to calculate the adjustment factors for the
c       emission factors for this equipment type ---
c
      call emsadj( adjems, asccod, statcd(idx), daymthfac )
c
c   --- call routine to get the monthly, daily, and hourly adjustment ---
c
cc    tplfac = dailyf( ndays, asccod, statcd(idx) )
c
c   --- call the routine to find the growth factor indicator in the
c       cross reference arrays ---
c
         if( .NOT. lgrwfl ) goto 7003
         idxgrw = fndgxf( statcd(idx), asccod, avghpc(icurec) )
         if( idxgrw .LE. 0 ) goto 7001
c
c   --- find the activity data for the state, SCC, and average HP ---
c
         idxact = fndact( asccod, statcd(idx), avghpc(icurec) )
c
c   --- if no activity data found, log a warning,
c       write missing records, and return ---
c
         if( idxact .LE. 0 ) then
            write(IOWMSG,9000,ERR=9999)
     &            'WARNING:  Could not find any activity data for: ',
     &            'State','SCC','HP range',
     &            statcd(idx),asccod,hprang(1,icurec),hprang(2,icurec)
            call chkwrn(jerr,IDXWAC)
            if( jerr .NE. ISUCES ) goto 9999
            call wrtdat(jerr,statcd(idx),subcur,asccod,hplev,
     &            RMISS,RMISS,RMISS,RMISS,RMISS,RMISS,RMISS,missems)
            if( jerr .NE. ISUCES ) goto 9999
            goto 9999
         endif
c
c   --- call routine to get the growth factor data from the file ---
c
         call getgrw( jerr, indgrx(idxgrw) )
         if( jerr .NE. ISUCES ) goto 9999
c
c   --- calculate the growth factor for the state
c       get growth factor from ipopyr to ipopyr+1, since this is used
c       only for the scrptime call in modyr, which uses the growth
c       factor at the base-population year to extrapolate a full
c       sales/scrappage history ---
c
         call grwfac( jerr, grwtmp, ipopyr(icurec),
     &         ipopyr(icurec)+1, statcd(idx), indgrx(idxgrw) )
         if( jerr .NE. ISUCES ) goto 9999
         grwsta(idx) = grwtmp
c
c   --- call routine to calculate the initial model year distribution
c       fractions (among other model-year related calculations) ---
c
         call modyr( yryrfrcscrp, modfrc, stradj, actadj, nyrlif,
     &         detage, usehrs(icurec), discod(icurec),
     &         starts(idxact), actlev(idxact), iactun(idxact),
     &         faclod(idxact), actage(idxact), grwsta(idx) )
c
c   --- call routine to calculate the grown model year distribution
c       fractions and possibly the backward-grown population (if
c       growth year less than population year) ---
c
         call agedist( jerr, popsta(idx), modfrc, ipopyr(icurec),
     &         igryr, nyrlif, yryrfrcscrp, statcd(idx),
     &         indgrx(idxgrw) )
         if( jerr .NE. ISUCES ) goto 9999
c
c   --- set exhaust population, activity, starts, fuel consumption,
c       fraction retrofitted, and units retrofitted totals to zero ---
c
         poptot = 0.
         acttot = 0.
         strtot = 0.
         fulcsm = 0.
         fracretro = 0.
         unitsretro = 0.
c
c   --- filter retrofit records to those that apply to the SCC and HP ---
c
         if( lrtrftfl ) then
            call fndrtrft( jerr, 1, asccod, hpval, 0, ' ' ) ! model year and tech type ignored
            if( jerr .NE. ISUCES ) goto 9999
         endif
c
c   --- set evap population, activity, and starts totals to zero ---
c
         evpoptot = 0.
         evacttot = 0.
         evstrtot = 0.
c
c   --- loop over model years ---
c
         do iyr = iepyr - nyrlif + 1, iepyr
c
c   --- compute relative index -- backwards from absolute year loop ---
c
            idxyr = iepyr - iyr + 1
c
c   --- skip if the model year fraction is zero (sometimes
c       happens for the first year in the distribution) ---
c
            if( modfrc(idxyr) .LE. 0. ) then
               cycle
            endif
c
c   --- initialize total bmy fuel consumption to zero
c       (value is used in evap calculations below) ---
c
            fulbmytot = 0.
c
c   --- if the model year is greater than the tech year, use the tech
c       year for tech type related searches that involve model year ---
c
            tchmdyr = min(iyr, itchyr)
c
c   --------- Exhaust --------
c
c   --- filter retrofit records to those that apply to the model year ---
c
            if( lrtrftfl ) then
               call fndrtrft( jerr, 2, ' ', 0., iyr, ' ' ) ! SCC, HP, and tech type ignored
               if( jerr .NE. ISUCES ) goto 9999
            endif
c
c   --- loop over exhaust technology types ---
c
            do i = 1, ntech(idxtch(idxyr))
c
c   --- skip over unused technology types ---
c
               if( tchfrc(idxtch(idxyr),i) .LE. 0. ) then
                  cycle
               endif
c
c   --- initialize the bmy array ---
c
               emsbmy = 0.
c
c   --- initialize total bmy fraction retrofitted
c       and units retrofitted ---
c
               fracretrobmy = 0.
               unitsretrobmy = 0.
c
c   --- calculate the population for this model year and tech type ---
c
               popbmy = popsta(idx) * modfrc(idxyr)
     &               * tchfrc(idxtch(idxyr),i)
c
c   --- filter retrofit records to those that apply to the tech type,
c       and calculate the fraction and units retrofitted and retrofit
c       emission effects ---
c
               if( lrtrftfl ) then
                  call fndrtrft( jerr, 3, ' ', 0., 0,
     &                  tectyp(idxtch(idxyr),i) ) ! SCC, HP, and model year ignored
                  if( jerr .NE. ISUCES ) goto 9999

                  call clcrtrft( jerr, fracretrobmy, unitsretrobmy,
     &                  popbmy, asccod, hpval, iyr,
     &                  tectyp(idxtch(idxyr),i) )
                  if( jerr .NE. ISUCES ) goto 9999
                  unitsretro = unitsretro + unitsretrobmy
               endif
c
c   --- set the local variable for temporal adjustment factor ---
c
               if( iactun(idxact) .EQ. IDXHRY
     &               .OR. iactun(idxact) .EQ. IDXGLY ) then
                  tpltmp = tplfac
               else if( iactun(idxact) .EQ. IDXHRD
     &               .OR. iactun(idxact) .EQ. IDXGLD ) then
                  tpltmp = 1.
               endif
c
c   --- fill emissions arrays ---
c
               call clcems (jerr,emsday, emsbmy,
     &               idxyr, i, idxtch(idxyr), detage(idxyr), detcap,
     &               adetcf, bdetcf, idxunt, tchfrc(idxtch(idxyr),i),
     &               hpval, denful, bsfc(idxyr,i), idxact, adjems,
     &               adjtime, emsfac, tpltmp, stradj(idxyr),
     &               popsta(idx), modfrc(idxyr), ndays, actadj(idxyr) )
               if( jerr .NE. ISUCES ) goto 9999

c
c   --- calculate the contribution from this model year and tech type ---
c
               actbmy = actadj(idxyr) * popsta(idx) * modfrc(idxyr)
     &               * tplful * tchfrc(idxtch(idxyr),i) * adjtime
               fulbmy = tplful * popsta(idx) * actadj(idxyr)
     &               * modfrc(idxyr) * tchfrc(idxtch(idxyr),i)
     &               * (hpval * faclod(idxact)
     &               * bsfc(idxyr,i) / denful ) * adjtime
c
c   --- add to total fuel consumption ---
c
               fulcsm = fulcsm + fulbmy
c
c   --- update total by-model-year fuel consumption
c       (value used in evap calculations below) ---
c
               fulbmytot = fulbmytot + fulbmy
c
c   --- call routine to write by-model-year output ---
c
               if ( lbmyfl )  then
                  call wrtbmy(jerr,statcd(idx),subcur,asccod,hplev,
     &                  tectyp(idxtch(idxyr),i),iyr,popbmy,emsbmy,
     &                  fulbmy,actbmy,faclod(idxact),hpval,
     &                  fracretrobmy,unitsretrobmy,1) ! 1=exhaust
                  if( jerr .NE. ISUCES ) goto 9999
               endif
c
c   --- call routine to add to totals for SI report ---
c
               if( lsifl ) then
                  call sitot(jerr,tectyp(idxtch(idxyr),i),popbmy,
     &                  actbmy,fulbmy,emsbmy)
                  if( jerr .NE. ISUCES .AND. jerr .NE. ISKIP ) goto 9999
               endif
c
c   --- next exhaust technology type ---
c
            end do
c
c   --- sum population, activity, and starts ---
c
            poptot = poptot + popsta(idx) * modfrc(idxyr)
            acttot = acttot + actadj(idxyr) * popsta(idx)
     &            * modfrc(idxyr) * tplful * adjtime
            strtot = strtot + stradj(idxyr) * popsta(idx)
     &            * modfrc(idxyr) * tplful * adjtime
c
c   --------- Evap --------
c
c      (Must loop through evap calculations for non-gas equipment since
c      spillage occurs for all fuels.)
c
c   --- loop over evap technology types ---
c
            do i = 1, nevtech(idxevtch(idxyr))
c
c   --- skip over unused technology types ---
c
               if( evtchfrc(idxevtch(idxyr),i) .LE. 0. ) then
                  cycle
               endif
c
c   --- initialize the bmy array ---
c
               emsbmy = 0.
c
c   --- set the local variable for temporal adjustment factor ---
c
               if( iactun(idxact) .EQ. IDXHRY
     &               .OR. iactun(idxact) .EQ. IDXGLY ) then
                  tpltmp = tplfac
               else if( iactun(idxact) .EQ. IDXHRD
     &               .OR. iactun(idxact) .EQ. IDXGLD ) then
                  tpltmp = 1.
               endif
c
c   --- estimate the fuel used by this technology and model year ---
c
               fulbmy = fulbmytot * evtchfrc(idxevtch(idxyr),i)
c
c   --- setup diurnal fraction array for clcevems (only need
c       current tech type for clcevems) ---
c
               do j = 1, 5
                  diufrac(j,i) = diufracbmy(idxyr,j,i)
               end do
c
c   --- fill emissions arrays ---
c
               call clcevems (jerr, asccod, emsday, emsbmy,
     &               idxyr, i, idxevtch(idxyr), detage(idxyr), evdetcap,
     &               aevdetcf, bevdetcf, idxevunt,
     &               evtchfrc(idxevtch(idxyr),i), hpval, adjems,
     &               adjtime, evemsfac, tpltmp, stradj(idxyr),
     &               popsta(idx), modfrc(idxyr), ndays, tank(idxyr,i),
     &               actadj(idxyr), rfmode(idxyr,i), fulbmy,
     &               tfull(idxyr,i), tmetal(idxyr,i), hlen(idxyr,i),
     &               hdia(idxyr,i), hmetal(idxyr,i), nlen(idxyr,i),
     &               ndia(idxyr,i), slen(idxyr,i), sdia(idxyr,i),
     &               vlen(idxyr,i), vdia(idxyr,i), hsstart(idxyr,i),
     &               diufrac, 
     &               tnke10fac(i), hose10fac(i), ncke10fac(i),
     &               sre10fac(i), vnte10fac(i), statcd(idx) )
               if( jerr .NE. ISUCES ) goto 9999
c
c   --- calculate the contribution from this tech type ---
c
               popbmy = popsta(idx) * modfrc(idxyr)
     &               * evtchfrc(idxevtch(idxyr),i)
               actbmy = actadj(idxyr) * popsta(idx) * modfrc(idxyr)
     &               * tplful * evtchfrc(idxevtch(idxyr),i) * adjtime
c
c   --- call routine to write by-model-year output ---
c
               if ( lbmyfl )  then
c                 pass RMISS for load factor, HP average, fraction
c                 retrofitted, and units retrofitted, as these do not
c                 apply to evap by-model-year output
                  call wrtbmy(jerr,statcd(idx),subcur,asccod,hplev,
     &                  evtectyp(idxevtch(idxyr),i),iyr,popbmy,emsbmy,
     &                  fulbmy,actbmy,RMISS,RMISS,RMISS,RMISS,2) ! 2=evap
                  if( jerr .NE. ISUCES ) goto 9999
               endif
c
c   --- call routine to add to totals for SI report ---
c
               if( lsifl ) then
                  call sitot(jerr,evtectyp(idxevtch(idxyr),i),popbmy,
     &                  actbmy,fulbmy,emsbmy)
                  if( jerr .NE. ISUCES .AND. jerr .NE. ISKIP ) goto 9999
               endif
c
c   --- next evap technology type ---
c
            end do
c
c   --- sum population, activity and starts ---
c
            evpoptot = evpoptot + popsta(idx) * modfrc(idxyr)
            evacttot = evacttot + actadj(idxyr) * popsta(idx)
     &            * modfrc(idxyr) * tplful * adjtime
            evstrtot = evstrtot + stradj(idxyr) * popsta(idx)
     &            * modfrc(idxyr) * tplful * adjtime
c
c   --- next model year ---
c
         end do
c
c************************************************************
c  NOTE:  Fix to alleviate some confusion about fuel
c         consumption in the reports.  LPG and CNG fuel
c         consumption means something different.
c         CNG is currently CUBIC FEET not Gallons due to DENCNG.
c************************************************************
c
ccc         if( ifuel .EQ. IDXLPG .OR. ifuel .EQ. IDXCNG ) fulcsm = 0.
c
c   --- calculate the fraction retrofitted ---
c
         fracretro = unitsretro / poptot
c
c   --- call routine to write the output to the data file ---
c
         call wrtdat(jerr,statcd(idx),subcur,asccod,hplev,poptot,acttot,
     &         fulcsm,faclod(idxact),hpval,fracretro,unitsretro,emsday)
         if( jerr .NE. ISUCES ) goto 9999
c
c   --- next state --
c
      end do
c
c   --- all states processed, return to the calling routine ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Could not find any ',
     &      'allocation coefficients for SCC code ',asccod
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Could not find any ',
     &      'allocation coefficients for SCC code ',asccod
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR: Could not find ',
     &      'match in growth indicator cross reference for: '
      write(IOWSTD,'(10X,A,/,10X,A5,3X,A10,6X,F6.1,2X,F6.1)',ERR=9999)
     &      'State     SCC              HP range',
     &      statcd(idx), asccod,hprang(1,icurec),hprang(2,icurec)
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR: Could not find ',
     &      'match in growth indicator cross reference for: '
      write(IOWMSG,'(10X,A,/,10X,A5,3X,A10,6X,F6.1,2X,F6.1)',ERR=9999)
     &      'State     SCC              HP range',
     &      statcd(idx), asccod,hprang(1,icurec),hprang(2,icurec)
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR:  Could not find the ',
     &      '/GROWTH FILES/ packet of options file.'
      write(IOWSTD,'(9X,2A)',ERR=9999) 'This packet is required ',
     &      'for future year projections or backcasting. '
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR:  Could not find the ',
     &      '/GROWTH FILES/ packet of options file.'
      write(IOWMSG,'(9X,2A)',ERR=9999) 'This packet is required ',
     &      'for future year projections or backcasting. '
      goto 9999
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(/,1X,A,/,10X,A,6X,A,11X,A,/,10X,A,4X,A,4X,F6.1,1X,F6.1)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
