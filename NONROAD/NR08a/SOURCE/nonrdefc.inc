c*** NONRDEFC.INC
c
c-----------------------------------------------------------------------
c
c   Include file for the emission factor data used by the NONROAD program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c   11/01/93 --gmw-- Original development
c   07/19/96 --jlf-- Added deterioration rates
c   06/09/97 --gwilson-- Added variables for the last emissions values
c                        retrieved
c   09/20/97 --gwilson-- Added variables for the CAP/NOCAP option for
c                        deterioration factors
c   04/14/98 --gwilson-- Changed the varables for the last record found
c                        to be by tech type 
c   05/01/98 --gwilson-- Changed how deterioration factors are stored
c                        and referenced
c   02/10/00 mjimenez    Added variables units and tank vol to spillage
c   05/18/04 --dfk--     expanded evap arrays to handle evap tech groups
c                        by species
c   05/28/04 --dfk--     added arrays for tank and hose data (diurnal and
c                        permeation), hot soak starts per hour of operation
c   06/17/04 --dfk--     added diurnal fractions
c   10/13/04 --dfk--     added IDXGMD and KEYGMD and increased MXEMCY 
c                        from 7 to 8
c   11/15/04 --dfk--     added rec-marine fill neck, supply/return, and
c                        vent hose permeation length and diameter
c   09/15/06 --epa--     added Base E10 permeation adj inputs. Ctl all = 2.0
c   10/03/07 --epa--     increased MXDTFC from 100 to 120
c   03/22/18 --dbc--     increased MXEMFC from 8000 to 13000
c
c-----------------------------------------------------------------------
c   Emission factor units parameters:
c-----------------------------------------------------------------------
c
c   MXEMCV  I  maximum number of emission factor units conversion
c   MXEMFC  I  maximum number of exhaust emission factors
c   MXDTFC  I  maximum number of deterioration factors
c   MXSPL   I  maximum number of spillage factors
c
      integer*4 MXEMCV 
      integer*4 MXEMFC
      integer*4 MXDTFC
      integer*4 MXSPL
c
      parameter( MXEMCV =      8 )
      parameter( MXEMFC =  13000 )
      parameter( MXDTFC =    120 )
      parameter( MXSPL =    2000 )
c
c   KEYGHR  C  keyword of g/hr emission factor units
c   KEYGHP  C  keyword of g/hp-hr emission factor units
c   KEYGAL  C  keyword of g/gallon emission factor units
c   KEYTNK  C  keyword of g/tank volume emission factor units
c   KEYGDY  C  keyword of g/day emission factor units
c   KEYGST  C  keyword of g/start emission factor units
c   KEYMLT  C  keyword of unitless emission factor units
c   KEYGMD  C  keyword of g/m2/day emission factor units
c
      character*10 KEYGHR           
      character*10 KEYGHP           
      character*10 KEYGAL           
      character*10 KEYTNK           
      character*10 KEYGDY           
      character*10 KEYGST
      character*10 KEYMLT
      character*10 KEYGMD
c
      parameter( KEYGHR = 'G/HR      ' )
      parameter( KEYGHP = 'G/HP-HR   ' )
      parameter( KEYGAL = 'G/GALLON  ' )
      parameter( KEYTNK = 'G/TANK    ' )
      parameter( KEYGDY = 'G/DAY     ' )
      parameter( KEYGST = 'G/START   ' )
      parameter( KEYMLT = 'MULT      ' )
      parameter( KEYGMD = 'G/M2/DAY  ' )
c
c   IDXGHR  I  index of g/hr emission factor units in arrays
c   IDXGHP  I  index of g/hp-hr emission factor units in arrays
c   IDXGAL  I  index of g/gallons emission factor units in arrays
c   IDXTNK  I  index of g/tank volume emission factor units in arrays
c   IDXGDY  I  index of g/day emission factor units in arrays
c   IDXGST  I  index of g/start emissions factor units array
c   IDXMLT  I  index of multiplicative (unitless) emissions factor units array
c
      integer*4 IDXGHR           
      integer*4 IDXGHP           
      integer*4 IDXGAL           
      integer*4 IDXTNK           
      integer*4 IDXGDY           
      integer*4 IDXGST
      integer*4 IDXMLT
      integer*4 IDXGMD
c
      parameter( IDXGHR = 1 )
      parameter( IDXGHP = 2 )
      parameter( IDXGAL = 3 )
      parameter( IDXTNK = 4 )
      parameter( IDXGDY = 5 )
      parameter( IDXGST = 6 )
      parameter( IDXMLT = 7 )
      parameter( IDXGMD = 8 )
c   
c   FACGHR  R  conversion factor from g/hr to g/hr
c   FACGHP  R  conversion factor from g/hp-hr to g/hr
c   FACGAL  R  conversion factor from g/gallons to g/hr
c   FACTNK  R  conversion factor from g/tank volume to g/hr
c   FACGDY  R  conversion factor from g/day to g/hr
c
      real*4 FACGHR           
      real*4 FACGHP           
      real*4 FACGAL           
      real*4 FACTNK           
      real*4 FACGDY           
c
      parameter( FACGHR = 1.0 )
      parameter( FACGHP = 1.0 )
      parameter( FACGAL = 1.0 )
      parameter( FACTNK = 1.0 )
      parameter( FACGDY = 1.0 )
c
c-----------------------------------------------------------------------
c   Parameters for the CAP option:
c-----------------------------------------------------------------------
c
c   FLGCAP   C  flag for apllying CAP
c   FLGNCP   C  flag for apllying NOCAP
c
      character*10 FLGCAP
      character*10 FLGNCP
c
      parameter( FLGCAP = "CAP       ")
      parameter( FLGNCP = "NOCAP     ")
c
c-----------------------------------------------------------------------
c   Exhaust Emission factors data:
c-----------------------------------------------------------------------
c
c   ascexh  C  SCC code
c   tecexh  C  technology code 
c   exhpcb  R  beginning of horsepower range
c   exhpce  R  ending of horsepower range
c   iexhun  I  array of units index for each exhaust emission factor
c   iyrexh  I  array of years for emission factors
c   exhfac  R  vector of exhaust emission factor array 
c   nexhfc  I  number of codes in factors array
c
      character*10 ascexh(MXEMFC)
      character*10 tecexh(MXEMFC)
      real*4       exhpcb(MXEMFC)
      real*4       exhpce(MXEMFC)
      integer*4    iyrexh(MXEMFC)
      integer*4    iexhun(MXEMFC,IDXTHC:IDXCRA)
      real*4       exhfac(MXEMFC,IDXTHC:IDXCRA)
      integer*4    nexhfc
c
      common /emxchr/ ascexh, tecexh
      common /emxdat/ exhpcb, exhpce, iexhun, iyrexh, exhfac, nexhfc
c
c-----------------------------------------------------------------------
c   Non-Exhaust Emission factors data:
c-----------------------------------------------------------------------
c
c   ascevp  C  SCC code for each evap species 
c   tecevp  C  technology code for each evap species 
c   evhpcb  R  beginning of horsepower range for each evap species 
c   evhpce  R  ending of horsepower range for each evap species 
c   evpfac  R  vector of evap emission factor array for each evap species  
c   ievpun  I  array of units index for each evap emission factor for each evap species 
c   iyrevp  I  array of years for emission factors for each evap species 
c   nevpfc  I  number of codes in factors array for each evap species 
c
      character*10 ascevp(MXEMFC,MXPOL)
      character*10 tecevp(MXEMFC,MXPOL)
      real*4       evhpcb(MXEMFC,MXPOL)
      real*4       evhpce(MXEMFC,MXPOL)
      integer*4    iyrevp(MXEMFC,MXPOL)
      integer*4    ievpun(MXEMFC,MXPOL)
      real*4       evpfac(MXEMFC,MXPOL)
      integer*4    nevpfc(MXPOL)
c
      common /emvchr/ ascevp, tecevp
      common /emvdat/ evhpcb, evhpce, ievpun, iyrevp, evpfac, nevpfc
c
c-----------------------------------------------------------------------
c   Start Emission factors data:
c   NOTE:  This code was commented out because starts emissions
c          are not currently supported.
c-----------------------------------------------------------------------
c
c   ascstr  C  SCC code
c   tecstr  C  technology code 
c   sthpcb  R  beginning of horsepower range
c   sthpce  R  ending of horsepower range
c   strfac  R  vector of exhaust emission factor array 
c   istrun  I  array of units index for each exhaust emission factor
c   iyrstr  I  array of years for emission factors
c   nstrfc  I  number of codes in factors array
c
cgmw      character*10 ascstr(MXEMFC)
cgmw      character*10 tecstr(MXEMFC)
cgmw      real*4       sthpcb(MXEMFC)
cgmw      real*4       sthpce(MXEMFC)
cgmw      integer*4    iyrstr(MXEMFC)
cgmw      integer*4    istrun(MXEMFC,IDSTHC:IDSPM)
cgmw      real*4       strfac(MXEMFC,IDSTHC:IDSPM)
cgmw      integer*4    nstrfc
c
cgmw      common /emschr/ ascstr, tecstr
cgmw      common /emsdat/ sthpcb, sthpce, istrun, iyrstr, strfac, nstrfc
c
c-----------------------------------------------------------------------
c   The last match on emissions factors:
c-----------------------------------------------------------------------
c
c  emasc    C  the SCC code of the last emission factor retrieved
c  emtech   C  the technology code of the last exhaust emission factor retrieved
c  evemtech C  the technology code of the last evap emission factor retrieved
c  iemyr    I  the span of years of the last emission factor retrieved
c  emhpc    R  the HP range of the last emission factor retrieved
c  idxems   I  the last match in the arrays 
c  idxeal   I  the last match for the global tech type 
c
      character*10 emasc(0:MXPOL)
      character*10 emtech(0:MXPOL,MXTECH)
      character*10 evemtech(0:MXPOL,MXEVTECH)
      integer*4    iemyr(0:MXPOL,0:MXTECH,2)
      real*4       emhpc(0:MXPOL)
      integer*4    idxems(0:MXPOL,MXTECH)
      integer*4    idxeal(0:MXPOL)
c
      common /emlchr/ emasc, emtech, evemtech
      common /emldat/ iemyr, emhpc, idxems, idxeal
c
c-----------------------------------------------------------------------
c   Deterioration factors
c-----------------------------------------------------------------------
c
c   tecdet  C  array of the tech types for deterioration factors
c   detavl  R  array of a-coefficients to deterioration factor equation
c   detbvl  R  array of b-coefficients to deterioration factor equation
c   capdet  R  value for the CAP on deteroioration (fraction of useful life)
c   ndtfac  I  number of codes in deterioration factors array
c
      character*10 tecdet(MXDTFC,MXPOL)
      real*4       detavl(MXDTFC,MXPOL) 
      real*4       detbvl(MXDTFC,MXPOL) 
      real*4       capdet(MXDTFC,MXPOL)
      integer*4    ndtfac(MXPOL)
c
      common /detchr/ tecdet
      common /detdat/ detavl, detbvl, ndtfac, capdet
c
c-----------------------------------------------------------------------
c   BSFC factors data:
c-----------------------------------------------------------------------
c
c   ascbsf  C  SCC code
c   tecbsf  C  technology code 
c   bsfpcb  R  beginning of horsepower range
c   bsfpce  R  ending of horsepower range
c   ibsfun  I  array of units index for each BSFC value
c   iyrbsf  I  array of years for BSFC value
c   bsffac  R  vector of BSFC values
c   nbsffc  I  number of codes in BSFC array
c
      character*10 ascbsf(MXEMFC)
      character*10 tecbsf(MXEMFC)
      real*4       bsfpcb(MXEMFC)
      real*4       bsfpce(MXEMFC)
      integer*4    iyrbsf(MXEMFC)
      integer*4    ibsfun(MXEMFC,0:0)
      real*4       bsffac(MXEMFC,0:0)
      integer*4    nbsffc
c
      common /bsxchr/ ascbsf, tecbsf
      common /bsxdat/ bsfpcb, bsfpce, ibsfun, iyrbsf, bsffac, nbsffc
c
c-----------------------------------------------------------------------
c   Parameters for adjustment factors for RFG adjustment factors:
c-----------------------------------------------------------------------
c
c   NRFGBIN  I   number of bins for years for RFG factors
c
      integer*4 NRFGBIN
c
      parameter( NRFGBIN = 3 )
c
c-----------------------------------------------------------------------
c   Variables for adjustment factors for RFG adjustment factors:
c-----------------------------------------------------------------------
c
c   iyrbin   I   years for RFG bins
c   rfggs2   R   array of adjustment factors for 2-stroke gasoline
c   rfggs4   R   array of adjustment factors for 4-stroke gasoline
c   soxbas   R   array for base level sulfur content
c   soxfrc   R   array for fraction fuel sulfur that becomes PM
c   soxful   R   array for episodic level sulfur content
c   altfac   R   array for altitude adjutment factors
c
      integer*4 iyrbin(IDXFAL,NRFGBIN,2)
      real*4    rfggs2(IDXFAL,NRFGBIN,MXPOL)
      real*4    rfggs4(IDXFAL,NRFGBIN,MXPOL)
      real*4    soxbas(IDXCNG)
      real*4    soxfrc(IDXCNG)
      real*4    soxful(IDXCNG)
      real*4    altfac(IDXCNG)
c
      common /rfgdat/ iyrbin, soxbas, soxfrc, soxful, altfac,
     &                rfggs2, rfggs4
c
c-----------------------------------------------------------------------
c   Parameters for Spillage input and calculation:
c-----------------------------------------------------------------------
c
c   CNTFAC   R  Factor (g/refueling) for portable container spillage
c   PMPFAC   R  Factor (g/refueling) for gasoline pump spillage
c   PUMP     C  Indicates pump refueling mode
c   CNTR     C  Indicates portable container refueling mode
c   TNKTYP   C  Indicates tank volume ranges provided
c   HP       C  Indicates horsepower ranges provided
c   GALLON   C  Indicates tank volume units of gallons
c   GALHP    C  Indicates tank volume units of gallons/HP
c
      real*4       CNTFAC
      real*4       PMPFAC
      character*9  PUMP
      character*9  CNTR
      character*4  TNKTYP
      character*4  HP
      character*10 GALLON
      character*10 GALHP
c
      parameter( CNTFAC = 17. )
      parameter( PMPFAC = 3.6 )
      parameter( PUMP = "PUMP     " )
      parameter( CNTR = "CONTAINER" )
      parameter( TNKTYP = "TANK" )
      parameter( HP   = "HP  " )
      parameter( GALLON = "GALLONS   ")
      parameter( GALHP  = "GAL/HP    ")
c
c-----------------------------------------------------------------------
c   Spillage, Diurnal and Tank/Hose Permeation data:
c-----------------------------------------------------------------------
c
c   ascspl  C  SCC code
c   tecspl  C  technology code 
c   modspl  C  refueling mode (PUMP or CONTAINER)
c   indspl  C  range indicator (TNKTYP or HP)
c   untspl  C  tank volume units indicator (GALLONS or GAL/HP)
c   splpcb  R  beginning of horsepower/tank volume range
c   splpce  R  ending of horsepower/tank volume range
c   nsplar  I  number of codes in spillage array
c   tnkful  R  fraction of tank full of fuel (for diurnal calculations)
c   tnkmtl  R  fraction of tanks that are metal
c   hoslen  R  non rec marine hose length (meters)
c   hosdia  R  non rec marine hose diameter (meters)
c   ncklen  R  rec marine fill neck hose length (meters)
c   nckdia  R  rec marine fill neck hose diameter (meters)
c   srlen   R  rec marine supply/return hose length (meters)
c   srdia   R  rec marine supply/return hose diameter (meters)
c   vntlen  R  rec marine vent hose length (meters)
c   vntdia  R  rec marine vent hose diameter (meters)
c   hosmtl  R  fraction of hoses that are metal
c   hssph   R  hot soak starts per hour of operations
c   diufrc  R  diurnal fractions(1=non rec/rec marine portable plastic,
c                                2=rec-marine installed plastic on trailer,
c                                3=rec-marine installed plastic in water,
c                                4=rec-marine installed metal on trailer,
c                                5=rec-marine installed metal in water)
c    tnke10  R  Non-metal tank permeation E10 adj factor, pre-control
c    hose10  R  Non-Recmarine Non-metal hose permeation E10 adj factor, pre-control
c    ncke10  R  Recmarine fill neck permeation E10 adj factor, pre-control
c    sre10   R  Recmarine supply/return hose permeation E10 adj factor, pre-control
c    vnte10  R  Recmarine vent hose permeation E10 adj factor, pre-control

c
      character*10 ascspl(MXEMFC)
      character*10 tecspl(MXEMFC)
      character*10 untspl(MXEMFC)
      character*9  modspl(MXEMFC)
      character*4  indspl(MXEMFC)
      real*4       splpcb(MXEMFC)
      real*4       splpce(MXEMFC)
      real*4       volspl(MXEMFC)
      integer*4    nsplar
      real*4       tnkful(MXEMFC)
      real*4       tnkmtl(MXEMFC)
      real*4       hoslen(MXEMFC)
      real*4       hosdia(MXEMFC)
      real*4       hosmtl(MXEMFC)
      real*4       hssph(MXEMFC)
      real*4       diufrc(5,MXEMFC)
      real*4       ncklen(MXEMFC)
      real*4       nckdia(MXEMFC)
      real*4       srlen(MXEMFC)
      real*4       srdia(MXEMFC)
      real*4       vntlen(MXEMFC)
      real*4       vntdia(MXEMFC)
      real*4       tnke10(MXEMFC)
      real*4       hose10(MXEMFC)
      real*4       ncke10(MXEMFC)
      real*4       sre10(MXEMFC)
      real*4       vnte10(MXEMFC)
c
      common /splchr/ ascspl, tecspl, modspl, indspl, untspl
      common /spldat/ splpcb, splpce, volspl, nsplar, tnkful,
     &                tnkmtl, hoslen, hosdia, hosmtl, hssph,
     &                diufrc, ncklen, nckdia, srlen, srdia,
     &                vntlen, vntdia, tnke10, hose10, ncke10,
     &                sre10, vnte10
