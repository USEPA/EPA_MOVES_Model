C**** NONRDUSR.INC
c
c-----------------------------------------------------------------------
c
c    Include file user input parameters and variables for OFFROAD program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     12/01/95  -djk-      Original development
c     02/15/97  -gwilson-  Wholesale changes for BETA version
c     10/29/01  -charvey-  Adds optional base sulfur array
c     06/25/04  -dfk-      Added daily temperature and RVP arrays
c     09/22/04  -dfk-      Added rec marine diesel sulfur
c     03/15/05  -cimulus-  Added growth year
c     03/15/05  -cimulus-  Added technology year
c     07/15/05  -cimulus-  Removed commercial marine related constants
c                          and variables
c
c-----------------------------------------------------------------------
c   Variables for user options for each model run:
c-----------------------------------------------------------------------
c
c    title1  C  first user supplied string of model run description
c    title2  C  second user supplied string of model run description
c    fulrvp  R  fuel RVP in psi (for evap temperature correction)
c    oxypct  R  percent by weight of oxygen in the fuel
c    ethmkt  R  ethanol blend market share, percent
c    ethvpct R  volume percent ethanol in ethanol blends
c    tempmn  R  minumum daily temperature (for diurnal adjustment)
c    tempmx  R  maximum daily temperature (for diurnal adjustment)
c    amtemp  R  representative ambient temperature
c    amrvp   R  representative daily fuel RVP 
c    soxgas  R  weight fraction of sulfur in gasoline
c    soxdsl  R  weight fraction of sulfur in diesel (land)
c    soxdsm  R  weight fraction of sulfur in rec marine diesel
c    soxcng  R  weight fraction of sulfur in LPG/CNG
c    lhigh   L  flag for determining if in high altitude region
c    lrfg    L  flag for determining if RFG adjustments should be applied
c    daytmp  R  daily maximum, minimum, and average temperatures, by state/region
c    dayrvp  R  daily fuel RVP, by state/region
c
      character*80 title1
      character*80 title2
      real*4       fulrvp
      real*4       oxypct
      real*4       ethmkt, ethvpct
      real*4       tempmn
      real*4       tempmx
      real*4       amtemp
      real*4       soxgas
      real*4       soxdsl
      real*4       soxdsm
      real*4       soxcng
      real*4       daytmp(MXDAYS,3,0:MXFIPS)
      real*4       dayrvp(MXDAYS,0:MXFIPS)
      logical*4    lhigh
      logical*4    lrfg
c
      common /optchr/ title1, title2
      common /optdat/ fulrvp, oxypct, ethmkt, ethvpct, tempmn,
     &                tempmx, amtemp, soxgas, soxdsl, soxdsm, soxcng,
     &                daytmp, dayrvp, lhigh, lrfg
c
c-----------------------------------------------------------------------
c   Variables for /PERIOD/ packet:
c-----------------------------------------------------------------------
c
c   iprtyp  I  code for period type
c   ismtyp  I  code for type of sum 
c   iepyr   I  year of episode
c   imonth  I  index of the month of the year
c   iday    I  index of the day of week
c   iseasn  I  index of the season of the year
c   ldays   L  flag for determining if day of week is used
c   lmonth  L  flag for determining if month of year is used
c   idseas  I  array of indexes for which season each month is in
c   igryr   I  year of growth calculation
c   itchyr  I  year of technology selection
c             
      integer*4 iprtyp
      integer*4 ismtyp
      integer*4 iepyr
      integer*4 imonth
      integer*4 iday
      integer*4 iseasn     
      logical*4 ldays(2)
      logical*4 lmonth(12)
      integer*4 idseas(12)
      integer*4 igryr
      integer*4 itchyr
c
      common /perdat/ iprtyp, ismtyp, iepyr, imonth, iday, iseasn,
     &                ldays, lmonth, idseas, igryr, itchyr
c
c-------------------------------------------------------------------------
c   Parameters for REGION packet:
c-------------------------------------------------------------------------
c
c  USTOT     C    code for national total inventory
c  NATION    C    code for 50-state inventory
c  STATE     C    code for state-level inventory
c  COUNTY    C    code for county-level inventory
c  SUBCTY    C    code for subcounty-level inventory
c
      character*10 USTOT
      character*10 NATION
      character*10 STATE
      character*10 COUNTY
      character*10 SUBCTY
c
      parameter( USTOT  = 'US TOTAL  ' )
      parameter( NATION = '50STATE   ' )
      parameter( STATE  = 'STATE     ' )
      parameter( COUNTY = 'COUNTY    ' )
      parameter( SUBCTY = 'SUBCOUNTY ' )
c
c-------------------------------------------------------------------------
c   Variables for REGION packet:
c-------------------------------------------------------------------------
c
c  reglvl     C  the level for emissions estimates
c  reglst     C  list of region codes to be used
c  regcty     C  coutny portopm of list of region codes to be used
c  nregin     I  number of region codes in the list
c
      character*10 reglvl
      character*10 reglst(MXSUBC)
      character*10 regcty(MXSUBC)
      integer*4    nregin
c
      common /rgdchr/ reglvl, reglst, regcty
      common /rgddat/ nregin
c
c-----------------------------------------------------------------------
c   Parameters for equipment names and codes:
c-----------------------------------------------------------------------
c
c   NEQNAM   I   number of equipment names and codes
c
      integer*4 NEQNAM
c
      parameter( NEQNAM = 575 )
c
c-----------------------------------------------------------------------
c   Variables for equipment categories:
c-----------------------------------------------------------------------
c
c    eqpcod   C   SCC codes of individual equipments
c    lascat   L   flag for determing if individual equipment was selected
c    lascal   L   flag for determining if all SCC are selected
c    nrcasc   I   number of records found for each SCC
c
      character*10 eqpcod(NEQNAM)
      integer*4    nrcasc(NEQNAM)
      logical*4    lascat(NEQNAM)
      logical*4    lascal
c
      common /eqchr/ eqpcod
      common /eqdat/ lascat, lascal, nrcasc
c
c-----------------------------------------------------------------------
c   Variables for pollutant names and SAROD codes:
c-----------------------------------------------------------------------
c
c    iscod    I   vector that holds SAROD codes indexed by pollutant
c    amspol   C   vector that holds AMS criteria pollutant names  
c
      integer*4    iscod(MXPOL)
      character*4  amspol(MXPOL)
c
      common /amschr/ amspol
      common /amsdat/ iscod
c
c   polnam   C    vector that holds model pollutant names
c
      character*10 polnam(MXPOL)
c
      common /polchr/ polnam
c
c-----------------------------------------------------------------------
c   Variables for optional Stage II controls:
c-----------------------------------------------------------------------
c
c   stg2fac  R    percent reduction due to stage II controls
c
      real*4        stg2fac
c
      common /stg2/ stg2fac
c
c-----------------------------------------------------------------------
c   Variables and parameters for optional Base Sulfur by Tech Type:
c-----------------------------------------------------------------------
c
c   MXSULF  I    maximum number of alternate tech records
c
      integer*4 MXSULF
c
      parameter( MXSULF = 100)
c
c   sultec  C    Tech types with alternate base (cert) sulfur
c   sulalt  R    Aternate base (cert) sulfur level for PM adjustment
c   sulcnv  R    Sulfur conversion factor oefficient
c   numalt  I    number of tecords read
c
      character*10  sultec(MXSULF)
      real*4        sulalt(MXSULF)
      real*4        sulcnv(MXSULF)
      integer*4     numalt
c
      common /sulchr/ sultec
      common /suldat/ sulalt, sulcnv, numalt
c
c-----------------------------------------------------------------------
c   Common block used in CALUDI:
c-----------------------------------------------------------------------
c
      real*4    feet
      integer*4 irejn
      real*4    alt
      integer*4 initpr
c
      COMMON /REGION/ FEET(2),IREJN,ALT,INITPR
