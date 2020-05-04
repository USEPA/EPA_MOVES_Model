c*** TEMPORAL.INC
c
c-----------------------------------------------------------------------
c
c   Include for the seasonality data for the OFFROAD program.
c
c-----------------------------------------------------------------------
c   LOG
c-----------------------------------------------------------------------
c   07/19/96  --jlf-- removed ithour (which was not used)
c   03/20/17  --dbc-- increased MXTASC for MOVES2014a
c-----------------------------------------------------------------------
c   Parameters for array dimensions:
c-----------------------------------------------------------------------
c
c   MXTASC  I  maximum number of SCC codes in the temporal arrays
c   MXMTH   I  maximum number of months
c   MXDAY   I  maximum number of days (weekday, weekend)
c   MXHOUR  I  maximum number of hours
c
      integer*4 MXTASC
      integer*4 MXMTH
      integer*4 MXDAY
      integer*4 MXHOUR
c
      parameter( MXTASC =  12000 )
      parameter( MXMTH  =     12 )
      parameter( MXDAY  =      2 )
      parameter( MXHOUR =     24 )
c
c-----------------------------------------------------------------------
c   Temporal allocation arrays:
c-----------------------------------------------------------------------
c
c    asctpm  C  SCC code of monthly temporal record
c    asctpl  C  SCC code of daily temporal records
c    sbrtpm  C  subregion code of monthly temporal code
c    sbrtpl  C  subregion code of daily temporal record
c    ntplcd  I  number of daily temporal records
c    ntplmn  I  number of monthly temporal records
c    mthfac  R  monthly temporal fractions
c    dayfac  R  daily temporal fractions
c
      character*10 asctpl(MXTASC)
      character*10 asctpm(MXTASC)
      character*5  sbrtpl(MXTASC)
      character*5  sbrtpm(MXTASC)
      integer*4    ntplcd
      integer*4    ntplmn
      real*4       mthfac(MXMTH,MXTASC)
      real*4       dayfac(MXDAY,MXTASC)
c
      common /tplchr/ asctpl, sbrtpl, sbrtpm, asctpm
      common /tpldat/ ntplcd, ntplmn, mthfac, dayfac
c
c-----------------------------------------------------------------------
c   Default allocation arrays:
c-----------------------------------------------------------------------
c
c   defmth  R  array of default monthly temporal allocation
c   defday  R  array of default daily temporal allocation
c   defhr   R  array of default hourly temporal allocation
c
      real*4 defmth(MXMTH)
      real*4 defday(MXDAY)
      real*4 defhr(MXHOUR)
c
      common /defdat/ defmth, defday, defhr
c
c-------------------------------------------------------------------------
c   Parameters for region definitions file:
c-------------------------------------------------------------------------
c
c  MXREGN    C    maximum number of region definitions
c
      integer*4 MXREGN
c
      parameter( MXREGN = 55 )
c
c-------------------------------------------------------------------------
c   Variables for regions definition file:
c-------------------------------------------------------------------------
c
c  isttcnt    I  number of FIPS for each region definition
c  nregcd     I  the number of region definitions
c  rgdfcd     C  list of region definition codes
c  rgstt      C  list of FIPS codes for each region
c
      character*5  rgdfcd(MXREGN)
      character*5  rgstt(MXREGN,NCNTY)
      integer*4    isttcnt(MXREGN)
      integer*4    nregcd
c
      common /rdfchr/ rgdfcd, rgstt
      common /rdfdat/ isttcnt, nregcd
