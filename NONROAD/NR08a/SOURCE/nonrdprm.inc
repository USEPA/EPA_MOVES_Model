C**** NONRDPRM.INC
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameter and variables for OFFROAD program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     09/01/93  -gmw-   Original development
c     06/14/01  -cah- Add Carbon Mass Fractions by fuel. Used in CLCEMS.
c     09/28/01  -cah- Add SOx Fractions SFCGS2, SFCxxx
c     12/18/01  -cah- Fixed DENCNG from lb/cu-ft to lb/gal (was 0.0517)
c     02/26/02  -cah- Changed SFCDSL from 0.022 to 0.02247 to match 0.157
c     06/25/03  -cah- changed MXHPC from 17 to 18 for 75hp bin
c     05/10/04  -dfk- Added evap tech groups, added tank and hose permeation
c                     which increased MXPOL to 21.
c     06/14/04  -dfk- Removed Resting Loss, reduced to 20.
c     09/22/04  -dfk- changed SOx to SO2 per version NR04, changed MXTCH
c                     and MXEVTCH from 10 to 15, added MXDAYS, MXFIPS
c     11/15/04  -dfk- added separate indexes for rec-marine fill neck,
c                     supply/return, and vent hose permeation. Increased
c                     MXPOL to 23, accordingly. Although calculated
c                     separately, the 3 rec-marine hose emissions are 
c                     added to the non-rec hose pereations at output.
c     03/16/05  -cimulus- increased NSTATE to 53 to accommodate
c                         Puerto Rico and US Virgin Islands
c     03/16/05  -cimulus- increased NCNTY to 3223 to accommodate
c                         Puerto Rico and US Virgin Islands
c     03/16/05  -cimulus- increased MXFIPS to 78 to accommodate
c                         Puerto Rico and US Virgin Islands
c     04/05/05  -cimulus- changed MXAGYR from 50 to 51
c     04/25/05  -cimulus- changed NCNTY from 3223 to 3400 to accommodate
c                         growth.  Another global, nmcnty, will contain
c                         the actual number of used counties.
c     05/05/05  -cimulus- added MINGRWIND
c     05/13/05  -cimulus- corrected the comments regarding CVTTON
c     05/16/05  -cimulus- added year-range validation parameters:
c                         MINYEAR and MAXYEAR
c     07/18/05  -epa- changed MINGRWIND from 1.0 to 0.0001 to fix ALO.
c     08/17/05  -epa- added DIUMIN minimum diurnal temperature.
c
c-----------------------------------------------------------------------
c   Parameter for string length
c-----------------------------------------------------------------------
c
c   MXSTR   I   maximum length of character strings
c
      integer*4 MXSTR
c
      parameter( MXSTR = 80 )
c
c-----------------------------------------------------------------------
c   Parameter for error codes and keywords
c-----------------------------------------------------------------------
c
c    ISUCES  I  error code for uccess
c    IFAIL   I  error code for failure (ome error occured)
c    IRDERR  I  error code for I/O read error
c    IMAIN   I  error code for Main menu
c    IEOF    I  error code for end-of-file encountered on read
c    ISKIP   I  error code for end-of-file encountered on read
c    KEYEND  C  parameter for /END/ keyword 
c    IMISS   I  parameter for missing value of type integer
c    RMISS   R  parameter for missing value of type real
c
      integer*4    IFAIL
      integer*4    ISUCES
      integer*4    IMAIN
      integer*4    IRDERR
      integer*4    IEOF
      integer*4    ISKIP
      character*20 KEYEND
      integer*4    IMISS
      real*4       RMISS
c
      parameter( ISUCES  = 0 )
      parameter( IFAIL   = 1 )
      parameter( IMAIN   = 2 )
      parameter( IRDERR  = 3 )
      parameter( IEOF    = 4 )
      parameter( ISKIP   = 5 )
      parameter( KEYEND  = '/END/' )
      parameter( IMISS   = -9 )
      parameter( RMISS   = -9.0 )
c
c-----------------------------------------------------------------------
c   Parameter for warning messages:
c-----------------------------------------------------------------------
c
c    MXWARN  I   maximum number of warning messages allowed before exiting
c    IDXWEM  I   index into warning counter for emissions warnings
c    IDXWAL  I   index into warning counter for allocation warnings
c    IDXWTC  I   index into warning counter for tech fractions warnings
c    IDXWAC  I   index into warning counter for acitivity warnings
c    IDXWPP  I   index into warning counter for population warnings
c    IDXWSE  I   index into warning counter for seasonality warnings
c
      integer*4 MXWARN
      integer*4 IDXWEM
      integer*4 IDXWAL
      integer*4 IDXWTC
      integer*4 IDXWAC
      integer*4 IDXWPP
      integer*4 IDXWSE
c
      parameter( MXWARN = 5000000 )
      parameter( IDXWEM =  1 )
      parameter( IDXWAL =  2 )
      parameter( IDXWTC =  3 )
      parameter( IDXWAC =  4 )
      parameter( IDXWPP =  5 )
      parameter( IDXWSE =  6 )
c
c-----------------------------------------------------------------------
c   Parameters for user input flags:
c-----------------------------------------------------------------------
c
c  FLAGHI    C   string for reponse of HIGH to altitude flag
c  FLAGLO    C   string for response of LOW to altitude flag
c  FLAGYES   C   string for response of YES to RFG flag
c  FLAGNO    C   string for response of NO to RFG flag
c
      character*5 FLAGHI
      character*5 FLAGLO
      character*5 FLAGYES
      character*5 FLAGNO
c
      parameter( FLAGHI  = 'HIGH ')
      parameter( FLAGLO  = 'LOW  ')
      parameter( FLAGYES = 'YES  ')
      parameter( FLAGNO  = 'NO   ')
c
c-----------------------------------------------------------------------
c    Array bound parameters for hash table:
c-----------------------------------------------------------------------
c
c   MXEQIP  I  maximum number of equipment type 
c
      integer*4 MXEQIP
c
      parameter ( MXEQIP = 25 )
c
c-----------------------------------------------------------------------
c   Parameters for fuel/engine types:
c-----------------------------------------------------------------------
c
c   IDXGS2   I  index for 2-stroke gasoline
c   IDXGS4   I  index for 4-stroke gasoline
c   IDXDSL   I  index for diesel
c   IDXLPG   I  index for LPG
c   IDXCNG   I  index for CNG
c
      integer*4 IDXGS2
      integer*4 IDXGS4
      integer*4 IDXDSL
      integer*4 IDXLPG
      integer*4 IDXCNG
c
      parameter( IDXGS2 = 1 )
      parameter( IDXGS4 = 2 )
      parameter( IDXDSL = 3 )
      parameter( IDXLPG = 4 )
      parameter( IDXCNG = 5 )
c
c-----------------------------------------------------------------------
c   Parameter for fuel density:
c-----------------------------------------------------------------------
c
c   DENGAS   R  fuel denity for gasoline
c   DENCNG   R  fuel denity for CNG
c   DENLPG   R  fuel denity for LPG
c   DENDSL   R  fuel denity for Diesel
c
      real*4 DENGAS
      real*4 DENCNG
      real*4 DENLPG
      real*4 DENDSL
c
      parameter( DENGAS = 6.17   )
      parameter( DENCNG = 0.0061 )
      parameter( DENLPG = 4.507 )
      parameter( DENDSL = 7.1   )
c
c-----------------------------------------------------------------------
c   Parameter for fuel carbon mass fraction:
c-----------------------------------------------------------------------
c
c   CMFGAS   R  carbon mass frac for gasoline
c   CMFCNG   R  carbon mass frac for CNG
c   CMFLPG   R  carbon mass frac for LPG
c   CMFDSL   R  carbon mass frac for Diesel
c
      real*4 CMFGAS
      real*4 CMFCNG
      real*4 CMFLPG
      real*4 CMFDSL
c
      parameter( CMFGAS = 0.87  )
      parameter( CMFCNG = 0.717 )
      parameter( CMFLPG = 0.817 )
      parameter( CMFDSL = 0.87  )
c
c-----------------------------------------------------------------------
c   Parameters for emission factor adjustments:
c-----------------------------------------------------------------------
c
c   SWTGS2   R   baseline sulfur weight content for 2-stroke gasoline
c   SWTGS4   R   baseline sulfur weight content for 4-stroke gasoline
c   SWTLPG   R   baseline sulfur weight content for LPG
c   SWTCNG   R   baseline sulfur weight content for CNG
c   SWTDSL   R   baseline sulfur weight content for Diesel
c
c   SFCGS2   R   fraction of 2-stroke gas sulfur that becomes PM
c   SFCGS4   R   fraction of 4-stroke gas sulfur that becomes PM
c   SFCLPG   R   fraction of LPG sulfur that becomes PM
c   SFCCNG   R   fraction of CNG sulfur that becomes PM
c   SFCDSL   R   fraction of Diesel sulfur that becomes PM
c
c   ALTGS2   R   altitude correction factor for 2-stroke gasoline
c   ALTGS4   R   altitude correction factor for 4-stroke gasoline
c   ALTCNG   R   altitude correction factor for LPG
c   ALTLPG   R   altitude correction factor for CNG
c   ALTDSL   R   altitude correction factor for Diesel
c
c   DIUMIN   R   diurnal minimum temperature
c
      real*4 SWTGS2 
      real*4 SWTGS4 
      real*4 SWTCNG 
      real*4 SWTLPG 
      real*4 SWTDSL 
c
      real*4 SFCGS2
      real*4 SFCGS4
      real*4 SFCLPG
      real*4 SFCCNG
      real*4 SFCDSL
c
      real*4 ALTGS2
      real*4 ALTGS4
      real*4 ALTLPG
      real*4 ALTCNG
      real*4 ALTDSL
c
      real*4 DIUMIN
c
      parameter( SWTGS2 = 0.0339 )
      parameter( SWTGS4 = 0.0339 )
      parameter( SWTCNG = 0.008  )
      parameter( SWTLPG = 0.008  )
      parameter( SWTDSL = 0.33   )
c
      parameter( SFCGS2 =  0.03 )
      parameter( SFCGS4 =  0.03 )
      parameter( SFCLPG =  0.03 )
      parameter( SFCCNG =  0.03 )
      parameter( SFCDSL =  0.02247 )
c
      parameter( ALTGS2 = 1.0 )
      parameter( ALTGS4 = 1.0 )
      parameter( ALTLPG = 1.0 )
      parameter( ALTCNG = 1.0 )
      parameter( ALTDSL = 1.0 )
c
      parameter( DIUMIN = 40.0 )
c
c-----------------------------------------------------------------------
c   Parameter for pollutant order:
c-----------------------------------------------------------------------
c
c   MXPOL   I  maximum number of pollutant categories
c   IDXTHC  I  index of exhaut HC emission factor in arrays
c   IDXCO   I  index of CO emission factor in arrays
c   IDXNOX  I  index of NOx emission factor in arrays
c   IDXPM   I  index of PM emiison factor in arrays
c   IDXCO2  I  index of CO2 emission factor in arrays
c   IDXSOX  I  index of SOx emission factor in arrays
c   IDXCRA  I  index of crank HC emission factor in arrays
c   IDXDIU  I  index of Diurnal evaporative emission factors in arrays
c   IDXTKP  I  index of Tank Permeation emission factors in arrays
c   IDXHOS  I  index of Non-rec marine Hose Permeation emission factors in arrays
c   IDXNCK  I  index of Rec-marine filler neck hose permeation emission factors in arrays
c   IDXSR   I  index of Rec-marine supply/return hose permeation emission factors in arrays
c   IDXVNT  I  index of Rec-marine vent hose permeation emission factors in arrays
c   IDXDIS  I  index of Refueling (Displacement) emission factor in arrays
c   IDXSPL  I  index of Spillage emission factor in arrays
c   IDXSOK  I  index of Hot Soak emission factor in arrays
c   IDXRLS  I  index of Running Loss emission factor in arrays
c   IDXRST  I  index of Resting Loss emission factor in arrays
c   IDSTHC  I  index of Start emissions for THC
c   IDSCO   I  index of Start emissions for CO
c   IDSNOX  I  index of Start emissions for NOx
c   IDSCO2  I  index of Start emissions for CO2
c   IDSSOX  I  index of Start emissions for SOx
c   IDSPM   I  index of Start emissions for PM
c
      integer*4 MXPOL
      integer*4 IDXTHC
      integer*4 IDXCO
      integer*4 IDXNOX
      integer*4 IDXCO2
      integer*4 IDXSOX
      integer*4 IDXPM
      integer*4 IDXCRA
      integer*4 IDXDIU
      integer*4 IDXDIS
      integer*4 IDXTKP
      integer*4 IDXHOS
      integer*4 IDXNCK
      integer*4 IDXSR
      integer*4 IDXVNT
      integer*4 IDXSPL
      integer*4 IDXSOK
      integer*4 IDXRLS
c      integer*4 IDXRST
      integer*4 IDSTHC 
      integer*4 IDSCO
      integer*4 IDSNOX
      integer*4 IDSCO2
      integer*4 IDSSOX
      integer*4 IDSPM
c
      parameter( MXPOL  = 23 )
      parameter( IDXTHC =  1 )
      parameter( IDXCO  =  2 )
      parameter( IDXNOX =  3 )
      parameter( IDXCO2 =  4 )
      parameter( IDXSOX =  5 )
      parameter( IDXPM  =  6 )
      parameter( IDXCRA =  7 )
      parameter( IDXDIU =  8 )
      parameter( IDXTKP =  9 )
      parameter( IDXHOS = 10 )
      parameter( IDXNCK = 11 )
      parameter( IDXSR  = 12 )
      parameter( IDXVNT = 13 )
      parameter( IDXSOK = 14 )
      parameter( IDXDIS = 15 )
      parameter( IDXSPL = 16 )
      parameter( IDXRLS = 17 )
c      parameter( IDXRST = 15 )
      parameter( IDSTHC = 18 )
      parameter( IDSCO  = 19 )
      parameter( IDSNOX = 20 )
      parameter( IDSCO2 = 21 )
      parameter( IDSSOX = 22 )
      parameter( IDSPM  = 23 )
c
c----------------------------------------------------------------------
c  Parameters for SAROAD pollutant codes:
c
c    ISCNOX   I  NOx       SAROAD code
c    ISCTHC   I  THC       SAROAD code
c    ISCCO    I  CO        SAROAD code
c    ISCPM    I  PM10      SAROAD code
c    ISCSOX   I  SOx       SAROAD code
c
      integer*4 ISCNOX
      integer*4 ISCTHC
      integer*4 ISCCO
      integer*4 ISCPM
      integer*4 ISCSOX
c
      parameter( ISCNOX = 42603 )
      parameter( ISCTHC = 43101 )
      parameter( ISCCO  = 42101 )
      parameter( ISCPM  = 81102 )
      parameter( ISCSOX = 42401 )
c
c
c----------------------------------------------------------------------
c  Parameters for AMS criteria pollutant names:
c
c    AMSNOX   C  NOx
c    AMSTHC   C  THC
c    AMSCO    C  CO   
c    AMSPM    C  PM10 
c    AMSSOX   C  SOx  
c    AMSCO2   C  CO2
c
      character*4 AMSNOX
      character*4 AMSTHC
      character*4 AMSCO
      character*4 AMSPM
      character*4 AMSSOX
      character*4 AMSCO2
c
      parameter( AMSNOX = 'NOX ' )
      parameter( AMSTHC = 'THC ' )
      parameter( AMSCO  = 'CO  ' )
      parameter( AMSPM  = 'PM  ' )
      parameter( AMSSOX = 'SO2 ' )
      parameter( AMSCO2 = 'CO2 ' )
c
c---------------------------------------------------------------------------
c  Parameters for pollutant names within model
c      
c      NMETHC     C  Exhaust THC
c      NMENOX     C  Exhaust NOX
c      NMECO      C  Exhaust CO
c      NMESOX     C  Exhaust SOX
c      NMECO2     C  Exhaust CO2
c      NMEPM      C  Exhaust PM
c      NMCRA      C  Crankcase
c      NMDIU      C  Diurnal
c      NMREF      C  Refueling
c      NMSPL      C  Spillage
c      NMHSOK     C  Hot Soak
c      NMTKP      C  Tank Permeation
c      NMHOS      C  Non-Rec-Marine Hose Permeation
c      NMNCK      C  Rec-Marine Fill Neck Hose Permeation
c      NMRS       C  Rec-Marine Supply/Return Hose Permeation
c      NMVNT      C  Rec-Marine Vent Hose Permeation
c      NMRST      C  Resting Loss
c      NMRLS      C  Running Loss
c      NMSTHC     C  Start THC
c      NMSNOX     C  Start NOX
c      NMSCO      C  Start CO
c      NMSSOX     C  Start NOX
c      NMSCO2     C  Start CO2
c      NMSPM      C  Start PM
c
      character*10   NMETHC
      character*10   NMENOX
      character*10   NMECO
      character*10   NMESOX
      character*10   NMECO2
      character*10   NMEPM
      character*10   NMCRA
      character*10   NMDIU
      character*10   NMREF
      character*10   NMSPL
      character*10   NMHSOK
      character*10   NMTKP
      character*10   NMHOS
      character*10   NMNCK
      character*10   NMSR
      character*10   NMVNT
c      character*10   NMRST
      character*10   NMRLS
      character*10   NMSTHC
      character*10   NMSNOX
      character*10   NMSCO
      character*10   NMSSOX
      character*10   NMSCO2
      character*10   NMSPM
c
      parameter(NMETHC = 'Exh. THC'  )
      parameter(NMENOX = 'Exh. NOX'  )
      parameter(NMECO  = 'Exh. CO'   )
      parameter(NMESOX = 'Exh. SO2'  )
      parameter(NMECO2 = 'Exh. CO2'  )
      parameter(NMEPM  = 'Exh. PM'   )
      parameter(NMCRA  = 'Crankcase' )
      parameter(NMDIU  = 'Diurnal'   )
      parameter(NMREF  = 'Refueling' )
      parameter(NMSPL  = 'Spillage'  )
      parameter(NMHSOK = 'Hot Soak'  )
      parameter(NMTKP  = 'Tank Perm' )
      parameter(NMHOS  = 'Hose Perm' )
      parameter(NMNCK  = 'Neck Perm' )
      parameter(NMSR   = 'S/R Perm' )
      parameter(NMVNT  = 'Vent Perm' )
      
      parameter(NMRLS  = 'RuningLoss')
c      parameter(NMRST  = 'RestngLoss')
      parameter(NMSTHC = 'Start THC' )
      parameter(NMSNOX = 'Start NOX' )
      parameter(NMSCO  = 'Start CO'  )
      parameter(NMSSOX = 'Start SO2' )
      parameter(NMSCO2 = 'Start CO2' )
      parameter(NMSPM  = 'Start PM'  )
c
c-----------------------------------------------------------------------
c   FUZZ factors:
c-----------------------------------------------------------------------
c
c    EFUZZ    R  value for insignificant emission
c
      real*4 EFUZZ
c
      parameter( EFUZZ = 1.0E-5 )
c
c-----------------------------------------------------------------------
c   Unit Conversion:
c-----------------------------------------------------------------------
c
c
c  CVTTON   R  converion factor from grams to tons
c
      real*4 CVTTON
c
      parameter( CVTTON =  1.102311E-06 )
c
c-----------------------------------------------------------------------
c   Parameters for array bounds:
c-----------------------------------------------------------------------
c
c  NSTATE   I   number of states in the country
c  NCNTY    I   number of conties in the country
c  NEQCAT   I   number of equipment categories
c  MXNAA    I   maximum number of NAA designations
c  MXPOP    I   maximum number of population records per SCC
c  MXPFIL   I   maximum number of population files
c  MXTECH   I   maximum number of exhaust tech types
c  MXEVTECH I   maximum number of evap tech types
c  MXHPC    I   maximum number of horepower categories
c  MXRGDF   I   maximum number of region definitions allowed
c  MXCTDF   I   maximum number of counties in each region
c  MXSUBC   I   maximum number of subcounty entries
c  MXAGYR   I   maximum number of years in model year distribution
c  MXDAYS   I   maximum number of days in a year
c  MXFIPS   I   maximum number of FIPS codes
c
      integer*4 NSTATE
      integer*4 NCNTY
      integer*4 NEQCAT
      integer*4 MXNAA
      integer*4 MXPOP
      integer*4 MXPFIL
      integer*4 MXTECH
      integer*4 MXEVTECH
      integer*4 MXHPC
      integer*4 MXRGDF
      integer*4 MXCTDF
      integer*4 MXSUBC
      integer*4 MXAGYR
      integer*4 MXDAYS
      integer*4 MXFIPS
c
      parameter( NSTATE =   53    )
      parameter( NCNTY  =   3400  )
      parameter( NEQCAT =   12    )
      parameter( MXNAA  =   50    )
      parameter( MXPOP  =   1000  )
      parameter( MXPFIL =   3265  )
      parameter( MXTECH =   15    )
      parameter( MXEVTECH = 15    )
      parameter( MXHPC  =   18    )
      parameter( MXRGDF =   50    )
      parameter( MXCTDF =   10    )
      parameter( MXSUBC =   300   )
      parameter( MXAGYR =   51    )
      parameter( MXDAYS =   365   )
      parameter( MXFIPS =   78    )
c
c-----------------------------------------------------------------------
c   Parameters for period definition:
c-----------------------------------------------------------------------
c
c    PERANN  C  string for specifying ANNUAL period
c    PERMTH  C  string for specifying MONTHLY period
c    PERSES  C  string for specifying SEASONAL period
c    SUMTYP  C  string for specifying sum of typical day in period
c    SUMTOT  C  string for specifying sum of total in period
c
      character*10 PERANN
      character*10 PERMTH
      character*10 PERSES
      character*15 SUMTYP
      character*15 SUMTOT
c
      parameter( PERANN = 'ANNUAL    '      )
      parameter( PERMTH = 'MONTHLY   '      )
      parameter( PERSES = 'SEASONAL  '      )
      parameter( SUMTYP = 'TYPICAL DAY    ' )
      parameter( SUMTOT = 'PERIOD TOTAL   ' )
c
c    IDXANN  I  index of type that is ANNUAL period
c    IDXMTH  I  index of type that is MONTHLY period
c    IDXSES  I  index of type that is SEASONAL period
c    IDXTYP  I  index of type that is DAILY period
c    IDXTOT  I  index of type that is DAILY period
c
      integer*4 IDXANN
      integer*4 IDXMTH
      integer*4 IDXSES
      integer*4 IDXTYP
      integer*4 IDXTOT
c
      parameter( IDXANN = 1 )
      parameter( IDXMTH = 2 )
      parameter( IDXSES = 3 )
      parameter( IDXTYP = 1 )
      parameter( IDXTOT = 2 )
c
c-----------------------------------------------------------------------
c   Parameters for specifying month of year:
c-----------------------------------------------------------------------
c
c    MON***  C  string for specifying month of ***
c  
      character*10 MONJAN
      character*10 MONFEB
      character*10 MONMAR
      character*10 MONAPR
      character*10 MONMAY
      character*10 MONJUN
      character*10 MONJUL
      character*10 MONAUG
      character*10 MONSEP
      character*10 MONOCT
      character*10 MONNOV
      character*10 MONDEC
c
      parameter( MONJAN = 'JANUARY   ' )
      parameter( MONFEB = 'FEBRUARY  ' )
      parameter( MONMAR = 'MARCH     ' )
      parameter( MONAPR = 'APRIL     ' )
      parameter( MONMAY = 'MAY       ' )
      parameter( MONJUN = 'JUNE      ' )
      parameter( MONJUL = 'JULY      ' )
      parameter( MONAUG = 'AUGUST    ' )
      parameter( MONSEP = 'SEPTEMBER ' )
      parameter( MONOCT = 'OCTOBER   ' )
      parameter( MONNOV = 'NOVEMBER  ' )
      parameter( MONDEC = 'DECEMBER  ' )
c
c    IDX***  I  index in arrays of month of ***
c  
      integer*4 IDXJAN
      integer*4 IDXFEB
      integer*4 IDXMAR
      integer*4 IDXAPR
      integer*4 IDXMAY
      integer*4 IDXJUN
      integer*4 IDXJUL
      integer*4 IDXAUG
      integer*4 IDXSEP
      integer*4 IDXOCT
      integer*4 IDXNOV
      integer*4 IDXDEC
c
      parameter( IDXJAN =  1 )
      parameter( IDXFEB =  2 )
      parameter( IDXMAR =  3 )
      parameter( IDXAPR =  4 )
      parameter( IDXMAY =  5 )
      parameter( IDXJUN =  6 )
      parameter( IDXJUL =  7 )
      parameter( IDXAUG =  8 )
      parameter( IDXSEP =  9 )
      parameter( IDXOCT = 10 )
      parameter( IDXNOV = 11 )
      parameter( IDXDEC = 12 )
c
c-----------------------------------------------------------------------
c    Parameters for day of week:
c-----------------------------------------------------------------------
c
c    WEEKDY  C   character string for specifying Weekday
c    WEEKDS  C   character string for specifying Weekday
c    WEEKND  C   character string for specifying Weekend
c
      character*10 WEEKDY
      character*10 WEEKDS
      character*10 WEEKND
c
      parameter( WEEKDY = 'WEEKDAY   ' )
      parameter( WEEKDS = 'WEEKDAYS  ' )
      parameter( WEEKND = 'WEEKEND   ' )
c
c    IDXWKD  I   index of weekday in arrays
c    IDXWKE  I   index of weekend in arrays
c
      integer*4 IDXWKD
      integer*4 IDXWKE
c
      parameter( IDXWKD = 1 )
      parameter( IDXWKE = 2 )
c
c-----------------------------------------------------------------------
c    Parameters for seasons:
c-----------------------------------------------------------------------
c
c    SESWTR  C  string for specifying Winter season
c    SESSPR  C  string for specifying Spring season
c    SESSUM  C  string for specifying Summer season
c    SESFAL  C  string for specifying Fall season
c
      character*10 SESWTR
      character*10 SESSPR
      character*10 SESSUM
      character*10 SESFAL
c
      parameter( SESWTR = 'WINTER    ' )
      parameter( SESSPR = 'SPRING    ' )
      parameter( SESSUM = 'SUMMER    ' )
      parameter( SESFAL = 'AUTUMN    ' )
c
c    IDXWTR  I  index of Winter season in arrays
c    IDXSPR  I  index of Spring season in arrays
c    IDXSUM  I  index of Summer season in arrays
c    IDXFAL  I  index of Fall season in arrays
c
      integer*4 IDXWTR
      integer*4 IDXSPR
      integer*4 IDXSUM
      integer*4 IDXFAL
c
      parameter( IDXWTR = 1 )
      parameter( IDXSPR = 2 )
      parameter( IDXSUM = 3 )
      parameter( IDXFAL = 4 )
c
c-----------------------------------------------------------------------
c    Parameters for growth factor:
c-----------------------------------------------------------------------
c
c    MINGRWIND  R   minimum growth indicator, used to solve issue caused
c                   by growth indicator of 0 (to avoid divide by 0)
c
      real*4 MINGRWIND
c
      parameter( MINGRWIND = 0.0001 )
c
c
c-----------------------------------------------------------------------
c    Parameters for input validation:
c-----------------------------------------------------------------------
c
c    MINYEAR  I   minimum year (episode, growth, tech, etc.)
c    MAXYEAR  I   maximum year (episode, growth, tech, etc.)
c
      integer*4 MINYEAR
      integer*4 MAXYEAR
c
      parameter( MINYEAR = 1970 )
      parameter( MAXYEAR = 2060 )
c
