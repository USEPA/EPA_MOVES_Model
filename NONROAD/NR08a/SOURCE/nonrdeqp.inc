C**** EQIPMENT.INC
c
c-----------------------------------------------------------------------
c
c    Include file for the equipment type and population data to be used
c    by the EPA Nonroad program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     09/01/93  --gmw--  Original development
c     07/22/96  --jlf--  Changes for handling of technology fractions
c     06/09/97  --gwilson-- Took out names of equipment types
c     05/10/04  --dfk--  Added evap tech fractions
c     09/22/04  --dfk--  Increased exh and evap tech code limits to 1500
c                        per version N04n2
c     04/05/05  -cimulus-  changed MXSCRP from 50 to 197
c     04/06/05  -cimulus-  removed scppct_mod, which is no longer used,
c                          due to the new handling implemented by scrptime
c
c-----------------------------------------------------------------------
c   Variables for HP Categories:
c-----------------------------------------------------------------------
c
c    hpclev  R   ranges of HP category 
c
      real*4 hpclev(MXHPC)
c
      common /hpcdat/ hpclev
c
c-----------------------------------------------------------------------
c   Variables for population data:
c-----------------------------------------------------------------------
c
c   regncd  C  region code for the population record 
c   ipopyr  I  base year for the population
c   popeqp  R  array of population values for each equipment type
c   ieqptr  I  index of next equipment type in link list
c   usehrs  I  number of hours in useful life
c   discod  C  string for identifying the data for distributing model years
c   avghpc  R  average HP
c   hprang  R  range of Horsepower
c   npoprc  I  number of records in arrays
c   lctlev  L  flag for determining if county has county specific records
c   lstlev  L  flag for determining if state has county specific records
c   lrglev  L  flag for determining if sub-county specific records
c   ifuel   I  index of the fuel type of the current record
c   
c
      character*10 regncd(MXPOP)
      character*10 discod(MXPOP)
      integer*4    ipopyr(MXPOP)
      real*4       popeqp(MXPOP)
      real*4       avghpc(MXPOP)
      real*4       hprang(2,MXPOP)
      real*4       usehrs(MXPOP)
      integer*4    npoprc
      logical*4    lctlev(NCNTY)
      logical*4    lstlev(NSTATE)
      logical*4    lrglev(NSTATE)
      integer*4    ifuel
c
      common /eqpchr/ regncd, discod
      common /eqpdat/ ipopyr, popeqp, avghpc, hprang, usehrs, npoprc, 
     &                lctlev, lstlev, lrglev, ifuel
c
c-----------------------------------------------------------------------
c   Parameters for technology fraction data:
c-----------------------------------------------------------------------
c
c   TECDEF    C   default value for exhaust technology types
c   MXTECD    I   maximum number of technology codes
c
      character*10 TECDEF
      integer*4    MXTECD
c
      parameter( TECDEF = 'ALL       ')
      parameter( MXTECD =  1500       )
c
c-----------------------------------------------------------------------
c   Variables for technology fraction data:
c-----------------------------------------------------------------------
c
c   tchfrc    R   array of technology fractions
c   asctch    C   array of SCC codes
c   tectyp    C   array of technology names
c   tchhpb    R   array of average horsepowers for the tech fractions
c   tchhpe    R   array of average horsepowers for the tech fractions
c   iyrtch    I   array of model years for the tech fractions
c   ntech     I   count of number of technologies for each equipment type
c   ntchcd    I   count of number of SCC codes with technology fractions
c   ascsav    C   saved prior SCC code (to check if it has changed from prior PRC* call)
c
      character*10 asctch(MXTECD)
      character*10 tectyp(MXTECD,MXTECH)
      integer*4    iyrtch(MXTECD)
      integer*4    ntech(MXTECD)
      integer*4    ntchcd
      real*4       tchfrc(MXTECD,MXTECH)
      real*4       tchhpb(MXTECD)
      real*4       tchhpe(MXTECD)
      integer*4    yrsav
c
      common /tchchr/ asctch, tectyp, yrsav
      common /tchdat/ tchfrc, tchhpb, tchhpe, iyrtch,
     &                ntech, ntchcd
c
c-----------------------------------------------------------------------
c   Parameters for evap technology fraction data:
c-----------------------------------------------------------------------
c
c   MXEVTECD    I   maximum number of evap technology codes
c
      integer*4    MXEVTECD
c
      parameter( MXEVTECD = 1500 )
c
c-----------------------------------------------------------------------
c   Variables for evap technology fraction data:
c-----------------------------------------------------------------------
c
c   evtchfrc    R   array of evap technology fractions
c   ascevtch    C   array of evap SCC codes
c   evtectyp    C   array of evap technology names
c   evtchhpb    R   array of average horsepowers for the evap tech fractions
c   evtchhpe    R   array of average horsepowers for the evap tech fractions
c   iyrevtch    I   array of model years for the evap tech fractions
c   nevtech     I   count of number of evap technologies for each equipment type
c   nevtchcd    I   count of number of SCC codes with evap technology fractions
c
      character*10 ascevtch(MXEVTECD)
      character*10 evtectyp(MXEVTECD,MXEVTECH)
      integer*4    iyrevtch(MXEVTECD)
      integer*4    nevtech(MXEVTECD)
      integer*4    nevtchcd
      real*4       evtchfrc(MXEVTECD,MXEVTECH)
      real*4       evtchhpb(MXEVTECD)
      real*4       evtchhpe(MXEVTECD)
c
      common /evtchchr/ ascevtch, evtectyp
      common /evtchdat/ evtchfrc, evtchhpb, evtchhpe, iyrevtch,
     &                nevtech, nevtchcd
c
c-----------------------------------------------------------------------
c   Parameters for scrappage data:
c-----------------------------------------------------------------------
c
c    MXSCRP   I   maximum number of scrappage bins
c    MXALT    I   maximum number of alternate scrappage curves
c
      integer*4 MXSCRP
      integer*4 MXALT
c
      parameter(MXSCRP = 197 )
      parameter(MXALT  = 10 )
c
c-----------------------------------------------------------------------
c   Variables for scrappage data:
c-----------------------------------------------------------------------
c
c    scpbin    R    array if scrappage bin values
c    scppct    R    percent of equipment left in each bin
c    altbin    R    array for alternate scrappage bin values
c    altpct    R    percent of equipment left in each bin for alternate
c                   scrappage curves
c    altnam    C    names of alternate scrappage curves
c    naltnm    I    number of alternative scrappage curves
c
      character*10 altnam(MXALT)
      real*4       scpbin(MXSCRP)
      real*4       scppct(MXSCRP)
      real*4       altbin(MXSCRP)
      real*4       altpct(MXALT,MXSCRP)
      integer*4    naltnm
c
      common /scpchr/ altnam
      common /scpdat/ scpbin, scppct, altbin, altpct, naltnm
c
c-----------------------------------------------------------------------
c   Variables for EPS2 AMS file:
c-----------------------------------------------------------------------
c
c   itype   C   invetory type
c   irefyr  I   reference year of inventory
c   ibasyr  I   base year of inventory
c   inetyp  C   emissions type
c   infips  C   FIPS state/county code
c   insbrg  C   subregion code
c   insic   C   SIC code
c   inasc   C   ASC code
c   iperod  C   inventory period code
c   ibegdt  I   beginning date/time of inventory
c   ienddt  I   ending date/time of inventory
c   cvtams  R   factor for converting emissions to AMS period
c   namsrc  I   number of AMS records found
c
      character*1  itype
      character*10 inasc
      character*10 insbrg
      character*5  infips
      character*4  insic
      character*2  inetyp
      character*2  iperod
      integer*4    irefyr
      integer*4    ibasyr
      integer*4    ibegdt
      integer*4    ienddt
      integer*4    namsrc
      real*4       cvtams 
c
      common /epschr/ itype, inasc, insbrg, infips, insic, 
     &                inetyp, iperod
      common /epsdat/ irefyr, ibasyr, ibegdt, ienddt, cvtams, namsrc
c
c-----------------------------------------------------------------------
c   Parameters for SI report option:
c-----------------------------------------------------------------------
c
c    MXITCH   I   maximum number of input tech types used for SI reporting
c    MXOTCH   I   maximum number of output tech types used for SI reporting
c
      integer*4 MXITCH 
      integer*4 MXOTCH 
c
      parameter( MXITCH = 42 )
      parameter( MXOTCH = 14 )
c
c-----------------------------------------------------------------------
c  Variables for SI report option:
c-----------------------------------------------------------------------
c
c     sitech   C   array of tech types to be mapped in SI report  
c     indxsi   C   index of mapped tech types to output tech types
c     popsi    R   array for population totals for SI reoprt
c     actsi    R   array for activity totals for SI reoprt
c     fuelsi   R   array for fuel consumption totals for SI report
c     emissi   R   array for emissions totals for SI reoprt
c
      character*10 sitech(MXITCH)
      integer*4    indxsi(MXITCH)
      real*4       popsi(MXOTCH)
      real*4       actsi(MXOTCH)
      real*4       fuelsi(MXOTCH)
      real*4       emissi(MXOTCH,MXPOL)
c
      common /sichr/ sitech
      common /sidat/ indxsi, popsi, actsi, fuelsi, emissi
