C**** BLKNON
c
      BLOCK DATA 
c
c-----------------------------------------------------------------------
c
c    Block data routine for initializing the global data structures 
c    used in the NONROAD model.
c  
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      12/06/95  --djk--  original development
c      07/27/98  --gwilson-- added the SI report data 
c      06/25/03  -cah- add 75hp split to hpclev
c      06/14/04  --dfk--  commented out resting loss variables
c      11/15/04  --dfk--  added code for rec-marine 3-hose
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdusr.inc'
      include 'nonrdio.inc'
      include 'nonrdefc.inc'
      include 'nonrdeqp.inc'
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   NSIEMS   I   number of elements in SI emissions array
c
      integer*4 NSIEMS
c
      parameter( NSIEMS = MXOTCH * MXPOL )
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- scratch filenames ----
c
      data spopfl /'poptmp.txt'/
      data indfl /'indtmp.txt'/
      data grwfl /'grwtmp.txt'/
c
c  --- counters for deterioation factor lists ---
c 
      data ndtfac /MXPOL*0/
c
c  --- /PERIOD/ packet data ---
c
      data ldays /2*.FALSE./
      data lmonth /12*.FALSE./
      data idseas /IDXWTR, IDXWTR, 
     &             IDXSPR, IDXSPR, IDXSPR, 
     &             IDXSUM, IDXSUM, IDXSUM, 
     &             IDXFAL, IDXFAL, IDXFAL, 
     &                             IDXWTR/
c
c --- AMS criteria pollutant names and SAROAD codes ---
c
      data amspol(IDXTHC) / AMSTHC /
      data amspol(IDXNOX) / AMSNOX /
      data amspol(IDXSOX) / AMSSOX /
      data amspol(IDXCO)  / AMSCO  /
      data amspol(IDXCO2) / AMSCO2 /
      data amspol(IDXPM)  / AMSPM  /
      data amspol(IDXCRA) / AMSTHC /
      data amspol(IDXDIU) / AMSTHC /
      data amspol(IDXDIS) / AMSTHC /
      data amspol(IDXTKP) / AMSTHC /
      data amspol(IDXHOS) / AMSTHC /
      data amspol(IDXNCK) / AMSTHC /
      data amspol(IDXSR ) / AMSTHC /
      data amspol(IDXVNT) / AMSTHC /
      data amspol(IDXSPL) / AMSTHC /
      data amspol(IDXSOK) / AMSTHC /
      data amspol(IDXRLS) / AMSTHC /
C      data amspol(IDXRST) / AMSTHC /
      data amspol(IDSTHC) / AMSTHC /
      data amspol(IDSNOX) / AMSNOX /
      data amspol(IDSCO)  / AMSCO  /
      data amspol(IDSPM)  / AMSPM  /
      data amspol(IDSSOX) / AMSSOX /
      data amspol(IDSCO2) / AMSCO2 /
c
      data iscod(IDXTHC) / ISCTHC /
      data iscod(IDXNOX) / ISCNOX /
      data iscod(IDXCO)  / ISCCO  /
      data iscod(IDXSOX) / ISCSOX /
      data iscod(IDXPM)  / ISCPM  /
c      data iscod(IDXCO2) / ISCCO2 /
      data iscod(IDXCRA) / ISCTHC /
      data iscod(IDXDIU) / ISCTHC /
      data iscod(IDXDIS) / ISCTHC /
      data iscod(IDXSPL) / ISCTHC /
      data iscod(IDXSOK) / ISCTHC /
      data iscod(IDXRLS) / ISCTHC /
C      data iscod(IDXRST) / ISCTHC /
      data iscod(IDSTHC) / ISCTHC /
      data iscod(IDSNOX) / ISCNOX /
      data iscod(IDSCO)  / ISCCO  /
      data iscod(IDSPM)  / ISCPM  /
      data iscod(IDSSOX) / ISCSOX /
c      data iscod(IDSCO2) / ISCCO2 /
c
c --- model pollutant names  ---
c
      data polnam(IDXTHC) / NMETHC /
      data polnam(IDXNOX) / NMENOX /
      data polnam(IDXSOX) / NMESOX /
      data polnam(IDXCO)  / NMECO  /
      data polnam(IDXCO2) / NMECO2 /
      data polnam(IDXPM)  / NMEPM  /
      data polnam(IDXCRA) / NMCRA  /
      data polnam(IDXDIU) / NMDIU  /
      data polnam(IDXDIS) / NMREF  /
      data polnam(IDXTKP) / NMTKP  /
      data polnam(IDXHOS) / NMHOS  /
      data polnam(IDXNCK) / NMNCK  /
      data polnam(IDXSR)  / NMSR   /
      data polnam(IDXVNT) / NMVNT  /
      data polnam(IDXSPL) / NMSPL  /
      data polnam(IDXSOK) / NMHSOK /
      data polnam(IDXRLS) / NMRLS  /
C      data polnam(IDXRST) / NMRST  /
      data polnam(IDSTHC) / NMSTHC /
      data polnam(IDSNOX) / NMSNOX /
      data polnam(IDSCO)  / NMSCO  /
      data polnam(IDSPM)  / NMSPM  /
      data polnam(IDSSOX) / NMSSOX /
      data polnam(IDSCO2) / NMSCO2 /
c
c --- intialize global horse power categories ---
c
      data hpclev / 1., 3., 6., 11., 16., 25., 40., 50., 75., 100., 
     &              175., 300., 600., 750., 1000., 1200., 2000., 
     &              3000./
c
c   ---- array of tech types used in SI report ---
c
      data sitech /
     & "G2N1"   ,"G2N11"   ,"G2N12"   ,"G4N1O"   ,"G4N1O1"   ,"G4N1O2" ,
     & "G4N1S"  ,"G4N1S1"  ,"G4N1S2"  ,"G4N1SC"  ,"G4N1SC1"  ,"G4N1SC2",
     & "G2N2"   ,"G2N21"   ,"G2N22"   ,"G4N2O"   ,"G4N2O1"   ,"G4N2O2" ,
     & "G4N2S"  ,"G4N2S1"  ,"G4N2S2"  ,"G2H3"    ,"G2H31"    ,"G2H32"  ,
     & "G2H3C"  ,"G2H3C1"  ,"G2H3C2"  ,"G2H4"    ,"G2H41"    ,"G2H42"  ,
     & "G2H4C"  ,"G2H4C1"  ,"G2H4C2"  ,"G4H4"    ,"G4H41"    ,"G4H42"  ,
     & "G2H5"   ,"G2H51"   ,"G2H52"   ,"G2H5C"   ,"G2H5C1"   ,"G2H5C2" /
c
c   ---- index into arrays for each tech type ---
c
      data indxsi /
     &    1,        1,         1,         2,         2,          2     ,
     &    3,        3,         3,         4,         4,          4     ,
     &    5,        5,         5,         6,         6,          6     ,
     &    7,        7,         7,         8,         8,          8     ,
     &    9,        9,         9,        10,        10,         10     ,
     &   11,       11,        11,        12,        12,         12     ,
     &   13,       13,        13,        14,        14,         14     /
c
c   --- totals arrays for SI report ---
c
      data popsi /MXOTCH*0.0/
      data actsi /MXOTCH*0.0/
      data fuelsi /MXOTCH*0.0/
      data emissi /NSIEMS*0.0/
c
      end

