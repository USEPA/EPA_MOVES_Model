C**** IN1FIP
c
      subroutine in1fip()
c
c-----------------------------------------------------------------------
c
c    Initiliazes the county and state names and the county codes.  The
c    code is separated into a couple of subroutines so the compiler 
c    can handle it.
c  
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      06/10/97  --gwilson--   original development
c      03/24/03  --charvey--   change 12025 Dade to 12086 Miami-Dade
c      04/22/05  --cimulus--   removed fipcod and cntynm array setup,
c                              and commented out nconty and idxcty setup
c                              because such information will be read from
c                              data\allocate\FIPS.DAT
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdusr.inc'
      include 'nonrdreg.inc'
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c    ----  ALABAMA ----
c
       statcd(1)    = '01000'
       statnm(1)    = 'Alabama'
C       nconty(1)    =    67
C       idxcty(1)    =     1
c
c
c    ----  ALASKA ----
c
       statcd(2)    = '02000'
       statnm(2)    = 'Alaska'
c       nconty(2)    =    27
c       idxcty(2)    =    68
c
c    ----  ARIZONA ----
c
       statcd(3)    = '04000'
       statnm(3)    = 'Arizona'
c       nconty(3)    =    15
c       idxcty(3)    =    95
c
c    ----  ARKANSAS ----
c
       statcd(4)    = '05000'
       statnm(4)    = 'Arkansas'
c       nconty(4)    =    75
c       idxcty(4)    =   110
c
c    ----  CALIFORNIA ----
c
       statcd(5)    = '06000'
       statnm(5)    = 'California'
c       nconty(5)    =    58
c       idxcty(5)    =   185
c
c    ----  COLORADO ----
c
       statcd(6)    = '08000'
       statnm(6)    = 'Colorado'
c       nconty(6)    =    63
c       idxcty(6)    =   243
c
c    ----  CONNECTICUT ----
c
       statcd(7)    = '09000'
       statnm(7)    = 'Connecticut'
c       nconty(7)    =     8
c       idxcty(7)    =   306
c
c    ----  DELAWARE ----
c
       statcd(8)    = '10000'
       statnm(8)    = 'Delaware'
c       nconty(8)    =     3
c       idxcty(8)    =   314
c
c    ----  WASH DC ----
c
       statcd(9)    = '11000'
       statnm(9)    = 'Wash DC'
c       nconty(9)    =     1
c       idxcty(9)    =   317
c
c    ----  FLORIDA ----
c
       statcd(10)    = '12000'
       statnm(10)    = 'Florida'
c       nconty(10)    =    67
c       idxcty(10)    =   318
c
c    ----  GEORGIA ----
c
       statcd(11)    = '13000'
       statnm(11)    = 'Georgia'
c       nconty(11)    =   159
c       idxcty(11)    =   385
c
c    ----  HAWAII ----
c
       statcd(12)    = '15000'
       statnm(12)    = 'Hawaii'
c       nconty(12)    =     5
c       idxcty(12)    =   544
c
c    ----  IDAHO ----
c
       statcd(13)    = '16000'
       statnm(13)    = 'Idaho'
c       nconty(13)    =    44
c       idxcty(13)    =   549
c
c    ----  ILLINOIS ----
c
       statcd(14)    = '17000'
       statnm(14)    = 'Illinois'
c       nconty(14)    =   102
c       idxcty(14)    =   593
c
c-----------------------------------------------------------------------
c  Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
