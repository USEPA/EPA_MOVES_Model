C**** IN4FIP
c
      subroutine in4fip()
c
c-----------------------------------------------------------------------
c
c    Initiliazes the county and state names and the county codes.  The
c    code is separated into a couple of subroutines so the compiler can
c    handle it.
c  
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      06/10/97  --gwilson--  original development
c      03/16/05  --cimulus--  moved Texas here from in5fip to make room
c                             for Puerto Rico and US Virgin Islands
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
c    ----  OHIO ----
c
       statcd(36)    = '39000'
       statnm(36)    = 'Ohio'
c       nconty(36)    =    88
c       idxcty(36)    =  2042
c
c    ----  OKLAHOMA ----
c
       statcd(37)    = '40000'
       statnm(37)    = 'Oklahoma'
c       nconty(37)    =    77
c       idxcty(37)    =  2130
c
c    ----  OREGON ----
c
       statcd(38)    = '41000'
       statnm(38)    = 'Oregon'
c       nconty(38)    =    36
c       idxcty(38)    =  2207
c
c    ----  PENNSYLVANIA ----
c
       statcd(39)    = '42000'
       statnm(39)    = 'Pennsylvania'
c       nconty(39)    =    67
c       idxcty(39)    =  2243
c
c    ----  RHODE ISLAND ----
c
       statcd(40)    = '44000'
       statnm(40)    = 'Rhode Island'
c       nconty(40)    =     5
c       idxcty(40)    =  2310
c
c    ----  SOUTH CAROLINA ----
c
       statcd(41)    = '45000'
       statnm(41)    = 'South Carolina'
c       nconty(41)    =    46
c       idxcty(41)    =  2315
c
c    ----  SOUTH DAKOTA ----
c
       statcd(42)    = '46000'
       statnm(42)    = 'South Dakota'
c       nconty(42)    =    66
c       idxcty(42)    =  2361
c
c    ----  TENNESSEE ----
c
       statcd(43)    = '47000'
       statnm(43)    = 'Tennessee'
c       nconty(43)    =    95
c       idxcty(43)    =  2427
c
c    ----  TEXAS ----
c
       statcd(44)    = '48000'
       statnm(44)    = 'Texas'
c       nconty(44)    =   254
c       idxcty(44)    =  2522
c
c  --- return to calling return ---
c
       goto 9999
c
c-----------------------------------------------------------------------
c  Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
