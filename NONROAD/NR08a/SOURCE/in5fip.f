C**** IN5FIP
c
      subroutine in5fip()
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
c      03/16/05  --cimulus--  moved Texas to in4fip to make room
c                             for Puerto Rico and US Virgin Islands
c      03/16/05  --cimulus--  added Puerto Rico and US Virgin Islands
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
c    ----  UTAH ----
c
       statcd(45)    = '49000'
       statnm(45)    = 'Utah'
c       nconty(45)    =    29
c       idxcty(45)    =  2776
c
c    ----  VERMONT ----
c
       statcd(46)    = '50000'
       statnm(46)    = 'Vermont'
c       nconty(46)    =    14
c       idxcty(46)    =  2805
c
c    ----  VIRGINIA ----
c
       statcd(47)    = '51000'
       statnm(47)    = 'Virginia'
c       nconty(47)    =   135
c       idxcty(47)    =  2819
c
c    ----  WASHINGTON ----
c
       statcd(48)    = '53000'
       statnm(48)    = 'Washington'
c       nconty(48)    =    39
c       idxcty(48)    =  2954
c
c    ----  WEST VIRGINIA ----
c
       statcd(49)    = '54000'
       statnm(49)    = 'West Virginia'
c       nconty(49)    =    55
c       idxcty(49)    =  2993
c
c    ----  WISCONSIN ----
c
       statcd(50)    = '55000'
       statnm(50)    = 'Wisconsin'
c       nconty(50)    =    72
c       idxcty(50)    =  3048
c
c    ----  WYOMING ----
c
       statcd(51)    = '56000'
       statnm(51)    = 'Wyoming'
c       nconty(51)    =    23
c       idxcty(51)    =  3120
c
c    ----  PUERTO RICO ----
c
       statcd(52)    = '72000'
       statnm(52)    = 'Puerto Rico'
c       nconty(52)    =    78
c       idxcty(52)    =  3143
c
c    ----  US VIRGIN ISLANDS ----
c
       statcd(53)    = '78000'
       statnm(53)    = 'US Virgin Islands'
c       nconty(53)    =     3
c       idxcty(53)    =  3221
c
c       idxcty(54)    =  3224
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
