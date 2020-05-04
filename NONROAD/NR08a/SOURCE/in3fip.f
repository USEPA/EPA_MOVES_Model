C**** IN3FIP
c
      subroutine in3fip()
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
c    ----  MISSISSIPPI ----
c
       statcd(25)    = '28000'
       statnm(25)    = 'Mississippi'
c       nconty(25)    =    82
c       idxcty(25)    =  1399
c
c    ----  MISSOURI ----
c
       statcd(26)    = '29000'
       statnm(26)    = 'Missouri'
c       nconty(26)    =   115
c       idxcty(26)    =  1481
c
c    ----  MONTANA ----
c
       statcd(27)    = '30000'
       statnm(27)    = 'Montana'
c       nconty(27)    =    57
c       idxcty(27)    =  1596
c
c    ----  NEBRASKA ----
c
       statcd(28)    = '31000'
       statnm(28)    = 'Nebraska'
c       nconty(28)    =    93
c       idxcty(28)    =  1653
c
c    ----  NEVADA ----
c
       statcd(29)    = '32000'
       statnm(29)    = 'Nevada'
c       nconty(29)    =    17
c       idxcty(29)    =  1746
c
c    ----  NEW HAMPSHIRE ----
c
       statcd(30)    = '33000'
       statnm(30)    = 'New Hampshire'
c       nconty(30)    =    10
c       idxcty(30)    =  1763
c
c    ----  NEW JERSEY ----
c
       statcd(31)    = '34000'
       statnm(31)    = 'New Jersey'
c       nconty(31)    =    21
c       idxcty(31)    =  1773
c
c    ----  NEW MEXICO ----
c
       statcd(32)    = '35000'
       statnm(32)    = 'New Mexico'
c       nconty(32)    =    33
c       idxcty(32)    =  1794
c
c    ----  NEW YORK ----
c
       statcd(33)    = '36000'
       statnm(33)    = 'New York'
c       nconty(33)    =    62
c       idxcty(33)    =  1827
c
c    ----  NORTH CAROLINA ----
c
       statcd(34)    = '37000'
       statnm(34)    = 'North Carolina'
c       nconty(34)    =   100
c       idxcty(34)    =  1889
c
c    ----  NORTH DAKOTA ----
c
       statcd(35)    = '38000'
       statnm(35)    = 'North Dakota'
c       nconty(35)    =    53
c       idxcty(35)    =  1989
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
