C**** IN2FIP
c
      subroutine in2fip()
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
c   --- state and county names and codes arrays ---
c
c    ----  INDIANA ----
c
       statcd(15)    = '18000'
       statnm(15)    = 'Indiana'
c       nconty(15)    =    92
c       idxcty(15)    =   695
c
c    ----  IOWA ----
c
       statcd(16)    = '19000'
       statnm(16)    = 'Iowa'
c       nconty(16)    =    99
c       idxcty(16)    =   787
c
c    ----  KANSAS ----
c
       statcd(17)    = '20000'
       statnm(17)    = 'Kansas'
c       nconty(17)    =   105
c       idxcty(17)    =   886
c
c    ----  KENTUCKY ----
c
       statcd(18)    = '21000'
       statnm(18)    = 'Kentucky'
c       nconty(18)    =   120
c       idxcty(18)    =   991
c
c    ----  LOUISIANA ----
c
       statcd(19)    = '22000'
       statnm(19)    = 'Louisiana'
c       nconty(19)    =    64
c       idxcty(19)    =  1111
c
c    ----  MAINE ----
c
       statcd(20)    = '23000'
       statnm(20)    = 'Maine'
c       nconty(20)    =    16
c       idxcty(20)    =  1175
c
c    ----  MARYLAND ----
c
       statcd(21)    = '24000'
       statnm(21)    = 'Maryland'
c       nconty(21)    =    24
c       idxcty(21)    =  1191
c
c    ----  MASSACHUSETTS ----
c
       statcd(22)    = '25000'
       statnm(22)    = 'Massachusetts'
c       nconty(22)    =    14
c       idxcty(22)    =  1215
c
c    ----  MICHIGAN ----
c
       statcd(23)    = '26000'
       statnm(23)    = 'Michigan'
c       nconty(23)    =    83
c       idxcty(23)    =  1229
c
c    ----  MINNISOTA ----
c
       statcd(24)    = '27000'
       statnm(24)    = 'Minnesota'
c       nconty(24)    =    87
c       idxcty(24)    =  1312
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
