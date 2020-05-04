C**** ENEESACT.INC
c
c-----------------------------------------------------------------------
c
c    Include file for the activity data to be used by the EPA Nonroad
c    program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     09/01/93  -gmw-   Original development
c     11/16/99  -mjimenez - added activity vs age curve
c     02/10/00  -mjimenez - removed tank volume from activity file
c     06/22/01  -charvey - changed TNKMAX from 50 to 1000
c     05/20/04  -dfk-      added evap tech groups to activity
c     04/05/05  -cimulus-  changed MXUSE from 50 to 51
c     07/11/05 --cimulus--  reverted back to a single technology type
c
c-----------------------------------------------------------------------
c   Parameters for array bounds:
c-----------------------------------------------------------------------
c
c    MXACTR   I  maximum number of activity records in activity file
c
      integer*4 MXACTR
c
      parameter( MXACTR = 5000 )
c
c-----------------------------------------------------------------------
c   Activity level units parameters:
c-----------------------------------------------------------------------
c
c   KEYHRY  C  keyword of hr/yr activity level units
c   KEYHRD  C  keyword of hr/day activity level units
c   KEYGLY  C  keyword of gal/yr activity level units
c   KEYGLD  C  keyword of gal/day activity level units
c
      character*10 KEYHRY
      character*10 KEYHRD
      character*10 KEYGLY
      character*10 KEYGLD
c
      parameter( KEYHRY = 'HRS/YR    ' )
      parameter( KEYHRD = 'HRS/DAY   ' )
      parameter( KEYGLY = 'GAL/YR    ' )
      parameter( KEYGLD = 'GAL/DAY   ' )
c
c   IDXHRY  C  index of hr/yr activity level units arrays
c   IDXHRD  C  index of hr/day activity level units arrays
c   IDXGLY  C  index of gal/yr activity level units arrays
c   IDXGLD  C  index of gal/day activity level units arrays
c
      integer*4 IDXHRY
      integer*4 IDXHRD
      integer*4 IDXGLY
      integer*4 IDXGLD
c
      parameter( IDXHRY = 1 )
      parameter( IDXHRD = 2 )
      parameter( IDXGLY = 3 )
      parameter( IDXGLD = 4 )
c
c-----------------------------------------------------------------------
c   Parameters for limits:
c-----------------------------------------------------------------------
c
c  TNKMAX   R   maximum allowed tank volume
c
      real*4 TNKMAX
c
      parameter( TNKMAX = 1000.0 )
c
c-----------------------------------------------------------------------
c   Variables for actitivty data
c-----------------------------------------------------------------------
c
c   ascact  C  SCC code for the activity level
c   subact  C  subregion code 
c   tecact  C  technology type for the activity record
c   nactrc  I  number of records in the arrays
c   hpcact  R  HP range for the activity record
c   faclod  R  load factor array
c   tnkvol  R  tank volume (gal) 
c   actlev  R  activity level 
c   starts  R  number of starts per year for equipment
c   iactun  I  index of the units of the activity level
c   actage  C  code indicating activity vs age curve
c
      character*10 ascact(MXACTR)
      character*10 tecact(MXACTR)
      character*5  subact(MXACTR)
      character*10 actage(MXACTR)
      integer*4    nactrc
      integer*4    iactun(MXACTR)
      real*4       hpcact(2,MXACTR)
      real*4       faclod(MXACTR)
cc      real*4       tnkvol(MXACTR)
      real*4       actlev(MXACTR)
      real*4       starts(MXACTR)
c
      common /actchr/ ascact, tecact, subact, actage
      common /actdat/ nactrc, hpcact, faclod, actlev, 
     &                starts, iactun
c
c-----------------------------------------------------------------------
c   Parameters for array bounds for % useful life vs activity:
c-----------------------------------------------------------------------
c
c    MXAGE    I  maximum number of age vs new activity curves
c    MXUSE    I  maximum number of age bins
c
      integer*4 MXAGE
      integer*4 MXUSE
c
      parameter( MXAGE = 10 )
      parameter( MXUSE = 51 )
c
c-----------------------------------------------------------------------
c   Variables for % useful life vs activity data:
c-----------------------------------------------------------------------
c
c    agebin    R    array for %useful life bin values
c    agenam    C    names of new activity curves
c    agepct    R    percent of new activity in each bin for age curves
c    nagenm    I    number of new activity curves
c
      character*10 agenam(MXAGE)
      real*4       agebin(MXUSE)
      real*4       agepct(MXAGE,MXUSE)
      integer*4    nagenm
c
      common /agechr/ agenam
      common /agedat/ agebin, agepct, nagenm
c
