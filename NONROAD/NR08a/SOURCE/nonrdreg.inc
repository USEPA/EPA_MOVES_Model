C**** NONRDREG.INC
c
c-----------------------------------------------------------------------
c
c    Include file for the region definition data used by the EPA
c    NONROAD program.  It contains data structures for the state names,
c    county names and code as well as the subregion designation.
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     09/01/93  -gmw-   Original development
c     04/25/05  -cimulus- Added nmcnty to hold the number of used counties.
c                         This should be used instead of NCNTY.
c     05/16/05  -cimulus- Increased size of strings in cntynm to 50
c
c-----------------------------------------------------------------------
c   Variables for county and state data:
c-----------------------------------------------------------------------
c
c    statnm    C    name of each state
c    statcd    C    state FIPS codes
c    nconty    I    array of number of counties in each state
c    idxcty    I    index into arrays of first county in the state
c    cntynm    C    name of each county
c    fipcod    C    array of fips codes selected
c    lfipcd    L    flags to determine if the counties were selected
c    lstacd    L    flag for determining if state was selected
c    nmcnty    I    number of counties actually used, upto NCNTY
c
      character*20 statnm(NSTATE)
      character*5  statcd(NSTATE)
      character*50 cntynm(NCNTY)
      character*5  fipcod(NCNTY)
      integer*4    nconty(NSTATE)
      integer*4    idxcty(0:NSTATE+1)
      logical*4    lfipcd(NCNTY)
      logical*4    lstacd(NSTATE)
      integer*4    nmcnty
c
      common /regchr/ statcd, fipcod, statnm, cntynm
      common /regdat/ nconty, idxcty, lfipcd, lstacd, nmcnty
