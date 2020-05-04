C**** ALOCTY
c
      subroutine alocty(ierr,popcty,grwcty,
     &                    icurec,popyr,idxsta,idxasc,luse,growth)
c
c-----------------------------------------------------------------------
c  
c    This routine calculates the indicator values for the current record
c    in the population arrays.  The current record should be a state 
c    record.  The state indicator is retrieved and then for each county in
c    the state the indicator is retrieved.  These data are used to calculate
c    the ratio of state to county which is applied to the state population
c    to get county population.  This routine will also store the growth
c    factors in the county arrays for all counties in the state.
c
c        Outputs:
c           ierr   I   error code
c           popcty R   population for each county
c           grwcty R   growth values for each county
c        Inputs:
c           icurec I   current record in population arrays
c           popyr  I   population to be allocated
c           idxsta I   index of state code in arrays
c           idxasc I   index of SCC codes in arrays
c           luse   I   flag for determining if want usage data 
c           growth R   growth value to store in county arrays
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/10/95  --gmw--  original development
c      07/06/05  epa -- change ipopyr(icurec) to iepyr for getind call
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdefc.inc'
      include 'nonrdeqp.inc'
      include 'nonrdalo.inc'
      include 'nonrdreg.inc'
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strlen   I   returns the actual length of a string
c
      integer*4 strlen
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      real*4    popcty(NCNTY)
      real*4    grwcty(NCNTY)
      integer*4 icurec
      real*4    popyr
      integer*4 idxsta
      integer*4 idxasc
      logical*4 luse
      real*4    growth
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*5  fiptmp, subtmp
      integer*4    i, idxfip
      integer*4    ibeg, iend
      real*4       valout, valalo, valsta(MXCOEF), valcty(MXCOEF)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- set the error code ----
c
      ierr =  IFAIL
c
c  ---- load the FIPS code into local variable ---
c
      fiptmp = regncd(icurec)(1:5)
      subtmp = '      '
c
c  --- get the allocation indicator for the state ----
c
      do 10 i=1,MXCOEF
         valsta(i) = 0
         if( strlen( indcod(idxasc,i) ) .EQ. 0 ) goto 111
         call getind( ierr, valout, indcod(idxasc,i), 
     &                                  fiptmp, subtmp, iepyr )
         if( ierr .EQ. IEOF ) goto 7002
         if( ierr .NE. ISUCES ) goto 9999
         valsta(i) = valsta(i) + valout
   10 continue
c
c  --- loop over counties in this state ----
c 
  111 continue
      ibeg = idxcty(idxsta)
      iend = ibeg + nconty(idxsta) - 1
      do 20 idxfip=ibeg,iend
         popcty(idxfip) = 0.
c
c  --- if county is not requested or has county specific population
c      data, then just skip it ---
c
         if( .NOT. lfipcd(idxfip) ) goto 20
         if( lctlev(idxfip) ) goto 20
c
c  --- get the indicator values for this county ---
c
         fiptmp = fipcod(idxfip)
         subtmp = '     '
         do 30 i=1,MXCOEF
            valcty(i) = 0
            if( strlen( indcod(idxasc,i) ) .EQ. 0 ) goto 222
            call getind( ierr, valout, indcod(idxasc,i), 
     &                                  fiptmp, subtmp, iepyr )
            if( ierr .EQ. IEOF ) goto 7002
            if( ierr .NE. ISUCES ) goto 9999
            valcty(i) = valcty(i) + valout
   30    continue
c
c  --- calculate ratio of state to county ---
c
  222    continue
         valalo = 0.
         do 40 i=1,MXCOEF
            if( valsta(i) .GT. 0 ) valalo = valalo +
     &                      (valcty(i) / valsta(i)) * coeffs(idxasc,i)
   40    continue
         popcty(idxfip) = popyr * valalo
c
c  --- store the growth value ---
c
         grwcty(idxfip) = growth
c
c  --- next county ----
c
   20 continue
c
c   --- set error code and return ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7002 continue
      write(IOWSTD,'(/1X,2A,/9X,A,/9X,A5,4X,A5,10X,A3,5X,I4)',ERR=9999) 
     &                   'ERROR: Could not find any spatial ',
     &    'indicator data for: ','FIPS   Subregion       Code     Year',
     &                fiptmp, subtmp, indcod(idxasc,i), iepyr
      write(IOWMSG,'(/1X,2A,/9X,A,/9X,A5,4X,A5,10X,A3,5X,I4)',ERR=9999) 
     &                   'ERROR: Could not find any spatial ',
     &    'indicator data for: ','FIPS   Subregion       Code     Year',
     &                fiptmp, subtmp, indcod(idxasc,i), iepyr
      goto 9999
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
