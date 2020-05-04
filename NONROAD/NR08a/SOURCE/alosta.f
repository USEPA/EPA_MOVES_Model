C**** ALOSTA
c
      subroutine alosta(ierr,popsta,grwsta,
     &                                 icurec,popyr,idxasc,luse,growth)
c
c-----------------------------------------------------------------------
c  
c    This routine calculates the indicator values for the current record
c    in the population arrays.  The current record should be a national
c    record.  The national indicator is retrieved and then for each state
c    the state indicator is retrieved.  These data are used to calculate
c    the ratio of nation to state which is applied to the national population
c    to get state population.  This routine will also store the growth
c    factors in the state arrays for all states.
c
c        Outputs:
c           ierr   I   error code
c           popsta R   population for each state
c           grwsta R   growth values for each state
c        Inputs:
c           icurec I   current record in population arrays
c           popyr  I   population to be allocated
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
      real*4    popsta(NCNTY)
      real*4    grwsta(NCNTY)
      integer*4 icurec
      real*4    popyr
      integer*4 idxasc
      logical*4 luse
      real*4    growth
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*5  fiptmp, subtmp
      integer*4    i, idxsta
      real*4       valout, valalo, valnat(MXCOEF), valsta(MXCOEF)
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
      subtmp = '     '
c
c  --- get the population for the nation ----
c
      do 10 i=1,MXCOEF
         valnat(i) = 0
         if( strlen( indcod(idxasc,i) ) .EQ. 0 ) goto 111
         call getind( ierr, valout, indcod(idxasc,i), 
     &                                 fiptmp, subtmp, iepyr )
         if( ierr .EQ. IEOF ) goto 7002
         if( ierr .NE. ISUCES ) goto 9999
         valnat(i) = valnat(i) + valout
   10 continue
c
c  --- loop over states ----
c 
  111 continue
      do 20 idxsta=1,NSTATE
         popsta(idxsta) = 0.
c
c  --- if state is not requested or if there is state specific ---
c      data, then just skip it ---
c
         if( .NOT. lstacd(idxsta) ) goto 20
         if( lstlev(idxsta) ) goto 20
c
c  --- get the indicator values for this state ---
c
         fiptmp = statcd(idxsta)
         subtmp = '     '
         do 30 i=1,MXCOEF
            valsta(i) = 0
            if( strlen( indcod(idxasc,i) ) .EQ. 0 ) goto 222
            call getind( ierr, valout, indcod(idxasc,i), 
     &                                 fiptmp, subtmp, iepyr )
            if( ierr .EQ. IEOF ) goto 7002
            if( ierr .NE. ISUCES ) goto 9999
            valsta(i) = valsta(i) + valout
   30    continue
c
c  --- calculate ratio of nation to state ---
c
  222    continue
         valalo = 0.
         do 40 i=1,MXCOEF
            if( valnat(i) .GT. 0 ) valalo = valalo + 
     &                      (valsta(i) / valnat(i)) * coeffs(idxasc,i)
   40    continue
         popsta(idxsta) = popyr * valalo
c
c  --- store the growth value ---
c
         grwsta(idxsta) = growth
c
c  --- next state ----
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
     &                   'ERROR:  Could not find any spatial ',
     &    'indicator data for: ','FIPS   Subregion       Code     Year',
     &                fiptmp, subtmp, indcod(idxasc,i), iepyr
      write(IOWMSG,'(/1X,2A,/9X,A,/9X,A5,4X,A5,10X,A3,5X,I4)',ERR=9999) 
     &                   'ERROR:  Could not find any spatial ',
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
