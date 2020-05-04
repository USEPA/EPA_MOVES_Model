C**** ALOSUB
c
      subroutine alosub(ierr,popsub,grwsub,subreg,
     &                    icurec,iyear,popyr,idxfip,idxasc,luse,growth)
c
c-----------------------------------------------------------------------
c  
c    This routine calculates the indicator values for the current record
c    in the population arrays.  The current record should be a county 
c    record.  The county indicator is retrieved and then for the specified
c    subregion the indicator is retrieved.  These data are used to calculate
c    the ratio of county to subcounty which is applied to the county population
c    to get subcounty population. 
c
c        Outputs:
c           ierr   I   error code
c           popsub R   population for the subregion
c           grwsub R   growth values the subregion
c        Inputs:
c           subreg C   subregion (subcounty) code
c           icurec I   current record in population arrays
c           iyear  I   year for predicting emissions
c           popyr  I   population to be allocated
c           idxfip I   index of state code in arrays
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
      character*5 subreg
      integer*4   ierr
      real*4      popsub
      real*4      grwsub
      integer*4   icurec
      integer*4   iyear
      real*4      popyr
      integer*4   idxfip
      integer*4   idxasc
      logical*4   luse
      real*4      growth
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*5  fiptmp, subtmp
      integer*4    i
      real*4       valout, valalo, valcty(MXCOEF), valsub(MXCOEF)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- set the error code ----
c
      ierr =  IFAIL
c
c  ---- load the county FIPS code into local variable ----
c
      fiptmp = regncd(icurec)(1:5)
      subtmp = '     '
c
c  --- get the allocation indicator for the county ----
c
      do 10 i=1,MXCOEF
         valcty(i) = 0
         if( strlen( indcod(idxasc,i) ) .EQ. 0 ) goto 111
         call getind( ierr, valout, indcod(idxasc,i), 
     &                                 fiptmp, subtmp, iepyr )
         if( ierr .EQ. IEOF ) goto 7002
         if( ierr .NE. ISUCES ) goto 9999
         valcty(i) = valcty(i) + valout
   10 continue
c
c  --- initialize the subcounty population ----
c 
  111 continue
      popsub = 0.
c
c  --- get the indicator values for the subcounty ---
c
      subtmp = subreg
      call lftjst( subtmp )
      do 20 i=1,MXCOEF
         valsub(i) = 0
         if( strlen( indcod(idxasc,i) ) .EQ. 0 ) goto 222
         call getind( ierr, valout, indcod(idxasc,i), 
     &                                 fiptmp, subtmp, iepyr )
         if( ierr .EQ. IEOF ) goto 7002
         if( ierr .NE. ISUCES ) goto 9999
         valsub(i) = valsub(i) + valout
   20 continue
c
c  --- calculate ratio of county to subcounty ---
c
  222 continue
      valalo = 0.
      do 30 i=1,MXCOEF
         if( valcty(i) .GT. 0 ) valalo = valalo +
     &                      (valsub(i) / valcty(i)) * coeffs(idxasc,i)
   30 continue
      popsub = popyr * valalo
c
c  --- store the growth value ---
c
      grwsub = growth
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
