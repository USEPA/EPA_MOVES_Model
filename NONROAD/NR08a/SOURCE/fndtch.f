C**** FNDTCH
c
      function fndtch( ascin, hpval, inyr )
C
      IMPLICIT NONE
C
      integer*4 fndtch
c
c-----------------------------------------------------------------------
c
c    this routine finds the technology fraction data in the arrays supplied
c    by the arguments.  It searches the arrays using the data
c    supplied in the other arguments.  It finds the best match by 
c    matching SCC code hierarchy, HP category and model year.  It uses the
c    year closest to but not less than the year supplied.
c
c    Important Assumption:  the number of technology types is the same
c     for each model year of a particular piece of equipment, although
c     the technology fraction may be zero for some of the technologies
c     in some of the model years.  Hence, the first time this gets called
c     in PRC???, it is just to find the maximum number of tech types possible
c     for the specific piece of equipment.
c
c    Return value:
c         > 0   =  index in array of best match
c         = 0   =  no match found
c
c    Argument declaration.
c     Inputs: 
c       ascin    C     SCC code to match
c       hpval    R     HP category
c       inyr     I     model year
c       
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      07/22/96  --jlf--  original development
c      02/03/03  --gwilson-- fixed bug in checking for HP range
c      07/15/05  --cimulus--  removed 2-digit SCC global matches
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'nonrdprm.inc'
      include 'nonrdefc.inc'
      include 'nonrdeqp.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      character*10  ascin
      real*4        hpval
      integer*4     inyr           
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   fndchr  I   returns the index of string in array of strings
c
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   MXGLB   I  number of codes for global matches
c
      integer*4 MXGLB
c
      parameter( MXGLB = 3 )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 ascglb(MXGLB)
      integer*4    idxfac, idxasc, iasc, idfhpc, idfyr, idiff
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- assume no match ----
c
      fndtch = 0
c
c   ---- set up the global matches ----
c
      ascglb(1) = ascin
      ascglb(2) = ascin(1:7)//'000'
      ascglb(3) = ascin(1:4)//'000000'
      iasc = 9999999
      idfhpc = 99999999
      idfyr = 99999999
c
c   ---- loop over all entries in the arrays ----
c
      do 10 idxfac=1,ntchcd
c
c   ---- skip if year is later than current year ---
c
         if( inyr .LT. iyrtch(idxfac) ) goto 10
c
c   ---- skip if HP category is larger than current HP category ----
c
         if( hpval .LE. tchhpb(idxfac) .OR. 
     &                           hpval .GT. tchhpe(idxfac) ) goto 10
c
c   ---- search in arrays for a match of ASC code ----
c
         idxasc = fndchr( asctch(idxfac), 10, ascglb, MXGLB )
         if( idxasc .LE. 0 ) goto 10
c
c   ---- if match is better than a previous match then set the differences ---
c
         idiff = INT( hpval - tchhpb(idxfac) )
         idiff = MAX( idiff, INT( tchhpe(idxfac) - hpval ) )
         if( idxasc .LT. iasc ) then
             fndtch = idxfac
             iasc = idxasc
             idfhpc = INT( hpval - tchhpb(idxfac) )
             idfhpc = MAX( idfhpc, INT( tchhpe(idxfac) - hpval ) )
             idfyr = inyr - iyrtch(idxfac)
c
c   ---- if same match the check for closer HP category ---
c
         else if( idxasc .EQ. iasc ) then
             if( idiff .LT. idfhpc ) then
                fndtch = idxfac
                idfhpc = idiff
                idfyr = inyr - iyrtch(idxfac)
c
c   ---- if HP cat is the same, check for better year ---
c
             else if( idiff .EQ. idfhpc ) then
                if( idfyr .GT. (inyr - iyrtch(idxfac) ) ) then
                   fndtch = idxfac
                   idfyr = inyr - iyrtch(idxfac)
                endif
             endif
        endif
c
c   --- get the next array element ----
c
   10 continue
c
c   ---- return to calling routine ----
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
