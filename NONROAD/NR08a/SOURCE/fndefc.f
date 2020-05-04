C**** FNDEFC
c
      function fndefc( ascin, ascar, idxtch, tecin, tecar, hpcin, 
     &                 arhpcb, arhpce, inyr, iaryr, facar, ifrst, 
     &                                                nfacar, idxpol )
C
      IMPLICIT NONE
C
      integer*4 fndefc
c
c-----------------------------------------------------------------------
c
c    this routine finds the emission factor in the arrays supplied 
c    by the array arguments.  It searches the arrays using the data 
c    supplied in the other arguments.  It finds the best match by 
c    matching SCC code heierchy, HP category and tech type.  It uses the 
c    year closest to but not greater than the year supplied.
c
c    Return value:
c         > 0   =  index in array of best match
c         = 0   =  no match found
c
c    Arguments:
c
c     Inputs: 
c       ascin      C   SCC code to match
c       ascar      C   list of SCCs to search
c       idxtch     I   index into the tech type array (0 = global)
c       tecin      C   technology type to match
c       tecar      C   list of tech types to search
c       hpcin      R   average horsepower to match
c       arhpcb     R   array of beginning ranges of HP categories to search
c       arhpce     R   array of ending ranges of HP categories to search
c       inyr       I   year to match
c       iaryr      I   array of years to search
c       facar      R   array of emissions factors
c       ifrst      I   first element of facar to search
c       nfacar     I   number of elements of facar
c       idxpol     I   pollutant code index
c       
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/01/95  --  original development
c      07/14/05  --cimulus--  changed the comparison of tech type to
c                             allow exact match or global 'ALL', and
c                             updated the check-for-better-match logic
c                             accordingly (precedence is now:  most
c                             specific SCC, most specific HP, most
c                             specific tech type, and highest year
c                             without going over)
c      07/15/05  --cimulus--  removed 2-digit SCC global matches
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdeqp.inc'
      include 'nonrdefc.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      character*10  ascar(MXEMFC)
      character*10  ascin
      integer*4     idxtch
      character*10  tecar(MXEMFC)
      character*10  tecin
      real*4        arhpcb(MXEMFC)
      real*4        arhpce(MXEMFC)
      real*4        hpcin
      integer*4     iaryr(MXEMFC)
      integer*4     inyr           
      integer*4     ifrst
      real*4        facar(MXEMFC,ifrst:*)
      integer*4     nfacar
      integer*4     idxpol
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
      character*10 tecmatch ! best match so far for tech type
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- assume no match ----
c
      fndefc = 0
c
c   ---- set up the global matches ----
c
      ascglb(1) = ascin
      ascglb(2) = ascin(1:7)//'000'
      ascglb(3) = ascin(1:4)//'000000'
      iasc = 9999999
      idfhpc = 99999999
      idfyr = 99999999
      tecmatch = ''
c
c   ---- loop over all entries in the arrays ----
c
      do 10 idxfac=1,nfacar
c
c   ---- skip if factor is missing for this pollutant ---
c
         if( facar(idxfac,idxpol) .LT. 0 ) goto 10
c
c   ---- skip if not correct techonolgy type (accepts exact match or global 'ALL') ---
c
         if( tecin .NE. tecar(idxfac)
     &         .AND. tecar(idxfac) .NE. TECDEF ) goto 10
c
c   ---- skip if HP category is larger than current HP category ----
c
         if( hpcin .LT. arhpcb(idxfac) .OR. 
     &                            hpcin .GT. arhpce(idxfac) ) goto 10
c
c   ---- search in arrays for a match of ASC code ----
c
         idxasc = fndchr( ascar(idxfac), 10, ascglb, MXGLB )
         if( idxasc .LE. 0 ) goto 10
c
c   ---- skip if year is later than current year ---
c        but set the upper year to this one, since all other criteria match ---
c
         if( inyr .LT. iaryr(idxfac) ) then
             iemyr(idxpol,idxtch,2) = 
     &                     MIN( iemyr(idxpol,idxtch,2), iaryr(idxfac) )
             goto 10
         endif
c
c   ---- calculate the HP difference ---
c
         idiff = INT( hpcin-arhpcb(idxfac) )
         idiff = MAX( idiff , INT( arhpce(idxfac)-hpcin ) )
c
c   ---- if SCC match is better than a previous match then set the differences ---
c
         if( idxasc .LT. iasc ) then
             fndefc = idxfac
             iasc = idxasc
             idfhpc = idiff
             tecmatch = tecar(idxfac)
             idfyr = inyr - iaryr(idxfac)
c
c   ---- if same SCC match then check for closer HP category ---
c
         else if( idxasc .EQ. iasc ) then
c
c   ---- if HP match is better than a previous match then set the differences ---
c
             if( idfhpc .GT. idiff ) then
                fndefc = idxfac
                idfhpc = idiff
                tecmatch = tecar(idxfac)
                idfyr = inyr - iaryr(idxfac)
c
c   ---- if same HP match then check for better tech type ---
c
             else if( idfhpc .EQ. idiff ) then
c
c   ---- if tech type match is better than a previous match then set the differences ---
c
                if( tecar(idxfac) .NE. TECDEF
     &                .AND. tecmatch .EQ. TECDEF ) then
                   fndefc = idxfac
                   tecmatch = tecar(idxfac)
                   idfyr = inyr - iaryr(idxfac)
c
c   ---- if same tech type match then check for better year ---
c
                else if( tecmatch .EQ. tecar(idxfac) ) then
c
c   ---- if year match is better than a previous match then set the differences ---
c
                   if( idfyr .GT. (inyr - iaryr(idxfac) ) ) then
                      fndefc = idxfac
                      idfyr = inyr - iaryr(idxfac)
                   endif
                endif
             endif
         endif
c
c   --- get the next array element ----
c
   10 continue
c
c   ---- set the last matches ---
c
      if( fndefc .GT. 0 ) iemyr(idxpol,idxtch,1) = iaryr(fndefc)
      emhpc(idxpol) = hpcin
      emasc(idxpol) = ascin
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
