C**** FNDRFM
c
      function fndrfm( ascin, hpval, ttyp )
C
      IMPLICIT NONE
C
      integer*4 fndrfm
c
c-----------------------------------------------------------------------
c
c    this routine finds the refueling mode data in the arrays supplied
c    by the arguments.  It searches the arrays using the data
c    supplied in the other arguments.  It finds the best match by 
c    matching SCC code hierarchy, HP category and technology type.
c
c    Return value:
c         > 0   =  index in array of best match
c         = 0   =  no match found
c
c    Argument declaration.
c     Inputs: 
c       ascin    C     SCC code to match
c       hpval    R     HP category
cc  removed       tvol     R     tank volume
c       ttyp     C     technology type
c       
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      10/13/98  --mjimenez--  original development taken from fndtch
c      02/10/00  --mjimenez--  removed tank volume in argument list
c                              and search
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
cc removed      real*4        tvol
      character*10  ttyp
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
      integer*4    idxarr, idxasc, iasc, idfhpc, idiff
      real*4       chkval
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- assume no match ----
c
      fndrfm = 0
c
c   ---- set up the global matches ----
c
      ascglb(1) = ascin
      ascglb(2) = ascin(1:7)//'000'
      ascglb(3) = ascin(1:4)//'000000'
      iasc = 9999999
      idfhpc = 99999999
c
c   ---- loop over all entries in the arrays ----
c
      do 10 idxarr=1,nsplar
c
c   ---- if not the same tech type then skip ----
c
         if ( tecspl(idxarr) .NE. ttyp ) goto 10
c
c   --- get range indicator and set comparison value ---
c
         if ( indspl(idxarr) .EQ. HP ) chkval = hpval
cc         if ( indspl(idxarr) .EQ. TNKTYP ) chkval = tvol
c
c   ---- skip if size (either hp or tank volume) is outside range ---
c
         if( chkval .LT. splpcb(idxarr) .OR. 
     &                           chkval .GT. splpce(idxarr) ) goto 10
c
c   ---- search in arrays for a match of ASC code ----
c
         idxasc = fndchr( ascspl(idxarr), 10, ascglb, MXGLB )
         if( idxasc .LE. 0 ) goto 10
c
c   ---- if match is better than a previous match then set the differences ---
c
         idiff = INT( chkval - splpcb(idxarr) )
         idiff = MAX( idiff, INT( splpce(idxarr) - chkval ) )
         if( idxasc .LT. iasc ) then
             fndrfm = idxarr
             iasc = idxasc
             idfhpc = INT( chkval - splpcb(idxarr) )
             idfhpc = MAX( idfhpc, INT( splpce(idxarr) - chkval ) )
c
c   ---- if same match then check for closer HP category ---
c
         else if( idxasc .EQ. iasc ) then
             if( idiff .LT. idfhpc ) then
                fndrfm = idxarr
                idfhpc = idiff
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
