C**** FNDDET
c
      function fnddet( idxpol, tecin )
C
      IMPLICIT NONE
C
      integer*4 fnddet
c
c-----------------------------------------------------------------------
c
c    This routine finds the deterioration factor data in the arrays 
c    stored in common blocks.  It searches the arrays using the data 
c    lookiing for an exact match of the tech type supplied in the
c    argument list.  Since the the deterioration data is specified by
c    species, the arrays are indexed by species.
c
c    Return value:
c         > 0   =  index in array of match
c         = 0   =  no match found
c
c    Arguments:
c
c     Inputs: 
c       idxpol     I   pollutant code index
c       tecin      C   technology type to match
c       
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      05/01/98  --  original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'nonrdprm.inc'
      include 'nonrdefc.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4     idxpol
      character*10  tecin
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- assume no match ----
c
      fnddet = 0
c
c   --- loop over all entries in the array for this species ---
c
      do 10 i=1,ndtfac(idxpol)
c
c   --- if match is made, set the return value and return ---
c
          if( tecdet(i,idxpol) .EQ. tecin ) then
                fnddet = i
                goto 9999
          endif
   10 continue
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
