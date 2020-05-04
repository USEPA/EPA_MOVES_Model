C**** SITOT
c
      subroutine sitot( ierr, tecin, popin, actin, fuelin, emsin )
c
c-----------------------------------------------------------------------
c
c    adds the values for the current tech type to the totals 
c    arrays for the SI report 
c
c    Argument description:
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c       tecin   C  technology type name
c       popin   R  population
c       actin   R  activity (hours)
c       fuelin  R  fuel consumption
c       emiss   R  array of emissions (tons)
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      07/27/98  --gwilson--  original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdeqp.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      character*10 tecin
      real*4       popin
      real*4       actin
      real*4       fuelin
      real*4       emsin(MXPOL)
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c  fndchr   I   returns the index of a string in an array of strings
c
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 idxtch, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- find the index of tech type in the SI array, if it
c       is not found then don't use this data ---
c
      idxtch = fndchr( tecin, 10, sitech, MXITCH )
      if( idxtch .LE. 0 ) then
         ierr = ISKIP
         goto 9999
      endif
c
c   --- make sure the index is within acceptable range ---
c
      if( indxsi(idxtch) .GT. MXOTCH ) goto 7000
c
c   --- add the population totals to arrays ---
c
      if( popin .GE. 0. ) then
          popsi(indxsi(idxtch)) = popsi(indxsi(idxtch)) + popin
      endif
c
c   --- add the activity totals to arrays ---
c
      if( actin .GE. 0. ) then
          actsi(indxsi(idxtch)) = actsi(indxsi(idxtch)) + actin
      endif
c
c   --- add the fuel consumption to arrays ---
c
      if( fuelin .GE. 0. ) then
          fuelsi(indxsi(idxtch)) = fuelsi(indxsi(idxtch)) + fuelin
      endif
c
c   --- add the emissions totals to arrays ---
c
      do 10 i=1,MXPOL
        if( emsin(i) .GE. 0. ) then
           emissi(indxsi(idxtch),i) = emissi(indxsi(idxtch),i)+emsin(i)
        endif
 10   continue
c
c   --- all is well, set error code and return ---
c
      ierr = ISUCES 
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,2A)') 
     &                 'ERROR:  Internal program inconsistency. ',
     &       'Index of tech type for SI report is out of valid range. '
      write(IOWMSG,'(/,1X,2A)') 
     &                 'ERROR:  Internal program inconsistency. ',
     &       'Index of tech type for SI report is out of valid range. '
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
