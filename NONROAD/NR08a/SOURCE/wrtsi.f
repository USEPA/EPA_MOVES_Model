C**** WRTSI
c
      subroutine wrtsi( ierr )
c
c-----------------------------------------------------------------------
c
c    writes one data record to the data file which will be read by
c    the reporting utility.
c
c    Argument description:
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      07/27/98  --gwilson-- original development
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
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c  strmin   I   returns the length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4     i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      write(IOWSI,9000,ERR=7000) (popsi(i),i=1,MXOTCH)
      write(IOWSI,9000,ERR=7000) (actsi(i),i=1,MXOTCH)
      write(IOWSI,9000,ERR=7000) (fuelsi(i),i=1,MXOTCH)
      write(IOWSI,9000,ERR=7000) (emissi(i,IDXTHC),i=1,MXOTCH)
      write(IOWSI,9000,ERR=7000) (emissi(i,IDXCRA),i=1,MXOTCH)
      write(IOWSI,9000,ERR=7000) (emissi(i,IDXDIU),i=1,MXOTCH)
      write(IOWSI,9000,ERR=7000) (emissi(i,IDXDIS),i=1,MXOTCH)
      write(IOWSI,9000,ERR=7000) (0.0,i=1,MXOTCH)
      write(IOWSI,9000,ERR=7000) (emissi(i,IDXNOX),i=1,MXOTCH)
      write(IOWSI,9000,ERR=7000) (emissi(i,IDXCO),i=1,MXOTCH)
c
c   --- set error code to succes and return ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,3A)') 
     &                 'ERROR:  Writing to the SI report file ',
     &                                           sifl(:strmin(sifl))
      write(IOWMSG,'(/,1X,3A)') 
     &                 'ERROR:  Writing to the SI report file ',
     &                                           sifl(:strmin(sifl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(E12.6,100(:,",",E12.6))
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
