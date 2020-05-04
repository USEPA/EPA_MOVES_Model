c**** CHKWRN
c
      subroutine chkwrn(ierr,idxwrn)
c
c-----------------------------------------------------------------------
c  
c   This routine increments the counter for the number of warnings
c   and checks if the limit has been exceeded.  If so, an error message
c   is produced and an error code of fail is returned.
c      Argument descriptions:
c        Outputs:
c          ierr    I   error code
c        Inputs:
c          idxwrn  I   index of type of warning in the counter array
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      05/28/98  --gmw-- original developnent
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      integer*4    idxwrn
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- initilialize the error code ---
c
      ierr = IFAIL
c
c  --- incrament the overall warning counter ----
c
      nwarn = nwarn + 1
c
c  --- incrament the counter for this warning type and check against
c      allowable limit ---
c
      nwrnct(idxwrn) = nwrnct(idxwrn) + 1
c
c  --- if over the limit, display an error message ----
c
      if( nwrnct(idxwrn) .GT. MXWARN ) goto 7000
c
c  --- everything OK, set error code to sucess and return ---
c
      ierr = ISUCES 
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,A,I3,A)',ERR=9999) 'ERROR:  There were ',
     &                  nwarn,' warnings written to the message file.'
      write(IOWSTD,'(1X,2A)',ERR=9999) 'The number of warnings ',
     &                 'produced during this run indicates a problem '
      write(IOWSTD,'(1X,2A)',ERR=9999) 'with the input data.  Update ',
     &                       'the appropriate data files and try again.'
      write(IOWMSG,'(//,1X,2A)',ERR=9999) 'The number of warnings ',
     &                 'produced during this run indicates a problem '
      write(IOWMSG,'(1X,2A)',ERR=9999) 'with the input data.  Update ',
     &                       'the appropriate data files and try again.'
      goto 9999
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
