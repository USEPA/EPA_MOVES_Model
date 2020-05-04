C**** RDBSFC.F
c
      subroutine rdbsfc( ierr )
c
c-----------------------------------------------------------------------
c
c    Description:
c      This routine reads the BSFC files.  This file is in the
c      same format as the emission factors file.
c    Arguments:
c     Outputs
c        ierr     I    error code
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      08/10/97  --gwilson--  original development (taken from rdefls.f)
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdefc.inc'
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I  returns the actual length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) fname
      integer*4         jerr
      logical*4         lcheck
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
c
c  --- open emission factor files ----
c
      fname = bsffl
      inquire(file=bsffl,exist=lcheck)
      if( .NOT. lcheck ) goto 7000
      open(unit=IORBSF,file=bsffl,ERR=7001,status='UNKNOWN')
      rewind(IORBSF)
c
c  --- call routine to get all exhaust factors ---
c
      call rdemfc( jerr, ascbsf, tecbsf, bsfpcb, bsfpce, 
     &                   ibsfun, iyrbsf, bsffac, 0, nbsffc,
     &                                     IORBSF, bsffl, 0, 'BSFC')
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- set error code and exit ---
c
      ierr = ISUCES
      goto 9999 
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 
     &      'ERROR:  Input file not found ', fname(:strmin(fname))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 
     &      'ERROR:  Input file not found ', fname(:strmin(fname))
      goto 9999
c
c
 7001 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fname(:strmin(fname))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fname(:strmin(fname))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(1X,5A)
c
c-----------------------------------------------------------------------
c   Exit point:
c-----------------------------------------------------------------------
c
 9999 continue
      end
