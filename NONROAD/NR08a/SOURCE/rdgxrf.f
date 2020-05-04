C**** RDGXRF
c
      subroutine rdgxrf( ierr, iounit, fname )
c
c-----------------------------------------------------------------------
c
c    reads the growth indicator cross reference data and stores it in
c    global arrays to be accessed by the Nonroad model.  The data is
c    in separate files by state.  It reads the data specified by the
c    unit number and filename on the argument list.
c
c    Argument declaration.
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c       iounit  I  unit number of file to read
c       fname   C  file name of file to read
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/01/93  --gmw--  original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdgrw.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4     ierr
      integer*4     iounit
      character*(*) fname
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string (minimum of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(2*MXSTR) line
      character*20        keywrd, keytmp
      character*10        asctmp, tectmp
      character*5         statmp
      character*4         indtmp
      integer*4           irec, jerr
      real*4              hplow, hphi
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 1
      rewind(iounit)
c
c   --- call routine to find the /INDICATORS/ packet ----
c
      keywrd = '/INDICATORS/'
      call fndkey( jerr, iounit, keywrd )
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read a record as a character string --- 
c
  111 continue
      read(iounit,8000,ERR=7001,END=7001) line
      irec = irec + 1
c
c   --- look for the /END/ keyword ----
c
      keytmp = line(1:10)
      call lftjst( keytmp )
      call low2up( keytmp ) 
      if( keytmp .EQ. KEYEND ) goto 222
c
c   --- read the indicator other data and store in arrays ----
c
      asctmp = line(12:21)
      statmp = line(1:5)
      indtmp = line(7:10) 
      call lftjst( indtmp )
      read(line(23:27),'(F5.0)',ERR=7001) hplow
      read(line(28:32),'(F5.0)',ERR=7001) hphi
      tectmp = line(34:43)
      call lftjst( tectmp )
c
c  --- add the data to the global arrays ----
c
      nrcgrx = nrcgrx + 1
      if( nrcgrx .GT. MXGXRF ) goto 7003
      fipgrx(nrcgrx) = statmp
      ascgrx(nrcgrx) = asctmp
      hpgrx(nrcgrx,1) = hplow
      hpgrx(nrcgrx,2) = hphi
      tecgrx(nrcgrx) = tectmp
      indgrx(nrcgrx) = indtmp 
c
c   --- get the next record ---
c
      goto 111
c
c   --- finished reading file ---
c
  222 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &             keywrd(:strmin(keywrd)),' packet of growth file ',
     &                                           fname(:strmin(fname))
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &             keywrd(:strmin(keywrd)),' packet of growth file ',
     &                                           fname(:strmin(fname))
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,4A,/,9X,A,I8)',ERR=9999) 'ERROR:  Reading ',
     &            keywrd(:strmin(keywrd)),' packet of growth file ',
     &                       fname(:strmin(fname)),'at record ',irec
      write(IOWMSG,'(/,1X,4A,/,9X,A,I8)',ERR=9999) 'ERROR:  Reading ',
     &            keywrd(:strmin(keywrd)),' packet of growth file ',
     &                       fname(:strmin(fname)),'at record ',irec
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,3A,/,9X,3A,I8)',ERR=9999) 
     &            'ERROR:  Number of records in ',
     &            keywrd(:strmin(keywrd)),' packet','of growth file ',
     &                   fname(:strmin(fname)),' exceeds max ',MXGXRF
      write(IOWMSG,'(/,1X,3A,/,9X,3A,I8)',ERR=9999) 
     &            'ERROR:  Number of records in ',
     &            keywrd(:strmin(keywrd)),' packet','of growth file ',
     &                   fname(:strmin(fname)),' exceeds max ',MXGXRF
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(A)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
