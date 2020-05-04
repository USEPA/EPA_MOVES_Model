C**** RDDETR
c
      subroutine rddetr( ierr, idxpol )
c
c-----------------------------------------------------------------------
c
c    Reads the deterioration factors file and stores the data
c    in a common block to be used by the NONROAD program.  The 
c    values in the file are the coefficients to the deterioration
c    equation:
c         DF = 1 + A * (age)^b
c
c    Argument declaration.
c     Inputs: 
c       idxpol  I  the pollutant index to fill
c     Outputs:
c       ierr    I  error flag
c       
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      07/18/96  --jlf--  developed from rdemfc
c      09/20/97  --gwilson-- added the CAP/NOCAP flag
c      04/27/98  --gwilson-- completely reworked for new type of data
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
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4     ierr
      integer*4     idxpol
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string (min of 1)
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
      character*(4*MXSTR) line
      character*(MXSTR)   fname
      character*20        keywrd, keyin
      character*10        tectmp, poltmp
      integer*4           iounit, irec, jerr
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      ndtfac(idxpol) = 0
      iounit = IORDAC+idxpol
      fname = detfl(idxpol)
c
c  ---- find the /DETFAC/ keyword ----
c
      keywrd = '/DETFAC/'
      call fndkey( jerr, iounit, keywrd )
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read a record as a character string --- 
c
  111 continue
      irec = irec + 1
      read(iounit,8000,ERR=7001,END=7004) line
c
c   --- look for /END/ keyword ---
c
      keyin = line(1:20)
      call lftjst( keyin )
      call low2up( keyin )
      if( keyin .EQ. KEYEND ) goto 222
c
c   --- bump up the counter and check for array overflow ----
c
      ndtfac(idxpol) = ndtfac(idxpol) + 1
      if( ndtfac(idxpol) .GT. MXDTFC ) goto 7002
c
c   --- load the data into the arrays ---
c
      tectmp = line(1:10)
      call lftjst( tectmp )
      call low2up( tectmp )
      tecdet(ndtfac(idxpol),idxpol) = tectmp
      read(line(21:30),'(F10.0)',ERR=7003) detavl(ndtfac(idxpol),idxpol)
      read(line(31:40),'(F10.0)',ERR=7003) detbvl(ndtfac(idxpol),idxpol)
      read(line(41:50),'(F10.0)',ERR=7003) capdet(ndtfac(idxpol),idxpol)
      poltmp = line(51:60)
      call lftjst( poltmp )
      call low2up( poltmp )
c
c   --- check for the correct pollutant name ---
c
      if( poltmp .NE. amspol(idxpol) ) goto 7005
c
c   --- get the next record ---
c
      goto 111
c
c  --- entire file read ---
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
      write(IOWSTD,'(/,1X,3A,/,9X,2A)',ERR=9999) 'ERROR:  Cannot find ',
     &                  keywrd(:strmin(keywrd)), ' packet',
     &                 'of deterioration factors file ',
     &                                   fname(:strmin(fname))
      write(IOWMSG,'(/,1X,3A,/,9X,2A)',ERR=9999) 'ERROR:  Cannot find ',
     &                  keywrd(:strmin(keywrd)), ' packet',
     &                 'of deterioration factors file ',
     &                                   fname(:strmin(fname))
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,3A,I6)',ERR=9999) 
     &                   'ERROR:  Reading deterioration factor file ',
     &                        fname(:strmin(fname)),' at record ',irec
      write(IOWMSG,'(/,1X,3A,I6)',ERR=9999) 
     &                   'ERROR:  Reading deterioration factor file ',
     &                        fname(:strmin(fname)),' at record ',irec
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,A,I8,/,9X,2A)',ERR=9999) 
     &   'ERROR:  Number of deterioration factors exceed max ',
     &                         MXDTFC,'in file ', fname(:strmin(fname))
      write(IOWMSG,'(/,1X,A,I8,/,9X,2A)',ERR=9999) 
     &   'ERROR:  Number of deterioration factors exceed max ',
     &                         MXDTFC,'in file ', fname(:strmin(fname))
c
 7003 continue
      write(IOWSTD,'(/,1X,2A,/,9X,2A)',ERR=9999) 
     &                'ERROR:  Reading deterioration factor file ',
     &                 fname(:strmin(fname)),
     &                'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,/,9X,2A)',ERR=9999) 
     &                'ERROR:  Reading deterioration factor file ',
     &                 fname(:strmin(fname)),
     &                'Line read: ',line(:strmin(line))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,A,/,9X,2A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached',
     &       'reading deterioration factors file ',fname(:strmin(fname))
      write(IOWMSG,'(/,1X,A,/,9X,2A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached',
     &       'reading deterioration factors file ',fname(:strmin(fname))
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR: Invalid pollutant ',
     &            'name in deterioration file: ',fname(:strmin(fname))
      write(IOWSTD,'(10X,3A,I4)') 'Read pollutant: ',
     &                    poltmp(:strmin(poltmp)),' at record: ',irec
      write(IOWSTD,'(10X,2A)') 'Looking for pollutant: ',
     &                           amspol(idxpol)(:strmin(amspol(idxpol)))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR: Invalid pollutant ',
     &            'name in deterioration file: ',fname(:strmin(fname))
      write(IOWMSG,'(10X,3A,I4)') 'Read pollutant: ',
     &                    poltmp(:strmin(poltmp)),' at record: ',irec
      write(IOWMSG,'(10X,2A)') 'Looking for pollutant: ',
     &                           amspol(idxpol)(:strmin(amspol(idxpol)))
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
