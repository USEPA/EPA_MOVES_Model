C**** GETGRW
c
      subroutine getgrw( ierr, indin )
c
c-----------------------------------------------------------------------
c
c    reads the growth indicator data from sorted scratch file.
c    the only data that is saved is for the indicator code specified in 
c    the argument list.
c
c    Argument declaration.
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c       indin   C  indicator code 
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/01/93  --gmw--  original development
c      04/06/05  --cimulus--  removed code that was calculating modified
c                             version of the scrappage curve; this
c                             handling is replaced by scrptime
c      07/20/05  --cimulus--  removed unused variable MLINE1
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE
C
      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdgrw.inc'
      include 'nonrdeqp.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4     ierr
      character*4   indin
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string (minumum of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) line
      character*10      keytmp
      character*5       fiptmp, subtmp
      character*4       indtmp
      integer*4         irec, iyrtmp
      real*4            valtmp
      logical           lpass
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 1
      nrcgrw = 0
      lpass = .TRUE.
c
c   --- read a record as a character string --- 
c
  111 continue
      read(IOSGRW,8000,ERR=7001,END=7001) line
      irec = irec + 1
c
c   --- look for the /END/ keyword ----
c
      keytmp = line(1:10)
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 222
c
c   --- get the indicator code, if it is not the one we want skip
c       the record ----
c
      indtmp = line(17:20)
      call lftjst( indtmp )
      if( indtmp .LT. indin ) goto 111
      if( indtmp .GT. indin ) goto 222
c
c   --- parse the line and load all of the data ----
c
      fiptmp = line(1:5)
      subtmp = line(6:10)
      read(line(11:15),'(I5)',ERR=7003) iyrtmp
      read(line(26:45),'(F20.0)',ERR=7003) valtmp
c
c   --- store the data in global arrays ----
c
      nrcgrw = nrcgrw + 1
      if( nrcgrw .GT. MXGROW ) goto 7004
      fipgrw(nrcgrw) = fiptmp
      subgrw(nrcgrw) = subtmp
      iyrgrw(nrcgrw) = iyrtmp
      valgrw(nrcgrw) = valtmp
c
c   --- get the next record ---
c
      goto 111
c
c   --- entire file processed ---
c
 222  continue
c
c --- check if we have input data ---
c
      if( nrcgrw .GE. 1 ) then
c
c --- back up one record for next call ---
c
         backspace IOSGRW
         ierr = ISUCES
         goto 9999
      else
c
c --- if the file was beyond the desired end, then rewind and start from
c     beginning of file ---
c
         if( lpass ) then
            rewind IOSGRW
            lpass = .FALSE.
            irec = 0
            goto 111
         else
             goto 7005
         endif
      endif
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
c
 7001 continue
      write(IOWSTD,'(/,1X,2A,/,9X,A,I10)',ERR=9999) 
     &             'ERROR:  Reading growth scratch file ',
     &                     grwfl(:strmin(grwfl)),'at record ',irec
      write(IOWMSG,'(/,1X,2A,/,9X,A,I10)',ERR=9999) 
     &             'ERROR:  Reading growth scratch file ',
     &                     grwfl(:strmin(grwfl)),'at record ',irec
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,2A,/,9X,2A)',ERR=9999) 
     &             'ERROR:  Reading growth scratch file ',
     &         grwfl(:strmin(grwfl)),
     &         'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,/,9X,2A)',ERR=9999) 
     &             'ERROR:  Reading growth scratch file ',
     &         grwfl(:strmin(grwfl)),
     &         'Line read: ',line(:strmin(line))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,2A,I5)',ERR=9999) 
     &                 'ERROR:  Number of growth indicator ',
     &                                    'records exceeds max ',MXGROW
      write(IOWMSG,'(/,1X,2A,I5)',ERR=9999) 
     &                 'ERROR:  Number of growth indicator ',
     &                                    'records exceeds max ',MXGROW
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999)
     &                            'ERROR:  Growth indicator ',indin,
     &                            ' not found in growth indicator file.'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 
     &                            'ERROR:  Growth indicator ',indin,
     &                            ' not found in growth indicator file.'
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
