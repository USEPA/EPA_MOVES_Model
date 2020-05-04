C**** RDSULF
c
      subroutine rdsulf( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the /PM BASE SULFUR/ packet used in the NONROAD program 
c
c    Argument description.
c
c     Outputs:
c       ierr    I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c 
c      10/30/01  --charvey-- original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) line
      character*20 keywrd
      integer*4    jerr, itec
      character*10 tmptec
      real*4       tmpalt, tmpcnv
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- initialize base sulfur arrays.
c      can't do in INTADJ because that comes after RDSULF. ---
c
      do 10 itec = 1,MXSULF
        sultec(itec) = '          '
        sulalt(itec) = -1.0
        sulcnv(itec) = -1.0
   10 continue
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c --- look for /PM BASE SULFUR/ packet ---
c
      keywrd = '/PM BASE SULFUR/'
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .EQ. IRDERR ) goto 7000
c
c  --- if packet was not found, then no special cert sulfur to apply ---
c
      if( jerr .EQ. IEOF ) then
         goto 222
      endif
c
c  --- if packet was found, read until /END/ keyword found ---
c
      numalt = 0
 111  continue
      read(IORUSR,8000,ERR=7001,END=7002) line
      read(line(1:10),'(A10)',ERR=7001,END=7002) tmptec
c
c   --- look for /END/ keyword ---
c
      call lftjst( tmptec )
      call low2up( tmptec )
      if( tmptec .EQ. KEYEND ) goto 222
c
c   --- incrament counter and check for overflow ----
c
      numalt = numalt + 1
      if( numalt .GT. MXSULF ) goto 7005
c
c   --- read rest of record and store in global arrays ---
c
      sultec(numalt) = tmptec
      read(line(11:20),'(F10.0)',ERR=7001,END=7002) tmpalt
      if( line(11:20) .NE. '          ' ) then
         if( tmpalt .LT. 0. .OR. tmpalt .GT. 1.00 ) goto 7003
         sulalt(numalt) = tmpalt
      else
         sulalt(numalt) = RMISS
         write(IOWMSG,'(/,1X,4A)') 'WARNING:  Blank value read in ',
     &            keywrd(:strmin(keywrd)),' packet of options file. '
         write(IOWMSG,'(11X,4A,F7.5)')
     &                  'Default base (cert) sulfur ',
     &                  'will be used for tech type: ',
     &                   tmptec(:strmin(tmptec)),
     &                  ' = ', SWTDSL
         nwarn = nwarn + 1
      endif
c
      read(line(21:30),'(F10.0)',ERR=7001,END=7002) tmpcnv
      if( line(21:30) .NE. '          ' ) then
         if( tmpcnv .LT. 0. .OR. tmpcnv .GT. 1.00 ) goto 7004
         sulcnv(numalt) = tmpcnv
      else
         sulcnv(numalt) = RMISS
         write(IOWMSG,'(/,1X,4A)') 'WARNING:  Blank value read in ',
     &            keywrd(:strmin(keywrd)),' packet of options file. '
         write(IOWMSG,'(11X,4A,F7.5)')
     &                  'Default sulfate conversion ',
     &                  'will be used for tech type: ',
     &                   tmptec(:strmin(tmptec)),
     &                  ' = ', SFCDSL
         nwarn = nwarn + 1
      endif
c
c  --- get next record ---
c
      goto 111
c
c  --- set error flag to success ----
c
 222  continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Reading the ',
     &                        'options file while searching for the ', 
     &                               keywrd(:strmin(keywrd)),' packet.'
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Reading the ',
     &                       'options file while searching for the ',
     &                               keywrd(:strmin(keywrd)),' packet.'
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR: Reading ',
     &           keywrd(:strmin(keywrd)),' packet of the options file.'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR: Reading ',
     &           keywrd(:strmin(keywrd)),' packet of the options file.'
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,A,/,9X,2A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &          keywrd(:strmin(keywrd)),' packet of the options file.'
      write(IOWMSG,'(/,1X,A,/,9X,2A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &          keywrd(:strmin(keywrd)),' packet of the options file.'
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 
     &     'ERROR:  Invalid value found in ',keywrd(:strmin(keywrd)),
     &                                       ' packet of options file.'
      write(IOWSTD,'(10X,A)',ERR=9999)
     &                'Valid values for Sulfur content are: 0.0 to 1.0'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 
     &     'ERROR:  Invalid value found in ',keywrd(:strmin(keywrd)),
     &                                       ' packet of options file.'
      write(IOWMSG,'(10X,A)',ERR=9999) 
     &                'Valid values for Sulfur content are: 0.0 to 1.0'
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 
     &     'ERROR:  Invalid value found in ',keywrd(:strmin(keywrd)),
     &                                       ' packet of options file.'
      write(IOWSTD,'(10X,A)',ERR=9999)
     &            'Valid values for Sulfur conversion are: 0.0 to 1.0'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 
     &     'ERROR:  Invalid value found in ',keywrd(:strmin(keywrd)),
     &                                       ' packet of options file.'
      write(IOWMSG,'(10X,A)',ERR=9999)
     &            'Valid values for Sulfur conversion are: 0.0 to 1.0'
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,3A,I5)',ERR=9999)
     &         'ERROR:  Number of records in ',keywrd(:strmin(keywrd)),
     &                     ' packet of options file exceeds max:',MXSULF
      write(IOWMSG,'(/,1X,3A,I5)',ERR=9999)
     &         'ERROR:  Number of records in ',keywrd(:strmin(keywrd)),
     &                     ' packet of options file exceeds max:',MXSULF
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
