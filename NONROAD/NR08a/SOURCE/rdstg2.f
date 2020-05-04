C**** RDSTG2
c
      subroutine rdstg2( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the /STAGE II/ packet used in the NONROAD program 
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
c      12/14/98  --mjimenez-- original development
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
      character*20 keywrd
      integer*4    jerr
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c --- look for /STAGE II/ packet ---
c
      keywrd = '/STAGE II/'
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .EQ. IRDERR ) goto 7000
c
c  --- if packet was not found, then no stage II control to be applied ---
c
      if( jerr .EQ. IEOF ) then
         stg2fac = 1.0
         goto 222
      endif
c
c  --- if packet was found, read the data ---
c
      read(IORUSR,8000,ERR=7001,END=7002) stg2fac
      if( stg2fac .LT. 0. .OR. stg2fac .GT. 100.0 ) goto 7003
      stg2fac = 1.0 - (stg2fac / 100.0)
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
     &                       'options file while searching for the ',
     &                               keywrd(:strmin(keywrd)),' packet.'
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Reading the ',
     &                       'options file while searching for the ',
     &                               keywrd(:strmin(keywrd)),' packet.'
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR: Reading ',
     &                                        keywrd(:strmin(keywrd)),
     &                                   ' packet of the options file.'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR: Reading ',
     &                                        keywrd(:strmin(keywrd)),
     &                                   ' packet of the options file.'
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
     &             'ERROR:  Invalid value found in ',
     &               keywrd(:strmin(keywrd)),' packet of options file.'
      write(IOWSTD,'(10X,A)',ERR=9999) 'Valid values are: 0.0 to 100.0'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 
     &             'ERROR:  Invalid value found in ',
     &               keywrd(:strmin(keywrd)),' packet of options file.'
      write(IOWMSG,'(10X,A)',ERR=9999) 'Valid values are: 0.0 to 100.0'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(20x,F7.0)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
