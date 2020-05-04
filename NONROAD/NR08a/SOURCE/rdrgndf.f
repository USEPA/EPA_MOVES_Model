C**** RDRGNDF
c
      subroutine rdrgndf( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the region definition.
c
c    Argument declaration.
c     Outputs:
c       ierr  I error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      05/22/97  --mmj--  original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdreg.inc'
      include 'nonrdtpl.inc'
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
c   strmin  I   returns the actual length of a string (minimum of 1)
c   fndchr  I   returns the index of a string in an array of strings
c
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(200) line
      character*20    keywrd, keyin
      character*5     sttmp, rgdtmp
      integer*4       irec, jerr, i, idxreg
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      nregcd = 0
c
c   --- open the file ---
c
      open(IORREG,file=regfl,ERR=7005,status='UNKNOWN')
      rewind(IORREG)
c
c   --- initialize states per region counter ---
c
      do 10 i=1,MXREGN
         isttcnt(i) = 0
   10 continue
c
c   --- call routine to find the /REGIONS/ packet ----
c
      keywrd = '/REGIONS/'
      call fndkey( jerr, IORREG, keywrd )
      if( jerr .NE. ISUCES )  goto 7000
c
c   --- read a record as a character string --- 
c
  111 continue
      irec = irec + 1
      read(IORREG,8000,ERR=7001,END=7002) line
      call spinit()
c
c   --- check for the end keyword ---
c
      keyin = line(1:10)
      call lftjst( keyin )
      call low2up( keyin )
      if( keyin .EQ. KEYEND ) goto 222
c
c   --- parse the line ---
c
      rgdtmp = line(1:5)
      call low2up( rgdtmp )
      call lftjst( rgdtmp )
      idxreg = fndchr( rgdtmp, 5, rgdfcd, nregcd )
      if( idxreg .LE. 0 ) then
         nregcd = nregcd + 1
         if( nregcd .GT. MXREGN ) goto 7003
         rgdfcd(nregcd) = rgdtmp
         idxreg = nregcd
      endif
c
c   --- check number of maximum region definitions ---
c
      sttmp = line(46:50)
      isttcnt(idxreg) = isttcnt(idxreg) + 1
      if ( isttcnt(idxreg) .GT. MXREGN ) goto 7004
      rgstt(idxreg,isttcnt(idxreg)) =  sttmp
c
c   --- get the next record ---
c
      goto 111
c
c   --- finished reading file, close it and return ---
c
  222 continue
      close(IORREG)
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,3A,/,9X,2A)',ERR=9999)
     &        'ERROR:  Cannot find ', keywrd(:strmin(keywrd)),' packet',
     &               'of region definition file ', regfl(:strmin(regfl))
      write(IOWMSG,'(/,1X,3A,/,9X,2A)',ERR=9999) 
     &        'ERROR:  Cannot find ', keywrd(:strmin(keywrd)),' packet',
     &               'of region definition file ', regfl(:strmin(regfl))
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1x,3A,I5)',ERR=9999)
     &                   'ERROR:  Reading region definition file: ',
     &                    regfl(:strmin(regfl)),' at record: ',irec
      write(IOWMSG,'(/,1x,3A,I5)',ERR=9999)
     &                   'ERROR:  Reading region definition file: ',
     &                    regfl(:strmin(regfl)),' at record: ',irec
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,A,/,9X,3A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached','reading ',
     &           'region definition file: ',regfl(:strmin(regfl))
      write(IOWMSG,'(/,1X,A,/,9X,3A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached','reading ',
     &           'region definition file: ',regfl(:strmin(regfl))
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,3A,/,9X,3A,I5)',ERR=9999) 
     &    'ERROR:  Number of regions in ',keywrd(:strmin(keywrd)),
     &   ' packet','of regions definition file: ',regfl(:strmin(regfl)),
     &                                           ' exceeds max: ',MXREGN
      write(IOWMSG,'(/,1X,3A,/,9X,3A,I5)',ERR=9999) 
     &    'ERROR:  Number of regions in ',keywrd(:strmin(keywrd)),
     &   ' packet','of regions definition file: ',regfl(:strmin(regfl)),
     &                                           ' exceeds max: ',MXREGN
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,2A,/,9X,3A,I5)',ERR=9999) 
     &   'ERROR:  Number of states for region ',rgdfcd(idxreg), 
     &     'in region definition file: ',regfl(:strmin(regfl)),     
     &                                           ' exceeds max: ',MXREGN
      write(IOWMSG,'(/,1X,2A,/,9X,3A,I5)',ERR=9999) 
     &   'ERROR:  Number of states for region ',rgdfcd(idxreg), 
     &     'in region definition file: ',regfl(:strmin(regfl)),     
     &                                           ' exceeds max: ',MXREGN
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file ',
     &                                           regfl(:strmin(regfl))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file ',
     &                                           regfl(:strmin(regfl))
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
