C**** RDALT
c
      subroutine rdalt( ierr, iounit, fname )
c
c-----------------------------------------------------------------------
c
c    reads the alternate scrappage curves and stores the data in 
c    common to be used by the EPA Nonroad model.
c
c    Argument declaration.
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c       iounit  I  unit number of file to read
c       fname   C  name of file to be read
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/25/97  --gwilson--  original development
c      04/05/05  --cimulus--  default percent scrapped to 100.0 and
c                             allow up to MXSCRP values instead of
c                             MXSCRP - 1
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdefc.inc'
      include 'nonrdeqp.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4         ierr
      integer*4         iounit
      character*(MXSTR) fname
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
      character*10        tmpstr(MXALT)
      integer*4           irec, jerr, ncount, ibeg, i, j
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 1
      ncount = 0
      naltnm = 0
c
c   --- initialize the scrappage values ---
c
      do 10 j=1,MXSCRP
         altbin(j) = 2.5
         do 20 i=1,MXALT
           altpct(i,j) = 100.0 ! any bin values beyond end of scrappage curve from file are fully scrapped
   20    continue
   10 continue
c
c   --- call routine to find the /ALTERNATE SCRAPPAGE/ keyword ----
c
      keywrd = '/ALTERNATE SCRAPPAGE'
      call fndkey(jerr, iounit, keywrd )
      if( jerr .EQ. IEOF ) then
          ierr = ISKIP 
          goto 9999
      endif
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read the labels ---
c
      read(iounit,8000,ERR=7001,END=222) line
      do 30 i=1,MXALT 
         ibeg = (i-1)*10 + 1
         if( line(ibeg:ibeg+9) .NE. '          ' ) then
            naltnm = naltnm + 1
            altnam(naltnm) = line(ibeg:ibeg+9)
            call low2up( altnam(naltnm) )
         endif
   30 continue
c
c   --- read a record as a character string --- 
c
  111 continue
      read(iounit,8000,ERR=7001,END=222) line
      call spinit()
      irec = irec + 1
c
c   --- search for the end of the packet ---
c
      keytmp = line(1:10)
      call lftjst( keytmp )
      call low2up( keytmp )
      if( keytmp .EQ. KEYEND ) goto 222
c
c   --- increment record count and finish if gone past maximum ---
c
      ncount = ncount + 1
      if( ncount .GT. MXSCRP ) goto 222
c
c   --- read the data and store it in common arrays ---
c
      read(line,8001,ERR=7002) altbin(ncount), 
     &                                (tmpstr(i),i=1,naltnm)
      do 40 i=1,naltnm
         if( tmpstr(i) .NE. '          ' ) then
             read(tmpstr,'(F10.0)',ERR=7002) altpct(i,ncount)
         else
             altpct(i,ncount) = 100.0
         endif
   40 continue
      call spinit()
c
c   --- make sure the file is ordered correctly ---  
c
      if( ncount .GT. 1 ) then
         if( altbin(ncount) .LT. altbin(ncount-1) ) goto 7003
      endif
c
c   --- read the next record ---
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
      write(IOWSTD,9000,ERR=9999) 'ERROR:  Reading growth file: ',
     &                                           fname(:strmin(fname))
      write(IOWMSG,9000,ERR=9999) 'ERROR:  Reading growth file: ',
     &                                           fname(:strmin(fname))
      goto 9999
c
 7001 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR:  Reading ',
     &           keywrd(:strmin(keywrd)),'/ packet of growth file.'
      write(IOWMSG,9000,ERR=9999) 'ERROR:  Reading ',
     &           keywrd(:strmin(keywrd)),'/ packet of growth file.'
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,3A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &         keywrd(:strmin(keywrd)),'/ packet of growth file.',
     &                              'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,3A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &         keywrd(:strmin(keywrd)),'/ packet of growth file.',
     &                              'Line read: ',line(:strmin(line))
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,3A,/,9X,A)',ERR=9999) 
     &             'ERROR:  Data in ',keywrd(:strmin(keywrd)),
     &       '/ packet','of growth file is not ordered correctly.'
      write(IOWMSG,'(/,1X,3A,/,9X,A)',ERR=9999) 
     &             'ERROR:  Data in ',keywrd(:strmin(keywrd)),
     &       '/ packet','of growth file is not ordered correctly.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(A)
 8001 format(F10.0,10A10)
 9000 format(/,1X,3A)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
