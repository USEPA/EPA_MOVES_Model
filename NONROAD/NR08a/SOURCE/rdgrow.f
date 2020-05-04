C**** RDGROW
c
      subroutine rdgrow( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the growth indicator data and writes it to a scratch file. 
c    All of the data is read and written to a direct access scratch file.
c    The data is then sorted based on indicator code and year and
c    rewritten to the scratch file in the sorted order.
c
c    Argument declaration.
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/01/93  --gmw--  original development
c      04/24/17  --dbc--  increased MXGREC for MOVES2014b
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdgrw.inc'
      include 'nonrdreg.inc'
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
c   strlen  I   returns the actual length of a string
c   strmin  I   returns the actual length of a string (minimum of 1)
c   fndchr  I   returns the index of a string in an array of strings
c
      integer*4 strlen
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local paramneters:
c-----------------------------------------------------------------------
c
c   MXGREC  I  maximum number of records in a state file
c
      integer*4 MXGREC
c
      parameter( MXGREC = 55000 ) 
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) line, fname, scrnam
      character*20      keywrd, keytmp
      character*14      srtkey(MXGREC)
      character*5       fiptmp
      character*5       yrtmp
      character*4       indtmp
      integer*4         idxrec(MXGREC), jerr, nrecs, irec, idxfip, i
      logical*4         lexist, lscrap
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data scrnam /'grwdir.txt'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      nrecs = 0
      lscrap = .FALSE.
c
c   --- look for the packet ----
c
      keywrd = '/GROWTH FILES/'
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .EQ. IRDERR ) goto 7000
      if( jerr .EQ. IEOF ) then
         lgrwfl = .FALSE.
         ierr = ISUCES
         goto 9999
      endif
      lgrwfl = .TRUE.
c
c   --- open the file that will be used for the data ----
c
      open(file=grwfl,unit=IOSGRW,ERR=7006,status='UNKNOWN')
c
c   --- open the direct access file ----
c
      open(file=scrnam,unit=IOSDIR,form='FORMATTED',status='UNKNOWN',
     &                            access='DIRECT',RECL=MXSTR,ERR=7005)
c
c   -- echo to message file ---
c
      write(IOWMSG,9002,ERR=9999)
      write(IOWMSG,9003,ERR=9999) '*** Growth Indicator Files ***'
      write(IOWMSG,9002,ERR=9999)
c
c   --- read a filename and open the file ---
c
  111 continue
      read(IORUSR,8000,ERR=7004,END=7004) keytmp, fname
      call spinit()
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 444
      if( strlen(fname) .LE. 0 ) goto 111
      call lftjst( fname )
      write(IOWMSG,9004,ERR=9999) ' ',fname(:strmin(fname))
      inquire(file=fname,exist=lexist)
      if( .NOT. lexist ) goto 7002
      open(file=fname,unit=IORGRW,ERR=7003,status='UNKNOWN')
c
c   --- call routine to read the growth cross reference data ---
c
      call rdgxrf( jerr, IORGRW, fname )
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c   --- call routine to find the /GROWTH/ keyword ----
c
      keywrd = '/GROWTH/'
      call fndkey( jerr, IORGRW, keywrd )
      if( jerr .NE. ISUCES ) goto 7011
c
c   --- read a record as a character string --- 
c
  222 continue
      read(IORGRW,8001,ERR=7001,END=333) line
      call spinit()
      irec = irec + 1
      keytmp = line(1:20)
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 333
c
c   --- get the data from the line and add to the array of 
c       sort keys ----
c
      fiptmp = line(1:5)
      yrtmp = line(11:15)
      indtmp = line(17:20) 
c
c   --- if county is not requested, skip it ---
c
      if( fiptmp .NE. '00000' ) then
         if( fiptmp(3:5) .EQ. '000' ) then
             idxfip = fndchr( fiptmp, 5, statcd, NSTATE )
             if( idxfip .LE. 0 ) goto 222
             if( .NOT. lstacd(idxfip) ) goto 222
         else
             idxfip = fndchr( fiptmp, 5, fipcod, NCNTY ) 
             if( idxfip .LE. 0 ) goto 222
             if( .NOT. lfipcd(idxfip) ) goto 222
         endif
      endif
c
c   --- load the data into sort key array ---
c
      nrecs = nrecs + 1
      if( nrecs .GT. MXGREC ) goto 7007
      srtkey(nrecs) = indtmp//fiptmp//yrtmp
c
c   --- write the data record to direct access file ----
c
      write(IOSDIR,'(A)',ERR=7008,REC=nrecs) line
c
c   --- get the next record ---
c
      goto 222
c
c   --- read all of the records for this file, call routine to sort
c       the keys ---
c
  333 continue
      if( nrecs .GT. 0 ) then
         call chrsrt( srtkey, 14, nrecs, idxrec )
         call spinit()
      else
         ierr = IEOF
         goto 9999
      endif
c
c   --- loop over the records and write the each record to 
c       the sorted scratch file ----
c
      do 10 i=1,nrecs
         read(IOSDIR,8001,ERR=7009,REC=idxrec(i)) line
         call spinit()
         write(IOSGRW,'(A)',ERR=7010) line
   10 continue
c
c  --- call routine to read the scrappage curve bins, if they exist in
c      this file  ---
c
      call rdscrp( jerr, IORGRW, fname )
      if( jerr .NE. ISUCES .AND. jerr .NE. ISKIP ) goto 9999
      if( jerr .EQ. ISUCES ) lscrap = .TRUE.
      call spinit()
c
c  --- call routine to read the alternate scrappage curves ---
c
      call rdalt( jerr, IORGRW, fname)
      if( jerr .NE. ISUCES .AND. jerr .NE. ISKIP ) goto 9999
c
c   --- get the next allocation file ----
c
      close(IORGRW)
      goto 111
c
c   --- processed all of the files, make sure last record has
c       a different FIPS code ---
c
  444 continue
      line(1:10) = KEYEND
      inquire(unit=IOSGRW,name=fname)
      write(IOSGRW,'(A)',ERR=7010) line
      rewind(IOSGRW)
c
c   --- close the direct acces file, and delete it ---
c
      close(IOSDIR,status='DELETE')
c
c  --- check to make sure the scrappage curve was found ---
c
      if( .NOT. lscrap ) goto 7012
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
      write(IOWSTD,'(/,1X,2A,/,9X,A,I10)',ERR=9999) 
     &                     'ERROR:  Reading growth indicator file ',
     &                        fname(:strmin(fname)),'at record ',irec
      write(IOWMSG,'(/,1X,2A,/,9X,A,I10)',ERR=9999) 
     &                     'ERROR:  Reading growth indicator file ',
     &                        fname(:strmin(fname)),'at record ',irec
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 
     &         'ERROR:  Cannot find growth file ',fname(:strmin(fname))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 
     &         'ERROR:  Cannot find growth file ',fname(:strmin(fname))
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 
     &             'ERROR:  Opening growth file ',fname(:strmin(fname))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 
     &             'ERROR:  Opening growth file ',fname(:strmin(fname))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &           keywrd(:strmin(keywrd)),' packet of options file.'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &           keywrd(:strmin(keywrd)),' packet of options file.'
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 
     &    'ERROR:  Cannot open direct-access file used for sorting ',
     &                                           scrnam(:strmin(scrnam))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 
     &    'ERROR:  Cannot open direct-access file used for sorting ',
     &                                           scrnam(:strmin(scrnam))
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot open scratch ',
     &                       'file used for sorted growth indicators ',
     &                                             grwfl(:strmin(grwfl))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot open scratch ',
     &                       'file used for sorted growth indicators ',
     &                                             grwfl(:strmin(grwfl))
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,3A,I10)',ERR=9999) 'ERROR:  Number of ',
     &                          'growth allocation indicator records',
     &                                           ' exceeds max ',MXGREC
      write(IOWMSG,'(/,1X,3A,I10)',ERR=9999) 'ERROR:  Number of ',
     &                          'growth allocation indicator records',
     &                                           ' exceeds max ',MXGREC
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 
     &                 'ERROR:  Writing direct-access file ',
     &                      'used for sorting ',scrnam(:strmin(scrnam))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 
     &                 'ERROR:  Writing direct-access file ',
     &                      'used for sorting ',scrnam(:strmin(scrnam))
      goto 9999
c
 7009 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 
     &                  'ERROR:  Reading direct-access file ',
     &                      'used for sorting ',scrnam(:strmin(scrnam))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 
     &                  'ERROR:  Reading direct-access file ',
     &                      'used for sorting ',scrnam(:strmin(scrnam))
      goto 9999
c
 7010 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 
     &                 'ERROR: Writing scratch file used ',
     &                         'for growth allocation indicators ',
     &                                           scrnam(:strmin(scrnam))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 
     &                 'ERROR: Writing scratch file used ',
     &                         'for growth allocation indicators ',
     &                                           scrnam(:strmin(scrnam))
      goto 9999
c
 7011 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &       keywrd(:strmin(keywrd)),' packet of the growth file ',
     &                                           fname(:strmin(fname))
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &       keywrd(:strmin(keywrd)),' packet of the growth file ',
     &                                           fname(:strmin(fname))
      goto 9999
c
 7012 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR:  Cannot find ',
     &           '/SCRAPPAGE/ packet in any of the growth files.'
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR:  Cannot find ',
     &           '/SCRAPPAGE/ packet in any of the growth files.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(A20,A)
 8001 format(A)
 9001 format(A)
 9002 format(1X,A)
 9003 format(T20,A)
 9004 format(T10,A,T30,:,':',A)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
