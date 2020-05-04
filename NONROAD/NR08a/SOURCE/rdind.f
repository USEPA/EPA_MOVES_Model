C**** RDIND
c
      subroutine rdind( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the spatial indicator data and writes it to a scratch file. 
c    All of the data is read and written to a direct access scratch file.
c    The data is then sorted on the indicator code and year and 
c    rewritten to the target file in sorted order.
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
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdalo.inc'
      include 'nonrdreg.inc'
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4         ierr
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
c   MXIREC  I  maximum number of records in a state file
c
      integer*4 MXIREC
c
      parameter( MXIREC = 100000 ) 
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) line, fname, scrnam, last
      character*20      keytmp, keywrd, keyin
      character*17      srtkey(MXIREC)
      character*5       fiptmp, subtmp
      character*4       yrtmp
      character*3       indtmp
      integer*4         idxrec(MXIREC), jerr, nrecs, irec, idxfip, i
      integer*4         idxcod
      logical*4         lexist
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data scrnam /'alodir.txt'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      nrecs = 0
      last = ' '
      keywrd = '/ALLOC FILES/'
c
c   --- look for the packet ----
c
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- open the file that will be used for the data ----
c
      open(file=indfl,unit=IOSIND,ERR=7006,status='UNKNOWN')
c
c   --- open the direct access file ----
c
      open(file=scrnam,unit=IOSDIR,form='FORMATTED',status='UNKNOWN',
     &                            access='DIRECT',RECL=MXSTR,ERR=7005)
c
c   -- echo to message file ---
c
      write(IOWMSG,9002,ERR=9999)
      write(IOWMSG,9003,ERR=9999) '*** Spatial Allocation Files ***'
      write(IOWMSG,9002,ERR=9999)
c
c   --- read a filename and open the file ---
c
  111 continue
      read(IORUSR,8000,ERR=7004,END=7004) keytmp, fname
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 333
      if( strlen(fname) .LE. 0 ) goto 111
      call lftjst( fname )
      write(IOWMSG,9004,ERR=9999) ' ',fname(:strmin(fname))
      inquire(file=fname,exist=lexist)
      if( .NOT. lexist ) goto 7002
      open(file=fname,unit=IORIND,ERR=7003,status='UNKNOWN')
c
c  ---- find the /INDICATORS/ keyword ----
c
      keywrd = '/INDICATORS/'
      call fndkey( jerr, IORIND, keywrd )
      if( jerr .NE. ISUCES ) goto 7011
c
c   --- read a record as a character string --- 
c
  222 continue
      read(IORIND,8001,ERR=7001,END=7012) line
      irec = irec + 1
      call spinit()
c
c   --- look for /END/ keyword ---
c
      keyin = line(1:20)
      call lftjst( keyin )
      call low2up( keyin )
      if( keyin .EQ. KEYEND ) goto 111
      last = line
c
c   --- get the data from the line and add to the array of 
c       sort keys ----
c
      indtmp = line(1:3)
      fiptmp = line(6:10)
      subtmp = line(11:15)
      yrtmp = line(16:20) 
c
c   --- left justify and convert to upper case where necessary ---
c
      call lftjst( fiptmp )
      call lftjst( subtmp )
      call low2up( subtmp )
c
c   --- if county is not requested, skip it ---
c
      if( fiptmp .EQ. '00000' ) then
          if(  reglvl .NE. NATION .AND. reglvl .NE. STATE ) goto 222
      else if( fiptmp(3:5) .EQ. '000' ) then
          idxfip = fndchr( fiptmp, 5, statcd, NSTATE )
          if( idxfip .LE. 0 ) goto 222
          if( .NOT. lstacd(idxfip) ) goto 222
      else
          idxfip = fndchr( fiptmp, 5, fipcod, NCNTY ) 
          if( idxfip .LE. 0 ) goto 222
          if( .NOT. lfipcd(idxfip) ) goto 222
      endif
c
c   --- if this code is not in list of codes used, skip it ---
c
      idxcod = fndchr( indtmp, 3, alocod, nalocd )
      if( idxcod .LE. 0 ) goto 222
c
c   --- load the data into sort key array ---
c
      nrecs = nrecs + 1
      if( nrecs .GT. MXIREC ) goto 7007
      srtkey(nrecs) = indtmp//fiptmp//subtmp//yrtmp
c
c   --- write the data record to direct access file ----
c
      write(IOSDIR,9001,ERR=7008,REC=nrecs) line
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
         call chrsrt( srtkey, 17, nrecs, idxrec )
         call spinit()
      endif
c
c   --- loop over the records and write the each record to 
c       the sorted scratch file ----
c
      do 10 i=1,nrecs
         read(IOSDIR,8001,ERR=7009,REC=idxrec(i)) line
         write(IOSIND,9001,ERR=7010) line
   10 continue
c
c   --- get the next allocation file ----
c
      close(IORIND)
c
c   --- processed all of the files, make sure last record has
c       a different FIPS code ---
c
  444 continue
      last(6:10) = 'xxxxx'
      write(IOSIND,9001,ERR=7010) last
      rewind(IOSIND)
c
c   --- close the direct acces file, and delete it ---
c
      close(IOSDIR,status='DELETE')
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot find ',
     &       keywrd(:strmin(keywrd)),' packet of the options file.'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot find ',
     &       keywrd(:strmin(keywrd)),' packet of the options file.'
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,2A,/,9X,A,I8)',ERR=9999) 
     &                     'ERROR:  Reading spatial indicator file ',
     &                        fname(:strmin(fname)),'at record ',irec
      write(IOWMSG,'(/,1X,2A,/,9X,A,I8)',ERR=9999) 
     &                     'ERROR:  Reading spatial indicator file ',
     &                        fname(:strmin(fname)),'at record ',irec
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot find spatial',
     &                        ' allocation file ',fname(:strmin(fname))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot find spatial',
     &                        ' allocation file ',fname(:strmin(fname))
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 
     &              'ERROR:  Opening ownership allocation file ',
     &                                          fname(:strmin(fname))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 
     &              'ERROR:  Opening ownership allocation file ',
     &                                          fname(:strmin(fname))
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
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot open ',
     &    'direct-acces file used for sorting ',scrnam(:strmin(scrnam))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot open ',
     &    'direct-acces file used for sorting ',scrnam(:strmin(scrnam))
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot open scratch ',
     &               'file used for sorted spatial indicators ',
     &                                        indfl(:strmin(indfl))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot open scratch ',
     &               'file used for sorted spatial indicators ',
     &                                        indfl(:strmin(indfl))
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,2A,I8)',ERR=9999) 'ERROR:  Number of ',
     &          'allocation indicator records excceds max: ',MXIREC
      write(IOWMSG,'(/,1X,2A,I8)',ERR=9999) 'ERROR:  Number of ',
     &          'allocation indicator records excceds max: ',MXIREC
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Writing ',
     &   'direct-access file used for sorting ',scrnam(:strmin(scrnam))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Writing ',
     &   'direct-access file used for sorting ',scrnam(:strmin(scrnam))
      goto 9999
c
 7009 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &   'direct-access file used for sorting ',scrnam(:strmin(scrnam))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &   'direct-access file used for sorting ',scrnam(:strmin(scrnam))
      goto 9999
c
 7010 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR: Writing scratch ',
     &                'file used for spatial allocation indicators ',
     &                                          indfl(:strmin(indfl))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR: Writing scratch ',
     &                'file used for spatial allocation indicators ',
     &                                          indfl(:strmin(indfl))
      goto 9999
c
 7011 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &     keywrd(:strmin(keywrd)),' packet of the allocation file ',
     &                                          fname(:strmin(fname))
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &     keywrd(:strmin(keywrd)),' packet of the allocation file ',
     &                                          fname(:strmin(fname))
      goto 9999
c
 7012 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &                      'allocation file ',fname(:strmin(fname))
      write(IOWMSG,'(/,1X,4A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &                      'allocation file ',fname(:strmin(fname))
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
