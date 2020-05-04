C**** RDPOP
c
      subroutine rdpop( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the base year population file and creates a new file with
c    just the populations needed for this run.  Each population file is
c    opened and the data is read from the file.  A direct access 
c    scratch file is written with just the data matching common block
c    variables. A string array is created with the information 
c    needed to sort the data.  This array is used to create a file of 
c    sorted data.
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
c      09/01/93  --gmw-- original development
c      07/21/96  --jlf-- changing format of population file
c      04/20/97  gwilson revised for BETA version
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
      include 'nonrdreg.inc'
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
c   strmin  I   returns the actual length of a string (minimum of 1)
c   fndchr  I   returns the index of a string in an array of strings
c
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local paramneters:
c-----------------------------------------------------------------------
c
c   MXPREC  I  maximum number of records in a state file
c
      integer*4 MXPREC
c
      parameter( MXPREC = 60000 )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(2*MXSTR) line
      character*(MXSTR)   fname, scrnam
      character*30        srtkey(MXPREC)
      character*20        keywrd, keyin
      character*10        asctmp
      character*5         fiptmp, subtmp, hptmp, hpmntmp, hpmxtmp
      character*5         hpavtmp
      character*4         yrtmp
      integer*4           idxrec(MXPREC), irec, nrecs, idxfip, idxeqp
      integer*4           jerr, ifile, i
      real*4              hpmin, hpmax, hpavg
      logical*4           lfound
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data scrnam /'popdir.txt'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      nrecs = 0
c
c   --- open the file that will be used for the sorted population data ----
c
      open(file=spopfl,unit=IOSPOP,ERR=7004,status='UNKNOWN')
      endfile(IOSPOP)
      rewind(IOSPOP)
c
c   --- open the direct access file ----
c
      open(file=scrnam,unit=IOSDIR,form='FORMATTED',status='UNKNOWN',
     &                            access='DIRECT',RECL=2*MXSTR,ERR=7003)
c
c  --- loop over all of the population files ----
c
      do 10 ifile=1,npopfl
         open(file=popfl(ifile),unit=IORPOP,ERR=7001,status='UNKNOWN')
c
c  ---- find the /POPULATION/ keywoed ----
c
         keywrd = '/POPULATION/'
         call fndkey( jerr, IORPOP, keywrd )
         if( jerr .NE. ISUCES ) goto 7000
c
c  --- read the file ----
c
  111    continue 
         read(IORPOP,8001,ERR=7002,END=7009) line
         call spinit()
         irec = irec + 1
c
c   --- look for /END/ keyword ---
c
         keyin = line(1:20)
         call lftjst( keyin )
         call low2up( keyin )
         if( keyin .EQ. KEYEND ) goto 222
c
c   --- parse the line and load all of the data ----
c
         asctmp = line(18:27)
         fiptmp = line(1:5)
         subtmp = line(7:11)
         yrtmp = line(13:16)
         hpmntmp = line(70:74)
         read(hpmntmp,'(F5.0)',ERR=7011) hpmin
         hpmxtmp = line(76:80)
         read(hpmxtmp,'(F5.0)',ERR=7011) hpmax
         hpavtmp = line(82:86)
c
c   --- left justify and convert to upper case where necessary ---
c
         if( hpavtmp .EQ. '     ' ) then
            hpavg = (hpmin+hpmax)/2.
         else 
            read(hpavtmp,'(F5.0)',ERR=7002) hpavg
         endif
         if( hpavg .LT. hpmin .OR. hpavg .GT. hpmax ) then
           write(IOWMSG,'(/,1X,2A)',ERR=9999) 'WARNING:  Average HP is',
     &                 ' outside the horsepower range.'
           write(IOWMSG,'(5X,2(A,10X),A)',ERR=9999) 'Min. HP','Max. HP',
     &                                                        'Avg. HP'
           write(IOWMSG,'(5X,2(I5,12X),I5)',ERR=9999) INT(hpmin),  
     &                                           INT(hpmax),  INT(hpavg)
           write(IOWMSG,'(11X,3A)',ERR=9999) 'Skipping record with ',
     &                                            'SCC code: ',asctmp
           call chkwrn(jerr,IDXWPP)
           if( jerr .NE. ISUCES ) goto 9999
           goto 111 
         endif
c
c   --- left justify and convert to upper case where necessary ---
c
         call lftjst( fiptmp )
         call lftjst( subtmp )
         call low2up( subtmp )
         call rgtjst( hpmntmp )
         call rgtjst( hpmxtmp )
c
c   --- check to make sure HP categories match the internal defaults ----
c
         lfound = .FALSE.
         if( INT(hpmin) .EQ. 0 .AND. INT(hpmax) .EQ. 
     &                                        INT(hpclev(1)) ) then
            lfound = .TRUE.
         else if( INT(hpmin) .EQ. INT(hpclev(MXHPC)) ) then
            lfound = .TRUE.
         else
            do 20 i=1,MXHPC-1
               if( INT(hpmin) .EQ. INT(hpclev(i)) .AND. 
     &               INT(hpmax) .EQ. INT(hpclev(i+1)) ) lfound = .TRUE.
   20       continue
         endif
         if( .NOT. lfound ) goto 7010
c
c   --- if national estimates and not doing national inventory, skip it ---
c
         if( fiptmp(1:5) .EQ. '00000' ) then
            if( reglvl .EQ. COUNTY .AND. reglvl .NE. SUBCTY ) goto 111
         endif
c
c   --- if doing a US total run and this is not a national record, skip it ----
c
         if( reglvl .EQ. USTOT .AND. fiptmp(1:5) .NE. '00000' ) goto 111
c
c   --- if sub-county estimates, check if we need this county ---
c
         if( reglvl .EQ. SUBCTY ) then
            idxfip = fndchr( fiptmp, 5, fipcod, NCNTY ) 
            if( idxfip .EQ. 0 ) goto 111
            if( .NOT. lfipcd(idxfip) ) goto 111
         endif
c
c   --- if state estimates, check that we need this state ---
c
         if( fiptmp(1:2) .NE. '00' ) then
            if( fiptmp(3:5) .EQ. '000' ) then
               idxfip = fndchr( fiptmp, 5, statcd, NSTATE )
               if( idxfip .LE. 0 ) goto 111
               if( .NOT. lstacd(idxfip) ) goto 111
c
c   --- if county estimates ---
c
            else
               idxfip = fndchr( fiptmp, 5, fipcod, NCNTY ) 
               if( idxfip .LE. 0 ) goto 111
               if( .NOT. lfipcd(idxfip) ) goto 111
            endif
         endif
c
c   --- if equipment type is not requested, skip it ---
c
         idxeqp = fndchr( asctmp, 10, eqpcod, NEQNAM )
         if( idxeqp .LE. 0 ) then
            write(IOWMSG,'(/,1X,3A)',ERR=9999) 'WARNING:  Invalid SCC ',
     &                 'code read from population file: ',
     &                       popfl(ifile)(:strmin(popfl(ifile)))
            write(IOWMSG,'(11X,3A)',ERR=9999) 'Skipping record with ',
     &                                            'SCC code: ',asctmp
            call chkwrn(jerr,IDXWPP)
            if( jerr .NE. ISUCES ) goto 9999
            goto 111
         endif
         if( .NOT. lascat(idxeqp) ) goto 111
c
c   --- load the data into sort key array ---
c
         ntotrc = ntotrc + 1
         nrecs = nrecs + 1
         if( nrecs .GT. MXPREC ) goto 7005
         write(hptmp,'(I5)') nint(hpavg)
         srtkey(nrecs) = asctmp//hptmp//fiptmp//subtmp//yrtmp
c
c   --- write the data record to direct access file ----
c
         write(IOSDIR,'(A)',ERR=7006,REC=nrecs) line
c
c   --- get the next record ---
c
         goto 111
c
c   --- read the next file ---
c
  222    continue
         close(IORPOP)
c
c   --- read all of the records for this file, call routine to sort
c       the keys ---
c
   10 continue
c
c   --- read all of the records for this file, call routine to sort
c       the keys ---
c
         if( nrecs .GT. 0 ) then
              call chrsrt( srtkey, 30, nrecs, idxrec )
              call spinit()
         else
              ierr = IEOF
              goto 9999
         endif
c
c   --- loop over the records and write each record to 
c       the sorted scratch file ----
c
         do 30 i=1,nrecs
            read(IOSDIR,'(A)',ERR=7007,REC=idxrec(i)) line
            call spinit()
            write(IOSPOP,'(A)',ERR=7008) line
   30    continue
c
c   --- close the direct access file, and delete it ---
c
         close(IOSDIR,status='DELETE')
c
c   --- processed all of the files, write the end keyword ---
c
      line = ' '
      line(18:27) = KEYEND
      write(IOSPOP,'(A)',ERR=7008) line
      rewind(IOSPOP)
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,2A,/,9X,2A)',ERR=9999) 'ERROR:  Cannot find ',
     &         keywrd(:strmin(keywrd)),'packet of population file ',
     &                              popfl(ifile)(:strmin(popfl(ifile)))
      write(IOWMSG,'(/,1X,2A,/,9X,2A)',ERR=9999) 'ERROR:  Cannot find ',
     &         keywrd(:strmin(keywrd)),'packet of population file ',
     &                              popfl(ifile)(:strmin(popfl(ifile)))
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Opening population ',
     &                      'file ',popfl(ifile)(:strmin(popfl(ifile)))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Opening population ',
     &                      'file ',popfl(ifile)(:strmin(popfl(ifile)))
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,4A,I10)',ERR=9999) 'ERROR:  Reading ',
     &                     'population file ',fname(:strmin(fname)),
     &                                               ' at record ',irec
      write(IOWMSG,'(/,1X,4A,I10)',ERR=9999) 'ERROR:  Reading ',
     &                     'population file ',fname(:strmin(fname)),
     &                                               ' at record ',irec
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot open ',
     &                       'direct-access file used for sorting ',
     &                                           scrnam(:strmin(scrnam))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Cannot open ',
     &                       'direct-access file used for sorting ',
     &                                           scrnam(:strmin(scrnam))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot open scratch ',
     &       'file used for sorted populations ',spopfl(:strmin(spopfl))
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot open scratch ',
     &       'file used for sorted populations ',spopfl(:strmin(spopfl))
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,2A,I10)',ERR=9999) 'ERROR:  Number of ',
     &                        'population records excceds max ',MXPREC
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'Reduce the number of ',
     &                         'population files or population records.'
      write(IOWMSG,'(/,1X,2A,I10)',ERR=9999) 'ERROR:  Number of ',
     &                        'population records excceds max ',MXPREC
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'Reduce the number of ',
     &                         'population files or population records.'
      goto 9999
c 
 7006 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Writing ',
     &                  'direct-access file used for sorting ',
     &                                           scrnam(:strmin(scrnam))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Writing ',
     &                  'direct-access file used for sorting ',
     &                                           scrnam(:strmin(scrnam))
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &                  'direct-access file used for sorting ',
     &                                           scrnam(:strmin(scrnam))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &                  'direct-access file used for sorting ',
     &                                           scrnam(:strmin(scrnam))
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Writing scratch ',
     &         'file used for sorted populations ',
     &                                           spopfl(:strmin(spopfl))
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Writing scratch ',
     &         'file used for sorted populations ',
     &                                           spopfl(:strmin(spopfl))
      goto 9999
c
 7009 continue
      write(IOWSTD,'(/,1X,A,/9X,3A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached','reading ',
     &           'population file ',popfl(ifile)(:strmin(popfl(ifile)))
      write(IOWMSG,'(/,1X,A,/9X,3A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &           'population file ',popfl(ifile)(:strmin(popfl(ifile)))
      goto 9999
c
 7010 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR:  HP range in ',
     &         'population file does not match internal default ranges.' 
      write(IOWSTD,'(10X,3A)',ERR=9999) '  County ','    SCC      ',
     &                                              '       HP Range  '
      write(IOWSTD,'(12X,A5,3X,A10,5X,I6,I6)') fiptmp, asctmp, 
     &                                           INT(hpmin), INT(hpmax)
      write(IOWSTD,'(/,12X,A)') 'Valid ranges are: '
      write(IOWSTD,'(16X,I4,A,I4)') 0, ' -- ',INT(hpclev(1))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR:  HP range in ',
     &         'population file does not match internal default ranges.' 
      write(IOWMSG,'(10X,3A)',ERR=9999) '  County ','    SCC      ',
     &                                              '       HP Range  '
      write(IOWMSG,'(12X,A5,3X,A10,5X,I6,I6)') fiptmp, asctmp, 
     &                                           INT(hpmin), INT(hpmax)
      write(IOWMSG,'(/,12X,A)') 'Valid ranges are: '
      write(IOWMSG,'(16X,I4,A,I4)') 0, ' -- ',INT(hpclev(1))
      do 50 i=1,MXHPC-1
         write(IOWSTD,'(16X,I4,A,I4)') INT(hpclev(i)), ' -- ',
     &                                                  INT(hpclev(i+1))
         write(IOWMSG,'(16X,I4,A,I4)') INT(hpclev(i)), ' -- ',
     &                                                  INT(hpclev(i+1))
   50 continue
      write(IOWSTD,'(16X,A,I4)') '      > ',INT(hpclev(MXHPC))
      write(IOWMSG,'(16X,A,I4)') '      > ',INT(hpclev(MXHPC))
      goto 9999
c
 7011 continue
      write(IOWSTD,'(/,1X,4A,I10)',ERR=9999) 'ERROR:  Reading ',
     &          'HP range from population file ',fname(:strmin(fname)),
     &                                               ' at record ',irec
      write(IOWSTD,'(10X,A,/,A)',ERR=9999) 'Line read: ',
     &                                              line(:strmin(line))
      write(IOWMSG,'(/,1X,4A,I10)',ERR=9999) 'ERROR:  Reading ',
     &          'HP range from population file ',fname(:strmin(fname)),
     &                                               ' at record ',irec
      write(IOWMSG,'(10X,A,/,A)',ERR=9999) 'Line read: ',
     &                                              line(:strmin(line))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8001 format(A)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
