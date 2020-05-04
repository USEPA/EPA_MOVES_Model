C**** OPNNON 
c
      subroutine opnnon( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the filenames to be used in the NONROAD program and opens 
c    some files
c
c    Argument description.
c     Outputs:
c       ierr    I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      12/10/95  --djk--  original development
c      07/19/96  --jlf--  removed mention of aircraft, vessels, and
c                         locomotives files
c      07/22/96  --jlf--  changed so population file opened here
c      05/10/04  --dfk--  added evap tech fractions
c      06/04/04  --dfk--  added exhaust and evap bmy output
c      01/21/05  --dfk--  added trap for non-existent Daily temp/RVP file
c      04/22/05  --cimulus-- added support for reading FIPS.DAT with
c                            the US COUNTIES FIPS option
c      05/16/05  --cimulus-- added support for reading retrofit file
c                            with the RETROFIT option
c      07/13/05  --epa--  added msg file & error output of evtchfl
c      03/25/08  --epa--  write Daily file info to msg file even if none
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
c   strlen  I   returns the actual length of a string
c   strmin  I   returns the actual length of a string (minimum of 1)
c
      integer*4 strlen
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   fname   C   character string for temporary storage of filenames
c
      character*(MXSTR) fname, line
      character*40      cdate
      character*20      keywrd, keyfil, keyin, keytmp
      integer*4         jerr, i
      logical*4         lcheck, lexist, lmsg
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c  --- flag to indicate message file not yet open ---
c
      lmsg = .FALSE.
c
c  ---- check for existence of the options file and open it ----
c
      fname = sysfl
      open(IORUSR,file=sysfl,ERR=7003,status='UNKNOWN')
c
c  --- now set the date and time for this run - you'll need 
c      different routines when running on unix ----
c
      call getime ( cdate )
c
c   --- search for the packet header ----
c
      lamsfl = .FALSE.
      keywrd = '/RUNFILES/'
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .NE. ISUCES ) goto 7006
c
  111 continue
      read(IORUSR,8000,ERR=7000,END=7008) line
      keyfil = line(1:19)
      call lftjst( keyfil )
      call low2up( keyfil )
c
c   --- check for /END/ keyword ----
c
      if( keyfil .EQ. KEYEND ) goto 222 
c
c   --- get the filename ---
c
      fname = line(21:)
      call lftjst( fname )
c
c    ---- check if file type is a allocation file -----
c
      if( keyfil .EQ. 'ALLOC XREF' ) then
         if( strlen( fname ) .GT. 0 ) then
            alofl = fname
            lalofl = .TRUE.
         endif
c
c    ---- check if file type is an activity file -----
c
      else if( keyfil .EQ. 'ACTIVITY' ) then
         if( strlen( fname ) .GT. 0 ) then
            actfl = fname
            lactfl = .TRUE.
         endif
c
c    ---- check if file type is an exhaust technology fractions file -----
c
      else if( keyfil .EQ. 'EXH TECHNOLOGY' ) then
         if( strlen( fname ) .GT. 0 ) then
            tchfl = fname
            ltchfl = .TRUE.
         endif
c
c    ---- check if file type is a technology fractions file -----
c
      else if( keyfil .EQ. 'EVP TECHNOLOGY' ) then
         if( strlen( fname ) .GT. 0 ) then
            evtchfl = fname
            levtchfl = .TRUE.
         endif
c
c   --- check if file type is a seasonality file ---
c
      else if( keyfil .EQ. 'SEASONALITY' ) then
         if( strlen( fname ) .GT. 0 ) then
            sesfl = fname
            lsesfl = .TRUE.
         endif
c
c   --- check if file type is a message file ----
c
      else if( keyfil .EQ. 'MESSAGE' ) then
         if( strlen( fname ) .GT. 0 ) then
            msgfl = fname
            lmsgfl = .TRUE.
            lmsg = .TRUE.
            fname = msgfl
            open(IOWMSG,file=msgfl,ERR=7003,status='UNKNOWN')
            rewind(IOWMSG)
         endif
c
c   --- check if file type is a regions file ----
c
      else if( keyfil .EQ. 'REGIONS' ) then
         if( strlen( fname ) .GT. 0 ) then
            regfl = fname
            lregfl = .TRUE.
         endif
c
c    ---- check if file type is an output file -----
c
      else if( keyfil .EQ. 'OUTPUT DATA' ) then
         if( strlen( fname ) .GT. 0 ) then
            datfl = fname
            ldatfl = .TRUE.
            fname = datfl
            open(IOWDAT,file=datfl,ERR=7003,status='UNKNOWN')
            rewind(IOWDAT)
         endif
c
c   --- check if file type is a AMS workfile ----
c
      else if( keyfil .EQ. 'EPS2 AMS' ) then
         if( strlen( fname ) .GT. 0 ) then
            amsfl = fname
            lamsfl = .TRUE.
         endif
c
c   --- check if file type is a US Counties FIPS file ----
c
      else if( keyfil .EQ. 'US COUNTIES FIPS' ) then
         if( strlen( fname ) .GT. 0 ) then
            fipsfl = fname
            lfipsfl = .TRUE.
         endif
c
c   --- check if file type is a retrofit file ----
c
      else if( keyfil .EQ. 'RETROFIT' ) then
         if( strlen( fname ) .GT. 0 ) then
            rtrftfl = fname
            lrtrftfl = .TRUE.
         endif
c
c   ---- unrecognized keyword, skip it ---
c
      else
         goto 7007
      endif
c
c  --- get the next record ---
c
      goto 111 
c
c  ---- read the entire file, check for output files ---
c
 222  continue
c
c  --- check to make sure all necessary files were provided, if so
c      open them ----
c
c  --- MESSAGE file, check already opened  ----
c
      if( .NOT. lmsgfl ) then
          keyin = ' MESSAGE'
          goto 7002
       endif
c
c  --- SPATIAL ALLOCATION cross-reference file ---
c      this file will be opened later to save file handles ---
c
      if( .NOT. lalofl ) then
          keyin = ' ALLOC XREF'
          goto 7002
      else
          fname = alofl
          inquire(file=alofl,exist=lcheck)
          if( .NOT. lcheck ) goto 7001
      endif
c
c  --- ACTIVITY file ---
c      this file will be opened later to save file handles ---
c
      if( .NOT. lactfl ) then
          keyin = '  ACTIVITY'
          goto 7002
      else
          fname = actfl
          inquire(file=actfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7001
      endif
c
c  --- EXHAUST TECHNOLOGY FRACTIONS file ---
c
      if( .NOT. ltchfl ) then
          keyin = ' EXH TECH'
          goto 7002
      else
          fname = tchfl
          inquire(file=tchfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7001
      endif
c
c  --- EVAP TECHNOLOGY FRACTIONS file ---
c
      if( .NOT. levtchfl ) then
          keyin = ' EVAP TECH'
          goto 7002
      else
          fname = tchfl
          inquire(file=tchfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7001
      endif
c
c  --- SEASONALITY file ----
c      this file will be opened later to save file handles ---
c
      if( .NOT. lsesfl ) then
          keyin = ' SEASONALITY'
          goto 7002
      else
          fname = sesfl
          inquire(file=sesfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7001
      endif
c
c  --- REGIONS file ----
c
      if( .NOT. lregfl ) then
          keyin = ' REGIONS'
          goto 7002
       else
          fname = regfl
          inquire(file=regfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7001
       endif
c
c  --- OUTPUT DATA file ----
c
      if( .NOT. ldatfl ) then
          keyin = ' OUTPUT DATA'
          goto 7002
      endif
c
c  --- AMS file - optional ----
c
      if( lamsfl ) then
          fname = amsfl
          open(IOWAMS,file=amsfl,ERR=7003,status='UNKNOWN')
          rewind(IOWAMS)
      endif
c
c  --- RETROFIT file - optional ----
c
      if( lrtrftfl ) then
          fname = rtrftfl
          inquire(file=rtrftfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7001
      endif
c
c   --- look for the /POP FILES/ packet ----
c
      keywrd = '/POP FILES/' 
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .NE. ISUCES ) goto 7006
c
c   --- read a filename and open the file ---
c
  333 continue
      read(IORUSR,8001,ERR=7000,END=7008) keytmp, fname
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 444
      if( strlen(fname) .LE. 0 ) goto 333
      call lftjst( fname )
      inquire(file=fname,exist=lexist)
      if( .NOT. lexist ) goto 7001
      npopfl = npopfl + 1
      if( npopfl .GT. MXPFIL ) goto 7009
      popfl(npopfl) = fname
      goto 333
c
c   --- look for the /MODELYEAR OUT/ packet (optional) ---
c
  444 continue
      lbmyfl = .FALSE.
      levbmyfl = .FALSE.
      keywrd = '/MODELYEAR OUT/' 
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .EQ. IRDERR ) goto 7010
      if( jerr .EQ. IEOF ) goto 555
c
c   --- read line ---
c
  777 continue
      read(IORUSR,8000,ERR=7000,END=7008) line
      keyfil = line(1:19)
      call lftjst( keyfil )
      call low2up( keyfil )
c
c   --- check for /END/ keyword ----
c
      if( keyfil .EQ. KEYEND ) goto 555 
c
c   --- get the filename ---
c
      fname = line(21:)
      call lftjst( fname )
c
c    ---- check if file type is an exhaust file -----
c
      if( keyfil .EQ. 'EXHAUST BMY OUT' ) then
         if( strlen( fname ) .GT. 0 ) then
            bmyfl = fname
            lbmyfl = .TRUE.
            open(IOWBMY,file=bmyfl,ERR=7003,status='UNKNOWN')
            rewind(IOWBMY)
            call hdrbmy(jerr,1)
            if( jerr .NE. ISUCES ) goto 9999 
         endif
c
c    ---- check if file type is an evap file -----
c
      else if( keyfil .EQ. 'EVAP BMY OUT' ) then
         if( strlen( fname ) .GT. 0 ) then
            evbmyfl = fname
            levbmyfl = .TRUE.
            open(IOWEVBMY,file=evbmyfl,ERR=7003,status='UNKNOWN')
            rewind(IOWEVBMY)
            call hdrbmy(jerr,2)
            if( jerr .NE. ISUCES ) goto 9999 
         endif
c
c   ---- unrecognized keyword, skip it ---
c
      else
         goto 7007
      endif
c
c  --- get the next record ---
c
      goto 777 
c
c   --- look for the /SI REPORT/ packet (optional) ---
c
  555 continue
      lsifl = .FALSE.
      keywrd = '/SI REPORT/' 
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .EQ. IRDERR ) goto 7010
      if( jerr .EQ. IEOF ) goto 666
c
c   --- read the filename and open the file ---
c
      read(IORUSR,8001,ERR=7000,END=7008) keytmp, fname
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 666
      if( strlen(fname) .LE. 0 ) goto 666
      call lftjst( fname )
      lsifl = .TRUE.
      sifl = fname
      open(IOWSI,file=sifl,ERR=7003,status='UNKNOWN')
      rewind(IOWSI)
c
c   --- look for the /DAILY FILES/ packet (optional) ---
c
  666 continue
      ldayfl = .FALSE.
      keywrd = '/DAILY FILES/' 
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .EQ. IRDERR ) goto 7010
      if( jerr .EQ. IEOF ) goto 888
c
c   --- read the filename and open the file ---
c
      read(IORUSR,8001,ERR=7000,END=7008) keytmp, fname
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 888
      if( strlen(fname) .LE. 0 ) goto 888
      call lftjst( fname )
      dayfl = fname
      inquire(file=dayfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
      ldayfl = .TRUE.
      open(IORDAY,file=dayfl,ERR=7003,status='UNKNOWN')
      rewind(IORDAY)
c
c   --- echo filenames ---
c
  888 continue
      write(IOWMSG,9001,ERR=7004) PROGNM,', ',VERSON,'    ',cdate
      write(IOWMSG,9001,ERR=7004)
      write(IOWMSG,9003,ERR=7004) '*** Output Files ***'
      write(IOWMSG,9001,ERR=7004)
      write(IOWMSG,9002,ERR=7004) 
     &                    'Output data file',datfl(:strmin(datfl))
      if( lamsfl ) then
          write(IOWMSG,9002,ERR=7004) 
     &                    'EPS2 AMS file',amsfl(:strmin(amsfl))
      endif
      if( lbmyfl ) then
          write(IOWMSG,9002,ERR=7004) 
     &                'Exhaust By-Model-Year file',bmyfl(:strmin(bmyfl))
      endif
      if( levbmyfl ) then
          write(IOWMSG,9002,ERR=7004) 
     &               'Evap By-Model-Year file',evbmyfl(:strmin(evbmyfl))
      endif
      if( lsifl ) then
          write(IOWMSG,9002,ERR=7004) 
     &                    'SI file',sifl(:strmin(sifl))
      endif
      write(IOWMSG,9001,ERR=7004)
      write(IOWMSG,9003,ERR=7004) '*** Input Files ***'
      write(IOWMSG,9001,ERR=7004)
      write(IOWMSG,9002,ERR=7004) 'Options file',sysfl(:strmin(sysfl))
      write(IOWMSG,9002,ERR=7004) 
     &                      'Allocation XREF file',alofl(:strmin(alofl))
      write(IOWMSG,9002,ERR=7004) 'Activity file',actfl(:strmin(actfl))
      write(IOWMSG,9002,ERR=7004) 
     &                    'State/Regions file',regfl(:strmin(regfl))
      write(IOWMSG,9002,ERR=7004) 
     &                      'Seasonality file',sesfl(:strmin(sesfl))
      write(IOWMSG,9002,ERR=7004) 
     &                   'Exh Tech fractions',tchfl(:strmin(tchfl))
      write(IOWMSG,9002,ERR=7004) 
     &                'Evap Tech fractions',evtchfl(:strmin(evtchfl))
      write(IOWMSG,9002,ERR=7004)
     &                   'US Counties FIPS',fipsfl(:strmin(fipsfl))
      if( lrtrftfl ) then
        write(IOWMSG,9002,ERR=7004)
     &                   'Retrofit file',rtrftfl(:strmin(rtrftfl))
      else
        write(IOWMSG,9002,ERR=7004)
     &                   'Retrofit file: (not used)'
      endif

      write(IOWMSG,9001,ERR=7004)
      write(IOWMSG,9003,ERR=7004) '*** Population Files ***'
      write(IOWMSG,9001,ERR=7004)
      do 10 i=1,npopfl
           write(IOWMSG,9002,ERR=7004) ' ',popfl(i)(:strmin(popfl(i)))
   10 continue
c      if( ldayfl ) then
          write(IOWMSG,9001,ERR=7004)
          write(IOWMSG,9003,ERR=7004) '*** Daily Files ***'
          write(IOWMSG,9001,ERR=7004)
          if( ldayfl ) then
            write(IOWMSG,9002,ERR=7004) 
     &                    'Daily Temp/RVP file',dayfl(:strmin(dayfl))
          else
            write(IOWMSG,9002,ERR=7004) 
     &                    'Daily Temp/RVP file: (not used)'
          end if
c      endif
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR: reading ',
     &               keywrd(:strmin(keywrd)),' packet of options file.'
      if ( lmsg )
     &   write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR: reading ',
     &               keywrd(:strmin(keywrd)),' packet of options file.'
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 
     &            'ERROR: Input file not found: ',fname(:strmin(fname))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 
     &            'ERROR: Input file not found: ',fname(:strmin(fname))
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,4A,/,9X,A)',ERR=9999) 'ERROR:  The',
     &       keyin(:strmin(keyin)),' file must be provided in the ',
     &         keywrd(:strmin( keywrd )),'packet of the options file.'
      if ( lmsg )
     &   write(IOWMSG,'(/,1X,4A,/,9X,A)',ERR=9999) 'ERROR:  The',
     &       keyin(:strmin(keyin)),' file must be provided in the ',
     &         keywrd(:strmin( keywrd )),'packet of the options file.'
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fname(:strmin(fname))
      if ( lmsg )  
     &   write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fname(:strmin(fname))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 
     &          'ERROR: Writing message file: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 
     &        'ERROR:  This program requires the ',
     &          keywrd(:strmin( keywrd )),' packet of the options file.'
      if ( lmsg )
     &   write(IOWMSG,'(/,1X,3A)',ERR=9999) 
     &        'ERROR:  This program requires the ',
     &          keywrd(:strmin( keywrd )),' packet of the options file.'
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,3A,/9X,3A)',ERR=9999) 
     &    'ERROR:  Invalid file identifier in ',keywrd(:strmin(keywrd)),
     &              ' packet','of options file: -->',line(1:19),'<--'
      if ( lmsg )
     &   write(IOWMSG,'(/,1X,3A,/9X,3A)',ERR=9999) 
     &    'ERROR:  Invalid file identifier in ',keywrd(:strmin(keywrd)),
     &              ' packet','of options file: -->',line(1:19),'<--'
      goto 9999 
c
 7008 continue
      write(IOWSTD,'(/,1X,3A,/9X,A)',ERR=9999) 
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &          keywrd(:strmin(keywrd)),' packet','of the options file.'
      if ( lmsg )
     &    write(IOWMSG,'(/,1X,3A,/9X,A)',ERR=9999) 
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &          keywrd(:strmin(keywrd)),' packet','of the options file.'
      goto 9999
c
 7009 continue
      write(IOWSTD,'(/,1X,2A,I10)',ERR=9999) 'ERROR:  Number of ',
     &             'population files supplied exceeds maximum: ',MXPFIL 
      write(IOWMSG,'(/,1X,2A,I10)',ERR=9999) 'ERROR:  Number of ',
     &             'population files supplied exceeds maximum: ',MXPFIL 
      goto 9999
c
 7010 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Reading the ',
     &                       'options file while searching for the ',
     &                               keywrd(:strmin(keywrd)),' packet.'
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Reading the ',
     &                       'options file while searching for the ',
     &                               keywrd(:strmin(keywrd)),' packet.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(A)
 8001 format(A20,A)
 9001 format(1X,5A)
 9002 format(T10,A,T30,:,':',A)
 9003 format(T20,A)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

