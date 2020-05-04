C**** RDNRPER
c
      subroutine rdnrper( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the /PERIOD/ packet used in the NONROAD program 
c
c    Argument description.
c     Outputs:
c       ierr    I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c 
c      03/29/97  --gwilson-- original development
c      03/15/05  --cimulus-- added growth year
c      03/15/05  --cimulus-- added technology year
c      03/16/05  --cimulus-- added error if technology year
c                            greater than episode year
c      05/16/05  -cimulus- integrated year-range validation parameters:
c                          MINYEAR and MAXYEAR
c      05/27/05  -cimulus- increment warning count for growth and tech
c                          year warnings
c      05/27/05  -cimulus- removed unnecessary warning message from
c                          tech year error section
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
c   strmin  I   returns the actual length of a string (minimum of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*80 line
      character*20 keywrd, keyin
      integer*4    irec, jerr, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- find the PERIOD packet ----
c
      keywrd = '/PERIOD/'
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .NE. ISUCES ) goto 7001
c
c  --- get the period type ---
c
      irec = 1
      read(IORUSR,8000,ERR=7000,END=7002) line
      call spinit()
      keyin = line(21:30)
      call lftjst( keyin )
      call low2up( keyin )
c
      iprtyp = 0
      if( keyin(:10) .EQ. PERANN ) iprtyp = IDXANN
      if( keyin(:10) .EQ. PERMTH ) iprtyp = IDXMTH
      if( keyin(:10) .EQ. PERSES ) iprtyp = IDXSES
      if( iprtyp .EQ. 0 ) goto 7003
c
c  --- get type of sum to calculate ---
c
      irec = irec + 1
      read(IORUSR,8000,ERR=7000,END=7002) line
      call spinit()
      keyin = line(21:35)
      call lftjst( keyin )
      call low2up( keyin )
c
      ismtyp = 0
      if( keyin(:15) .EQ. SUMTYP ) ismtyp = IDXTYP
      if( keyin(:15) .EQ. SUMTOT ) ismtyp = IDXTOT
      if( ismtyp .EQ. 0 ) goto 7008
      if( ismtyp .EQ. IDXTOT ) then
         ldays(IDXWKD) = .TRUE.
         ldays(IDXWKE) = .TRUE.
      endif
c
c  --- if doing annual inventory, set all month and day flags to on ---
c
      if( iprtyp .EQ. IDXANN ) then
         do 10 i=1,12
            lmonth(i) = .TRUE.
   10    continue
      endif
c
c  --- always need to read the episode year ----
c
      irec = irec + 1
      read(IORUSR,8000,ERR=7000,END=7002) line
      call spinit()
      read(line(21:30),8001,ERR=7004,END=7002) iepyr
      call spinit()
c
c  --- adjust the year to be 4 digits, assume less than 50
c      means 21st century  ---
c
      if( iepyr .LT. 100 ) then
          if( iepyr .GT. 50 ) then
               iepyr = iepyr + 1900
          else
               iepyr = iepyr + 2000
          endif
       endif
       if( iepyr .LT. MINYEAR .OR. iepyr .GT. MAXYEAR ) goto 7009
c
c  --- if doing a seasonal, read the season specification ----
c
      irec = irec + 1
      read(IORUSR,8000,ERR=7000,END=7002) line
      call spinit()
      if( iprtyp .EQ. IDXSES ) then
         keyin = line(21:30)
         call lftjst( keyin )
         call low2up( keyin )
c
         iseasn = 0
         if( keyin .EQ. SESWTR ) iseasn = IDXWTR
         if( keyin .EQ. SESSPR ) iseasn = IDXSPR
         if( keyin .EQ. SESSUM ) iseasn = IDXSUM
         if( keyin .EQ. SESFAL ) iseasn = IDXFAL
         if( iseasn .EQ. 0 ) goto 7005
c
c  --- set the say of year flags and day of week flags for this season ---
c
         do 30 i=1,12
            if( idseas(i) .EQ. iseasn ) lmonth(i) = .TRUE.
   30    continue
      endif
c
c  --- if doing a monthly or daily, read the month specification ----
c
      irec = irec + 1
      read(IORUSR,8000,ERR=7000,END=7002) line
      call spinit()
      if( iprtyp .EQ. IDXMTH .OR. lrfg ) then
         keyin = line(21:30)
         call lftjst( keyin )
         call low2up( keyin )
c
         imonth = 0
         if( keyin .EQ. MONJAN ) imonth = IDXJAN
         if( keyin .EQ. MONFEB ) imonth = IDXFEB
         if( keyin .EQ. MONMAR ) imonth = IDXMAR
         if( keyin .EQ. MONAPR ) imonth = IDXAPR
         if( keyin .EQ. MONMAY ) imonth = IDXMAY
         if( keyin .EQ. MONJUN ) imonth = IDXJUN
         if( keyin .EQ. MONJUL ) imonth = IDXJUL
         if( keyin .EQ. MONAUG ) imonth = IDXAUG
         if( keyin .EQ. MONSEP ) imonth = IDXSEP
         if( keyin .EQ. MONOCT ) imonth = IDXOCT
         if( keyin .EQ. MONNOV ) imonth = IDXNOV
         if( keyin .EQ. MONDEC ) imonth = IDXDEC
         if( imonth .EQ. 0 ) goto 7006
c
c  --- set the flag for this month to on ---
c
         lmonth(imonth) = .TRUE.
         if( ismtyp .EQ. IDXTOT ) then
            ldays(IDXWKD) = .TRUE.
            ldays(IDXWKE) = .TRUE.
         endif
      endif
c
c  --- if doing a monthly or daily, read the month specification ----
c
      irec = irec + 1
      read(IORUSR,8000,ERR=7000,END=7002) line
      call spinit()
      if( ismtyp .EQ. IDXTYP ) then
         keyin = line(21:30)
         call lftjst( keyin )
         call low2up( keyin )
c
         iday = 0
         if( keyin .EQ. WEEKDY ) iday = IDXWKD
         if( keyin .EQ. WEEKDS ) iday = IDXWKD
         if( keyin .EQ. WEEKND ) iday = IDXWKE
         if( iday .EQ. 0 ) goto 7007
c
c  --- set the flag for this month to on ---
c
         ldays(iday) = .TRUE.
      endif
c
c  --- always need to read the growth year (although value may
c      be blank, in which case it defaults to episode year) ----
c
      irec = irec + 1
      read(IORUSR,8000,ERR=7000,END=7002) line
      call spinit()
      keyin = line(21:30)
      call lftjst( keyin )
      if( keyin(1:1) .EQ. ' ' ) then ! if blank
          igryr = iepyr
      else
          read(keyin(:10),8001,ERR=7004,END=7002) igryr
      endif
      call spinit()
c
c  --- adjust the growth year to be 4 digits, assume less than 50
c      means 21st century  ---
c
      if( igryr .LT. 100 ) then
          if( igryr .GT. 50 ) then
               igryr = igryr + 1900
          else
               igryr = igryr + 2000
          endif
       endif
       if( igryr .LT. MINYEAR .OR. igryr .GT. MAXYEAR ) goto 7010
c
c  --- warn if growth year does not match episode year ---
c
      if( igryr .NE. iepyr ) then
          write(IOWMSG,8002,ERR=9999) 
     &            'WARNING:  Growth year does not match episode year: ',
     &            'Episode Year','Growth Year',iepyr,igryr
          nwarn = nwarn + 1
      endif
c
c  --- always need to read the technology year (although value may
c      be blank, in which case it defaults to episode year) ----
c
      irec = irec + 1
      read(IORUSR,8000,ERR=7000,END=7002) line
      call spinit()
      keyin = line(21:30)
      call lftjst( keyin )
      if( keyin(1:1) .EQ. ' ' ) then ! if blank
          itchyr = iepyr
      else
          read(keyin(:10),8001,ERR=7004,END=7002) itchyr
      endif
      call spinit()
c
c  --- adjust the technology year to be 4 digits, assume less than 50
c      means 21st century  ---
c
      if( itchyr .LT. 100 ) then
          if( itchyr .GT. 50 ) then
               itchyr = itchyr + 1900
          else
               itchyr = itchyr + 2000
          endif
       endif
       if( itchyr .LT. MINYEAR .OR. itchyr .GT. MAXYEAR ) goto 7011
c
c  --- error if technology year greater than episode year ---
c
      if( itchyr .GT. iepyr ) goto 7012
c
c  --- warn if technology year does not match episode year ---
c
      if( itchyr .NE. iepyr ) then
          write(IOWMSG,8002,ERR=9999) 
     &        'WARNING:  Technology year does not match episode year: ',
     &        'Episode Year','Technology Year',iepyr,itchyr
          nwarn = nwarn + 1
      endif
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
      write(IOWSTD,'(/,1X,A,I5,3A)',ERR=9999) 'ERROR: Reading record',
     &                        irec,' of the ',keywrd(:strmin(keywrd)),
     &                                  ' packet of the options file.'
      write(IOWMSG,'(/,1X,A,I5,3A)',ERR=9999) 'ERROR: Reading record',
     &                        irec,' of the ',keywrd(:strmin(keywrd)),
     &                                  ' packet of the options file.'
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999)
     &        'ERROR:  This program requires the ',
     &          keywrd(:strmin( keywrd )),' packet of the options file.'
      write(IOWMSG,'(/,1X,3A)',ERR=9999)
     &        'ERROR:  This program requires the ',
     &          keywrd(:strmin( keywrd )),' packet of the options file.'
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
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response to ',
     &             'period type flag: -->',line(21:30),'<--'
      write(IOWSTD,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWSTD,'(15X,A)',ERR=9999) PERANN
      write(IOWSTD,'(15X,A)',ERR=9999) PERMTH
      write(IOWSTD,'(15X,A)',ERR=9999) PERSES
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response to ',
     &             'period type flag: -->',line(21:30),'<--'
      write(IOWMSG,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWMSG,'(15X,A)',ERR=9999) PERANN
      write(IOWMSG,'(15X,A)',ERR=9999) PERMTH
      write(IOWMSG,'(15X,A)',ERR=9999) PERSES
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,2A,I5,/,9X,3A)',ERR=9999) 'ERROR:  Reading ',
     &            'numeric data field at record ',irec,'of the ',
     &                 keywrd(:strmin(keywrd)),' packet of options file'
      write(IOWMSG,'(/,1X,2A,I5,/,9X,3A)',ERR=9999) 'ERROR:  Reading ',
     &            'numeric data field at record ',irec,'of the ',
     &                 keywrd(:strmin(keywrd)),' packet of options file'
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response to ',
     &                  'season flag: -->',keyin(:strmin(keyin)),'<--'
      write(IOWSTD,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWSTD,'(15X,A)',ERR=9999) SESWTR
      write(IOWSTD,'(15X,A)',ERR=9999) SESSPR
      write(IOWSTD,'(15X,A)',ERR=9999) SESSUM
      write(IOWSTD,'(15X,A)',ERR=9999) SESFAL
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response to ',
     &                  'season flag: -->',keyin(:strmin(keyin)),'<--'
      write(IOWMSG,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWMSG,'(15X,A)',ERR=9999) SESWTR
      write(IOWMSG,'(15X,A)',ERR=9999) SESSPR
      write(IOWMSG,'(15X,A)',ERR=9999) SESSUM
      write(IOWMSG,'(15X,A)',ERR=9999) SESFAL
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response to ',
     &             'month of year flag: -->',keyin(:strmin(keyin)),'<--'
      write(IOWSTD,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWSTD,'(15X,A)',ERR=9999) MONJAN
      write(IOWSTD,'(15X,A)',ERR=9999) MONFEB
      write(IOWSTD,'(15X,A)',ERR=9999) MONMAR
      write(IOWSTD,'(15X,A)',ERR=9999) MONAPR
      write(IOWSTD,'(15X,A)',ERR=9999) MONMAY
      write(IOWSTD,'(15X,A)',ERR=9999) MONJUN
      write(IOWSTD,'(15X,A)',ERR=9999) MONJUL
      write(IOWSTD,'(15X,A)',ERR=9999) MONAUG
      write(IOWSTD,'(15X,A)',ERR=9999) MONSEP
      write(IOWSTD,'(15X,A)',ERR=9999) MONOCT
      write(IOWSTD,'(15X,A)',ERR=9999) MONNOV
      write(IOWSTD,'(15X,A)',ERR=9999) MONDEC
      if( lrfg ) write(IOWSTD,'(/,5X,3A)',ERR=9999) 'Note: The month ',
     &                     'of year is a required input when the RFG ',
     &                                              'flag is specified.'
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response to ',
     &             'month of year flag: -->',keyin(:strmin(keyin)),'<--'
      write(IOWMSG,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWMSG,'(15X,A)',ERR=9999) MONJAN
      write(IOWMSG,'(15X,A)',ERR=9999) MONFEB
      write(IOWMSG,'(15X,A)',ERR=9999) MONMAR
      write(IOWMSG,'(15X,A)',ERR=9999) MONAPR
      write(IOWMSG,'(15X,A)',ERR=9999) MONMAY
      write(IOWMSG,'(15X,A)',ERR=9999) MONJUN
      write(IOWMSG,'(15X,A)',ERR=9999) MONJUL
      write(IOWMSG,'(15X,A)',ERR=9999) MONAUG
      write(IOWMSG,'(15X,A)',ERR=9999) MONSEP
      write(IOWMSG,'(15X,A)',ERR=9999) MONOCT
      write(IOWMSG,'(15X,A)',ERR=9999) MONNOV
      write(IOWMSG,'(15X,A)',ERR=9999) MONDEC
      if( lrfg ) write(IOWMSG,'(/,5X,3A)',ERR=9999) 'Note: The month ',
     &                     'of year is a required input when the RFG ',
     &                                              'flag is specified.'
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response to ',
     &             'day of week flag: -->',keyin(:strmin(keyin)),'<--'
      write(IOWSTD,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWSTD,'(15X,A)',ERR=9999) WEEKDY
      write(IOWSTD,'(15X,A)',ERR=9999) WEEKDS
      write(IOWSTD,'(15X,A)',ERR=9999) WEEKND
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response to ',
     &             'day of week flag: -->',keyin(:strmin(keyin)),'<--'
      write(IOWMSG,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWMSG,'(15X,A)',ERR=9999) WEEKDY
      write(IOWMSG,'(15X,A)',ERR=9999) WEEKDS
      write(IOWMSG,'(15X,A)',ERR=9999) WEEKND
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response to ',
     &             'type of sum flag: -->',keyin(:strmin(keyin)),'<--'
      write(IOWSTD,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWSTD,'(15X,A)',ERR=9999) SUMTYP
      write(IOWSTD,'(15X,A)',ERR=9999) SUMTOT
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response to ',
     &             'type of sum flag: -->',keyin(:strmin(keyin)),'<--'
      write(IOWMSG,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWMSG,'(15X,A)',ERR=9999) SUMTYP
      write(IOWMSG,'(15X,A)',ERR=9999) SUMTOT
      goto 9999
c
 7009 continue
      write(IOWSTD,'(/,1X,A,I6)',ERR=9999) 
     &              'ERROR:  Invalid episode year value ',iepyr
      write(IOWSTD,'(9X,A,I4,A,I4)') 'Valid range is: ',
     &        MINYEAR,' to ',MAXYEAR
      write(IOWMSG,'(/,1X,A,I6)',ERR=9999) 
     &              'ERROR:  Invalid episode year value ',iepyr
      write(IOWMSG,'(9X,A,I4,A,I4)') 'Valid range is: ',
     &        MINYEAR,' to ',MAXYEAR
      goto 9999
c
 7010 continue
      write(IOWSTD,'(/,1X,A,I6)',ERR=9999) 
     &              'ERROR:  Invalid growth year value ',igryr
      write(IOWSTD,'(9X,A,I4,A,I4)') 'Valid range is: ',
     &        MINYEAR,' to ',MAXYEAR
      write(IOWMSG,'(/,1X,A,I6)',ERR=9999) 
     &              'ERROR:  Invalid growth year value ',igryr
      write(IOWMSG,'(9X,A,I4,A,I4)') 'Valid range is: ',
     &        MINYEAR,' to ',MAXYEAR
      goto 9999
c
 7011 continue
      write(IOWSTD,'(/,1X,A,I6)',ERR=9999) 
     &              'ERROR:  Invalid technology year value ',itchyr
      write(IOWSTD,'(9X,A,I4,A,I4)') 'Valid range is: ',
     &        MINYEAR,' to ',MAXYEAR
      write(IOWMSG,'(/,1X,A,I6)',ERR=9999) 
     &              'ERROR:  Invalid technology year value ',itchyr
      write(IOWMSG,'(9X,A,I4,A,I4)') 'Valid range is: ',
     &        MINYEAR,' to ',MAXYEAR
      goto 9999
c
 7012 continue
      write(IOWSTD,8003,ERR=9999) 
     &              'ERROR:  Invalid technology year; cannot be',
     &              ' greater than episode year',
     &              'Episode Year','Technology Year',iepyr,itchyr
      write(IOWMSG,8003,ERR=9999) 
     &              'ERROR:  Invalid technology year; cannot be',
     &              ' greater than episode year',
     &              'Episode Year','Technology Year',iepyr,itchyr
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(A)
 8001 format(I10)
 8002 format(/,1X,A,/,T10,A,T30,A,/,T10,I4,T30,I4)
 8003 format(/,1X,A,A,/,T10,A,T30,A,/,T10,I4,T30,I4)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
