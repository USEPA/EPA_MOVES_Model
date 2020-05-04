C**** RDNROPT
c
      subroutine rdnropt( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the options /OPTIONS/ packet used in the NONROAD program 
c
c    Argument description.
c     Outputs:
c       ierr    I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c 
c      03/21/97  --gwilson-- origiginal development
c      08/30/01  --charvey-- fixes RVP valid max from 14 to 16. 
c      06/17/04  --charvey-- adds marine diesel sulfur input.
c      07/15/05  --cimulus-- removed commercial marine related handling
c      07/20/05  --cimulus-- removed unused variable
c      09/21/06  --epa-- add ethanol mkt share; vol% inputs for 
c                        permeation effects
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
      character*80 line
      character*20 keywrd
      character*10 respons
      integer*4    i, jerr, irec
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- find the OPTIONS packet ----
c
      keywrd = '/OPTIONS/'
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .NE. ISUCES ) goto 7001
c
c  --- read the title records ----
c
      title1 = ' '
      title2 = ' '
      irec = 1
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      title1 = line(21:)
      call lftjst( title1 ) 
      do 10 i=1,strmin(title1)
        if( title1(i:i) .EQ. '"' ) title1(i:i) = ' '
   10 continue
      irec = irec + 1
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      title2 = line(21:)
      call lftjst( title2 ) 
      do 20 i=1,strmin(title1)
        if( title2(i:i) .EQ. '"' ) title2(i:i) = ' '
   20 continue
c
c  --- get the fuel RVP value ---
c
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      read(line(21:30),8002,ERR=7002) fulrvp
c
c   --- check for valid range ---
c
      if( fulrvp .LT. 6.0 .OR. fulrvp .GT. 16.0 ) goto 7007
      call spinit()
      irec = irec + 1
c
c  --- get the fuel oxygen weight percent ---
c
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      read(line(21:30),8002,ERR=7002) oxypct
      if( oxypct .LT. 0. .OR. oxypct .GT. 5. ) goto 7008
      call spinit()
      irec = irec + 1
c
c  --- get the sulfur content for gasoline engines ---
c
      irec = irec + 1
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      read(line(21:30),8002,ERR=7002) soxgas
      if( soxgas .LT. 0. .OR. soxgas .GT. 0.5 ) goto 7009
      call spinit()
      irec = irec + 1
c
c  --- get sulfur content for land-based diesel engines ---
c
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      read(line(21:30),8002,ERR=7002) soxdsl
      if( soxdsl .LT. 0. .OR. soxdsl .GT. 0.5 ) goto 7010
      call spinit()
      irec = irec + 1
c
c  --- get sulfur content for marine diesel engines ---
c
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      soxdsm = soxdsl
      read(line(1:6),8000,ERR=7000) respons
      call low2up(respons)
      if(respons(1:6) .EQ. 'MARINE') then
        read(line(21:30),8002,ERR=7002) soxdsm
        if( soxdsm .LT. 0. .OR. soxdsm .GT. 0.5 ) goto 7010
        call spinit()
        irec = irec + 1
c
c  --- go ahead to next line; else assume current line is CNG/LPG ---
c
        read(IORUSR,8000,ERR=7000,END=7003) line
        call spinit()
      endif
c
c  --- get the sulfur content for CNG/LPG engines ---
c
      read(line(21:30),8002,ERR=7002) soxcng
      if( soxcng .LT. 0. .OR. soxcng .GT. 0.5 ) goto 7011
      call spinit()
      irec = irec + 1
c
c  --- get the minimum daily temperature ---
c
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      read(line(21:30),8002,ERR=7002) tempmn
      if( tempmn .LT. -40. .OR. tempmn .GT. 120. ) goto 7012
      call spinit()
      irec = irec + 1
c
c  --- get the maximum daily temperature ---
c
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      read(line(21:30),8002,ERR=7002) tempmx
      if( tempmx .LT. -40. .OR. tempmx .GT. 120. ) goto 7013
      call spinit()
      irec = irec + 1
c
c  --- get the representative daily ambient temperature ---
c
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      read(line(21:30),8002,ERR=7002) amtemp
      call spinit()
      irec = irec + 1
c
c  --- check that average is between min and max ---
c
      if( amtemp .LT. tempmn .OR. amtemp .GT. tempmx ) goto 7006
c
c  --- get the reponse to the altitude flag ---
c
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      respons = line(21:30)
      call lftjst( respons ) 
      call low2up( respons )
      if( respons(1:5) .EQ. FLAGHI ) then
          lhigh = .TRUE.
          irejn = 2
      else if( respons(1:5) .EQ. FLAGLO ) then
          irejn = 1
          lhigh = .FALSE.
      else
          goto 7004
      endif
c
c  --- get the reponse to the RFG flag ---
c      NOTE: eliminated by gwilson (09/01/97) ----
c
cgwilson      read(IORUSR,8000,ERR=7000,END=7003) line
cgwilson      call spinit()
cgwilson      respons = line(21:30)
cgwilson      call lftjst( respons ) 
cgwilson      call low2up( respons )
cgwilson      if( respons(1:5) .EQ. FLAGYES ) then
cgwilson          lrfg = .TRUE.
cgwilson      else if( respons(1:5) .EQ. FLAGNO ) then
cgwilson          lrfg = .FALSE.
cgwilson      else
cgwilson          goto 7005
cgwilson      endif
      lrfg = .FALSE.
c
c  --- get Ethanol market share and volume percent ---
c
      read(IORUSR,8000,ERR=7000,END=7003) line
      call spinit()
      ethmkt = 0.0
      ethvpct = 0.0
      read(line(1:4),8000,ERR=7000) respons
      call low2up(respons)
      if(respons(1:4) .EQ. 'ETOH') then
        read(line(21:30),8002,ERR=7002) ethmkt
        if( ethmkt .LT. 0. .OR. ethmkt .GT. 100. ) goto 7014
        call spinit()
        irec = irec + 1
        read(IORUSR,8000,ERR=7000,END=7003) line
        read(line(21:30),8002,ERR=7002) ethvpct
        if( ethvpct .LT. 0. .OR. ethvpct .GT. 100. ) goto 7015
        call spinit()
        if( ABS(ethvpct * 0.35 * 0.01 * ethmkt - oxypct) .GT. 0.1 ) then
           write(IOWSTD,'(/,1X,2A,3F7.2,/)',ERR=9999) 
     &    'WARNING:  Oxygen content mismatch ',
     &     'with Ethanol Blend Mkt Share & Vol %: ',
     &    oxypct, ethmkt, ethvpct
          write(IOWMSG,'(/,1X,2A,3F7.2,/)',ERR=9999) 
     &    'WARNING:  Oxygen content mismatch ',
     &     'with Ethanol Blend Mkt Share & Vol %: ',
     &    oxypct, ethmkt, ethvpct
        endif
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
     &                                      ' packet of options file.'
      write(IOWMSG,'(/,1X,A,I5,3A)',ERR=9999) 'ERROR: Reading record',
     &                        irec,' of the ',keywrd(:strmin(keywrd)),
     &                                      ' packet of options file.'
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
      write(IOWSTD,'(/,1X,2A,I5,/,9X,3A)',ERR=9999) 'ERROR:  Reading ',
     &            'numeric data field at record  ',irec,'of the ',
     &                 keywrd(:strmin(keywrd)),' packet of options file'
      write(IOWSTD,'(9X,2A)',ERR=9999) 
     &                               'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,I5,/,9X,3A)',ERR=9999) 'ERROR:  Reading ',
     &            'numeric data field at record  ',irec,'of the ',
     &                 keywrd(:strmin(keywrd)),' packet of options file'
      write(IOWMSG,'(9X,2A)',ERR=9999) 
     &                               'Line read: ',line(:strmin(line))
      goto 9999 
c
 7003 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &          keywrd(:strmin(keywrd)),' packet of the options file.'
      write(IOWMSG,'(/,1X,4A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &          keywrd(:strmin(keywrd)),' packet of the options file.'
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response ',
     &                    'to the altitude flag -> ',line(21:30),'<-'
      write(IOWSTD,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWSTD,'(15X,A)',ERR=9999) FLAGHI 
      write(IOWSTD,'(15X,A)',ERR=9999) FLAGLO 
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response ',
     &                    'to the altitude flag -> ',line(21:30),'<-'
      write(IOWMSG,'(9X,A)',ERR=9999) 'Valid responses are: '
      write(IOWMSG,'(15X,A)',ERR=9999) FLAGHI 
      write(IOWMSG,'(15X,A)',ERR=9999) FLAGLO 
      goto 9999
c
cgwilson 7005 continue
cgwilson      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response ',
cgwilson     &                         'to the RFG flag -> ',line(21:30),'<-'
cgwilson      write(IOWSTD,'(10X,A)',ERR=9999) 'Valid responses are: '
cgwilson      write(IOWSTD,'(15X,A)',ERR=9999) FLAGYES 
cgwilson      write(IOWSTD,'(15X,A)',ERR=9999) FLAGNO 
cgwilson      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Invalid response ',
cgwilson     &                         'to the RFG flag -> ',line(21:30),'<-'
cgwilson      write(IOWMSG,'(10X,A)',ERR=9999) 'Valid responses are: '
cgwilson      write(IOWMSG,'(15X,A)',ERR=9999) FLAGYES 
cgwilson      write(IOWMSG,'(15X,A)',ERR=9999) FLAGNO 
cgwilson      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR:  Average temperature ',
     &    'must be between minimum and maximum temperatures.'
      write(IOWSTD,'(10X,A,F10.2)') 'Minimum temperature: ',tempmn
      write(IOWSTD,'(10X,A,F10.2)') 'Maximum temperature: ',tempmx
      write(IOWSTD,'(10X,A,F10.2)') 'Average temperature: ',amtemp
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR:  Average temperature ',
     &    'must be between minimum and maximum temperatures.'
      write(IOWMSG,'(10X,A,F10.2)') 'Minimum temperature: ',tempmn
      write(IOWMSG,'(10X,A,F10.2)') 'Maximum temperature: ',tempmx
      write(IOWMSG,'(10X,A,F10.2)') 'Average temperature: ',amtemp
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,A,F6.1)',ERR=9999) 
     &                     'ERROR:  Invalid Fuel RVP value: ',fulrvp
      write(IOWSTD,'(9X,3A)') 'Valid range is: 6 to 16'
      write(IOWMSG,'(/,1X,A,F6.1)',ERR=9999) 
     &                     'ERROR:  Invalid Fuel RVP value: ',fulrvp
      write(IOWMSG,'(9X,3A)') 'Valid range is: 6 to 16'
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,1X,A,F6.1)',ERR=9999) 
     &              'ERROR:  Invalid Oxygen weight % value: ',oxypct
      write(IOWSTD,'(9X,3A)') 'Valid range is: 0 to 5'
      write(IOWMSG,'(/,1X,A,F6.1)',ERR=9999) 
     &              'ERROR:  Invalid Oxygen weight % value: ',oxypct
      write(IOWMSG,'(9X,3A)') 'Valid range is: 0 to 5'
      goto 9999
c
 7009 continue
      write(IOWSTD,'(/,1X,A,F6.1)',ERR=9999) 
     &              'ERROR:  Invalid Gasoline sulfur % value: ',soxgas
      write(IOWSTD,'(9X,3A)') 'Valid range is: 0 to 0.5'
      write(IOWMSG,'(/,1X,A,F6.1)',ERR=9999) 
     &              'ERROR:  Invalid Gasoline sulfur % value: ',soxgas
      write(IOWMSG,'(9X,3A)') 'Valid range is: 0 to 0.5'
      goto 9999
c
 7010 continue
      write(IOWSTD,'(/,1X,A,2F6.1)',ERR=9999) 
     &    'ERROR:  Invalid Diesel sulfur % value: ',
     &    soxdsl,soxdsm
      write(IOWSTD,'(9X,3A)') 'Valid range is: 0 to 0.5'
      write(IOWMSG,'(/,1X,A,2F6.1)',ERR=9999) 
     &    'ERROR:  Invalid Diesel sulfur % value: ',
     &    soxdsl,soxdsm
      write(IOWMSG,'(9X,3A)') 'Valid range is: 0 to 0.5'
      goto 9999
c
 7011 continue
      write(IOWSTD,'(/,1X,A,F6.1)',ERR=9999) 
     &              'ERROR:  Invalid CNG/LPG sulfur % value: ',soxcng
      write(IOWSTD,'(9X,3A)') 'Valid range is: 0 to 0.5'
      write(IOWMSG,'(/,1X,A,F6.1)',ERR=9999) 
     &              'ERROR:  Invalid CNG/LPG sulfur % value: ',soxcng
      write(IOWMSG,'(9X,3A)') 'Valid range is: 0 to 0.5'
      goto 9999
c
 7012 continue
      write(IOWSTD,'(/,1X,A,F6.1)',ERR=9999) 
     &              'ERROR:  Invalid Minimum temperature value: ',tempmn
      write(IOWSTD,'(9X,3A)') 'Valid range is: -40 to 120' 
      write(IOWMSG,'(/,1X,A,F6.1)',ERR=9999) 
     &              'ERROR:  Invalid Minimum temperature value: ',tempmn
      write(IOWMSG,'(9X,3A)') 'Valid range is: -40 to 120' 
      goto 9999
c
 7013 continue
      write(IOWSTD,'(/,1X,A,F6.1)',ERR=9999) 
     &              'ERROR:  Invalid Maximum temperature value: ',tempmx
      write(IOWSTD,'(9X,3A)') 'Valid range is: -40 to 120' 
      write(IOWMSG,'(/,1X,A,F6.1)',ERR=9999) 
     &              'ERROR:  Invalid Maximum temperature value: ',tempmx
      write(IOWMSG,'(9X,3A)') 'Valid range is: -40 to 120' 
      goto 9999
c
 7014 continue
      write(IOWSTD,'(/,1X,A,F6.1)',ERR=9999) 
     &    'ERROR:  Invalid Ethanol Mkt Share % value: ',
     &    ethmkt
      write(IOWSTD,'(9X,3A)') 'Valid range is: 0 to 100'
      write(IOWMSG,'(/,1X,A,F6.1)',ERR=9999) 
     &    'ERROR:  Invalid Ethanol Mkt Share % value: ',
     &    ethmkt
      write(IOWMSG,'(9X,3A)') 'Valid range is: 0 to 100'
      goto 9999
c
 7015 continue
      write(IOWSTD,'(/,1X,A,F6.1)',ERR=9999) 
     &    'ERROR:  Invalid Ethanol Blend Vol % value: ',
     &    ethvpct
      write(IOWSTD,'(9X,3A)') 'Valid range is: 0 to 100'
      write(IOWMSG,'(/,1X,A,F6.1)',ERR=9999) 
     &    'ERROR:  Invalid Ethanol Blend Vol % value: ',
     &    ethvpct
      write(IOWMSG,'(9X,3A)') 'Valid range is: 0 to 100'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(A)
 8002 format(F10.0)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
