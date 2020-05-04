**** RDNRREG
c
      subroutine rdnrreg( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the /REGION/ packet of the options file for the NONROAD program 
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
c      04/21/97  --gwilson-- original development 
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdusr.inc'
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
c   strmin  I   returns the actual length of a string (minimum of 1)
c   fndchr  I   returns index of string in array of strings
c
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*80 line
      character*20 keywrd, keyin
      character*10 regin
      character*5  fipin
      integer*4    idxfip, jerr, irec, i
      logical*4    lfound
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c --- look for /REGION/ packet ---
c
      keywrd = '/REGION/'
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .NE. ISUCES ) goto 7001
c
c --- read and check the region level flag ---
c
      irec = 1
      read(IORUSR,8000,ERR=7000,END=7002) line
      call spinit()
      reglvl = line(21:30)
      call low2up( reglvl )
      call lftjst( reglvl )
      lfound = .FALSE.
      if( reglvl .EQ. USTOT ) lfound = .TRUE.
      if( reglvl .EQ. NATION ) lfound = .TRUE.
      if( reglvl .EQ. STATE  ) lfound = .TRUE.
      if( reglvl .EQ. COUNTY ) lfound = .TRUE.
      if( reglvl .EQ. SUBCTY ) lfound = .TRUE.
      if( .NOT. lfound ) goto 7003
c
c  --- make sure that proper region is set for EPS2 AMS file ---
c
      if( lamsfl ) then
         if( reglvl .EQ. NATION .OR. reglvl .EQ. STATE .OR.
     &                                         reglvl .EQ. USTOT ) then
             write(IOWMSG,'(/,1X,3A,/)',ERR=9999) 'WARNING:  Cannot ',
     &               'produce EPS2 AMS file with region level ',reglvl
             nwarn = nwarn + 1
             lamsfl = .FALSE.
          endif
      endif
c
c -- check for /END/ keyword ---
c
  111 continue
      if( reglvl .NE. USTOT .AND. reglvl .NE. NATION ) then
         read(IORUSR,8000,ERR=7000,END=7002) line
         call spinit()
         keyin = line(1:19)
         call lftjst( keyin )
         call low2up( keyin )
         if( keyin .EQ. KEYEND ) goto 222
         regin = line(21:30)
         call lftjst( regin )
         nregin = nregin + 1
         if( nregin .GT. MXSUBC ) goto 7007
         reglst(nregin) = regin
         regcty(nregin) = regin(1:5)//'     '
      endif
c
c  --- if the region level is national, then finshed with packet ---
c
      if( reglvl .EQ. USTOT ) goto 222
c
c  -- if the region level is type NATION then this should be 
c     all states ---
c
      if( reglvl .EQ. NATION ) then
         do 10 i=1,NSTATE
            lstacd(i) = .TRUE.
   10    continue
         goto 222
      endif
c
c  --- if code is national, then check for state level ---
c
      fipin = regin(1:5)
      if( fipin .EQ. '00000' ) then
         if( reglvl .NE. USTOT .AND. reglvl .NE. STATE ) goto 7004
         do 20 i=1,NSTATE
            lstacd(i) = .TRUE.
   20    continue
c
c  --- check if code is a state code ---
c
      else
         idxfip = fndchr( fipin, 5, statcd, NSTATE )
         if( idxfip .GT. 0 ) then
            if( reglvl .EQ. STATE ) then
               lstacd(idxfip) = .TRUE.
            else if( reglvl .EQ. COUNTY ) then
               lstacd(idxfip) = .TRUE.
               do 30 i=idxcty(idxfip),idxcty(idxfip+1)-1 
                 lfipcd(i) = .TRUE.
   30          continue             
            else
               goto 7004
            endif
c
c  --- check if code is a county code ---
c
         else
            if( reglvl .NE. COUNTY .AND. reglvl .NE. SUBCTY ) goto 7008
            idxfip = fndchr( fipin, 5, fipcod, NCNTY )
            if( idxfip .LE. 0 ) goto 7006
            lfipcd(idxfip) = .TRUE.
            idxfip = fndchr( fipin(1:2)//'000', 5, statcd, NSTATE )
            if( idxfip .GT. 0 ) lstacd(idxfip) = .TRUE.
         endif
      endif
      goto 111
c
c  --- set error flag to success ----
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
      write(IOWSTD,'(/,1X,A,/9X,2A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &          keywrd(:strmin(keywrd)),' packet of the options file.'
      write(IOWMSG,'(/,1X,A,/9X,2A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &          keywrd(:strmin(keywrd)),' packet of the options file.'
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,5A)',ERR=9999) 'ERROR:  Invalid response to ',
     &                  'region level flag in /REGION/ packet: -->',
     &                                                 line(21:30),'<--'
      write(IOWSTD,'(9X,A)',ERR=9999) 'Acceptable responses are: '
      write(IOWSTD,'(15X,A)',ERR=9999) USTOT
      write(IOWSTD,'(15X,A)',ERR=9999) NATION
      write(IOWSTD,'(15X,A)',ERR=9999) STATE
      write(IOWSTD,'(15X,A)',ERR=9999) COUNTY
      write(IOWSTD,'(15X,A)',ERR=9999) SUBCTY
      write(IOWMSG,'(/,1X,5A)',ERR=9999) 'ERROR:  Invalid response to ',
     &                  'region level flag in /REGION/ packet: -->',
     &                                                 line(21:30),'<--'
      write(IOWMSG,'(9X,A)',ERR=9999) 'Acceptable responses are: '
      write(IOWMSG,'(15X,A)',ERR=9999) USTOT
      write(IOWMSG,'(15X,A)',ERR=9999) NATION
      write(IOWMSG,'(15X,A)',ERR=9999) STATE
      write(IOWMSG,'(15X,A)',ERR=9999) COUNTY
      write(IOWMSG,'(15X,A)',ERR=9999) SUBCTY
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,3A,/,9X,5A)',ERR=9999) 'ERROR:  In ',
     &         keywrd(:strmin(keywrd)),' packet.','Cannot supply ',
     &                'global FIPS code ',fipin(:strmin(fipin)),
     &                ' with region level ',reglvl(:strmin(reglvl))
      write(IOWMSG,'(/,1X,3A,/,9X,5A)',ERR=9999) 'ERROR:  In ',
     &         keywrd(:strmin(keywrd)),' packet.','Cannot supply ',
     &                'global FIPS code ',fipin(:strmin(fipin)),
     &                ' with region level ',reglvl(:strmin(reglvl))
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,7A)',ERR=9999) 'ERROR:  FIPS code ',
     &     'supplied in ',keywrd(:strmin(keywrd)),' packet is invalid.',
     &                                ' -->',fipin(:strmin(fipin)),'<--'
      write(IOWMSG,'(/,1X,7A)',ERR=9999) 'ERROR:  FIPS code ',
     &     'supplied in ',keywrd(:strmin(keywrd)),' packet is invalid.',
     &                                ' -->',fipin(:strmin(fipin)),'<--'
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,3A,I5)',ERR=9999) 
     &         'ERROR:  Number of regions in ',keywrd(:strmin(keywrd)),
     &                                    ' packet exceeds max ',MXSUBC
      write(IOWMSG,'(/,1X,3A,I5)',ERR=9999) 
     &         'ERROR:  Number of regions in ',keywrd(:strmin(keywrd)),
     &                                    ' packet exceeds max ',MXSUBC
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,1X,7A)',ERR=9999) 'ERROR:  State FIPS code ',
     &     'supplied in ',keywrd(:strmin(keywrd)),' packet is invalid.',
     &                                ' -->',fipin(:strmin(fipin)),'<--'
      write(IOWMSG,'(/,1X,7A)',ERR=9999) 'ERROR:  State FIPS code ',
     &     'supplied in ',keywrd(:strmin(keywrd)),' packet is invalid.',
     &                                ' -->',fipin(:strmin(fipin)),'<--'
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
