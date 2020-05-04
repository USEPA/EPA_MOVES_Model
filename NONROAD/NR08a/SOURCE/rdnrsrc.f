C**** RDNRSRC
c
      subroutine rdnrsrc( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the /SOURCE CATEGORY/ packet used in the NONROAD program 
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
c      07/15/05  --cimulus--  removed 2-digit SCC global matches
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdreg.inc'
      include 'nonrdusr.inc'
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
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*80 line
      character*20 keywrd, keyin
      character*10 ascin
      integer*4    irec, jerr, i 
      logical*4    lmatch
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      lascal = .FALSE.
c
c --- look for /SOURCE CATEGORY/ packet ---
c
      keywrd = '/SOURCE CATEGORY/'
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .EQ. IRDERR ) goto 7000
c
c  --- if packet was not found, flag all source categories ---
c
      if( jerr .EQ. IEOF ) then
         do 10 i=1,NEQNAM
            lascat(i) = .TRUE.
   10     continue
          lascal = .TRUE.
          goto 222
      endif
c
c  --- if packet was found, read the data ---
c
      irec = 0
 111  continue
      irec = irec + 1
      read(IORUSR,8000,ERR=7001,END=7002) line
      lmatch = .FALSE.
c
c -- check for /END/ keyword ---
c
      keyin = line(1:19)
      call lftjst( keyin )
      call low2up( keyin )
      if( keyin .EQ. KEYEND ) goto 222
      ascin = line(21:30)
      call lftjst( ascin )
c
c  --- loop over all equipment codes to find codes that match this one ---
c
      do 20 i=1,NEQNAM
c
c  --- first look for exact match ---
c
          if( ascin .EQ. eqpcod(i) ) then
              lascat(i) = .TRUE.
              lmatch = .TRUE.
              goto 111
          endif
c
c  --- look for various levels of global codes ---
c 
          if( ascin(5:10) .EQ. '000000' ) then
              if( ascin(1:4) .EQ. eqpcod(i)(1:4) ) then
                 lascat(i) = .TRUE.
                 lmatch = .TRUE.
              endif
          else if( ascin(8:10) .EQ. '000' ) then
              if( ascin(1:7) .EQ. eqpcod(i)(1:7) ) then
                 lascat(i) = .TRUE.
                 lmatch = .TRUE.
              endif
          endif
   20 continue
c
c  ---- if no match was found, then exit with error ---
c
      if( .NOT. lmatch ) goto 7003
      goto 111
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
      write(IOWSTD,'(/,1X,A,I4,3A)',ERR=9999) 'ERROR: Reading record ',
     &                         irec,' of the ',keywrd(:strmin(keywrd)),
     &                                   ' packet of the options file.'
      write(IOWMSG,'(/,1X,A,I4,3A)',ERR=9999) 'ERROR: Reading record ',
     &                         irec,' of the ',keywrd(:strmin(keywrd)),
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
      write(IOWSTD,'(/,1X,3A,/,9X,2A)',ERR=9999) 
     &                   'ERROR:  Equipment code found in ',
     &              keywrd(:strmin(keywrd)),' packet',
     &                          'of the options file is invalid: -->',
     &                                       ascin(:strmin(ascin)),'<--'
      write(IOWMSG,'(/,1X,3A,/,9X,2A)',ERR=9999) 
     &                   'ERROR:  Equipment code found in ',
     &              keywrd(:strmin(keywrd)),' packet',
     &                          'of the options file is invalid: -->',
     &                                       ascin(:strmin(ascin)),'<--'
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
