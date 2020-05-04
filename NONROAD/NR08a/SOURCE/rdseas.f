C**** RDSEAS
c
      subroutine rdseas( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the seasonality file and stores the data in common
c    to be used by the NONROAD program.
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
c      09/01/93  --gmw--  original development
c      12/06/95  --djk--  modified for epaoffroad
c      07/20/96  --jlf--  added subregion code for hourly and daily
c                         temporal factors
c      03/04/97  --gwilson-- removed the hourly values
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
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
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(3*MXSTR) line
      character*(MXSTR)   keywrd
      character*10        asctmp
      character*5         sbrtmp
      integer*4           irec, jerr, i
      real*4              valtmp(MXHOUR), summth, sumday
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      ntplmn = 0
      ntplcd = 0
c
c   --- open the file ---
c
      open(IORSES,file=sesfl,ERR=7002,status='UNKNOWN')
      rewind(IORSES)
c
c   --- find /MONTHLY/ keyword ----
c
      keywrd = '/MONTHLY/'
      call fndkey( jerr, IORSES, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) then
         write(IOWMSG,'(/,1X,4A)') 'WARNING:  Could not find ',
     &        keywrd(:strmin(keywrd)),' packet of seasonality file ',
     &                                             sesfl(:strmin(sesfl))
         write(IOWMSG,'(11X,3A)') 'Default values will be used for ',
     &                                                  'all equipment.'
         nwarn = nwarn + 1
         goto 222
      endif
c
c   --- read a record as a character string ---
c
  111 continue
      irec = irec + 1
      read(IORSES,8000,ERR=7001,END=222)line
      call spinit()
c
c   --- check for /END/ keyword ---
c
      keywrd = line
      call lftjst( keywrd )
      call low2up( keywrd )
      if( keywrd(:5) .EQ. KEYEND ) goto 222
c
c   --- parse the character string ----
c
      sbrtmp = line(1:5)
      asctmp = line(7:16)
      read(line,'(51X,12F10.0)',ERR=7003) (valtmp(i),i=1,MXMTH)
c
c   --- renormalize the monthly factors to make sure they add to
c       one, write warning if more than 5% error ---

      summth = 0.
      do 50 i=1,MXMTH
         summth = summth + valtmp(i)
   50 continue 
      if( ABS(summth-1.0) .GT. 0.05 ) then
          write(IOWMSG,'(/,1X,2A)',ERR=9999) 'WARNING:  Monthly ',
     &        'seasonality factors do not add to 1.  Renormalizing...'
          write(IOWMSG,'(12X,3(A,3X))') 'Region','Equipment',
     &                                                    'Original Sum'
          write(IOWMSG,'(13X,A,3X,A,4X,F8.4)') sbrtmp, asctmp, summth
          call chkwrn(jerr,IDXWSE)
          if( jerr .NE. ISUCES ) goto 9999
      endif
      do 60 i=1,MXMTH
         if( summth .GT. 0. ) valtmp(i) = valtmp(i) / summth
   60 continue
c
c   --- increment monthly array counter and initialize ---
c
      ntplmn = ntplmn + 1
      if( ntplmn .GT. MXTASC ) goto 7004
      asctpm(ntplmn) = asctmp
      sbrtpm(ntplmn) = sbrtmp
      do 10 i=1,MXMTH
         mthfac(i,ntplmn) = valtmp(i)
   10 continue 
c
c   --- get next record ---
c
      goto 111
c
c   --- find the /DAILY/ packet ----
c
  222 continue
      keywrd = '/DAILY/'
      call fndkey( jerr, IORSES, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) then 
         write(IOWMSG,'(/,1X,4A)') 'WARNING:  Could not find ',
     &        keywrd(:strmin(keywrd)),' packet of seasonality file ',
     &                                             sesfl(:strmin(sesfl))
         write(IOWMSG,'(11X,3A)') 'Default values will be used for ',
     &                                                  'all equipment.'
         nwarn = nwarn + 1
         goto 444
      endif
c
c   --- read a record as a character string --- 
c
  333 continue
      irec = irec + 1
      read(IORSES,8000,ERR=7001,END=444) line
      call spinit()
c
c   --- check for /END/ keyword ---
c
      keywrd = line
      call lftjst( keywrd )
      call low2up( keywrd )
      if( keywrd(:5) .EQ. KEYEND ) goto 444
c
c   --- parse the character string ----
c
      sbrtmp = line(1:5)
      asctmp = line(7:16)
      read(line,'(51X,2F10.0)',ERR=7003) (valtmp(i),i=1,MXDAY)
      ntplcd = ntplcd + 1
      if( ntplcd .GT. MXTASC ) goto 7004
      asctpl(ntplcd) = asctmp
      sbrtpl(ntplcd) = sbrtmp
c
c   --- renormalize the daily factors to make sure they add to
c       one, write warning if more than 5% error ---
c
      sumday = 5.0*valtmp(IDXWKD) + 2.0*valtmp(IDXWKE)
      if( ABS(sumday-1.0) .GT. 0.05 ) then
          write(IOWMSG,'(/,1X,2A)',ERR=9999) 'WARNING:  Daily ',
     &        'seasonality factors do not add to 1.  Renormalizing...'
          write(IOWMSG,'(11X,3(A,3X))') 'Region','Equipment',
     &                          'Original Sum (5*weekday + 2*weekend)'
          write(IOWMSG,'(12X,A,3X,A,4X,F8.4)') sbrtmp, asctmp, sumday
          call chkwrn(jerr,IDXWTC)
          if( jerr .NE. ISUCES ) goto 9999
      endif
      if( sumday .GT. 0. ) then
         valtmp(IDXWKD) = valtmp(IDXWKD) / sumday
         valtmp(IDXWKE) = valtmp(IDXWKE) / sumday
      endif
c
c  --- put into global array ---
c
      do 20 i=1,MXDAY
         dayfac(i,ntplcd) = valtmp(i) 
   20 continue 
c
c   --- get next record ---
c
      goto 333
c
c --- store default values ----
c
  444 continue
      do 30 i=1,MXMTH
        defmth(i) = 1.0/12.0
   30  continue
c
      do 40 i=1,MXDAY
        defday(i) = 1./7.
   40 continue
c
c   --- close the file ---
c
      close(IORSES)
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7001 continue
      write(IOWSTD,'(/,1X,3A,I10)',ERR=9999) 
     &                   'ERROR:  Reading seasonality file ',
     &                        sesfl(:strmin(sesfl)),' at record ',irec
      write(IOWMSG,'(/,1X,3A,I10)',ERR=9999) 
     &                   'ERROR:  Reading seasonality file ',
     &                        sesfl(:strmin(sesfl)),' at record ',irec
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file ',
     &                                           sesfl(:strmin(sesfl))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file ',
     &                                           sesfl(:strmin(sesfl))
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,3A,I10,/,9X,2A)',ERR=9999) 
     &                          'ERROR:  Reading seasonality file ',
     &                        sesfl(:strmin(sesfl)),' at record ',irec,
     &                               'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,3A,I10,/,9X,2A)',ERR=9999) 
     &                          'ERROR:  Reading seasonality file ',
     &                        sesfl(:strmin(sesfl)),' at record ',irec,
     &                               'Line read: ',line(:strmin(line))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,A,I10)',ERR=9999) 
     &     'ERROR:  Number of seasonality records exceeds max ',MXTASC
      write(IOWMSG,'(/,1X,A,I10)',ERR=9999) 
     &     'ERROR:  Number of seasonality records exceeds max ',MXTASC
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
