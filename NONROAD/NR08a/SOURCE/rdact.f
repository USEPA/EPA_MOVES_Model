C**** RDACT
c
      subroutine rdact( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the activity file and stores the data in common
c    block to be used by the EPA Nonroad program
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
c      02/10/00 mjimenez  removed tank volume, file format changed
c      10/31/01 charvey  add lftjst to agenam reading
c      12/11/01 charvey  finishes fix for multiple alt activity inputs
c      05/20/04 --dfk--  added evap tech type input
c      04/05/05 --cimulus--  default percent of new activity to 100.0
c                            and allow up to MXUSE values instead of
c                            MXUSE - 1
c      05/18/05 --cimulus--  pass true for new chkasc parameter
c      07/11/05 --cimulus--  reverted back to a single technology type
c      07/13/05 --cimulus--  tech type is no longer used, but the files
c                            will still contain the tech-type column,
c                            so just skip over those characters
c      07/20/05 --cimulus--  commented variable that was only used
c                            in commented code
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdusr.inc'
      include 'nonrdact.inc'
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
c   chkasc  L   returns true if SCC code is needed for current run
c
      integer*4 strmin
      logical*4 chkasc
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(2*MXSTR) line
      character*20        keywrd, keyin
      character*40        eqptmp
      character*10        asctmp, unitmp, acttmp, subtmp, agetmp
c      character*10        tectmp
      character*10        tmpstr(MXAGE)
      integer*4           idigit, iend, irec, i, jerr, idxtmp
      integer*4           ncount, ibeg, j
      real*4              hpcmin, hpcmax, lodtmp
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 1
      nactrc = 0
c
c   --- open the file ----
c
      open(IORACT,file=actfl,ERR=7006,status='UNKNOWN')
      rewind(IORACT)
c
c  ---- find the /ACTIVITY/ keyword ----
c
      keywrd = '/ACTIVITY/'
      call fndkey( jerr, IORACT, keywrd )
      if( jerr .NE. ISUCES ) goto 7004
c
c   --- read a record as a character string --- 
c
  111 continue
      read(IORACT,8000,ERR=7000,END=7005) line
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
c   --- make sure the SCC code is needed ---
c
      asctmp = line(1:10)
      if( .NOT. chkasc( asctmp, .TRUE. ) ) goto 111
c
c   --- parse the character string ----
c
      eqptmp   = line(12:51)
      subtmp   = line(52:56)
c     tech type is no longer used, but the files will still contain
c     the tech-type column, so just skip over those characters
c      tectmp   = line(57:66)
      read(line(67:71),'(F5.0)',ERR=7001) hpcmin
      read(line(72:76),'(F5.0)',ERR=7001) hpcmax
      read(line(77:81),'(F5.0)',ERR=7001) lodtmp
cc tank volume removed from activity       read(line(82:86),'(F5.0)',ERR=7001) tnktmp
      call lftjst(   subtmp )
      call low2up(   subtmp )
c     tech type is no longer used, but the files will still contain
c     the tech-type column, so just skip over those characters
c      call lftjst(   tectmp )
c      call low2up(   tectmp )
c
c  --- check the units string ---
c
      unitmp = line(87:96)
      call lftjst( unitmp )
      call low2up( unitmp )
      if( (unitmp .EQ. KEYHRD .OR. unitmp .EQ. KEYGLD) .AND. 
     &                                     ismtyp .NE. IDXTYP) goto 7002
      if( unitmp .EQ. KEYHRY ) then
         idxtmp = IDXHRY 
      else if( unitmp .EQ. KEYHRD ) then
         idxtmp = IDXHRD 
      else if( unitmp .EQ. KEYGLY ) then
         idxtmp = IDXGLD 
      else if( unitmp .EQ. KEYGLD ) then
         idxtmp = IDXGLD 
      else
         goto 7007
      endif 
c
c   --- get activity value ---
c
      acttmp = '          '
      idigit = 1
      iend = 106
      do 10 i=97,iend
         if(line(i:i) .NE. ',' .AND. line(i:i) .NE. ' ') then
             acttmp(idigit:idigit) = line(i:i)
             idigit = idigit + 1
         endif
   10 continue
      call rgtjst( acttmp )
c
c   --- get activity vs age curve identifier ---
c
      agetmp = line(107:116)
      call lftjst( agetmp )
      call low2up( agetmp )
c
c   --- get starts value ----
c
c      strtmp = ' '
c      idigit = 1
c      iend = 106
c      do 20 i=97,iend
c         if(line(i:i) .NE. ',' ) then
c             strtmp(idigit:idigit) = line(i:i)
c             idigit = idigit + 1
c         endif
c   20 continue
c      call rgtjst( strtmp )
c
c   --- put data in common arrays ---
c
      nactrc = nactrc + 1
      if( nactrc .GT. MXACTR ) goto 7003
      ascact(nactrc) = asctmp
      subact(nactrc) = subtmp
c     tech type is no longer used, but the files will still contain
c     the tech-type column, so just skip over those characters
c      tecact(nactrc) = tectmp
      hpcact(1,nactrc) = hpcmin
      hpcact(2,nactrc) = hpcmax
      faclod(nactrc) = lodtmp
      iactun(nactrc) = idxtmp
c removed volume from activity file      tnkvol(nactrc) = tnktmp
      read(acttmp,'(F10.0)',ERR=7001) actlev(nactrc)
c      read(strtmp,'(F15.0)',ERR=7001) starts(nactrc)
      starts(nactrc) = 0.
      actage(nactrc) = agetmp
c
c   --- get next record ---
c
      goto 111
c
c  --- read and store the %useful life vs % of new activity data ---
c
  222 continue
c
c   --- initialize the new % activity values ---
c
      ncount = 0
      nagenm = 0
c
      do 30 j=1,MXUSE
         agebin(j) = 2.5
         do 40 i=1,MXAGE
           agepct(i,j) = 100.0 ! any bin values beyond end of activity curve from file are all new activity
   40    continue
   30 continue
c
c   --- call routine to find the /AGE ADJUSTMENT/ keyword ----
c
      keywrd = '/AGE ADJUSTMENT/'
      call fndkey(jerr, IORACT, keywrd )
      if( jerr .EQ. IEOF ) then
          ierr = ISKIP 
          goto 444
      endif
      if( jerr .NE. ISUCES ) goto 7008
c
c   --- read the labels ---
c
      read(IORACT,8000,ERR=7009,END=444) line
      do 50 i=1,MXAGE 
         ibeg = (i-1)*10 + 1
         if( line(ibeg:ibeg+9) .NE. '          ' ) then
            nagenm = nagenm + 1
            agenam(nagenm) = line(ibeg:ibeg+9)
            call lftjst( agenam(nagenm) )
            call low2up( agenam(nagenm) )
         endif
   50 continue
c
c   --- read a record as a character string --- 
c
  333 continue
      read(IORACT,8000,ERR=7009,END=444) line
      call spinit()
      irec = irec + 1
c
c   --- search for the end of the packet ---
c
      keyin = line(1:10)
      call lftjst( keyin )
      call low2up( keyin )
      if( keyin .EQ. KEYEND ) goto 444
c
c   --- increment record count and finish if gone past maximum ---
c
      ncount = ncount + 1
      if( ncount .GT. MXUSE ) goto 444
c
c   --- read the data and store it in common arrays ---
c
      read(line,8001,ERR=7010) agebin(ncount), 
     &                                (tmpstr(i),i=1,nagenm)
ccc
cc      write(IOWMSG,'(1X,I3,1X,I3,1X,3A)',ERR=9999) 
cc     &   ncount,i,tmpstr(1),tmpstr(2),tmpstr(3)
ccc
      do 60 i=1,nagenm
         if( tmpstr(i) .NE. '          ' ) then
             read(tmpstr(i),'(F10.0)',ERR=7010) agepct(i,ncount)
         else
             agepct(i,ncount) = 100.0
         endif
ccc
cc      write(IOWMSG,'(1X,I3,A,I2,1X,2A,F8.2,1X,F8.2)',ERR=9999) 
cc     &   ncount,' agenam ',i,agenam(i),':',
cc     &   agebin(ncount),agepct(i,ncount)
ccc
   60 continue
      call spinit()
c
c   --- make sure the file is ordered correctly ---  
c
      if( ncount .GT. 1 ) then
         if( agebin(ncount) .LT. agebin(ncount-1) ) goto 7011
      endif
c
c   --- read the next record ---
c
      goto 333
c
c   --- finished reading file, close it and return ---
c
  444 continue
      close(IORACT)
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,3A,I6)',ERR=9999) 
     &          'ERROR:  Reading activity file: ',actfl(:strmin(actfl)),
     &                                               ' at record: ',irec
      write(IOWMSG,'(/,1X,3A,I6)',ERR=9999) 
     &          'ERROR:  Reading activity file: ',actfl(:strmin(actfl)),
     &                                               ' at record: ',irec
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,3A,I6,/,9X,A,/,A)',ERR=9999)
     &                      'ERROR:  Reading activity file: ',
     &                        actfl(:strmin(actfl)),' at record: ',irec,
     &                               'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,3A,I6,A,/,A)',ERR=9999)
     &                      'ERROR:  Reading activity file: ',
     &                        actfl(:strmin(actfl)),' at record: ',irec,
     &                               'Line read: ',line(:strmin(line))
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 
     &      'ERROR:  Invalid units type --> ',line(87:96),
     &         ' <-- found in activity file: ',actfl(:strmin(actfl))
      write(IOWSTD,'(9X,4A)',ERR=9999) 'Cannot have daily units when ',
     &                              'doing a ',SUMTOT,' simulation.'
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 
     &      'ERROR:  Invalid units type --> ',line(87:96),
     &         ' <-- found in activity file: ',actfl(:strmin(actfl))
      write(IOWMSG,'(9X,4A)',ERR=9999) 'Cannot have daily units when ',
     &                              'doing a ',SUMTOT,' simulation.'
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,4A,I10)',ERR=9999) 'ERROR:  Number of ',
     &              'records in activity file ',actfl(:strmin(actfl)),
     &                                       ' exceeds max: ',MXACTR   
      write(IOWMSG,'(/,1X,4A,I10)',ERR=9999) 'ERROR:  Number of ',
     &              'records in activity file ',actfl(:strmin(actfl)),
     &                                       ' exceeds max: ',MXACTR   
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &                                   keywrd(:strmin(keywrd)),
     &               ' packet of activity data file ',
     &                                   actfl(:strmin(actfl))
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &                                   keywrd(:strmin(keywrd)),
     &               ' packet of activity data file ',
     &                                   actfl(:strmin(actfl))
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &        'activity data file ',actfl(:strmin(actfl))
      write(IOWMSG,'(/,1X,4A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &        'activity data file ',actfl(:strmin(actfl))
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file ',
     &                                           actfl(:strmin(actfl))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file ',
     &                                           actfl(:strmin(actfl))
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,3A,/,9X,2A,5X,A)',ERR=9999)
     &       'ERROR:  Invalid units type --> ',line(87:96),
     &                          ' <-- specified in activity file.',
     &                                       'Source: ',asctmp,eqptmp
      write(IOWMSG,'(/,1X,3A,/,9X,2A,5X,A)',ERR=9999)
     &       'ERROR:  Invalid units type --> ',line(87:96),
     &                          ' <-- specified in activity file.',
     &                                       'Source: ',asctmp,eqptmp
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 
     &         'ERROR:  Reading activity file: ', actfl(:strmin(actfl))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 
     &         'ERROR:  Reading activity file: ', actfl(:strmin(actfl))
      goto 9999
c
 7009 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &           keywrd(:strmin(keywrd)),' packet of activity file.'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &           keywrd(:strmin(keywrd)),' packet of activity file.'
      goto 9999
c
 7010 continue
      write(IOWSTD,'(/,1X,3A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &         keywrd(:strmin(keywrd)),' packet of activity file.',
     &                              'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,3A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &         keywrd(:strmin(keywrd)),' packet of activity file.',
     &                              'Line read: ',line(:strmin(line))
      goto 9999
c
 7011 continue
      write(IOWSTD,'(/,1X,3A,/,9X,A)',ERR=9999) 
     &             'ERROR:  Data in ',keywrd(:strmin(keywrd)),
     &       ' packet','of activity file is not ordered correctly.'
      write(IOWMSG,'(/,1X,3A,/,9X,A)',ERR=9999) 
     &             'ERROR:  Data in ',keywrd(:strmin(keywrd)),
     &       ' packet','of activity file is not ordered correctly.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(A)
 8001 format(F10.0,10A10)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
