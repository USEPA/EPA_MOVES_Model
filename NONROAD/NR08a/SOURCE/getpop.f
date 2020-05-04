C**** GETPOP
c
      subroutine getpop( ierr, asccod, iyrin )
c
c-----------------------------------------------------------------------
c
c    reads the base year sorted population file and stores the data 
c    in common to be used by the NONROAD program.  Population 
c    figures are given by equipment type, fuel type, Horsepower 
c    category, and region code.  All of the data for the current 
c    SCC is read and stored.
c
c    Argument declaration.
c     Outputs:
c       ierr    I  error flag
c       asccod  C  SCC code of records read
c     Inputs:
c       iyrin   I  current year
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/01/93  --gmw-- original development
c      07/22/96  --gwl-- fixes
c      07/22/96  --jlf-- took out reading of technology info - this is
c                        now handled elsewhere
c      07/20/05  --cimulus-- floating-point comparison for equality
c                            okay; lasthp value is hard-coded to -9 by
c                            default, and lasthp stores previous values
c                            of hpavg and then is compared back against
c                            hpavg
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
      integer*4    ierr
      character*10 asccod
      integer*4    iyrin
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string (minimum of 1)
c   fndchr  I   returns the index of a string in array of string
c
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(2*MXSTR) line
      character*15        poptmp
      character*10        ascin, asctmp, regtmp, regstr, distmp
      character*5         fiptmp, subtmp, hpavtmp
      integer*4           irec, idigit, iend, iyrtmp
      integer*4           idxfip, idxsta, i
      real*4              hpmin, hpmax, valtmp, usetmp, lasthp, hpavg
      logical*4           lupper
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- intialize some local variables ---
c
      irec = 1
      ascin = '          '
      regstr = '          '
      lasthp = -9
      lupper = .FALSE.
      npoprc = 0
c
c   --- read a record as a character string --- 
c
  111 continue
      read(IOSPOP,8000,ERR=7001,END=7002) line
      irec = irec + 1
c
c   --- get the SCC code, check for match with last record ---
c
      asctmp = line(18:27)
      if( ascin .EQ. '          ' ) ascin = asctmp
      if( asctmp .NE. ascin ) then
           asccod = ascin
           backspace(IOSPOP)
           ierr = ISUCES
           goto 9999
      endif
c
c   --- check for /END/ keyword ---
c
      if( asctmp .EQ. KEYEND ) then
          ierr = IEOF
          goto 9999
      endif 
c
c   --- parse the line and load all of the data ----
c
      fiptmp = line(1:5)
      subtmp = line(7:11)
      read(line(13:16),'(I4)',ERR=7003) iyrtmp
      read(line(70:74),'(F5.0)',ERR=7003) hpmin
      read(line(76:80),'(F5.0)',ERR=7003) hpmax
      hpavtmp = line(82:86)
      read(line(88:92),'(F5.0)',ERR=7003) usetmp
      distmp = line(93:102)
c
c   --- left justify and convert to upper case where necessary ---
c
      if( hpavtmp .EQ. '     ' ) then
         hpavg = (hpmin+hpmax)/2.
      else 
         read(hpavtmp,'(F5.0)',ERR=7002) hpavg
      endif
      call lftjst( fiptmp )
      call lftjst( subtmp )
      call low2up( subtmp )
      regtmp = fiptmp//subtmp
      call lftjst( distmp )
      call low2up( distmp )
c
c   --- set the last to current if first time through ---
c
      if( regstr .EQ. '          ' ) regstr = regtmp
      if( lasthp .EQ. -9 ) lasthp = hpavg ! floating-point comparison for equality okay; lasthp value is hard-coded to -9 by default
c
c   --- if this record matches the last one, check for which
c       one to use for the current year ---
c
      if( regstr .EQ. regtmp
     &      .AND. hpavg .EQ. lasthp .AND. npoprc .GT. 0 ) then ! floating-point comparison for equality okay; lasthp stores previous values of hpavg and then is compared back against hpavg
         nrecds = nrecds + 1
         if( lupper ) then
            goto 111
         endif
         if( iyrtmp .GT. iyrin ) then
             if( ipopyr(npoprc) .NE. iyrin ) then
               if( irec .GT. 1 ) then
                  npoprc = npoprc + 1
                  nrecds = nrecds - 1
                  if( npoprc .GT. MXPOP ) goto 7004
               endif
            else
               goto 111
            endif
            lupper = .TRUE.          
         endif
      else 
         npoprc = npoprc + 1
         if( npoprc .GT. MXPOP ) goto 7004
         regstr = regtmp
         lasthp = hpavg
         if( iyrtmp .GE. iyrin ) then
            lupper = .TRUE.
         else
            lupper = .FALSE.
         endif
      endif
c
c  --- if fips code is not a state code, find the index into county array
c      and set the county specific flag ---
c
      if( regtmp(3:5) .NE. '000' ) then
          idxfip = fndchr( regtmp(1:5), 5, fipcod, NCNTY )
          if( idxfip .GT. 0 ) lctlev(idxfip) = .TRUE.
          if( regtmp(6:10) .NE. '     ' ) then
             idxfip = fndchr( regtmp, 10, reglst, nregin )
             if( idxfip .GT. 0 ) lrglev(idxfip) = .TRUE.
          endif
      else
          if( regtmp(1:2) .NE. '00' ) then
             idxsta = fndchr( regtmp(1:5), 5, statcd, NSTATE )
             if( idxsta .GT. 0 ) lstlev(idxsta) = .TRUE.
          endif
      endif
c
c   --- get population value, if population value is zero, skip it ---
c
      poptmp = '               '
      idigit = 1
      iend = 122
      do 20 i=108,iend
         if(line(i:i) .NE. ',' .AND. line(i:i) .NE. ' ') then
             poptmp(idigit:idigit) = line(i:i)
             idigit = idigit + 1
         endif
   20 continue  
      call rgtjst( poptmp )
      read(poptmp,'(F20.0)',ERR=7003) valtmp
c
c   -- load this data into global arrays ---
c
      if( npoprc .EQ. 0 ) npoprc = 1
      regncd(npoprc) = regtmp
      ipopyr(npoprc) = iyrtmp
      avghpc(npoprc) = hpavg
      hprang(1,npoprc) = hpmin
      hprang(2,npoprc) = hpmax
      usehrs(npoprc) = usetmp
      popeqp(npoprc) = valtmp
      discod(npoprc) = distmp
c
      goto 111
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7001 continue
      write(IOWSTD,'(/,1X,2A,/,9X,A,I7)',ERR=9999) 
     &                     'ERROR:  Reading sorted population file ',
     &                       spopfl(:strmin(popfl)),'at record ',irec
      write(IOWMSG,'(/,1X,2A,/,9X,A,I7)',ERR=9999) 
     &                     'ERROR:  Reading sorted population file ',
     &                       spopfl(:strmin(popfl)),'at record ',irec
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,A,/,9X,2A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached',
     &                'reading sorted population file ',
     &                spopfl(:strmin(spopfl))
      write(IOWMSG,'(/,1X,A,/,9X,2A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached',
     &                'reading sorted population file ',
     &                spopfl(:strmin(spopfl))
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,2A,/,9X,A,I7,/,9X,2A)',ERR=9999) 
     &                     'ERROR:  Reading sorted population file ',
     &                     spopfl(:strmin(spopfl)),'at record ',irec,
     &                     'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,/,9X,A,I7,/,9X,2A)',ERR=9999) 
     &                     'ERROR:  Reading sorted population file ',
     &                     spopfl(:strmin(spopfl)),'at record ',irec,
     &                     'Line read: ',line(:strmin(line))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,3A,/,9X,A,I7)',ERR=9999) 
     &      'ERROR:  Number of population records ',
     &      'for equipment type ',asctmp,
     &      'exceeds max ',MXPOP
      write(IOWMSG,'(/,1X,3A,/,9X,A,I7)',ERR=9999) 
     &      'ERROR:  Number of population records ',
     &      'for equipment type ',asctmp,
     &      'exceeds max ',MXPOP
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
