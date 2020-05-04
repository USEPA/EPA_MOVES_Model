C**** RDTECH
c
      subroutine rdtech( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the technology fractions by equipment type for each
c    model year.
c
c    Argument declaration.
c     Inputs: 
c     Outputs:
c       ierr    I  error flag
c       
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      07/22/96  --jlf--  original development
c      07/20/05  --cimulus--  floating-point comparison for equality
c                             okay; tchhpb, hptmpb, tchhpe, and hptmpe
c                             values are read from file, rather than
c                             being calculated at runtime
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
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4     ierr
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(4*MXSTR) line
      character*20        keywrd, keyin
      character*10        asctmp
      character*10        tectmp, tecsav(MXTECH)
      integer*4           irec, iyrtmp, istrt, ntch, itch, ifac, jerr
      real*4              tchtmp(MXTECH), hptmpb, hptmpe
      real*4              sumtec
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      ntchcd = 0
c
c  --- open the file ---
c
      open(IORTCH,file=tchfl,ERR=7008,status='UNKNOWN')
      rewind(IORTCH)
c
c  ---- find the /TECH FRAC/ keyword ----
c
      keywrd = '/TECH FRAC/'
      call fndkey( jerr, IORTCH, keywrd )
      if( jerr .EQ. IEOF ) then
          ierr = ISKIP
          goto 9999
      endif
      if( jerr .NE. ISUCES ) goto 7006
c
c   --- read a record as a character string --- 
c
  111 continue
      irec = irec + 1
      read(IORTCH,8000,ERR=7000,END=7007) line
      call spinit()
c
c   --- look for /END/ keyword ---
c
      keyin = line(1:20)
      call lftjst( keyin )
      call low2up( keyin )
      if( keyin .EQ. KEYEND ) goto 222
c
c   --- if this is a new code, get the tech types ---
c
      if( line(6:15) .NE. '          ' ) then
          ntch = 0
          asctmp = line(6:15)
          read(line(21:25),'(F5.0)',ERR=7004) hptmpb
          read(line(26:30),'(F5.0)',ERR=7004) hptmpe
          istrt = 35
  333     continue
          tectmp = line(istrt:istrt+9)
          if( tectmp .EQ. '          ' ) goto 444
          call lftjst( tectmp )
          call low2up( tectmp )
c
c   --- add tech type to array ---
c
          ntch = ntch + 1
          if( ntch .GT. MXTECH ) goto 7003
          tecsav(ntch) = tectmp
          istrt = istrt + 10
          goto 333
c
c
  444     continue
          if( ntch .LE. 0) goto 7005
c
c   ---- line contains technology fractions data --
c
      else
c
c   ---- read the year ---
c
          read(line(1:5),'(I5)',ERR=7004) iyrtmp
          istrt = 35
          do 20 itch = 1,ntch
c
c   --- read the data ---
c
             read(line(istrt:istrt+9),'(F10.0)',ERR=7004) tchtmp(itch)
c
c   --- find the index into arrays for line of data ---
c
             do 30 ifac=1,ntchcd
                 if( asctch(ifac) .EQ. asctmp .AND.
     &                         iyrtch(ifac) .EQ. iyrtmp .AND.
     &                             tchhpb(ifac) .EQ. hptmpb .AND. ! floating-point comparison for equality okay; tchhpb and hptmpb values are read from file, rather than being calculated at runtime
     &                                   tchhpe(ifac) .EQ. hptmpe) then ! floating-point comparison for equality okay; tchhpe and hptmpe values are read from file, rather than being calculated at runtime
                      tchfrc(ifac,itch) = tchtmp(itch)
                      tectyp(ifac,itch) = tecsav(itch)
                      istrt = istrt + 10
                      goto 20
                  endif
   30        continue
c
c  --- not found, add it to list ----
c
             ntchcd = ntchcd + 1
             if( ntchcd .GT. MXTECD ) goto 7002
             asctch(ntchcd) = asctmp
             iyrtch(ntchcd) = iyrtmp
             tchhpb(ntchcd) = hptmpb
             tchhpe(ntchcd) = hptmpe
             ntech(ntchcd)  = ntch
             tectyp(ntchcd,itch) = tecsav(itch)
             tchfrc(ntchcd,itch) = tchtmp(itch)
             istrt = istrt + 10
   20     continue
      endif
c
c   --- get the next record ---
c
      goto 111
c
c  --- entire file read, close it ---
c
  222 continue
      close(IORTCH)
c
c   --- renormalize the technology fractions to make sure they add to
c       one, write warning if more than 5% error ---

      do 40 ntch = 1, ntchcd
c
         sumtec = 0.
c
         do 50 itch = 1, ntech(ntch)
             sumtec = sumtec + tchfrc(ntch,itch)
   50    continue
c
         if ( ABS(sumtec-1.0) .GT. 0.002 ) then
           write(IOWMSG,'(/,1X,2A)',ERR=9999) 'WARNING:  Technology ',
     &        'fractions do not add to 1.  Renormalizing...'
           write(IOWMSG,'(12X,4(A,3X))') 'Equipment', '    HP range',
     &                                    '  Year', 'Original Sum'
           write(IOWMSG,'(12X,A,2(3X,F5.0),3X,I4,3X,F6.4)') 
     &                      asctch(ntch), tchhpb(ntch), tchhpe(ntch), 
     &                      iyrtch(ntch), sumtec
           call chkwrn(jerr,IDXWTC)
           if( jerr .NE. ISUCES ) goto 9999
         endif
         do 45 itch = 1,ntech(ntch)
            if( sumtec .GT. 0. ) 
     &             tchfrc(ntch,itch) = tchfrc(ntch,itch) / sumtec
   45    continue
   40 continue
c
c  --- return ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,3A,I6)',ERR=9999) 
     &             'ERROR:  Reading technology fraction file ',
     &                       tchfl(:strmin(tchfl)),' at record ',irec
      write(IOWMSG,'(/,1X,3A,I6)',ERR=9999) 
     &             'ERROR:  Reading technology fraction file ',
     &                       tchfl(:strmin(tchfl)),' at record ',irec
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,3A,/,9X,A,I5)',ERR=9999)
     &         'ERROR:  Number of distinct ',
     &         'technology fractions in file ', tchfl(:strmin(tchfl)),
     &         'exceeds max ',MXTECD
      write(IOWMSG,'(/,1X,3A,/,9X,A,I5)',ERR=9999)
     &         'ERROR:  Number of distinct ',
     &         'technology fractions in file ', tchfl(:strmin(tchfl)),
     &         'exceeds max ',MXTECD
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,3A,/,9X,A,I5)',ERR=9999) 'ERROR:  Number of ',
     &       'technology types in technology fractions file ',
     &       tchfl(:strmin(tchfl)),'exceeds max ', MXTECH
      write(IOWMSG,'(/,1X,3A,/,9X,A,I5)',ERR=9999) 'ERROR:  Number of ',
     &       'technology types in technology fractions file ',
     &       tchfl(:strmin(tchfl)),'exceeds max ', MXTECH
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,3A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &         'technology fractions file ',tchfl(:strmin(tchfl)),
     &                           'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,3A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &         'technology fractions file ',tchfl(:strmin(tchfl)),
     &                           'Line read: ',line(:strmin(line))
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,2A,/,9X,2A,/,9X,2A)',ERR=9999) 
     &   'ERROR:  Missing or ',
     &   'invalid technology type','in technology fractions file ',
     &      tchfl(:strmin(tchfl)),'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,/,9X,2A,/,9X,2A)',ERR=9999)
     &   'ERROR:  Missing or ',
     &   'invalid technology type','in technology fractions file ',
     &      tchfl(:strmin(tchfl)),'Line read: ',line(:strmin(line))
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,3A,/,9X,3A)',ERR=9999) 
     &       'ERROR:  Cannot find ', keywrd(:strmin(keywrd)),
     &                   ' packet','of technology fractions file ',
     &                                   tchfl(:strmin(tchfl))
      write(IOWMSG,'(/,1X,3A,/,9X,3A)',ERR=9999)
     &       'ERROR:  Cannot find ', keywrd(:strmin(keywrd)),
     &                                   keywrd(:strmin(keywrd)),
     &                   ' packet','of technology fractions file ',
     &                                   tchfl(:strmin(tchfl))
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,A,/,9X,3A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached','reading ',
     &        'technology fractions file ', tchfl(:strmin(tchfl))
      write(IOWMSG,'(/,1X,A,/,9X,3A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached','reading ',
     &        'technology fractions file ', tchfl(:strmin(tchfl))
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file ',
     &                                           tchfl(:strmin(tchfl))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file ',
     &                                           tchfl(:strmin(tchfl))
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
