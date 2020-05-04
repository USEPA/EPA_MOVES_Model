C**** RDEVTECH
c
      subroutine rdevtech( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the evap technology fractions by equipment type for each
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
c      10May04  --dfk--  original development (from RDTECH)
c      20Jul05  --cimulus--  commented out usage of ichk
c      20Jul05  --cimulus--  floating-point comparison for equality
c                            okay; evtchhpb, hptmpb, evtchhpe, and
c                            hptmpe values are read from file, rather
c                            than being calculated at runtime
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
      character*10        tectmp,tecsav(MXEVTECH)
      integer*4           irec, iyrtmp, istrt, ntch, itch, ifac, jerr
c      integer*4           ichk
      real*4              tchtmp(MXEVTECH), hptmpb, hptmpe
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
      nevtchcd = 0
c
c  --- open the file ---
c
      open(IOREVTCH,file=evtchfl,ERR=7008,status='UNKNOWN')
      rewind(IOREVTCH)
c
c  ---- find the /EVAP TECH FRAC/ keyword ----
c
      keywrd = '/EVAP TECH FRAC/'
      call fndkey( jerr, IOREVTCH, keywrd )
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
      read(IOREVTCH,8000,ERR=7000,END=7007) line
      call spinit()
c
c   --- look for /END/ keyword ---
c
      keyin = line(1:20)
      call lftjst( keyin )
      call low2up( keyin )
      if( keyin .EQ. KEYEND ) goto 222
c
c   --- if this is a new SCC code, get the tech types ---
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
c  ---- an evap tech group consists of an E followed by 8 numbers.
c       each number corresponds to a tech group name for each evap
c       species, as follows:
c
c        12345678
c       E00000000
c
c       E: Evap Tech Group
c       1: Diurnal
c       2: Tank Permeation
c       3: Hose Permeation (applies to non-rec-marine hose and 3 rec-marine hoses)
c       4: Hot Soak
c       5: Displacement
c       6: Spillage
c       7: Running Loss
c       8: Resting Loss
c
c       the tech type name format for each evap species is En, where n=0-9
c
          if(tectmp(1:1).ne.'E' .or.
     &       strmin(tectmp).ne.9) goto 7005
c          read(tectmp(2:8),*,ERR=7005) ichk
c
c   --- add tech type to array ---
c
          ntch = ntch + 1
          if( ntch .GT. MXEVTECH ) goto 7003
          tecsav(ntch) = tectmp
          istrt = istrt + 10
          goto 333
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
             do 30 ifac=1,nevtchcd
                 if( ascevtch(ifac) .EQ. asctmp .AND.
     &                       iyrevtch(ifac) .EQ. iyrtmp .AND.
     &                           evtchhpb(ifac) .EQ. hptmpb .AND. ! floating-point comparison for equality okay; evtchhpb and hptmpb values are read from file, rather than being calculated at runtime
     &                                 evtchhpe(ifac) .EQ. hptmpe) then ! floating-point comparison for equality okay; evtchhpe and hptmpe values are read from file, rather than being calculated at runtime
                      evtchfrc(ifac,itch) = tchtmp(itch)
                      evtectyp(ifac,itch) = tecsav(itch)
                      istrt = istrt + 10
                      goto 20
                  endif
   30        continue
c
c  --- not found, add it to list ----
c
             nevtchcd = nevtchcd + 1
             if( nevtchcd .GT. MXEVTECD ) goto 7002
             ascevtch(nevtchcd) = asctmp
             iyrevtch(nevtchcd) = iyrtmp
             evtchhpb(nevtchcd) = hptmpb
             evtchhpe(nevtchcd) = hptmpe
             nevtech(nevtchcd)  = ntch
             evtectyp(nevtchcd,itch) = tecsav(itch)
             evtchfrc(nevtchcd,itch) = tchtmp(itch)
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
      close(IOREVTCH)
c
c   --- renormalize the technology fractions to make sure they add to
c       one, write warning if more than 5% error ---

      do 40 ntch = 1, nevtchcd
c
         sumtec = 0.
c
         do 50 itch = 1, nevtech(ntch)
             sumtec = sumtec + evtchfrc(ntch,itch)
   50    continue
c
         if ( ABS(sumtec-1.0) .GT. 0.002 ) then
           write(IOWMSG,'(/,1X,2A)',ERR=9999) 
     &        'WARNING:  Evap Technology ',
     &        'fractions do not add to 1.  Renormalizing...'
           write(IOWMSG,'(12X,4(A,3X))') 'Equipment', '    HP range',
     &                                    '  Year', 'Original Sum'
           write(IOWMSG,'(12X,A,2(3X,F5.0),3X,I4,3X,F6.4)') 
     &                   ascevtch(ntch), evtchhpb(ntch), evtchhpe(ntch), 
     &                   iyrevtch(ntch), sumtec
           call chkwrn(jerr,IDXWTC)
           if( jerr .NE. ISUCES ) goto 9999
         endif
         do 45 itch = 1,nevtech(ntch)
            if( sumtec .GT. 0. ) 
     &             evtchfrc(ntch,itch) = evtchfrc(ntch,itch) / sumtec
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
     &             'ERROR:  Reading evap technology fraction file ',
     &                      evtchfl(:strmin(evtchfl)),' at record ',irec
      write(IOWMSG,'(/,1X,3A,I6)',ERR=9999) 
     &             'ERROR:  Reading evap technology fraction file ',
     &                      evtchfl(:strmin(evtchfl)),' at record ',irec
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,3A,/,9X,A,I5)',ERR=9999)
     &         'ERROR:  Number of distinct ',
     &         'evap technology fractions in file ',
     &          evtchfl(:strmin(evtchfl)),
     &         'exceeds max ',MXEVTECD
      write(IOWMSG,'(/,1X,3A,/,9X,A,I5)',ERR=9999)
     &         'ERROR:  Number of distinct ',
     &         'evap technology fractions in file ',
     &          evtchfl(:strmin(evtchfl)),
     &         'exceeds max ',MXEVTECD
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,3A,/,9X,A,I5)',ERR=9999) 'ERROR:  Number of ',
     &       'evap technology types in evap technology fractions file ',
     &       evtchfl(:strmin(evtchfl)),'exceeds max ', MXEVTECH
      write(IOWMSG,'(/,1X,3A,/,9X,A,I5)',ERR=9999) 'ERROR:  Number of ',
     &       'evap technology types in evap technology fractions file ',
     &       evtchfl(:strmin(evtchfl)),'exceeds max ', MXEVTECH
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,3A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &      'evap technology fractions file ',evtchfl(:strmin(evtchfl)),
     &                        'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,3A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &      'evap technology fractions file ',evtchfl(:strmin(evtchfl)),
     &                         'Line read: ',line(:strmin(line))
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,2A,/,9X,2A,/,9X,2A)',ERR=9999) 
     &   'ERROR:  Missing or ',
     &   'invalid evap technology type',
     &   'in evap technology fractions file ',
     &    evtchfl(:strmin(evtchfl)),'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,/,9X,2A,/,9X,2A)',ERR=9999)
     &   'ERROR:  Missing or ',
     &   'invalid evap technology type',
     &   'in evap technology fractions file ',
     &    evtchfl(:strmin(evtchfl)),'Line read: ',line(:strmin(line))
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,3A,/,9X,3A)',ERR=9999) 
     &       'ERROR:  Cannot find ', keywrd(:strmin(keywrd)),
     &                   ' packet','of evap technology fractions file ',
     &                                   evtchfl(:strmin(evtchfl))
      write(IOWMSG,'(/,1X,3A,/,9X,3A)',ERR=9999)
     &       'ERROR:  Cannot find ', keywrd(:strmin(keywrd)),
     &                                   keywrd(:strmin(keywrd)),
     &                   ' packet','of evap technology fractions file ',
     &                                   evtchfl(:strmin(evtchfl))
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,A,/,9X,3A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached','reading ',
     &     'evap technology fractions file ', evtchfl(:strmin(evtchfl))
      write(IOWMSG,'(/,1X,A,/,9X,3A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached','reading ',
     &     'evap technology fractions file ', evtchfl(:strmin(evtchfl))
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file ',
     &                                        evtchfl(:strmin(evtchfl))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file ',
     &                                        evtchfl(:strmin(evtchfl))
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
