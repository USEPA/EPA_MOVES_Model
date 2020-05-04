C**** RDSPIL
c
      subroutine rdspil( ierr,
     &                   iounit, fname )
c
c-----------------------------------------------------------------------
c
c    reads the spillage, tank permeation, hose permeation, and hot soak
c    starts per hour values and stores the data in common block to be 
c    used by the NONROAD program.  
c
c    Argument declaration.
c     Inputs: 
c       iounit  I  unit number of file to read
c       fname   I  file name of file to read
c     Outputs:
c       ierr    I  error flag
c       
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    02/10/00  mjimenez  added units and tank volume fields
c    05/28/04  -dfk-     added tank and hose permeation fields
c    06/17/04  -dfk-     added dirunal fraction fields
c    11/15/04  -dfk-     separated rec-marine into special section in
c                        order to read 3 hose permeation data
c    09/21/06 --epa--    added Base E10 permeation adj inputs. Ctl all = 2.0.
c                        and init rdsplar. (why wasn't this a compile error?)
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdefc.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4     ierr
      integer*4     iounit
      character*(*) fname
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string (min of 1)
c   fndchr  I   returns the index of string in array of strings
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(5*MXSTR) line
      character*20        keywrd, keyin
      character*10        tectmp, unitmp
      character*9         modtmp
      character*6         indtmp
      integer*4           irec
      integer*4           jerr
      integer*4           i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      nsplar = 0
c
c  ---- zero out non-overlapping hose permeation arrays
c
      hoslen=0.
      hosdia=0.
      hosmtl=0.
      ncklen=0.
      nckdia=0.
      srlen= 0.
      srdia= 0.
      vntlen=0.
      vntdia=0.
      tnke10 = 1.0
      hose10 = 1.0
      ncke10 = 1.0
      sre10  = 1.0
      vnte10 = 1.0

c
c  ---- find the /EMSFAC/ keyword for non-rec marine data ----
c
      keywrd = '/EMSFAC/'
      call fndkey( jerr, iounit, keywrd )
      if( jerr .NE. ISUCES ) goto 7007
c
c   --- read a record as a character string --- 
c
  111 continue
      irec = irec + 1
      read(iounit,8000,ERR=7000,END=7008) line
      call spinit()
c
c   --- look for /END/ keyword ---
c
      keyin = line(1:20)
      call lftjst( keyin )
      call low2up( keyin )
      if( keyin .EQ. KEYEND ) goto 222
c
c   --- parse data fields ---
c
          nsplar = nsplar + 1
          if ( nsplar .GT. MXSPL)  goto 7002
          ascspl(nsplar) = line(1:10)
c
          modtmp = line(54:62)
          call lftjst( modtmp )
          call low2up( modtmp )
          if ( modtmp .NE. PUMP .AND. modtmp .NE. CNTR )  goto 7003
          modspl(nsplar) = modtmp

          indtmp = line(64:67)
          call lftjst( indtmp )
          call low2up( indtmp )
          if ( indtmp .NE. TNKTYP .AND. indtmp .NE. HP )  goto 7006
          indspl(nsplar) = indtmp
c
          read(line(69:73),'(F5.0)',ERR=7004) splpcb(nsplar)
          read(line(74:78),'(F5.0)',ERR=7004) splpce(nsplar)
c
          tectmp = line(79:88)
          if( tectmp .EQ. '          ' )  goto 7005
          call lftjst( tectmp )
          call low2up( tectmp )
          tecspl(nsplar) = tectmp
c
          unitmp = line(90:99)
          call lftjst( unitmp )
          call low2up( unitmp )
          if(unitmp .NE. 'GALLONS' .AND. unitmp .NE. 'GAL/HP') goto 7009
          untspl(nsplar) = unitmp
c --- tank
          read(line(103:112),'(F10.0)',ERR=7004) volspl(nsplar)
          read(line(113:120),'(F 8.0)',ERR=7004) tnkful(nsplar)
          read(line(121:130),'(F10.0)',ERR=7004) tnkmtl(nsplar)
c --- non-rec-marine hose
          read(line(131:140),'(F10.0)',ERR=7004) hoslen(nsplar)
          read(line(141:150),'(F10.0)',ERR=7004) hosdia(nsplar)
          read(line(151:160),'(F10.0)',ERR=7004) hosmtl(nsplar)
c --- rec-marine fill neck (assumed to be 100% non-metal)
          read(line(161:170),'(F10.0)',ERR=7004) ncklen(nsplar)
          read(line(171:180),'(F10.0)',ERR=7004) nckdia(nsplar)
c --- rec-marine supply/return (assumed to be 100% non-metal)
          read(line(181:190),'(F10.0)',ERR=7004) srlen(nsplar)
          read(line(191:200),'(F10.0)',ERR=7004) srdia(nsplar)
c --- rec-marine vent (assumed to be 100% non-metal)
          read(line(201:210),'(F10.0)',ERR=7004) vntlen(nsplar)
          read(line(211:220),'(F10.0)',ERR=7004) vntdia(nsplar)
c --- hot soak
          read(line(221:230),'(F10.0)',ERR=7004) hssph(nsplar)
c --- diurnal
          read(line(231:280),'(5F10.0)',ERR=7004) 
     &                                    (diufrc(i,nsplar),i=1,5)
c
c --- Ethanol (E10) base case adjustment factors.  
c --- Other code adjusts these to any other ethanol percentage.
          read(line(281:290),'(F10.0)',ERR=7004) tnke10(nsplar)
          if( tnke10(nsplar) .EQ. 0. ) tnke10(nsplar) = 1.0
          read(line(291:300),'(F10.0)',ERR=7004) hose10(nsplar)
          if( hose10(nsplar) .EQ. 0. ) hose10(nsplar) = 1.0
          read(line(301:310),'(F10.0)',ERR=7004) ncke10(nsplar)
          if( ncke10(nsplar) .EQ. 0. ) ncke10(nsplar) = 1.0
          read(line(311:320),'(F10.0)',ERR=7004) sre10(nsplar)
          if( sre10(nsplar) .EQ. 0. ) sre10(nsplar) = 1.0
          read(line(321:330),'(F10.0)',ERR=7004) vnte10(nsplar)
          if( vnte10(nsplar) .EQ. 0. ) vnte10(nsplar) = 1.0
c
c   --- get the next non-rec marine record ---
c
      goto 111
c
c  --- entire file read ---
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
      write(IOWSTD,'(/,1X,3A,I5)',ERR=9999) 
     &                      'ERROR:  Reading emission factor file ',
     &                        fname(:strmin(fname)),' at record ',irec
      write(IOWMSG,'(/,1X,3A,I5)',ERR=9999) 
     &                      'ERROR:  Reading emission factor file ',
     &                        fname(:strmin(fname)),' at record ',irec
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,A,I8,/,9X,2A)',ERR=9999) 
     &   'ERROR:  Number of distinct emission factors exceed max ',
     &     MXSPL,'in file ', fname(:strmin(fname))
      write(IOWMSG,'(/,1X,A,I8,/,9X,2A)',ERR=9999) 
     &   'ERROR:  Number of distinct emission factors exceed max ',
     &     MXSPL,'in file ', fname(:strmin(fname))
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,2A,/,9X,2A)',ERR=9999) 
     &            'ERROR: Invalid refueling mode specified in ',
     &                    fname(:strmin(fname)), 'Line read: ',
     &                                       line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,/,9X,2A)',ERR=9999) 
     &            'ERROR: Invalid refueling mode specified in ',
     &                    fname(:strmin(fname)), 'Line read: ',
     &                                       line(:strmin(line))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,2A,/,9X,2A)',ERR=9999) 
     &                 'ERROR:  Reading emission factor file ',
     &                         fname(:strmin(fname)),'Line read: ',
     &                                               line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,/,9X,2A)',ERR=9999) 
     &                 'ERROR:  Reading emission factor file ',
     &                         fname(:strmin(fname)),'Line read: ',
     &                                               line(:strmin(line))
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,A,/,9X,2A,/,9X,2A)',ERR=9999) 
     &        'ERROR:  Missing or invalid tech type ',
     &            'in emissions factors file ',fname(:strmin(fname)),
     &                           'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,A,/,9X,2A,/,9X,2A)',ERR=9999) 
     &        'ERROR:  Missing or invalid tech type ',
     &            'in emissions factors file ',fname(:strmin(fname)),
     &                           'Line read: ',line(:strmin(line))
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,2A,/,9X,2A)',ERR=9999) 
     &            'ERROR: Invalid range indicator specified in ',
     &                    fname(:strmin(fname)), 'Line read: ',
     &                                       line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,/,9X,2A)',ERR=9999) 
     &            'ERROR: Invalid range indicator specified in ',
     &                    fname(:strmin(fname)), 'Line read: ',
     &                                       line(:strmin(line))
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &                                   keywrd(:strmin(keywrd)),
     &                      ' packet of emission factors file ',
     &                                   fname(:strmin(fname))
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &                                   keywrd(:strmin(keywrd)),
     &                      ' packet of emission factors file ',
     &                                   fname(:strmin(fname))
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,1X,A,/,9X,2A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &        'emission factors file ', fname(:strmin(fname))
      write(IOWMSG,'(/,1X,A,/,9X,2A)',ERR=9999)
     &     'ERROR:  Unexpected end-of-file reached reading ',
     &        'emission factors file ', fname(:strmin(fname))
      goto 9999
c

 7009 continue
      write(IOWSTD,'(/,1X,2A,/,9X,2A)',ERR=9999)
     &            'ERROR: Invalid units specified in ',
     &                    fname(:strmin(fname)), 'Line read: ',
     &                                       line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,/,9X,2A)',ERR=9999)
     &            'ERROR: Invalid units specified in ',
     &                    fname(:strmin(fname)), 'Line read: ',
     &                                       line(:strmin(line))
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
