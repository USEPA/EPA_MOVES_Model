C**** RDEVEMFC
c
      subroutine rdevemfc( ierr, ascar, tecar, arhpcb, arhpce, 
     &                   iarhun, iaryr, facar, ifrst, nfacar,
     &                   iounit, fname, idxpol, polin )
c
c-----------------------------------------------------------------------
c
c    reads the evap emisisons factors file and stores the data in common
c    block to be used by the NONROAD program. Since each evap species
c    can have its own tech group, each species is treated individually.
c
c    Argument declaration.
c     Inputs: 
c       iounit  I  unit number of file to read
c       fname   I  file name of file to read
c       idxpol  I  the pollutant index to fill
c       polin   C  pollutant name to use
c     Outputs:
c       ierr    I  error flag
c       ascar   C  array of SCC code to fill
c       tecar   C  array of tech id codes to fill
c       arhpcb  I  array of beginning horsepower categories to fill
c       arhpce  I  array of ending horsepower categories to fill
c       iarhun  I  array of units for conversion to fill
c       iaryr   I  array of years to fill
c       facar   I  array of emission factors fo fill  
c       ifrst   I  first value of second index for iarhun and facar
c       nfacar  I  number of factors in the arrays
c       
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      05/18/04  --dfk--  original development (from RDEMFC)
c      10/13/04  --dfk--  added code to handle tank/hose permeation
c                         units of g/m2/day
c      11/16/04  --dfk--  modified to handle 13 character tech names
c      07/20/05  --cimulus--  floating-point comparison for equality
c                             okay; arhpcb, hptmpb, arhpce, and hptmpe
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
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4     ierr
      integer*4     ifrst
      character*10  ascar(MXEMFC,ifrst:*)
      character*10  tecar(MXEMFC,ifrst:*)
      real*4        arhpcb(MXEMFC,ifrst:*)
      real*4        arhpce(MXEMFC,ifrst:*)
      integer*4     iaryr(MXEMFC,ifrst:*)
      integer*4     iarhun(MXEMFC,ifrst:*)
      real*4        facar(MXEMFC,ifrst:*)
      integer*4     iounit
      character*(*) fname
      integer*4     idxpol
      integer*4     nfacar(ifrst:*)
      character*4   polin
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string (min of 1)
c   fndchr  I   returns the index of string in array of strings
c
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(4*MXSTR) line
      character*20        keywrd, keyin
      character*10        asctmp, untkey(MXEMCV)
      character*10        tectmp, tecsav(MXTECH)
      character*10        poltmp
      integer*4           irec, i, iyrtmp, idxunt, istrt, ntch
      integer*4           itch, ifac, iuntmp, jerr
      real*4              emftmp(100), hptmpb, hptmpe
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
c
c   --- put units keywords and conversion factors into local arrays ---
c
      do 10 i=1,MXEMCV
         untkey(i) = 'xxxxxxxxxx'
   10 continue
      untkey(IDXGHR) = KEYGHR
      untkey(IDXGHP) = KEYGHP
      untkey(IDXGAL) = KEYGAL
      untkey(IDXTNK) = KEYTNK
      untkey(IDXGDY) = KEYGDY
      untkey(IDXGST) = KEYGST
      untkey(IDXMLT) = KEYMLT
      untkey(IDXGMD) = KEYGMD
c
c  ---- find the /EMSFAC/ keyword ----
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
          if( tectmp .EQ. '          ' ) then
              if( iounit .EQ. IORBSF ) then
                  goto 444
              else
                  goto 7005
              endif
          endif
          call lftjst( tectmp )
          call low2up( tectmp )
          if( line(istrt:istrt+9) .EQ. '          ' ) goto 333
          idxunt = fndchr( tectmp, 10, untkey, MXEMCV ) 
          if( idxunt .GT. 0 ) goto 444
c
c   --- add tech type to array ---
c
          ntch = ntch + 1
          if( ntch .GT. MXTECH ) goto 7003 
          tecsav(ntch) = tectmp
          istrt = istrt + 10
          goto 333
c
c   ---- found the units field, store it and read the pollutant code ---
c
  444     continue
          if( idxpol .EQ. IDXDIU .AND. idxunt .NE. IDXMLT ) goto 7009
          if( idxpol .EQ. IDXTKP .AND. idxunt .NE. IDXGMD ) goto 7010
          if( idxpol .GE. IDXHOS .AND. idxpol .LE. IDXVNT 
     &        .AND. idxunt .NE. IDXGMD ) goto 7011
          iuntmp = idxunt
          istrt = istrt + 10
          poltmp = line(istrt:istrt+9)
          call lftjst( poltmp )
          call low2up( poltmp )
          if( poltmp .NE. polin ) goto 7006
c
c   ---- line contains emissions factor data ----
c
      else
c
c   ---- read the year ---
c
          read(line(1:5),'(I5)',ERR=7004) iyrtmp
          istrt = 35
c
c   --- read the data ---
c
          do 20 itch = 1,ntch
             read(line(istrt:istrt+9),'(F10.0)',ERR=7004) emftmp(itch)
             istrt = istrt + 10
   20     continue
c
c   --- find the index into arrays for line of data ---
c
          
          do 30 itch = 1,ntch
             do 40 ifac=1,nfacar(idxpol)
                 if( ascar(ifac,idxpol) .EQ. asctmp .AND. 
     &               iaryr(ifac,idxpol) .EQ. iyrtmp .AND.
     &              arhpcb(ifac,idxpol) .EQ. hptmpb .AND. ! floating-point comparison for equality okay; arhpcb and hptmpb values are read from file, rather than being calculated at runtime
     &              arhpce(ifac,idxpol) .EQ. hptmpe .AND. ! floating-point comparison for equality okay; arhpce and hptmpe values are read from file, rather than being calculated at runtime
     &               tecar(ifac,idxpol) .EQ. tecsav(itch) ) then
                     facar(ifac,idxpol) = emftmp(itch) 
                    iarhun(ifac,idxpol) = iuntmp
                      goto 30
                  endif
   40        continue
c
c  --- not found, add it to list ----
c
             nfacar(idxpol) = nfacar(idxpol) + 1
             if( nfacar(idxpol) .GT. MXEMFC ) goto 7002
             ascar(nfacar(idxpol),idxpol) = asctmp
             iaryr(nfacar(idxpol),idxpol) = iyrtmp
            arhpcb(nfacar(idxpol),idxpol) = hptmpb
            arhpce(nfacar(idxpol),idxpol) = hptmpe
             tecar(nfacar(idxpol),idxpol) = tecsav(itch)
             facar(nfacar(idxpol),idxpol) = emftmp(itch)
            iarhun(nfacar(idxpol),idxpol) = iuntmp
   30     continue
      endif
c
c   --- get the next record ---
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
     &     MXEMFC,'in file ', fname(:strmin(fname))
      write(IOWMSG,'(/,1X,A,I8,/,9X,2A)',ERR=9999) 
     &   'ERROR:  Number of distinct emission factors exceed max ',
     &     MXEMFC,'in file ', fname(:strmin(fname))
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,A,I8,/,9X,2A)',ERR=9999) 
     &        'ERROR:  Number of technology types exceeds max: ',
     &        MXTECH, 'for emission factors in file ',
     &                                             fname(:strmin(fname))
      write(IOWSTD,'(9X,2A)',ERR=9999) 'Make sure that the units ',
     &               'field is included and is in the correct columns.'
      write(IOWSTD,'(1X,A,/,A)') 'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,A,I8,/,9X,2A)',ERR=9999) 
     &        'ERROR:  Number of technology types exceeds max: ',
     &        MXTECH, 'for emission factors in file ',
     &                                             fname(:strmin(fname))
      write(IOWMSG,'(9X,2A)',ERR=9999) 'Make sure that the units ',
     &               'field is included and is in the correct columns.'
      write(IOWMSG,'(1X,A,/,A)') 'Line read: ',line(:strmin(line))
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
     &        'ERROR:  Missing or invalid tech type or units type',
     &            'in emissions factors file ',fname(:strmin(fname)),
     &                           'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,A,/,9X,2A,/,9X,2A)',ERR=9999) 
     &        'ERROR:  Missing or invalid tech type or units type',
     &            'in emissions factors file ',fname(:strmin(fname)),
     &                           'Line read: ',line(:strmin(line))
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,2A,/,9X,4A,/,9X,2A)',ERR=9999) 
     &                 'ERROR:  Invalid pollutant code ',
     &         poltmp(:strmin(poltmp)),'for ',polin(:strmin(polin)),
     &            ' emissions factors file ',fname(:strmin(fname)),
     &                           'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,/,9X,4A,/,9X,2A)',ERR=9999) 
     &                 'ERROR:  Invalid pollutant code ',
     &         poltmp(:strmin(poltmp)),'for ',polin(:strmin(polin)),
     &            ' emissions factors file ',fname(:strmin(fname)),
     &                           'Line read: ',line(:strmin(line))
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
     &    'ERROR:  Units for Diurnal emission factor must be ',
     &                    untkey(IDXMLT)(:strmin(untkey(IDXMLT))),
     &        'Check emission factor file: ',fname(:strmin(fname))
      write(IOWMSG,'(/,1X,2A,/,9X,2A)',ERR=9999)
     &    'ERROR:  Units for Diurnal emission factor must be ',
     &                    untkey(IDXMLT)(:strmin(untkey(IDXMLT))),
     &        'Check emission factor file: ',fname(:strmin(fname))
      goto 9999
c
 7010 continue
      write(IOWSTD,'(/,1X,2A,/,9X,2A)',ERR=9999)
     &    'ERROR:  Units for Tank Permeation emission factor must be ',
     &                    untkey(IDXMLT)(:strmin(untkey(IDXMLT))),
     &        'Check emission factor file: ',fname(:strmin(fname))
      write(IOWMSG,'(/,1X,2A,/,9X,2A)',ERR=9999)
     &    'ERROR:  Units for Tank Permeation emission factor must be ',
     &                    untkey(IDXMLT)(:strmin(untkey(IDXMLT))),
     &        'Check emission factor file: ',fname(:strmin(fname))
      goto 9999
c
 7011 continue
      write(IOWSTD,'(/,1X,2A,/,9X,2A)',ERR=9999)
     &    'ERROR:  Units for Hose Permeation emission factor must be ',
     &                    untkey(IDXMLT)(:strmin(untkey(IDXMLT))),
     &        'Check emission factor file: ',fname(:strmin(fname))
      write(IOWMSG,'(/,1X,2A,/,9X,2A)',ERR=9999)
     &    'ERROR:  Units for Hose Permeation emission factor must be ',
     &                    untkey(IDXMLT)(:strmin(untkey(IDXMLT))),
     &        'Check emission factor file: ',fname(:strmin(fname))
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
