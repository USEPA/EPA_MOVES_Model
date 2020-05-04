C**** RDFIPS
c
      subroutine rdfips( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the FIPS.DAT file or equivalent as specified by the user in
c    the option file
c
c    Argument description.
c     Outputs:
c       ierr    I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      04/22/05  --cimulus--  original development
c      05/13/05  --cimulus--  added handling to ignore information
c                             outside of a delimited block of data lines
c      05/16/05  --cimulus--  increased size of strings in cntynm to 50
c      05/16/05  --cimulus--  end execution on state-not-found error
c      05/16/05  --cimulus--  log error if packet delimiter not found
c      07/11/05  --cimulus--  allow 4-digit FIPS codes, adding leading
c                             zero; error if less than 4 digits
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
c   strlen  I   returns the actual length of a string
c   strmin  I   returns the actual length of a string (minimum of 1)
c
      integer*4 strlen
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
      character*(MXSTR) line
      character*20      keywrd, keytmp
      integer*4         jerr, i, ctread
      integer*4         yrstrt, yrend, stidx, ctyidx
      character*50      tname
      character*5       tfips
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c     Reset global variables relating to county names and counts
      ctread = 0
      do 10 i=0, NSTATE+1
        idxcty(i) = 0
 10   continue
      do 20 i=1, NSTATE
        nconty(i) = 0
 20   continue
      idxcty(NSTATE+1) = 1
      nmcnty = 0
c
c     Set default value in fipsfl if none was specified
      if( .NOT. lfipsfl ) then
        fipsfl = 'DATA/ALLOCATE/FIPS.DAT'
        lfipsfl = .TRUE.
      endif
c
c     Open the FIPS.DAT file
      open(IOFIPS,file=fipsfl,ERR=7000,status='UNKNOWN')
      rewind(IOFIPS)
c
c     Call routine to find the /FIPS/ keyword
c
      keywrd = '/FIPS/'
      call fndkey(jerr, IOFIPS, keywrd )
      if( jerr .NE. ISUCES ) goto 7001
c
c     Start of loop reading each record
 100  continue
c
c       Read a record as a character string
        read(IOFIPS,8000,ERR=7002,END=999) line
        call spinit()
        ctread = ctread + 1
c
c       Search for the end of the packet
        keytmp = line(1:10)
        call lftjst( keytmp )
        call low2up( keytmp )
        if( keytmp .EQ. KEYEND ) goto 999
c
c       Parse the line of text into the field values
        yrstrt = 0
        yrend = 0
        tfips = '00000'
        tname = '00000'
        read(line,8001,err=9999) tfips, yrstrt, yrend, tname
c
c       Skip counties with blank FIPS or names
        call lftjst( tfips )
        call lftjst( tname )
        if( (tfips(1:1) .EQ. ' ') .OR. (tname(1:1) .EQ. ' ')) goto 100
c
c       Add leading zero to 4-digit FIPS codes; minimum 4 digits
        if( strlen(tfips) .EQ. 4 ) then
            tfips = '0'//tfips
        else if( strlen(tfips) .LT. 4 ) then
            goto 7003
        endif
c
c       Check year span for the county, skipping counties not present in the evaluation year
        if( ((yrstrt .GT. 0) .AND. (yrstrt .GT. iepyr))
     &      .OR.
     &      ((yrend .GT. 0) .AND. (yrend .LT. iepyr))) goto 100
c
c       Find the state
        stidx = 0
        do 110 i=1,NSTATE
            if( statcd(i)(1:2) .EQ. tfips(1:2) ) then
                stidx = i
                goto 115
            endif
 110    continue
 115    continue
c
c       If state not found, log error and return
        if( stidx .LE. 0 ) then
            goto 7004
        endif
c
c       The state has been found (in stidx), now store the county information
        nmcnty = nmcnty + 1
        ctyidx = nmcnty
        nconty(stidx) = nconty(stidx) + 1
        if( idxcty(stidx) .LE. 0 ) idxcty(stidx) = ctyidx
        cntynm(ctyidx) = tname
        fipcod(ctyidx) = tfips
c
c       Get the next record
        goto 100
c
c     All records have been read
 999  continue
      close(IOFIPS)
      idxcty(NSTATE+1) = 1 + nmcnty
c      write(IOWSTD,'(/,1X,A,I,A,I)',ERR=9999)
c     &          'Counties Read:',ctread,' Good: ',nmcnty
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
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fipsfl(:strmin(fipsfl))
      if ( lmsgfl ) then
         write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fipsfl(:strmin(fipsfl))
      endif
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR:  Reading FIPS file ',
     &                                           fipsfl(:strmin(fipsfl))
      if ( lmsgfl ) then
         write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR: Reading FIPS file ',
     &                                           fipsfl(:strmin(fipsfl))
      endif
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &           keywrd(:strmin(keywrd)),' packet of FIPS file.'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &           keywrd(:strmin(keywrd)),' packet of FIPS file.'
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999)
     &          'ERROR: Invalid FIPS (',tfips(:strmin(tfips)),
     &          '); must be at least 4 digits'
      if ( lmsgfl ) then
         write(IOWMSG,'(/,1X,3A)',ERR=9999)
     &             'ERROR: Invalid FIPS (',tfips(:strmin(tfips)),
     &             '); must be at least 4 digits'
      endif
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999)
     &          'ERROR: Unknown state for county FIPS: ',
     &          tfips(:strmin(tfips))
      if ( lmsgfl ) then
         write(IOWMSG,'(/,1X,2A)',ERR=9999)
     &             'ERROR: Unknown state for county FIPS: ',
     &             tfips(:strmin(tfips))
      endif
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(A)
 8001 format(A5,I5,I5,1X,A50)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
