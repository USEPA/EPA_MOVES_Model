C**** RDALO
c
      subroutine rdalo( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the regression coefficients for the spatial allocation data 
c    to be used by the EPA nonroad model.  Since the data is not region 
c    dependent, it is read one time and stored in global arrays.
c
c    Argument declaration.
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/01/93  --gmw--  original development
c      05/18/05  --cimulus--  pass true for new chkasc parameter
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdalo.inc'
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
c   fndchr  I   returns the index of string in array of strings
c   chkasc  L   returns true if SCC code is needed for current run
c
      integer*4 strmin
      integer*4 fndchr
      logical*4 chkasc
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(2*MXSTR) line
      character*20        keywrd, keyin
      character*10        asctmp, indtmp(MXCOEF)
      integer*4           jerr, irec, istr, idxunq, i
      real*4              coftmp(MXCOEF), sumcoef
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 1
      nalorc = 0
c
c   --- open the file ---
c
      open(IORALO,file=alofl,ERR=7007,status='UNKNOWN')
      rewind(IORALO)
c
c  --- call routine to find the /ALLOC XREF/ keyword ---
c
      keywrd = '/ALLOC XREF/'
      call fndkey( jerr, IORALO, keywrd )
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read a record as a character string --- 
c
  111 continue
      read(IORALO,8000,ERR=7001,END=7002) line
      irec = irec + 1
      call spinit()
c
c   --- check for /END/ keyword ---
c
      keyin = line(:20)
      call low2up( keyin )
      call lftjst( keyin )
      if( keyin .EQ. KEYEND ) goto 222
c
c   --- check to make sure this SCC will need to be used 
c       (could be a global code), if not skip it and the next
c       line  ----
c
      asctmp = line(1:10)
      if( .NOT. chkasc( asctmp, .TRUE. ) ) then
          read(IORALO,8000,ERR=7001,END=7002) line
          goto 111
      endif
c
c   --- parse the line and load all of the data ----
c
      istr = 1
      sumcoef = 0.
      do 10 i=1,MXCOEF
         istr = istr + 10
         if( line(istr:istr+9 ) .EQ. '          ' ) goto 333
         read(line(istr:istr+9),'(F10.0)',ERR=7003) coftmp(i)
         sumcoef = sumcoef + coftmp(i)
   10 continue
c
c   ---- check to make sure the coefficients add to 1 ----
c
 333  continue
      if ( ABS(sumcoef-1.0) .GT. 0.001 ) goto 7008
c
c   --- read the next line which should be the same SCC but has
c       the indicator codes ---
c
      read(IORALO,8000,ERR=7001,END=7002) line
      irec = irec + 1             
c
c   --- make sure the SCC code is the same ----
c
      if( line(1:10) .NE. asctmp ) goto 7004
c
c   --- get the indicator codes and left justify ---
c
      istr = 1
      do 20 i=1,MXCOEF
         istr = istr + 10
         indtmp(i) = line(istr:istr+9)
         call lftjst( indtmp(i) )
   20 continue
c
c  --- load the data into global arrays ----
c
      nalorc = nalorc + 1
      if( nalorc .GT. MXALO ) goto 7005 
      ascalo(nalorc) = asctmp
      do 30 i=1,MXCOEF
          coeffs(nalorc,i) = coftmp(i)
          indcod(nalorc,i) = indtmp(i)(1:3)
c
c  --- add codes to unique list of codes ---
c
          idxunq = fndchr( indtmp(i)(1:3), 3, alocod, nalocd )
          if( idxunq .LE. 0 ) then
              nalocd = nalocd + 1
              if( nalocd .GT. MXCODE ) goto 7006
              alocod(nalocd) = indtmp(i)(1:3)
          endif
  30  continue
c
c  --- get the next record ---
c
      goto 111
c
c   --- finished reading file, close it and return ---
c
  222 continue
      close(IORALO)
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &         keywrd(:strmin(keywrd)),' packet of allocation file ',
     &                                            alofl(:strmin(alofl))
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Cannot find ',
     &         keywrd(:strmin(keywrd)),' packet of allocation file ',
     &                                            alofl(:strmin(alofl))
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,4A,I5)',ERR=9999) 'ERROR:  Reading file ',
     &               'containing allocation regression coefficients ',
     &                        alofl(:strmin(alofl)),' at record ',irec
      write(IOWMSG,'(/,1X,4A,I5)',ERR=9999) 'ERROR:  Reading file ',
     &               'containing allocation regression coefficients ',
     &                        alofl(:strmin(alofl)),' at record ',irec
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999)
     &         'ERROR:  Unexpected end-of-file reached reading ',
     &                      'allocation file ',alofl(:strmin(alofl))
      write(IOWMSG,'(/,1X,4A)',ERR=9999)
     &         'ERROR:  Unexpected end-of-file reached reading ',
     &                      'allocation file ',alofl(:strmin(alofl))
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,A,/,9X,3A,I5,/,9X,2A)',ERR=9999)
     &         'ERROR:  Reading allocation regression coefficients ',
     &          'in file ', alofl(:strmin(alofl)),' at record ',irec,
     &                              'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,A,/,9X,3A,I5,/,9X,2A)',ERR=9999)
     &         'ERROR:  Reading allocation regression coefficients ',
     &          'in file ', alofl(:strmin(alofl)),' at record ',irec,
     &                              'Line read: ',line(:strmin(line))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,A,/,9X,2A,/,9X,4A)',ERR=9999) 
     &       'ERROR:  Mismatch of SCC codes',
     &       'in file containing regression coefficients ',
     &        alofl(:strmin(alofl)),'Codes: ',asctmp,' ',line(1:10)
      write(IOWMSG,'(/,1X,A,/,9X,2A,/,9X,4A)',ERR=9999) 
     &       'ERROR:  Mismatch of SCC codes',
     &       'in file containing regression coefficients ',
     &        alofl(:strmin(alofl)),'Codes: ',asctmp,' ',line(1:10)
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,A,I6,/,9X,2A)',ERR=9999) 
     &          'ERROR:  Number of SCC codes exceeds max: ', MXALO,
     &           'in spatial allocation cross reference file: ',
     &           alofl(:strmin(alofl))
      write(IOWMSG,'(/,1X,A,I6,/,9X,2A)',ERR=9999) 
     &          'ERROR:  Number of SCC codes exceeds max: ', MXALO,
     &           'in spatial allocation cross reference file: ',
     &           alofl(:strmin(alofl))
      goto 9999
c
 7006 continue
      write(IOWSTD,'(/,1X,A,I6,/,9X,2A)',ERR=9999) 
     &      'ERROR:  Number of unique indicator codes exceeds max ',
     &       MXCODE,'in file containing regression coefficents ',
     &       alofl(:strmin(alofl))
      write(IOWMSG,'(/,1X,A,I6,/,9X,2A)',ERR=9999) 
     &      'ERROR:  Number of unique indicator codes exceeds max ',
     &       MXCODE,'in file containing regression coefficents ',
     &       alofl(:strmin(alofl))
      goto 9999
c
 7007 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           alofl(:strmin(alofl))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           alofl(:strmin(alofl))
      goto 9999
c
 7008 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Spatial indicator ',
     &                         'coefficients for SCC code: ', asctmp,
     &                                             ' do not add to 1.'
      write(IOWSTD,'(10X,2A)',ERR=9999) 'Check file: ',
     &                                          alofl(:strmin(alofl))
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Spatial indicator ',
     &                         'coefficients for SCC code: ', asctmp,
     &                                             ' do not add to 1.'
      write(IOWMSG,'(10X,2A)',ERR=9999) 'Check file: ',
     &                                          alofl(:strmin(alofl))
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
