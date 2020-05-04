C**** rdday
c
      subroutine rdday( ierr, iounit, fname )
c
c-----------------------------------------------------------------------
c
c    reads the daily temperatures (max, min, avg) and fuel RVP values
c    and stores the data in common to be used by the EPA Nonroad model.
c
c    Argument declaration.
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c       iounit  I  unit number of file to read
c       fname   C  name of file to be read
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      06/15/04  --dfk--  original development
c      06/25/04  --dfk--  added daily RVP
c      09/22/04  --dfk--  added MXDAYS and MXFIPS
c      01/21/05  --dfk--  added trap to catch files with no data
c      02/03/05  --dfk--  changed code so that all years have 365 days
c                         (data for Feb 29 will be ignored)
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
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4         ierr
      integer*4         iounit
      character*(MXSTR) fname
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
      character*20        keywrd, keytmp
      character*4         typtmp
      integer*4           irec, jerr, ncount, i
      real*4              valtmp(31)
      integer*4           numday(12)
      integer*4           fiptmp, jday, jtype, month
c
      data numday/0,31,59,90,120,151,181,212,243,273,304,334/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 1
      ncount = 0
c
c   --- initialize the temperature and RVP values to those input
c       in the /OPTIONS/ packet ---
c
      do fiptmp=0,MXFIPS
      do i=1,MXDAYS
         daytmp(i,1,fiptmp) = tempmx
         daytmp(i,2,fiptmp) = tempmn
         daytmp(i,3,fiptmp) = amtemp
         dayrvp(i,  fiptmp) = fulrvp
      end do
      end do
c
c   --- call routine to find the /DAYTEMPRVP/ keyword ----
c
      keywrd = '/DAYTEMPRVP/'
      call fndkey(jerr, iounit, keywrd )
      if( jerr .EQ. IEOF ) then
          ierr = ISKIP 
          goto 9999
      endif
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read a record as a character string --- 
c
  111 continue
      read(iounit,8000,ERR=7001,END=222) line
      call spinit()
      irec = irec + 1
c
c   --- search for the end of the packet ---
c
      keytmp = line(1:10)
      call lftjst( keytmp )
      call low2up( keytmp )
      if( keytmp .EQ. KEYEND ) goto 222
c
c   --- determine fips region ---
c
      read(line(1:2),*,ERR=7002) fiptmp
      if(fiptmp.lt.0.or.fiptmp.gt.MXFIPS) goto 7002
c
c   --- determine type of data (tmax, tmin, tavg or RVP)
c
      typtmp=line(7:10)
      call lftjst( typtmp )
      call low2up( typtmp )
c
      if(typtmp.eq.'TMAX') then
       jtype=1
      else if (typtmp.eq.'TMIN') then
       jtype=2
      else if (typtmp.eq.'TAVG') then
       jtype=3
      else if (typtmp.eq.'RVP') then
       jtype=4
      else
       goto 7002
      end if
c
c   --- determine month ---
c
      read(line(12:13),*,ERR=7002) month
c
c   --- read data values ---
c
      read(line(14:199),*,ERR=7002) valtmp
c
c   --- place data into arrays ---
c   --- Note: per EPA, all years have 365 days. This code ensures
c       that Feb 29, even if available in the file, will be ignored.
c
      do i=1,31
      if(valtmp(i).le.-99.) cycle
      jday=i+numday(month)
      if(jtype.le.3) then
        daytmp(jday,jtype,fiptmp)=valtmp(i)
      else
        dayrvp(jday,fiptmp)=valtmp(i)
      endif
      end do
c
      call spinit()
c
      ncount = ncount + 1
c
c   --- read the next record ---
c
      goto 111
c
c   --- finished reading file ---
c
  222 continue
c
c   --- check that some data was read
c
      if(ncount.eq.0) goto 7004
c
c   --- validate temperatures (Max>=Min, Max>=Ambient, Min<=Ambient)
c
      do fiptmp=0,MXFIPS
      do i=1,MXDAYS
      if(daytmp(i,1,fiptmp).lt.daytmp(i,2,fiptmp) .or.
     *   daytmp(i,1,fiptmp).lt.daytmp(i,3,fiptmp) .or.
     *   daytmp(i,3,fiptmp).lt.daytmp(i,2,fiptmp)) goto 7003
      end do
      end do
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,2A)',ERR=9999) 
     &'ERROR:  Reading daily temp/RVP file ',fname(:strmin(fname))
      write(IOWMSG,'(/,1X,2A)',ERR=9999) 
     &'ERROR:  Reading daily temp/RVP file ',fname(:strmin(fname))
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &   keywrd(:strmin(keywrd)),' packet of daily temp/RVP file.'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Reading ',
     &   keywrd(:strmin(keywrd)),' packet of daily temp/RVP file.'
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1x,4A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &   keywrd(:strmin(keywrd)),' packet of daily temp/RVP file.',
     &   'Invalid temperatures found (Max<Min, Max<Ambient,',
     &   ' and/or Min>Ambient).',' Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1x,4A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &   keywrd(:strmin(keywrd)),' packet of daily temp/RVP file.',
     &   'Invalid temperatures found (Max<Min, Max<Ambient,',
     &   ' and/or Min>Ambient).',' Line read: ',line(:strmin(line))
c
 7003 continue
      write(IOWSTD,'(/,1x,3A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &   keywrd(:strmin(keywrd)),' packet of daily temp/RVP file.',
     &                              'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1x,3A,/,9X,2A)',ERR=9999) 'ERROR:  Reading ',
     &   keywrd(:strmin(keywrd)),' packet of daily temp/RVP file.',
     &                              'Line read: ',line(:strmin(line))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 
     &             'ERROR:  Data in ',keywrd(:strmin(keywrd)),
     &' packet of daily temp/RVP file contains no data or',
     &' incorrect number of values.'
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 
     &             'ERROR:  Data in ',keywrd(:strmin(keywrd)),
     &' packet of daily temp/RVP file contains no data or',
     &' incorrect number of values.'
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





