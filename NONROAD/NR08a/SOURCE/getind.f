C**** GETIND
c
      subroutine getind( ierr, valout, indin, fipin, subin, iyrin )
c
c-----------------------------------------------------------------------
c
c    This routine finds the record in the indicator file that 
c    matches the data in the argument list.  It marches through the file 
c    looking for the correct indicator code.  Once at the correct code
c    it continues the march until it reaches the correct fip/subregion 
c    code.  It then gets the data surrounding the year and 
c    chooses the closest year prior to or equal to the eval year; or if
c    none, then the earliest year after the eval year. 
c    to get the exact year.
c
c    Argument declaration.
c     Outputs:
c       ierr    I  error code
c       valout  R  indicator value
c     Inputs:
c       indin   C  indicator code to search for
c       fipin   C  FIPS code to search for 
c       subin   C  subregion code to search for 
c       iyrin   I  evaluation year to determine geo allocations for
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/01/93  --gmw--  original development
c      07/13/05  -epa-  remove interpolation; use closest/earlier year.
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4         ierr
      real*4            valout
      character*3       indin
      character*5       fipin
      character*5       subin
      integer*4         iyrin
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
      character*(MXSTR) line
      character*5       fiptmp, subtmp
      character*3       indtmp
      integer*4         irec, iyrtmp, iyrlow, iyrhi
      real*4            valtmp, valhi, vallow
c     real*4            slope
      logical*4         lpass, lyrlow, lyrhi
c
c-----------------------------------------------------------------------
c   Save and data statements:
c-----------------------------------------------------------------------
c
      save irec
      data irec /1/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      lpass = .FALSE.
      iyrlow = -9
      iyrhi = 99999999
      lyrlow = .FALSE.
      lyrhi = .FALSE.
c
c   --- read a record as a character string --- 
c
  111 continue
      read(IOSIND,8000,ERR=7000,END=333) line
      irec = irec + 1
c
c   --- get the indicator code, if it is not the one take appropriate
c       action to get to there ----
c
      indtmp = line(1:3)
      if( indin .GT. indtmp ) then
          goto 111
      else if( indin .LT. indtmp ) then
          if( lpass ) then
              ierr = IEOF
              goto 9999
          endif
          lpass = .TRUE.
          rewind(IOSIND)
          goto 111
      endif
      backspace(IOSIND,ERR=222)
c
c   --- read the next record with this code ---
c
  222 continue
      read(IOSIND,8000,ERR=7000,END=333) line
      irec = irec + 1
c
c   --- if the indicator code changed then we are done with the county,
c       so calculate the indicator value ---
c
      indtmp = line(1:3)
      if( indin .NE. indtmp ) then
          if( .NOT. lyrlow .AND. .NOT. lyrhi ) then
              if( lpass ) then
                  ierr = IEOF 
                  goto 9999
              endif
              lpass = .TRUE.
              rewind(IOSIND)
              goto 111
          else
             if( lyrlow .AND. lyrhi ) then
c               slope = (valhi - vallow)/FLOAT(iyrhi - iyrlow)
c               valout = vallow + slope * FLOAT(iyrin - iyrlow)
c
c   --- don't interpolate, just use earlier year ---
c
                 valout = vallow
             else if( lyrlow ) then
                valout = vallow
             else if( lyrhi ) then
                valout = valhi
             endif
             ierr = ISUCES
             backspace(IOSIND,ERR=9999)
             goto 9999
         endif
      endif
c
c   --- parse the line and load all of the data ----
c
      fiptmp = line(6:10)
      subtmp = line(11:15)
      call lftjst(subtmp)
      read(line(16:20),'(I5)',ERR=7001) iyrtmp
      read(line(21:40),'(F20.0)',ERR=7001) valtmp
c
c   --- check the FIPS codes, if we haven't reached it yet keep reading ---
c
      if( fipin .GT. fiptmp ) then
           goto 222
c
c   --- FIPS code is greater than the one we want but we haven't found it
c       yet so rewind and go look some more ---
c
      else if( fipin .LT. fiptmp ) then
          if( .NOT. lyrlow .AND. .NOT. lyrhi ) then
              if( lpass ) then
                  ierr = IEOF 
                  goto 9999
              endif
              lpass = .TRUE.
              rewind(IOSIND)
              goto 111
c
c   --- new FIPS code and we have our data so calculate the value and
c       return ---
c
          else
              if( lyrlow .AND. lyrhi ) then
c                slope = (valhi - vallow)/FLOAT(iyrhi - iyrlow)
c                valout = vallow + slope * FLOAT(iyrin - iyrlow)
c
c   --- don't interpolate, just use earlier year ---
c
                 valout = vallow
              else if( lyrlow ) then
                 valout = vallow
              else if( lyrhi ) then
                 valout = valhi
              endif
              ierr = ISUCES
              backspace(IOSIND,ERR=9999)
              goto 9999
          endif
      endif
c
c   --- FIPS code matches now do checks with subregion code ---
c
c   --- check the subregion codes, if we haven't reached it yet keep reading ---
c
      if( subin .GT. subtmp ) then
           goto 222
c
c   --- subregion code is greater than the one we want but we haven't 
c       found it yet so rewind and go look some more ---
c
      else if( subin .LT. subtmp ) then
          if( .NOT. lyrlow .AND. .NOT. lyrhi ) then
              if( lpass ) then
                  ierr = IEOF 
                  goto 9999
              endif
              lpass = .TRUE.
              rewind(IOSIND)
              goto 111
c
c   --- new subregion code and we have our data so calculate
c       the value and return ---
c
          else
              if( lyrlow .AND. lyrhi ) then
c                slope = (valhi - vallow)/FLOAT(iyrhi - iyrlow)
c                valout = vallow + slope * FLOAT(iyrin - iyrlow)
c
c   --- don't interpolate, just use earlier year ---
c
                 valout = vallow
              else if( lyrlow ) then
                 valout = vallow
              else if( lyrhi ) then
                 valout = valhi
              endif
              ierr = ISUCES
              backspace(IOSIND,ERR=9999)
              goto 9999
          endif
      endif
c
c   --- all of the data matches, check for the years ---
c
      if( iyrtmp .LE. iyrin ) then
          if( iyrtmp .GT. iyrlow ) then
              lyrlow = .TRUE.
              iyrlow = iyrtmp
              vallow = valtmp
          endif
      else if( iyrtmp .GT. iyrin ) then
          if( iyrtmp .LT.  iyrhi ) then
              lyrhi = .TRUE.
              iyrhi = iyrtmp
              valhi = valtmp
          endif
      endif
c
c   --- get the next record ---
c
      goto 222
c
c   --- entire file processed, if we've made a pass already then
c       set error code  and return, otherwise, take another pass ----
c
 333  continue
      if( lpass ) then
          ierr = IEOF
          goto 9999
      else
          lpass = .TRUE.
          rewind(IOSIND)
          goto 111
      endif 
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,2A,/,9X,A,I10)',ERR=9999) 
     &               'ERROR:  Reading sorted spatial indicator file ',
     &                        indfl(:strmin(indfl)),'at record ',irec
      write(IOWMSG,'(/,1X,2A,/,9X,A,I10)',ERR=9999) 
     &               'ERROR:  Reading sorted spatial indicator file ',
     &                        indfl(:strmin(indfl)),'at record ',irec
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,2A,/,9X,A,I10,/9X,2A)',ERR=9999) 
     &                     'ERROR:  Reading spatial indicator file ',
     &                        indfl(:strmin(indfl)),'at record ',irec,
     &                               'Line read: ',line(:strmin(line))
      write(IOWMSG,'(/,1X,2A,/,9X,A,I10,/9X,2A)',ERR=9999) 
     &                     'ERROR:  Reading spatial indicator file ',
     &                        indfl(:strmin(indfl)),'at record ',irec,
     &                               'Line read: ',line(:strmin(line))
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
