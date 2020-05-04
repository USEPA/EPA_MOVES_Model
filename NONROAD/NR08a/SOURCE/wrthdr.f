C**** WRTHDR
c
      subroutine wrthdr( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the header data of the data file which will be read by
c    the reporting utility.
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
c      10/19/98  --mjimenez -- included spillage emissions as separate
c                              entry
c      05/11/04  --dfk--  added tank and hose permeation
c      09/21/04  --dfk--  changed order of emission output
c      09/22/04  --dfk--  changed SOx to SO2 per NR04
c      01/11/05  --dfk--  removed duplicate entries in month selection section
c      02/25/05  -cimulus-  added LF and HPAvg
c      02/25/05  -cimulus-  added spacing to header row to facilitate
c                           viewing in a text editor, changed 'County'
c                           to 'Cnty', and changed 'SubRegion' to 'SubR'
c      03/08/05  -cimulus-  changed 'RuningLoss' to 'RunLoss'
c                           changed the order of the output of the
c                           fields to be consistent with the BMX
c                           and BMV files and the postprocessors
c      05/19/05  -cimulus-  output new retrofit fields, if running
c                           retrofit functionality; changed 'Fuel-Cons.'
c                           to 'FuelCons.' and 'Displacmnt' to
c                           'Displacement' for consistency with the
c                           by-model-year output files; made sure the
c                           first row output has correct number of fields
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdusr.inc'
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
c  strmin   I   returns the length of a string (minimum of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c  COMMA   C   parameter for the comma character
c  COMMA   C   parameter for the quote  character
c
      character*1 COMMA
      character*1 QUOTE
c
      parameter( COMMA = ',' )
      parameter( QUOTE = '"' )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*80 perstr
      character*40 cdate
      character*15 unitmp
      character*15 namesp(MXPOL), name(11)
      integer*4    ilen, idum, i
      real*4       rdum(MXPOL)
      integer*4    order(IDXRLS)
c      data order/1,2,3,4,5,6,7,11,8,12,13,14,9,10/
      data order/1,2,3,4,5,6,7,14,8,15,16,17,9,10,11,12,13/
      integer*4    tmpend
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c    --- initialize dummy array ---
c
      do 10 i=1,MXPOL
         rdum(i) = 0.
   10 continue
      idum = 0
c
c   --- call routine to get the data and time ---
c
      call getime( cdate )
c
c   --- write a record to drive the type of data to come ---
c
      if( lrtrftfl ) then ! if running retrofit functionality
          tmpend = 7
      else
          tmpend = 5
      endif
      write(IOWDAT,9000,ERR=7000) ' ', COMMA, ' ', COMMA, 
     &                            ' ', COMMA, idum, COMMA, 
     &                            (rdum(i), COMMA,i=IDXTHC,IDXHOS),
     &                            (rdum(i), COMMA,i=IDXSOK,IDXRLS),
     &                            (rdum(1), COMMA,i=1,tmpend)
c
c   --- write the version number and date and time ---
c
      write(IOWDAT,'(100(:A))',ERR=7000) QUOTE, PROGNM, QUOTE
      write(IOWDAT,'(100(:A))',ERR=7000) QUOTE, VERSON, QUOTE
      write(IOWDAT,'(100(:A))',ERR=7000) QUOTE, cdate(:strmin(cdate)), 
     &                                                     QUOTE
c
c   --- write the titles ---
c
      write(IOWDAT,'(3A)',ERR=7000) QUOTE, title1(:strmin(title1)),QUOTE
      write(IOWDAT,'(3A)',ERR=7000) QUOTE, title2(:strmin(title2)),QUOTE
c
c   --- write the name of options file used ---
c
      write(IOWDAT,'(2A)',ERR=7000) 'Options file used: ',
     &                                           sysfl(:strmin(sysfl))
c
c   --- build the period string from the user specified parameters ---
c
      if( ismtyp .EQ. IDXTOT ) then
        perstr = 'Total for '
      else if( ismtyp .EQ. IDXTYP ) then
        if( iday .EQ. IDXWKD ) then
           perstr = 'Typical weekday for '
        else if( iday .EQ. IDXWKE ) then
           perstr = 'Typical weekend day for '
        endif
        unitmp = 'Tons/Day'
      endif
c
c   --- add part of string that tells which time period was done ---
c
      ilen = strmin(perstr) + 2
      if( iprtyp .EQ. IDXANN ) then
         if( ismtyp .EQ. IDXTOT ) unitmp = 'Tons/Year'
         write(perstr(ilen:),'(A,I4)') 'year: ',iepyr
      else if( iprtyp .EQ. IDXSES ) then
         if( ismtyp .EQ. IDXTOT ) unitmp = 'Tons/Season'
         if( iseasn .EQ. IDXWTR ) then
             write(perstr(ilen:),'(A,I4)') 'Winter Season, ',iepyr
         else if( iseasn .EQ. IDXSPR ) then
             write(perstr(ilen:),'(A,I4)') 'Spring Season, ',iepyr
         else if( iseasn .EQ. IDXSUM ) then
             write(perstr(ilen:),'(A,I4)') 'Summer Season, ',iepyr
         else if( iseasn .EQ. IDXFAL ) then
             write(perstr(ilen:),'(A,I4)') 'Fall Season, ',iepyr
         endif
      else if( iprtyp .EQ. IDXMTH) then
         if( ismtyp .EQ. IDXTOT ) unitmp = 'Tons/Month'
         if( imonth .EQ. IDXJAN ) then
             write(perstr(ilen:),'(A,I4)') 'January, ',iepyr
         else if( imonth .EQ. IDXFEB ) then
             write(perstr(ilen:),'(A,I4)') 'February, ',iepyr
         else if( imonth .EQ. IDXMAR ) then
             write(perstr(ilen:),'(A,I4)') 'March, ',iepyr
         else if( imonth .EQ. IDXAPR ) then
             write(perstr(ilen:),'(A,I4)') 'April, ',iepyr
         else if( imonth .EQ. IDXMAY ) then
             write(perstr(ilen:),'(A,I4)') 'May, ',iepyr
         else if( imonth .EQ. IDXJUN ) then
             write(perstr(ilen:),'(A,I4)') 'June, ',iepyr
         else if( imonth .EQ. IDXJUL ) then
             write(perstr(ilen:),'(A,I4)') 'July, ',iepyr
         else if( imonth .EQ. IDXAUG ) then
             write(perstr(ilen:),'(A,I4)') 'August, ',iepyr
         else if( imonth .EQ. IDXSEP ) then
             write(perstr(ilen:),'(A,I4)') 'September, ',iepyr
         else if( imonth .EQ. IDXOCT ) then
             write(perstr(ilen:),'(A,I4)') 'October, ',iepyr
         else if( imonth .EQ. IDXNOV ) then
             write(perstr(ilen:),'(A,I4)') 'November, ',iepyr
         else if( imonth .EQ. IDXDEC ) then
             write(perstr(ilen:),'(A,I4)') 'December, ',iepyr
         endif
      endif
c
c  --- write the period string to the output file ---
c
      write(IOWDAT,'(3A)',ERR=7000) QUOTE, perstr(:strmin(perstr)),QUOTE
c
c  --- write the period string to the output file ---
c
      write(IOWDAT,'(3A)',ERR=7000) QUOTE, unitmp(:strmin(unitmp)),QUOTE
c
c  --- intialize the field name strings ----
c
      name(1) = 'Cnty'
      name(2) = 'SubR'
      name(3) = 'SCC'
      name(4) = 'HP'
      name(5) = 'Population'
      name(6) = 'FuelCons.'
      name(7) = 'Activity'
      name(8) = 'LF'
      name(9) = 'HPAvg'
      name(10) = 'FracRetro'
      name(11) = 'UnitsRetro'
c
c  --- names of species ---
c
      namesp(IDXTHC) = 'THC-Exhaust'
      namesp(IDXNOX) = 'NOx-Exhaust'
      namesp(IDXCO)  = 'CO-Exhaust'
      namesp(IDXPM)  = 'PM-Exhaust'
      namesp(IDXCRA) = 'Crankcase'
      namesp(IDXDIU) = 'Diurnal'
      namesp(IDXDIS) = 'Displacement'
      namesp(IDXSPL) = 'Spillage'
      namesp(IDXSOK) = 'Hot-Soaks'
      namesp(IDXTKP) = 'TankPerm'
      namesp(IDXHOS) = 'HosePerm'
      namesp(IDXRLS) = 'RunLoss'
c      namesp(IDXRST) = 'RestngLoss'
      namesp(IDXSOX) = 'SO2-Exhaust'
      namesp(IDXCO2) = 'CO2-Exhaust'
      namesp(IDSTHC) = 'THC-Starts'
      namesp(IDSNOX) = 'NOx-Starts'
      namesp(IDSCO)  = 'CO-Starts'
      namesp(IDSPM)  = 'PM-Starts'
      namesp(IDSSOX) = 'SO2-Starts'
      namesp(IDSCO2) = 'CO2-Starts'
c
c  --- write the lines with species names and units ---
c
      
      if( lrtrftfl ) then ! if running retrofit functionality
          tmpend = 11
      else
          tmpend = 9
      endif
      write(IOWDAT,9001,ERR=7000) 
     &                   (name(i),COMMA,i=1,5), 
     &                   (namesp(order(i)),COMMA,i=IDXTHC,IDXRLS-3),
     &                   (name(i),COMMA,i=6,tmpend)
c
c   --- set error code to succes and return ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,3A)') 
     &                 'ERROR:  Writing to the output data file ',
     &                                           datfl(:strmin(datfl))
      write(IOWMSG,'(/,1X,3A)') 
     &                 'ERROR:  Writing to the output data file ',
     &                                           datfl(:strmin(datfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(6A,I5,A,30(:,F3.0,A))
 9001 format(A5,A,A5,A,A10,A,A5,A,100(:A15,A))
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

