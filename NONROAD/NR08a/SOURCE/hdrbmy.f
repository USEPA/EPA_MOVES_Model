C**** HDRBMY
c
      subroutine hdrbmy( ierr, iexev )
c
c-----------------------------------------------------------------------
c
c    writes header record to the output by-model-year data file
c
c    Argument description:
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c       iexev   I  switch between exhaust (1) and evap (2) output
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      08/06/98  --gwilson--  original development (started w/wrthdr)
c      06/04/04  -dfk--   modified to output exhaust and evap separatly
c                         due to introduction of evap techs
c      09/22/04  -dfk--   changed SOx to SO2 per version NR04
c      10/06/04  -dfk--   changed Refueling to Displacement
c      02/24/05  -cimulus-  added Activity
c      02/25/05  -cimulus-  added LF and HPAvg (exhaust only)
c      02/25/05  -cimulus-  added spacing to header row to facilitate
c                           viewing in a text editor, and changed
c                           'Tech Type' to 'TechType'
c      03/08/05  -cimulus-  changed 'Year' to 'MYr' and 'RuningLoss'
c                           to 'RunLoss'
c                           changed the order of the output of the
c                           columns to be consistent with the OUT file
c                           and the postprocessors
c      05/19/05  -cimulus-  output new retrofit fields, if running
c                           retrofit functionality
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
      integer*4    ierr
      integer*4    iexev
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c  COMMA   C   parameter for the comma character
c
      character*1 COMMA
c
      parameter( COMMA = ',' )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*400 line
      character*15  name(13), namesp(MXPOL)
      integer*4     ilen, i
      integer*4     io
      character*(MXSTR) iofl
      integer*4     tmpend
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- set I/O depending on exhaust/evap
c
      if(iexev.eq.1) then
        io=IOWBMY
        iofl=bmyfl
      else
        io=IOWEVBMY
        iofl=evbmyfl
      end if
c
c   --- initialize
c
      line = ' '
c
c  --- write the lines with first set of labels ---
c
      name(1) = 'Cnty'
      name(2) = 'SubR'
      name(3) = 'SCC'
      name(4) = 'HP'
      name(5) = 'TechType'
      name(6) = 'MYr'
      name(7) = 'Population'
      write(line,9000,ERR=7000) (name(i),COMMA,i=1,7)
c
c  --- write the lines with first set of labels ---
c
      namesp(IDXTHC) = 'THC-Exhaust'
      namesp(IDXCO)  = 'CO-Exhaust'
      namesp(IDXNOX) = 'NOx-Exhaust'
      namesp(IDXCO2) = 'CO2-Exhaust'
      namesp(IDXSOX) = 'SO2-Exhaust'
      namesp(IDXPM)  = 'PM-Exhaust'
      namesp(IDXCRA) = 'Crankcase'
      namesp(IDXSOK) = 'Hot-Soaks'
      namesp(IDXDIU) = 'Diurnal'
      namesp(IDXDIS) = 'Displacement'
      namesp(IDXSPL) = 'Spillage'
      namesp(IDXRLS) = 'RunLoss'
C      namesp(IDXRST) = 'RestngLoss'
      namesp(IDSTHC) = 'THC-Starts'
      namesp(IDSNOX) = 'NOx-Starts'
      namesp(IDSCO)  = 'CO-Starts'
      namesp(IDSPM)  = 'PM-Starts'
      namesp(IDSSOX) = 'SO2-Starts'
      namesp(IDSCO2) = 'CO2-Starts'
      namesp(IDXTKP) = 'TankPerm'
      namesp(IDXHOS) = 'HosePerm' ! sum of non-rec-marine hose and 3 rec-marine hoses
c
      if(iexev.eq.1) then
          ilen = strmin( line ) + 1
          write(line(ilen:),'(100(:15A,A))',ERR=7000) 
     &                         (namesp(i),COMMA,i=IDXTHC,IDXCRA)
      else
          ilen = strmin(line) + 1
          write(line(ilen:),'(15A,A)',ERR=7000) namesp(IDXSOK),COMMA
          ilen = strmin(line) + 1
          write(line(ilen:),'(15A,A)',ERR=7000) namesp(IDXDIU),COMMA
          ilen = strmin(line) + 1
          write(line(ilen:),'(15A,A)',ERR=7000) namesp(IDXDIS),COMMA
          ilen = strmin(line) + 1
          write(line(ilen:),'(15A,A)',ERR=7000) namesp(IDXSPL),COMMA
          ilen = strmin(line) + 1
          write(line(ilen:),'(15A,A)',ERR=7000) namesp(IDXRLS),COMMA
          ilen = strmin(line) + 1
          write(line(ilen:),'(15A,A)',ERR=7000) namesp(IDXTKP),COMMA
          ilen = strmin(line) + 1
          write(line(ilen:),'(15A,A)',ERR=7000) namesp(IDXHOS),COMMA
      endif
c
      ilen = strmin( line ) + 1
      name(8) = 'FuelCons.'
      name(9) = 'Activity'
      write(line(ilen:),'(100(:15A,A))',ERR=7000)
     &                         (name(i),COMMA,i=8,9)

      if(iexev.eq.1) then ! if exhaust
          ilen = strmin( line ) + 1
          name(10) = 'LF'
          name(11) = 'HPAvg'
          name(12) = 'FracRetro'
          name(13) = 'UnitsRetro'
          if( lrtrftfl ) then ! if running retrofit functionality
              tmpend = 13
          else
              tmpend = 11
          endif
          write(line(ilen:),'(100(15A:,A))',ERR=7000)
     &                             (name(i),COMMA,i=10,tmpend)
      end if
c
c   --- write the record to the output file ---
c
      write(io,'(A)',ERR=7000) line(:strmin(line))
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
     &                 'ERROR:  Writing to the by-model-year file ',
     &                                           iofl(:strmin(iofl))
      write(IOWMSG,'(/,1X,3A)') 
     &                 'ERROR:  Writing to the by-model-year file ',
     &                                           iofl(:strmin(iofl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(A5,A,A5,A,A10,A,A5,A,A10,A,A4,A,A15,A)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
