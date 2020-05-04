C**** INTAMS
c
      subroutine intams( )
c
c-----------------------------------------------------------------------
c  
c   subroutine to perform all initializaton of the EPS2 AMS file
c   parameters for the NONROAD model
c
c    Argument description:
c        Outputs:
c           ierr   I   error code
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/07/97  --gwilson--  original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdusr.inc'
      include 'nonrdeqp.inc'
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 nodays(12)
c
c-----------------------------------------------------------------------
c    Data statments:
c-----------------------------------------------------------------------
c
      data nodays /31,28,31,30,31,30,31,31,30,31,30,31/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- set some variables to defaults ---
c
      itype = 'B'
      inetyp = 'AC'
c
c  --- set some variables that depend on user supplied parameters ---
c      
      irefyr = MOD(iepyr,100)
      ibasyr = irefyr

      cvtams = 1.0
      iperod = 'S'
      if( iprtyp .EQ. IDXANN ) then
          iperod = ' '
          if( ismtyp .EQ. IDXTYP) cvtams = 365.0 
          ibegdt = irefyr*1000000 + 010100
          ienddt = irefyr*1000000 + 123124
          if ( ismtyp .EQ. IDXTYP )  goto 6000
      else if( iprtyp .EQ. IDXSES ) then
          if( ismtyp .EQ. IDXTYP) cvtams = 91.21
          if( iseasn .EQ. IDXWTR ) then
             ibegdt = (irefyr-1)*1000000 + 120100
             ienddt = irefyr*1000000 + 022824
          else if( iseasn .EQ. IDXSPR ) then
             ibegdt = irefyr*1000000 + 030100
             ienddt = irefyr*1000000 + 053124
          else if( iseasn .EQ. IDXSUM ) then
             ibegdt = irefyr*1000000 + 060100
             ienddt = irefyr*1000000 + 083124
          else if( iseasn .EQ. IDXFAL ) then
             ibegdt = irefyr*1000000 + 090100
             ienddt = irefyr*1000000 + 113024
          endif
          if ( ismtyp .EQ. IDXTYP )  then
             if ( iseasn .EQ. IDXSUM .OR. iseasn .EQ. IDXWTR )  then
                cvtams = 1.0
                if ( iseasn .EQ. IDXSUM )  iperod = 'PO'
                if ( iseasn .EQ. IDXWTR )  iperod = 'PC'
             else
                goto 6000
             endif
          endif
      else if( iprtyp .EQ. IDXMTH ) then
          if( ismtyp .EQ. IDXTYP) cvtams = FLOAT( nodays(imonth) )
          ibegdt = irefyr*1000000 + imonth*10000 + 100
          ienddt = irefyr*1000000 + imonth*10000 + 
     &                                     nodays(imonth)*100 + 24
          if ( ismtyp .EQ. IDXTYP )  goto 6000
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c    Warning messages:
c-----------------------------------------------------------------------
c
 6000 continue
      write(IOWMSG,'(/,1X,A/,A/,A,A)',ERR=9999)
     & 'WARNING:  The output EPS file contains period total emissions.',
     & '          This does NOT correspond to the user specified ',
     & '          typical day emissions written to the NONROAD ',
     &           'output file.'

      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
