C**** WRTDAT
c
      subroutine wrtdat( ierr, fipin, subin, ascin, hpin, popin, actin,
     &                   fulin, ldfcin, hpavin, fracretro, unitsretro,
     &                   emiss )
c
c-----------------------------------------------------------------------
c
c    writes one data record to the data file which will be read by
c    the reporting utility.
c
c    Argument description:
c     Outputs:
c       ierr        I  error flag
c     Inputs:
c       fipin       C  FIPS code
c       subin       C  subregion code
c       ascin       C  SCC code
c       hpin        R  HP category
c       popin       R  population
c       actin       R  activity
c       fulin       R  fuel consumption
c       ldfcin      R  load factor
c       hpavin      R  HP average
c       fracretro   R  fraction of population that was retrofitted
c                      (ignored if not running retrofit functionality)
c       unitsretro  R  number of units that were retrofitted (ignored
c                      if not running retrofit functionality)
c       emiss       R  array of emissions
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      06/01/97  --gmw--  original development
c      05/21/98  --gwilson-- temporarily writes zeros for RESTINGLOSS,
c                            RUNNINGLOSS, and HOTSOAK automatically
c      10/19/98  --mjimenez-- included spillage emissions as separate
c                             entry
c      09/21/04  --dfk--      changed order of emission output
c      11/15/04  --dfk--      changed to add 3-hose rec-marine and 
c                             non-rec-marine hose into generic hose 
c      02/25/05  -cimulus-    added ldfcin and hpavin (new parameters
c                             and output)
c      03/08/05  -cimulus-    changed the order of the output of the
c                             columns to be consistent with the BMX
c                             and BMV files and the postprocessors
c      05/20/05  -cimulus-    output new retrofit fields, if running
c                             retrofit functionality
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
      character*5  fipin
      character*5  subin
      character*10 ascin
      real*4       hpin
      real*4       popin
      real*4       actin
      real*4       fulin
      real*4       ldfcin
      real*4       hpavin
      real*4       fracretro
      real*4       unitsretro
      real*4       emiss(MXPOL)
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c  strmin   I   returns the length of a string (min of 1)
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
      integer*4     ilen, i, j
      integer*4     order(IDXRLS)
      real*4        tmpemis(IDXRLS)
c      data order/1,2,3,4,5,6,7,11,8,12,13,14,9,10/
      data order/1,2,3,4,5,6,7,14,8,15,16,17,9,10,11,12,13/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      line = ' '
COLDc
COLDc   --- automatically set RESTINGLOSS, RUNNINGLOSS, and HOTSOAK
COLDc       to zero ----
COLDc
COLD      emiss(IDXSOK) = 0.
COLD      emiss(IDXRLS) = 0.
COLD      emiss(IDXRST) = 0.
c
c   --- set up temporary emission array, adding all hose permeations together
c
      tmpemis=0.
c
      do j=IDXTHC,IDXRLS
        if(j.ge.IDXHOS.and.j.le.IDXVNT) then
          tmpemis(IDXHOS)=tmpemis(IDXHOS)+emiss(j)
        else
        tmpemis(j)=emiss(j)
      end if
      end do
c
c   --- write the key identifiers ---
c
      write(line,9000,ERR=7000) fipin, COMMA, subin, COMMA, 
     &                                   ascin, COMMA, INT(hpin)
      ilen = strmin(line) + 1
      if( popin .GE. 0 ) then
          write(line(ilen:),9001,ERR=7000) COMMA, popin
      else 
          write(line(ilen:),9002,ERR=7000) COMMA
      endif
c
c   --- calculate SPILLAGE as a function of REFUELING ---
c
c     if( emiss(IDXSPL) .GT. 0. ) then
c        if( emiss(IDXDIS) .GT. 0. ) then
c            emiss(IDXDIS) = emiss(IDXDIS) + emiss(IDXSPL)
c        else
c            emiss(IDXDIS) = emiss(IDXSPL)
c        endif
c     endif
      do 10 j=IDXTHC,IDXRLS
         i = order(j)
         if(i.ge.IDXNCK.and.i.le.IDXVNT) cycle ! skip rec-marine, they're included in hose
         ilen = strmin(line) + 1
         if( tmpemis(i) .GE. 0. ) then
             write(line(ilen:),9001,ERR=7000) COMMA,tmpemis(i)
         else 
             write(line(ilen:),9002,ERR=7000) COMMA
         endif
   10 continue
c     do 20 i=IDXRLS,IDXRLS
c        ilen = strmin(line) + 1
c        if( emiss(i) .GE. 0. ) then
c            write(line(ilen:),9001,ERR=7000) COMMA,emiss(i)
c        else 
c            write(line(ilen:),9002,ERR=7000) COMMA
c        endif
c  20 continue
      ilen = strmin(line) + 1
      if( fulin .GE. 0 ) then
          write(line(ilen:),9001,ERR=7000) COMMA, fulin
      else 
          write(line(ilen:),9002,ERR=7000) COMMA
      endif
      ilen = strmin(line) + 1
      if( actin .GE. 0 ) then
          write(line(ilen:),9001,ERR=7000) COMMA, actin
      else 
          write(line(ilen:),9002,ERR=7000) COMMA
      endif
      ilen = strmin(line) + 1
      if( ldfcin .GE. 0 ) then
          write(line(ilen:),9001,ERR=7000) COMMA, ldfcin
      else 
          write(line(ilen:),9002,ERR=7000) COMMA
      endif
      ilen = strmin(line) + 1
      if( hpavin .GE. 0 ) then
          write(line(ilen:),9001,ERR=7000) COMMA, hpavin
      else 
          write(line(ilen:),9002,ERR=7000) COMMA
      endif
      if( lrtrftfl ) then
          ilen = strmin(line) + 1
          if( fracretro .GE. 0 ) then
              write(line(ilen:),9001,ERR=7000) COMMA, fracretro
          else 
              write(line(ilen:),9002,ERR=7000) COMMA
          endif
          ilen = strmin(line) + 1
          if( unitsretro .GE. 0 ) then
              write(line(ilen:),9001,ERR=7000) COMMA, unitsretro
          else 
              write(line(ilen:),9002,ERR=7000) COMMA
          endif
      endif
c
c   --- write the entire line to output file ---
c
      write(IOWDAT,'(2A)',ERR=7000) line(:strmin(line)), COMMA
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
 9000 format(6A,I5,A)
 9001 format(A,E15.8)
 9002 format(A)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

