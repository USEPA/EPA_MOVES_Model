C**** WRTBMY
c
      subroutine wrtbmy( ierr, fipin, subin, ascin, hpin, tecin, iyrin,
     &                   popin, emiss, fulin, actin, ldfcin, hpavin,
     &                   fracretro, unitsretro, iexev )
c
c-----------------------------------------------------------------------
c
c    writes one data record to the output by-model-year data file which will be read by
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
c       tecin       C  technology type name
c       iyrin       I  model year
c       popin       R  population
c       emiss       R  array of emissions
c       fulin       R  fuel consumption
c       actin       R  activity
c       ldfcin      R  load factor (exhaust only)
c       hpavin      R  HP average (exhaust only)
c       fracretro   R  fraction of population that was retrofitted
c                      (exhaust only; also ignored if not running
c                      retrofit functionality)
c       unitsretro  R  number of units that were retrofitted (exhaust
c                      only; also ignored if not running retrofit
c                      functionality)
c       iexev       I  switch between exhaust and evap output
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      07/20/98  --mmj--  original development (started w/wrtdat)
c      10/19/98  --mmj--  included spillage emissions as separate entry
c      08/03/01 -1 charvey adds fuel consumption output
c      06/04/04  -dfk--   modified to output exhaust and evap separatly
c                         due to introduction of evap techs
c      11/15/04  -dfk--   changed to add 3-hose rec-marine and 
c                         non-rec-marine hose into generic hose 
c      02/24/05  -cimulus-  added activity (new parameter and output)
c      02/25/05  -cimulus-  added ldfcin and hpavin (new parameters
c                           and output for exhaust only)
c      03/08/05  -cimulus-  changed the order of the output of the
c                           columns to be consistent with the OUT file
c                           and the postprocessors
c      05/20/05  -cimulus-  output new retrofit fields, if running
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
      character*5  fipin
      character*5  subin
      character*10 ascin
      character*10 tecin
      real*4       hpin
      integer*4    iyrin
      real*4       popin
      real*4       emiss(MXPOL)
      real*4       fulin
      real*4       actin
      real*4       ldfcin
      real*4       hpavin
      real*4       fracretro
      real*4       unitsretro
      integer      iexev
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
      integer*4     ilen, i
      integer*4     io,ib,ie
      character*(MXSTR) iofl
      real*4        tmpemis(IDXRLS)
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      line = ' '
c
c   --- set up temporary emission array, adding all hose permeations together
c
      tmpemis=0.
c
      do i=IDXTHC,IDXRLS
        if(i.ge.IDXHOS.and.i.le.IDXVNT) then
          tmpemis(IDXHOS)=tmpemis(IDXHOS)+emiss(i)
        else
        tmpemis(i)=emiss(i)
      endif
      end do
c
c   --- set loop parameters and I/O depending on exhaust/evap
c
      if(iexev.eq.1) then
        ib=IDXTHC
        ie=IDXCRA
        io=IOWBMY
        iofl=bmyfl
      else
c        ib=IDXDIU
c        ie=IDXRLS
        io=IOWEVBMY
        iofl=evbmyfl
      endif
c
c   --- write the data to line ---
c
      write(line,9000,ERR=7000) fipin, COMMA,
     &                          subin, COMMA, 
     &                          ascin, COMMA,
     &                          INT(hpin), COMMA,
     &                          tecin, COMMA,
     &                          iyrin, COMMA,
     &                          popin
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
c
      if(iexev.eq.1) then
          do 10 i=ib,ie
              if(i.ge.IDXNCK.and.i.le.IDXVNT) goto10 ! skip rec-marine, they're included in hose
              ilen = strmin(line) + 1
              write(line(ilen:),9001,ERR=7000) COMMA,tmpemis(i)
   10     continue
      else
          ilen = strmin(line) + 1
          write(line(ilen:),9001,ERR=7000) COMMA,tmpemis(IDXSOK)
          ilen = strmin(line) + 1
          write(line(ilen:),9001,ERR=7000) COMMA,tmpemis(IDXDIU)
          ilen = strmin(line) + 1
          write(line(ilen:),9001,ERR=7000) COMMA,tmpemis(IDXDIS)
          ilen = strmin(line) + 1
          write(line(ilen:),9001,ERR=7000) COMMA,tmpemis(IDXSPL)
          ilen = strmin(line) + 1
          write(line(ilen:),9001,ERR=7000) COMMA,tmpemis(IDXRLS)
          ilen = strmin(line) + 1
          write(line(ilen:),9001,ERR=7000) COMMA,tmpemis(IDXTKP)
          ilen = strmin(line) + 1
          write(line(ilen:),9001,ERR=7000) COMMA,tmpemis(IDXHOS)
      endif
c
c     do 20 i=IDXRLS,IDXRLS
c        ilen = strmin(line) + 1
c        write(line(ilen:),9001,ERR=7000) COMMA,emiss(i)
c  20 continue
c
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

      if(iexev.eq.1) then ! if exhaust
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
      endif

c   --- write the entire line to output file ---
c
      write(io,'(2A)',ERR=7000) line(:strmin(line)), COMMA
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
 9000 format(6A,I5,3A,I4,A,E15.8)
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

