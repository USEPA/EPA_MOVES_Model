C**** RDEFLS.F
c
      subroutine rdefls( ierr )
c
c-----------------------------------------------------------------------
c
c    Description:
c      This routine reads the emission factors files for each of
c      the pollutants.  It accomplishes this by successive calls
c      to the RDEMFC and RDDETR routines.
c    Arguments:
c     Outputs
c        ierr     I    error code
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/10/95  --gmw--  original development
c      07/19/96  --jlf-- added deterioration rates and changed so that
c                        files are opened here to conserve file handles
c      05/11/04  --dfk-- updated to include expanded evap
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdefc.inc'
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I  returns the actual length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4         ierr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) fname
      integer*4         idxpol, jerr, i
      logical*4         lcheck
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
c
c  --- open emission factor files ----
c
      do 10 idxpol=1,MXPOL
        if( lfacfl(idxpol) ) then 
           fname = facfl(idxpol)
           inquire(file=facfl(idxpol),exist=lcheck)
           if( .NOT. lcheck ) goto 7007
           open(unit=IORFAC+idxpol,file=facfl(idxpol),ERR=7009,
     &                                                 status='UNKNOWN')
           rewind(IORFAC+idxpol)
        endif
   10 continue
c
c  --- call routine to get all exhaust factors ---
c
      do 20 idxpol=IDXTHC,IDXCRA
          do 25 i=1,MXEMFC
             exhfac(i,idxpol) = RMISS
   25     continue
          if( lfacfl(idxpol) ) then
              call rdemfc( jerr, ascexh, tecexh, exhpcb, exhpce, 
     &            iexhun, iyrexh, exhfac, IDXTHC, nexhfc,
     &            IORFAC+idxpol, facfl(idxpol), idxpol, amspol(idxpol) )
              if( jerr .NE. ISUCES ) goto 9999
              call spinit()
          endif
   20 continue
c
c  --- call routine to get all evaporative factors ---
c
      do 30 idxpol=IDXDIU,IDXRLS
          do 35 i=1,MXEMFC
             evpfac(i,idxpol) = RMISS
   35     continue
          if( lfacfl(idxpol) ) then
              if ( idxpol .EQ. IDXSPL )  then
                call rdspil( jerr,
     &                       IORFAC+idxpol, facfl(idxpol) )
                if( jerr .NE. ISUCES ) goto 9999
                call spinit()
              else
                call rdevemfc( jerr, ascevp, tecevp, evhpcb, evhpce,
     &              ievpun, iyrevp, evpfac, IDXTHC, nevpfc,
     &            IORFAC+idxpol, facfl(idxpol), idxpol, amspol(idxpol) )
                if( jerr .NE. ISUCES ) goto 9999
                call spinit()
              endif
          endif
   30 continue
c
c  --- make sure that HOT SOAK units are grams/start ----
c
      do 40 i=1,nevpfc(IDXSOK)
          if( evpfac(i,IDXSOK) .GT. 0 ) then
             if( ievpun(i,IDXSOK) .NE. IDXGST ) goto 7000
          endif
   40 continue
cgmwc
cgmwc  --- call routine to get all start exhaust factors ---
cgmwc
cgmw      do 50 idxpol=IDSTHC,IDSPM
cgmw          do 55 i=1,MXEMFC
cgmw             strfac(i,idxpol) = RMISS
cgmw   55     continue
cgmw          if( lfacfl(idxpol) ) then
cgmw             call rdemfc( jerr, ascstr, tecstr, sthpcb, sthpce,
cgmw     &          istrun, iyrstr, strfac, IDSTHC, nstrfc,
cgmw     &          IORFAC+idxpol, facfl(idxpol), idxpol, amspol(idxpol) )
cgmw             if( jerr .NE. ISUCES ) goto 9999
cgmw             call spinit()
cgmw           endif
cgmwc
cgmwc  --- make sure that units are grams/start ----
cgmwc
cgmw          do 60 i=1,nstrfc
cgmw              if( strfac(i,idxpol) .GT. 0 ) then
cgmw                 if( istrun(i,idxpol) .NE. IDXGST ) goto 7002
cgmw              endif
cgmw   60     continue
cgmw   50 continue
c
c   ---- now close these files and open the deterioration factors files ----
c
      do 70 idxpol=1,MXPOL
         if( lfacfl(idxpol) ) close(IORFAC+idxpol)
         if( ldetfl(idxpol) ) then
             fname = detfl(idxpol)
             inquire(file=detfl(idxpol),exist=lcheck)
             if( .NOT. lcheck ) goto 7007
             open(unit=IORDAC+idxpol,file=detfl(idxpol),ERR=7009,
     &                                                 status='UNKNOWN')
             rewind(IORDAC+idxpol)
          endif
   70 continue
c
c  --- call routine to get all deterioration factor data ---
c
      do 80 idxpol=1,MXPOL
          if( ldetfl(idxpol) ) then
              call rddetr( jerr, idxpol )
              if( jerr .NE. ISUCES ) goto 9999
              call spinit()
              close(IORDAC+idxpol)
          endif
   80 continue
c
c  --- set error code and exit ---
c
      ierr = ISUCES
      goto 9999 
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,A,/,9X,2A)',ERR=9999) 
     &       'ERROR:  Units code for HOT SOAKS must be G/START',
     &       'in emission factors file ',
     &                           facfl(IDXSOK)(:strmin(facfl(IDXSOK)))
      write(IOWMSG,'(/,1X,A,/,9X,2A)',ERR=9999) 
     &       'ERROR:  Units code for HOT SOAKS must be G/START',
     &       'in emission factors file ',
     &                           facfl(IDXSOK)(:strmin(facfl(IDXSOK)))
      goto 9999
c
cgmw 7002 continue
cgmw      write(IOWSTD,'(/,1X,2A,/,9X,2A)',ERR=9999) 
cgmw     &       'ERROR:  Units code must be G/START for ',
cgmw     &            polnam(idxpol)(:strmin(polnam(idxpol))),
cgmw     &       'in emission factors file ',
cgmw     &                           facfl(idxpol)(:strmin(facfl(idxpol)))
cgmw      write(IOWMSG,'(/,1X,2A,/,9X,2A)',ERR=9999) 
cgmw     &       'ERROR:  Units code must be G/START for ',
cgmw     &            polnam(idxpol)(:strmin(polnam(idxpol))),
cgmw     &       'in emission factors file ',
cgmw     &                           facfl(idxpol)(:strmin(facfl(idxpol)))
c
 7007 continue
      write(IOWSTD,9003,ERR=9999) 'ERROR: Input file not found ',
     &                                           fname(:strmin(fname))
      write(IOWMSG,9003,ERR=9999) 'ERROR: Input file not found ',
     &                                           fname(:strmin(fname))
      goto 9999
c
c
 7009 continue
      write(IOWSTD,9003,ERR=9999) 'ERROR: Opening file ',
     &                                           fname(:strmin(fname))
      write(IOWMSG,9003,ERR=9999) 'ERROR: Opening file ',
     &                                           fname(:strmin(fname))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(1X,5A)
 9003 format(/,1X,A,A,/)
c
c-----------------------------------------------------------------------
c   Exit point:
c-----------------------------------------------------------------------
c
 9999 continue
      end
