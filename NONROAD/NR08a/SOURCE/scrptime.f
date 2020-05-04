C**** SCRPTIME
c
      subroutine scrptime( ierr, yryrfrcscrp, nyrlif, mdyrfrc,
     &                     mdlfhrs, ldfctr, acthpy, scrpnam, popgrwfac )
c
c---------------------------------------------------------------------
c
c     This is based on ScrapDir.doc and scrap-2.xls (ScrpTime
c     worksheet).
c
c     Calculates the number of years in lifetime (full scrappage
c     time), the year-to-year fraction scrapped by age, and the
c     model-year fractions by age (age distribution) for the first
c     full scrappage year.
c
c     NOTE:  Uses a single activity in hours per year value (acthpy),
c     since the alternate-activity-curve functionality is not actually
c     being used.  To use an alternate activity curve, actadj would
c     have to be passed in and the calculation of the frcmlusd values
c     would have to be modified.
c   
c     Argument declaration
c       Outputs:
c             ierr         I  error indicator flag
c             yryrfrcscrp  R  year-to-year fraction scrapped by age
c             nyrlif       I  number of years in the lifetime
c                             (first year of full scrappage)
c             mdyrfrc      R  model-year fractions by age (age
c                             distribution) for first full-scrappage year
c       Inputs:
c             mdlfhrs      R  median life (hours at full load)
c             ldfctr       R  load factor
c             acthpy       R  activity in hours per year
c             scrpnam      C  name of the scrappage curve to use
c             popgrwfac    R  population growth factor
c
c---------------------------------------------------------------------
c  LOG:
c---------------------------------------------------------------------
c
c      03/23/05  -cimulus-  original development based on
c                           ScrapDir.doc and scrap-2.xls
c      04/06/05  -cimulus-  changed yryrfrcscrp, nyrlif, and mdyrfrc
c                           from local variables to output parameters;
c                           changed fullscrpyr to nyrlif; integrated
c                           getscrp for efficiency, so that fndscrp
c                           doesn't search for the scrappage curve
c                           in every call
c      04/30/05  -cimulus-  removed debug output
c      05/02/05  -cimulus-  added decimal to real number to correct
c                           compiler error
c      05/12/05  -cimulus-  ensure that nyrlif is set when first
c                           year of full scrappage is MXAGYR; moved
c                           initialization of initsales to right
c                           before usage
c                           
c
c---------------------------------------------------------------------
c  Include files:
c---------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdeqp.inc'
c
c---------------------------------------------------------------------
c  Argument declarations:
c---------------------------------------------------------------------
c
      integer*4    ierr
      real*4       yryrfrcscrp(MXAGYR)
      integer*4    nyrlif
      real*4       mdyrfrc(MXAGYR)
      real*4       mdlfhrs
      real*4       ldfctr
      real*4       acthpy
      character*10 scrpnam
      real*4       popgrwfac
c
c---------------------------------------------------------------------
c  External functions:
c---------------------------------------------------------------------
c
c   fndscrp  R   returns the percent scrapped for the
c                fraction of median life used
c
      real*4 fndscrp
c
c---------------------------------------------------------------------
c  Local variables:
c---------------------------------------------------------------------
c
      integer*4 jerr
      real*4    mdlfyrs             ! median life in years
      real*4    mdlfupy             ! median life used per year
      real*4    scrpbin(MXSCRP)     ! array of scrappage bin values
      real*4    scrppct(MXSCRP)     ! percent of equipment left in each bin
      integer*4 iage                ! engine-age year iterator
      real*4    frcmlusd(MXAGYR)    ! fraction of median life used by age
      real*4    pctscrp(MXAGYR)     ! percent scrapped by age
      real*4    yrfrcscrp(MXAGYR)   ! fraction scrapped by age
      real*4    salesgrwfac         ! sales growth factor
      real*4    initsales           ! initial sales in the base year
      integer*4 isaleyr             ! sales year iterator
      real*4    sales(MXAGYR)       ! sales by sales year
      real*4    survunits(MXAGYR)   ! relative number of surviving units by age for first full-scrappage year
      real*4    srvunttot           ! total surviving units for first full-scrappage year
c
c---------------------------------------------------------------------
c  Entry point:
c---------------------------------------------------------------------
c
      ierr = IFAIL
c
c   --- calculate the median life in years ---
c
      mdlfyrs = min(real(int(MXAGYR / 2.)), mdlfhrs / ldfctr / acthpy)
c
c   --- calculate the median life used per year ---
c
      mdlfupy = 1. / mdlfyrs
c
c   --- determine which scrappage curve to use ---
c
      call getscrp( jerr, scrpbin, scrppct, scrpnam )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- calculate the first year of full scrappage, while populating
c       the various utility arrays indexed by age ---
c
      frcmlusd(1) = 0.
      pctscrp(1) = 0.
      yrfrcscrp(1) = 0.
      yryrfrcscrp(1) = 0.
      nyrlif = 0
      do iage=2,MXAGYR
          frcmlusd(iage) = (iage - 1) * mdlfupy
          pctscrp(iage) = fndscrp(frcmlusd(iage), scrpbin, scrppct)
          yrfrcscrp(iage) = (pctscrp(iage) - pctscrp(iage - 1)) / 100.
          if( pctscrp(iage - 1) .GE. 100. ) then
              if( nyrlif .EQ. 0 ) then
                  nyrlif = iage - 1
              endif
              yryrfrcscrp(iage) = 0.
          else
              yryrfrcscrp(iage) = 100. *
     &                yrfrcscrp(iage) / (100. - pctscrp(iage - 1))
          endif
      end do
      if( pctscrp(iage - 1) .GE. 100. ) then
          if( nyrlif .EQ. 0 ) then
              nyrlif = iage - 1
          endif
      endif
c
c   --- calculate sales growth factor ---
c
      salesgrwfac = popgrwfac / (((-1.4306 * popgrwfac) * mdlfyrs)
     &        + (-0.24 * popgrwfac) + 1.0)
c
c   --- calculate the sales by sales year ---
c
      initsales = 1000. ! arbitrary initial sales
      do isaleyr=1,MXAGYR
          sales(isaleyr) = initsales +
     &            (initsales * salesgrwfac * (isaleyr - 1))
      end do
c
c   --- calculate the relative number of units surviving by age
c       for the first full-scrappage year ---
c
      srvunttot = 0.
      do iage=1,MXAGYR
          if( iage .LE. nyrlif ) then
              survunits(iage) = sales(nyrlif - iage + 1)
     &                * (1. - pctscrp(iage) / 100.)
          else
              survunits(iage) = 0.
          endif
          srvunttot = srvunttot + survunits(iage)
      end do
c
c   --- divide each relative number of units surviving by the sum of
c       all values to yield the initial model-year fractions ---
c
      do iage=1,MXAGYR
          mdyrfrc(iage) = survunits(iage) / srvunttot
      end do
c
c   --- set error code to sucess and return ---
c
      ierr = ISUCES
      goto 9999
c
c---------------------------------------------------------------------
c  Error messages:
c---------------------------------------------------------------------
c
c
c---------------------------------------------------------------------
c  Format statements:
c---------------------------------------------------------------------
c
c
c---------------------------------------------------------------------
c  Return point:
c---------------------------------------------------------------------
c
 9999 continue
      return
      end
