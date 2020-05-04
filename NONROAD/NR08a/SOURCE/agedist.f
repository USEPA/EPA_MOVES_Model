C**** AGEDIST
c
      subroutine agedist( ierr, baspop, mdyrfrc, ibaspopyr, igrwthyr,
     &                    nyrlif, yryrfrcscrp, infips, indcod )
c
c---------------------------------------------------------------------
c
c     This is based on ScrapDir.doc and scrap-2.xls (AgeDist
c     worksheet).
c
c     This routine grows the model-year fractions (age distribution)
c     from the base-population year to the growth year.
c
c     If the two years are the same or the growth year is less than
c     the base-population year, age distribution is untouched.
c     Otherwise, the age distribution is grown forward to the growth
c     year.
c
c     If the growth year is less than the base-population year, the
c     base population is grown backward to the value for the growth
c     year.  This is necessary because there is no way to grow
c     the age distribution backward, so the initial age distribution
c     is used, and since the age distribution is relative to the
c     base-year population, the population must be adjusted to
c     maintain this relationship.
c
c     Argument declaration
c       Outputs:
c             ierr         I  error indicator flag
c             baspop       R  base-year population (also input)
c             mdyrfrc      R  model-year fractions by age (age
c                             distribution) for growth year (also input)
c       Inputs:
c             mdyrfrc      R  model-year fractions by age (age
c                             distribution) for first full-scrappage year
c             ibaspopyr    I  base population year
c             igrwthyr     I  year to which to grow
c             nyrlif       I  number of years in the lifetime (years
c                             from initial sale to full scrappage)
c             yryrfrcscrp  R  year-to-year faction scrapped by age
c             infips       I  FIPS code
c             indcod       C  indicator code
c
c---------------------------------------------------------------------
c  LOG:
c---------------------------------------------------------------------
c
c      03/24/05  -cimulus-  original development based on
c                           ScrapDir.doc and scrap-2.xls
c      04/05/05  -cimulus-  do not allow populations or
c                           age-distribution fractions to go below
c                           zero; also, removed backward growth
c                           handling (keep initial age-distribution
c                           fractions when growth year less than
c                           base-population year)
c      04/30/05  -cimulus-  removed debug output
c      05/04/05  -cimulus-  changed calculation of totpopfrc to use
c                           compound growth
c      05/05/05  -cimulus-  added special case handling that uses a
c                           minimum base population of MINGRWIND in
c                           case grwfac had to adjust the base-year
c                           indicator to avoid divide by zero
c
c---------------------------------------------------------------------
c  Include files:
c---------------------------------------------------------------------
c
      IMPLICIT NONE
c
      include 'nonrdprm.inc'
c
c---------------------------------------------------------------------
c  Argument declarations:
c---------------------------------------------------------------------
c
      integer*4    ierr
      real*4       baspop
      real*4       mdyrfrc(MXAGYR)
      integer*4    ibaspopyr
      integer*4    igrwthyr
      integer*4    nyrlif
      real*4       yryrfrcscrp(MXAGYR)
      character*5  infips
      character*4  indcod
c
c---------------------------------------------------------------------
c  External functions:
c---------------------------------------------------------------------
c
c
c---------------------------------------------------------------------
c  Local variables:
c---------------------------------------------------------------------
c
      integer*4 iyear          ! year loop iterator
      integer*4 iage           ! engine-age year iterator
      real*4    tmpfrc(MXAGYR) ! temporary storage of model-year fractions
      real*4    totpop         ! total population for the current growth year
      real*4    totpopfrc      ! total population for the current growth year relative to base population
      real*4    grwthfc        ! growth factor between previous year and current growth year
      real*4    frcsum         ! sum of new model-year fractions
c
c---------------------------------------------------------------------
c  Entry point:
c---------------------------------------------------------------------
c
      ierr = IFAIL
c
c   --- if growth year greater than base-population year,
c       grow forward to growth year ---
c
      if( igrwthyr .GT. ibaspopyr ) then
          totpop = baspop
          do iyear=ibaspopyr+1,igrwthyr
c           --- store the current model-year fractions ---
              do iage=1,MXAGYR
                  tmpfrc(iage) = mdyrfrc(iage)
              end do
c
c           --- calculate new total population fraction ---
              call grwfac( ierr, grwthfc, iyear - 1, iyear,
     &                infips, indcod )
              if( ierr .NE. ISUCES ) goto 9999
              if( grwthfc .NE. 0. ) then
c               --- if non-zero growth factor, don't use an initial base population
c                   value below minimum growth indicator, since grwfac never goes
c                   below this for the base-year indicator (to avoid divide by zero);
c                   therefore if grwfac adjusted the base-year indicator value,
c                   base population must be adjusted accordingly ---
                  totpop = max(MINGRWIND, totpop)
              endif
              totpop = max(0., totpop * (1. + grwthfc)) ! do not allow population to go below 0
c                 technically this should be "totpop = max(0., totpop * (1 + (grwthfc * (year2 - year1)))",
c                 but "year2" is known to be one greater than "year1", hence the above simplification
              totpopfrc = totpop / baspop
c
c           --- increment model-year fractions to the next year ---
              frcsum = 0.
              do iage=2,MXAGYR
                  mdyrfrc(iage) = max(0., tmpfrc(iage - 1) *
     &                    (1. - yryrfrcscrp(iage))) ! do not allow fraction to go below 0
                  frcsum = frcsum + mdyrfrc(iage)
              end do
              mdyrfrc(1) = totpopfrc - frcsum
          end do
      endif
c
c   --- if growth year less than base-population year,
c       grow base-year population backward to growth year ---
c
      if( igrwthyr .LT. ibaspopyr ) then
c
c       --- calculate growth factor from base-population year
c           back to growth year ---
          call grwfac( ierr, grwthfc, ibaspopyr, igrwthyr,
     &            infips, indcod )
          if( ierr .NE. ISUCES ) goto 9999
c
c       --- grow base-year population backward ---
          if( grwthfc .NE. 0. ) then
c           --- if non-zero growth factor, don't use an initial base population
c               value below minimum growth indicator, since grwfac never goes
c               below this for the base-year indicator (to avoid divide by zero);
c               therefore if grwfac adjusted the base-year indicator value,
c               base population must be adjusted accordingly ---
              baspop = max(MINGRWIND, baspop)
          endif
          baspop = max(0., baspop *
     &            (1. + ((igrwthyr - ibaspopyr) * grwthfc)))
      endif
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
