C**** CLCRTRFT
c
      subroutine clcrtrft( ierr, fracretro, unitsretro, pop, scc, hpavg,
     &                     mdlyr, techtype )
c
c-----------------------------------------------------------------------
c
c     Calculates the retrofit reductions for every pollutant affect by
c     the retrofits that affect the current model iteration (i.e.,
c     the current combination of SCC, HP, model year, and tech type).
c     Calculates the fraction and units retrofitted for the current
c     iteration.  Assumes that fndrtrft() has been called to filter the
c     retrofit arrays to match the current iteration.
c
c     Argument declaration
c       Outputs:
c             ierr        I  error flag
c             fracretro   R  fraction retrofitted for the current model
c                            iteration
c             unitsretro  R  units retrofitted for the current model
c                            iteration
c       Inputs:
c             pop         R  engine population in existence for the
c                            current model iteration
c             scc         C  SCC for the current model iteration
c             hpavg       R  HP average for the current model iteration
c             mdlyr       I  model year for the current model iteration
c             techtype    C  tech type for the current model iteration
c
c-----------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------
c
c      05/26/05  -cimulus-  original development
c
c-----------------------------------------------------------------------
c  Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdusr.inc'
      include 'nonrdrtrft.inc'
c
c-----------------------------------------------------------------------
c  Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      real*4       fracretro
      real*4       unitsretro
      real*4       pop
      character*10 scc
      real*4       hpavg
      integer*4    mdlyr
      character*10 techtype
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c   strmin  I  returns the actual length of a string (minimum of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c  Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i
      integer*4 plltnt ! index into rtrftplltnt for the current iteration
      integer*4 rtrft ! index into the below arrays for current iteration
      integer*4 rtrftidxid(MXRTRFT) ! map index into first dimension
                                    ! of below arrays to retrofit ID
      real*4    rtrftplltntfracretro(MXRTRFT, NRTRFTPLLTNT) ! fraction retrofitted for a given retrofit-pollutant combination
                                                            ! (can be > 1, but will be limitted to 1 for recuction calculation)
      logical*4 rtrftplltnthasnunits(MXRTRFT, NRTRFTPLLTNT) ! whether or not a given retrofit-pollutant
                                                            ! combination has any cases where N-units are
                                                            ! retrofitted (as opposed to a fraction)
      real*4    rtrftplltnteffect(MXRTRFT, NRTRFTPLLTNT) ! the effect fraction for a given retrofit-pollutant combination
      integer*4 rtrftidx ! index into the retrofit-record arrays
      integer*4 plltntidx ! index into the rtrftplltnt array
      integer*4 rystart ! first retrofit year that applies
      integer*4 ryend ! last retrofit year that applies
      integer*4 rycount ! number of retrofit years that apply
      real*4    tmpfrac
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- assume no engines affected ---
c
      fracretro = 0.
      unitsretro = 0.
c
c   --- clear any values from previous iteration ---
c
      do rtrft = 1, numrtrft
          rtrftidxid(rtrft) = 0
          do plltnt = 1, NRTRFTPLLTNT
              rtrftplltntfracretro(rtrft, plltnt) = 0.
              rtrftplltnthasnunits(rtrft, plltnt) = .FALSE.
              rtrftplltnteffect(rtrft, plltnt) = 0.
          end do
      end do
      numrtrft = 0
c
c   --- calculate total retrofit-pollutant-level fraction retrofitted
c       and effect of the filtered retrofits (i.e., the ones that affect
c       the current model iteration) ---
c
      do i = 1, rtrftfltr3cnt
          rtrftidx = rtrftfltr3(i)
c
c       --- find the retrofit index in the retrofit-pollutant arrays ---
c
          do rtrft = 1, numrtrft
              if( rtrftidxid(rtrft)
     &                .EQ. rtrftid(rtrftidx) ) then
                  exit
              endif
          end do
          if( rtrft .GT. numrtrft ) then ! if retrofit not in array yet
              numrtrft = rtrft
              rtrftidxid(numrtrft) = rtrftid(rtrftidx)
          endif
c
c       --- calculate the number of retrofit years ---
c
          rystart = rtrftryst(rtrftidx)
          ryend = min(rtrftryen(rtrftidx), iepyr)
          rycount = ryend - rystart + 1
c
c       --- apply the current fraction retrofitted and effect to the
c           retrofit-pollutant arrays ---
c
          plltntidx = rtrftplltntidx(rtrftidx)
          if( rtrftannualfracorn(rtrftidx) .GT. 1. ) then ! if N units retrofitted
              rtrftplltnthasnunits(rtrft, plltntidx) = .TRUE.
              tmpfrac = rtrftannualfracorn(rtrftidx) / pop
          else
              tmpfrac = rtrftannualfracorn(rtrftidx)
          endif
          rtrftplltntfracretro(rtrft, plltntidx) =
     &            rtrftplltntfracretro(rtrft, plltntidx) +
     &            (tmpfrac * rycount)
          rtrftplltnteffect(rtrft, plltntidx) = rtrfteffect(rtrftidx) ! effect always same for given retrofit-pollutant combination
c
      end do
c
c   --- calculate the fraction and units retrofitted for the current
c       model iteration ---
c
      do rtrft = 1, numrtrft
          do plltnt = 1, NRTRFTPLLTNT
              if( rtrftplltntfracretro(rtrft, plltnt) .GT. 0. ) then
c
c           --- if N units retrofitted requires more engines than exist,
c               log error and return ---
c
                  if( rtrftplltnthasnunits(rtrft, plltnt) .AND.
     &                    rtrftplltntfracretro(rtrft, plltnt) .GT. 1.
     &                    ) then
                      goto 7000
                  endif
c
                  fracretro = fracretro +
     &                    rtrftplltntfracretro(rtrft, plltnt)
c
c           --- fraction retrofitted always same for all pollutants for
c               a given retrofit and must only be added once, so once
c               a non-zero value is found exit the loop ---
c
                  exit
              endif
          end do
      end do
      fracretro = min(1., fracretro)
      unitsretro = pop * fracretro
c
c   --- calculate the emission reduction fractions for each pollutant ---
c
      do plltnt = 1, NRTRFTPLLTNT
          rtrftplltntrdfrc(rtrftplltntidxmp(plltnt)) = 0.
      end do
      do rtrft = 1, numrtrft
          do plltnt = 1, NRTRFTPLLTNT
              if( rtrftplltntfracretro(rtrft, plltnt) .EQ. 0. ) then
                  cycle
              else if( rtrftplltntfracretro(rtrft, plltnt) .GT. 1.
     &                ) then
c
c           --- fraction retrofitted exceeds 1 for a given retrofit-
c               pollutant combination; log warning and fix value ---
c
                  write(IOWSTD,8001,ERR=9999)
     &                    'WARNING: Fraction retrofitted exceeds 1',
     &                    '; setting value to 1',
     &                    'Retrofit ID','Pollutant','Frac Retro',
     &                    'SCC','HP Average','Model Year',
     &                    'Tech Type',rtrftidxid(rtrft),
     &                    rtrftplltnt(plltnt),
     &                    rtrftplltntfracretro(rtrft, plltnt),scc,
     &                    hpavg,mdlyr,techtype(:strmin(techtype))
                  if( lmsgfl ) then
                      write(IOWMSG,8001,ERR=9999)
     &                        'WARNING: Fraction retrofitted exceeds 1',
     &                        '; setting value to 1',
     &                        'Retrofit ID','Pollutant','Frac Retro',
     &                        'SCC','HP Average','Model Year',
     &                        'Tech Type',rtrftidxid(rtrft),
     &                        rtrftplltnt(plltnt),
     &                        rtrftplltntfracretro(rtrft, plltnt),scc,
     &                        hpavg,mdlyr,techtype(:strmin(techtype))
                  endif
                  rtrftplltntfracretro(rtrft, plltnt) = 1.
              endif
c
c           --- apply the effect to the pollutant ---
c
              rtrftplltntrdfrc(rtrftplltntidxmp(plltnt)) =
     &                rtrftplltntrdfrc(rtrftplltntidxmp(plltnt)) +
     &                (rtrftplltntfracretro(rtrft, plltnt) *
     &                rtrftplltnteffect(rtrft, plltnt))
          end do
      end do
c
c   --- limit emission reduction fractions to 1 ---
c
      do plltnt = 1, NRTRFTPLLTNT
          if( rtrftplltntrdfrc(rtrftplltntidxmp(plltnt)) .GT. 1. ) then
c
c       --- retrofit reduction fraction exceeds 1 for a given
c           pollutant; log warning and fix value ---
c
              write(IOWSTD,8002,ERR=9999)
     &                'WARNING: Retrofit reduction fraction ',
     &                'exceeds 1; setting value to 1',
     &                'Pollutant','SCC','HP Average',
     &                'Model Year','Tech Type',
     &                rtrftplltnt(plltnt),scc,hpavg,mdlyr,
     &                techtype(:strmin(techtype))
              if( lmsgfl ) then
                  write(IOWMSG,8002,ERR=9999)
     &                'WARNING: Retrofit reduction fraction ',
     &                'exceeds 1; setting value to 1',
     &                'Pollutant','SCC','HP Average',
     &                'Model Year','Tech Type',
     &                rtrftplltnt(plltnt),scc,hpavg,mdlyr,
     &                techtype(:strmin(techtype))
              endif
              rtrftplltntrdfrc(rtrftplltntidxmp(plltnt)) = 1.
          endif
      end do
c
c   --- set error code to sucess and return ---
c
  999 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c  Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,8000,ERR=9999)
     &        'ERROR: Request for N units retrofitted exceeds ',
     &        'engines in existence ','Retrofit ID','Pollutant',
     &        'SCC','HP Average','Model Year','Tech Type',
     &        'N Units Requested','N Units Existing',
     &        rtrftidxid(rtrft),rtrftplltnt(plltnt),scc,hpavg,mdlyr,
     &        techtype,rtrftplltntfracretro(rtrft, plltnt)*pop,pop
      if( lmsgfl ) then
          write(IOWMSG,8000,ERR=9999)
     &            'ERROR: Request for N units retrofitted exceeds ',
     &            'engines in existence ','Retrofit ID','Pollutant',
     &            'SCC','HP Average','Model Year','Tech Type',
     &            'N Units Requested','N Units Existing',
     &            rtrftidxid(rtrft),rtrftplltnt(plltnt),scc,hpavg,mdlyr,
     &            techtype,rtrftplltntfracretro(rtrft, plltnt)*pop,pop
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c  Format statements:
c-----------------------------------------------------------------------
c
 8000 format(/,1X,2A,/,6X,A,3X,A,4X,A,10X,A,3X,A,3X,A,4X,A,3X,A,
     &/,6X,I11,3X,A10,3X,A10,3X,F10.3,3X,I10,3X,A10,3X,F17.3,3X,F16.3,/)
 8001 format(/,1X,2A,/,6X,A,3X,A,4X,A,3X,A,10X,A,3X,A,3X,A,
     &/,6X,I11,3X,A10,3X,F10.3,3X,A10,3X,F10.3,3X,I10,3X,A,/)
 8002 format(/,1X,2A,/,6X,A,4X,A,10X,A,3X,A,3X,A,
     &/,6X,A10,3X,A10,3X,F10.3,3X,I10,3X,A,/)
c
c-----------------------------------------------------------------------
c  Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
