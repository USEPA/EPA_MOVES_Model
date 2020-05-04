C**** VLDRTRFTRECS
c
      logical*4 function vldrtrftrecs()
c
c-----------------------------------------------------------------------
c
c     Validates the entire retrofit record-set to ensure that there are
c     no conflicts among the records.  Some issues are only warning
c     conditions, in which case a warning message is logged and
c     validation continues.  However, as soon as an error condition
c     occurs, an error message is logged and the function returns false.
c
c     Return value:
c         .TRUE.   =  retrofit records are valid
c         .FALSE.  =  retrofit records are not valid
c
c     Argument declaration
c       Inputs:
c             (none)
c
c-----------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------
c
c      05/26/05  -cimulus-  original development
c      05/31/05  -cimulus-  changed expctplltntidx to integer
c      05/31/05  -cimulus-  compare fracretro to expctfracretro with a
c                           tolerance instead of direct comparison
c      06/02/05  -cimulus-  more specific floating point format
c                           specifications
c      07/20/05  -cimulus-  floating-point comparison for equality okay;
c                           rtrfteffect and plltnteffect values are read
c                           from file, which are hard-coded, rather than
c                           being calculated at runtime, and must be
c                           exactly the same
c
c-----------------------------------------------------------------------
c  Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdrtrft.inc'
c
c-----------------------------------------------------------------------
c  Argument declarations:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c   rtrftengovrlp  L  returns true if sets of engines overlap for two retrofit records
c   strmin         I  returns the actual length of a string (minimum of 1)
c
      logical*4 rtrftengovrlp
      integer*4 strmin
c
c-----------------------------------------------------------------------
c  Local variables:
c-----------------------------------------------------------------------
c
      character*300 errprefix ! common prefix for error messages
      character*300 wrnprefix ! common prefix for warning messages
      integer*4 errlbl
      integer*4 i, j, k, tmp
      integer*4 rtrftidstidx(MXRTRFT) ! index into the retrofit-record
                                      ! arrays of the start of each
                                      ! retrofit ID (assumes arrays
                                      ! sorted by ID, then pollutant)
      integer*4 rtrftidstcount ! number of retrofit-ID start indexes
      integer*4 prevrtrftid ! previous retrofit ID
      integer*4 currtrftid ! current retrofit ID
      integer*4 rtrftididxst ! first index into retrofit-record arrays
                             ! for current retrofit ID
      integer*4 rtrftididxen ! last index into retrofit-record arrays
                             ! for current retrofit ID
      integer*4 plltntidx ! index into rtrftplltnt
      real*4    fracretro ! fraction retrofitted
      integer*4 plltntinstcnt ! number of instances of a given
                              ! ID-pollutant combination with
                              ! overlapping engines
      logical*4 plltntdidwrn(NRTRFTPLLTNT) ! whether or not did warning
                                           ! for duplicate pollutant
      real*4    plltntfracretro(NRTRFTPLLTNT) ! sum of fraction
                                              ! retrofitted for a given
                                              ! ID-pollutant combination
                                              ! with overlapping engines
      real*4    plltnteffect(NRTRFTPLLTNT) ! reduction effect for
                                           ! a given ID-pollutant
                                           ! combination with
                                           ! overlapping engines
      integer*4 expctplltntidx ! index into rtrftplltnt for pollutant
                               ! used to determine expected fraction
                               ! retrofitted
      real*4    expctfracretro ! expected fraction retrofitted
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c   --- assume not valid ---
c
      vldrtrftrecs = .FALSE.
c
c   --- common prefixes for messages ---
c
      write(errprefix,'(2A)',ERR=9999)
     &        'ERROR:  Reading retrofit data file: ',
     &        rtrftfl(:strmin(rtrftfl))
      write(wrnprefix,'(2A)',ERR=9999)
     &        'WARNING:  Reading retrofit data file: ',
     &        rtrftfl(:strmin(rtrftfl))
c
c   --- sort the record-set ---
c
      call srtrtrft(1, 1, rtrftcount)
c
c   --- find retrofit ID start indexes ---
c
      rtrftidstcount = 0
      prevrtrftid = 0 ! default to invalid value for retrofit ID
      do i = 1, rtrftcount
          currtrftid = rtrftid(i)
c
c       --- handle change in retrofit ID ---
c
          if( currtrftid .NE. prevrtrftid ) then ! if retrofit ID changed
              rtrftidstcount = rtrftidstcount + 1
              rtrftidstidx(rtrftidstcount) = i
          endif
c
          prevrtrftid = currtrftid
      end do
c
c   --- validate each group of records with the same retrofit ID ---
c
      do i = 1, rtrftidstcount
c
c       --- determine the range of retrofit-record indexes
c           for the current retrofit ID ---
c
          rtrftididxst = rtrftidstidx(i)
          if( i .EQ. rtrftidstcount ) then ! if last start index
c
c           --- end index is highest retrofit-record index ---
c
              rtrftididxen = rtrftcount
c
          else ! else not last start index
c
c           --- end index is one less than next start index ---
c
              rtrftididxen = rtrftidstidx(i + 1) - 1
c
          endif
c
c       --- skip the retrofit if it only has one record ---
c
          if( rtrftididxst .EQ. rtrftididxen ) then
              cycle
          endif
c
c       --- validate each retrofit record against
c           the other records with the same ID ---
c
          plltntdidwrn = .FALSE.
          do j = rtrftididxst, rtrftididxen
c
c           --- clear fraction-retrofitted-sum and
c               pollutant-effect tracking variables ---
c
              plltntinstcnt = 0
              plltntfracretro = 0.
              plltnteffect = -1. ! set to invalid value to flag that
                                 ! the first effect value not found yet
c
              do k = rtrftididxst, rtrftididxen
c
c               --- skip record if sets of engines do not overlap ---
c
                  if( .NOT. rtrftengovrlp(j, k) ) then
                      cycle
                  endif
c
c               --- make sure the retrofit year range is the same ---
c
                  if( rtrftryst(k) .NE. rtrftryst(j) .OR.
     &                    rtrftryen(k) .NE. rtrftryen(j) ) then
                      errlbl = 7000
                      goto 6999
                  endif
c
c               --- make sure the model year range is the same ---
c
                  if( rtrftmyst(k) .NE. rtrftmyst(j) .OR.
     &                    rtrftmyen(k) .NE. rtrftmyen(j) ) then
                      errlbl = 7001
                      goto 6999
                  endif
c
c               --- get the retrofit pollutant index
c                   and fraction retrofitted ---
c
                  plltntidx = rtrftplltntidx(k)
                  fracretro = rtrftannualfracorn(k)
c
c               --- add the fraction retrofitted for the pollutant ---
c
                  plltntfracretro(plltntidx) =
     &                    plltntfracretro(plltntidx) + fracretro
c
c               --- make sure reduction effect is always the same
c                   for a given pollutant within the retrofit ---
c
                  if( plltnteffect(plltntidx) .LT. 0. ) then ! if first time pollutant encountered for this retrofit ID
                      plltnteffect(plltntidx) = rtrfteffect(k) ! set the expected reduction effect for this pollutant
                  else ! else pollutant already found for this retrofit ID
                      if( rtrfteffect(k) .NE.
     &                        plltnteffect(plltntidx) ) then ! floating-point comparison for equality okay; rtrfteffect and plltnteffect values are read from file, which are hard-coded, rather than being calculated at runtime, and must be exactly the same
                          errlbl = 7002
                          goto 6999
                      endif
                  endif
c
c               --- if a given pollutant appears more than one time
c                   within the retrofit, log a warning ---
c
                  if( plltntidx .EQ. rtrftplltntidx(j) ) then
                      plltntinstcnt = plltntinstcnt + 1
c
c                   --- only log warning if not already logged for
c                       this retrofit ID and pollutant ---
c
                      if( plltntinstcnt .GT. 1 .AND.
     &                        .NOT. plltntdidwrn(plltntidx) ) then
                          plltntdidwrn(plltntidx) = .TRUE.
                          write(IOWSTD,8100,ERR=9999)
     &                            wrnprefix(:strmin(wrnprefix)),
     &                            'Pollutant ',rtrftpollutant(j)
     &                            (:strmin(rtrftpollutant(j))),
     &                            ' appears more than once',
     &                            ' for retrofit ID ',rtrftid(j)
                          if( lmsgfl ) then
                              write(IOWMSG,8100,ERR=9999)
     &                                wrnprefix(:strmin(wrnprefix)),
     &                                'Pollutant ',rtrftpollutant(j)
     &                                (:strmin(rtrftpollutant(j))),
     &                                ' appears more than once',
     &                                ' for retrofit ID ',rtrftid(j)
                          endif
                      endif
                  endif
              end do
c
c           --- make sure the sum of fraction retrofitted is the
c               same for each pollutant within the retrofit ---
c
              expctplltntidx = 0
              expctfracretro = 0.
              do k = 1, NRTRFTPLLTNT
                  fracretro = plltntfracretro(k)
                  if( fracretro .GT. 0. ) then
                      if( expctfracretro .GT. 0. ) then
c
c                   --- if fraction retrofitted values not the same
c                       within a certain tolerance, log error and return ---
c
                          if( abs(fracretro - expctfracretro)
     &                            .GT. 0.0049 ) then
                              goto 7003
                          endif
                      else
                          expctplltntidx = k
                          expctfracretro = fracretro
                      endif
                  endif
              end do
c
          end do
c
      end do
c
c   --- set result to sucess and return ---
c
  999 continue
      vldrtrftrecs = .TRUE.
      goto 9999
c
c-----------------------------------------------------------------------
c  Error messages:
c-----------------------------------------------------------------------
c
 6999 continue
c
c   --- order records by record number for error messages
c       that output the details of two records ---
c
      if( rtrftrec(j) .GT. rtrftrec(k) ) then
         tmp = j
         j = k
         k = tmp
      endif
      if( errlbl .EQ. 7000 ) then
          goto 7000
      else if( errlbl .EQ. 7001 ) then
          goto 7001
      else if( errlbl .EQ. 7002 ) then
          goto 7002
      else
          goto 9999
      endif
c
 7000 continue
      write(IOWSTD,8000,ERR=9999)
     &        errprefix(:strmin(errprefix)),
     &        'Different retrofit year range for same retrofit ID ',
     &        'and overlapping engines','Record','Retro ID','SCC',
     &        'Tech Type','Min HP','Max HP','RY Start','RY End',
     &        rtrftrec(j),rtrftid(j),rtrftscc(j),rtrfttechtype(j),
     &        rtrfthpmn(j),rtrfthpmx(j),rtrftryst(j),rtrftryen(j),
     &        rtrftrec(k),rtrftid(k),rtrftscc(k),rtrfttechtype(k),
     &        rtrfthpmn(k),rtrfthpmx(k),rtrftryst(k),rtrftryen(k)
      if( lmsgfl ) then
          write(IOWMSG,8000,ERR=9999)
     &            errprefix(:strmin(errprefix)),
     &            'Different retrofit year range for same retrofit ID ',
     &            'and overlapping engines','Record','Retro ID','SCC',
     &            'Tech Type','Min HP','Max HP','RY Start','RY End',
     &            rtrftrec(j),rtrftid(j),rtrftscc(j),rtrfttechtype(j),
     &            rtrfthpmn(j),rtrfthpmx(j),rtrftryst(j),rtrftryen(j),
     &            rtrftrec(k),rtrftid(k),rtrftscc(k),rtrfttechtype(k),
     &            rtrfthpmn(k),rtrfthpmx(k),rtrftryst(k),rtrftryen(k)
      endif
      goto 9999
c
 7001 continue
      write(IOWSTD,8001,ERR=9999)
     &        errprefix(:strmin(errprefix)),
     &        'Different model year range for same retrofit ID ',
     &        'and overlapping engines','Record','Retro ID','SCC',
     &        'Tech Type','Min HP','Max HP','MY Start','MY End',
     &        rtrftrec(j),rtrftid(j),rtrftscc(j),rtrfttechtype(j),
     &        rtrfthpmn(j),rtrfthpmx(j),rtrftmyst(j),rtrftmyen(j),
     &        rtrftrec(k),rtrftid(k),rtrftscc(k),rtrfttechtype(k),
     &        rtrfthpmn(k),rtrfthpmx(k),rtrftmyst(k),rtrftmyen(k)
      if( lmsgfl ) then
          write(IOWMSG,8001,ERR=9999)
     &            errprefix(:strmin(errprefix)),
     &            'Different model year range for same retrofit ID ',
     &            'and overlapping engines','Record','Retro ID','SCC',
     &            'Tech Type','Min HP','Max HP','MY Start','MY End',
     &            rtrftrec(j),rtrftid(j),rtrftscc(j),rtrfttechtype(j),
     &            rtrfthpmn(j),rtrfthpmx(j),rtrftmyst(j),rtrftmyen(j),
     &            rtrftrec(k),rtrftid(k),rtrftscc(k),rtrfttechtype(k),
     &            rtrfthpmn(k),rtrfthpmx(k),rtrftmyst(k),rtrftmyen(k)
      endif
      goto 9999
c
 7002 continue
      write(IOWSTD,8002,ERR=9999)
     &        errprefix(:strmin(errprefix)),
     &        'Different effectiveness for same retrofit ID, ',
     &        'pollutant, and overlapping engines','Record',
     &        'Retro ID','Pollutant','SCC','Tech Type',
     &        'Min HP','Max HP','Effectiveness',
     &        rtrftrec(j),rtrftid(j),rtrftpollutant(j),rtrftscc(j),
     &        rtrfttechtype(j),rtrfthpmn(j),rtrfthpmx(j),
     &        rtrfteffect(j),
     &        rtrftrec(k),rtrftid(k),rtrftpollutant(k),rtrftscc(k),
     &        rtrfttechtype(k),rtrfthpmn(k),rtrfthpmx(k),
     &        rtrfteffect(k)
      if( lmsgfl ) then
          write(IOWMSG,8002,ERR=9999)
     &            errprefix(:strmin(errprefix)),
     &            'Different effectiveness for same retrofit ID, ',
     &            'pollutant, and overlapping engines','Record',
     &            'Retro ID','Pollutant','SCC','Tech Type',
     &            'Min HP','Max HP','Effectiveness',
     &            rtrftrec(j),rtrftid(j),rtrftpollutant(j),rtrftscc(j),
     &            rtrfttechtype(j),rtrfthpmn(j),rtrfthpmx(j),
     &            rtrfteffect(j),
     &            rtrftrec(k),rtrftid(k),rtrftpollutant(k),rtrftscc(k),
     &            rtrfttechtype(k),rtrfthpmn(k),rtrfthpmx(k),
     &            rtrfteffect(k)
      endif
      goto 9999
c
 7003 continue
      write(IOWSTD,8003,ERR=9999)
     &        errprefix(:strmin(errprefix)),
     &        'Sum of fraction-or-N retrofitted per pollutant ',
     &        'not the same for for retrofit ID ',rtrftid(j),
     &        'Pollutant','Frac-or-N Retro',
     &        rtrftplltnt(expctplltntidx),
     &        plltntfracretro(expctplltntidx),
     &        rtrftplltnt(k),plltntfracretro(k)
      if( lmsgfl ) then
          write(IOWMSG,8003,ERR=9999)
     &            errprefix(:strmin(errprefix)),
     &            'Sum of fraction-or-N retrofitted per pollutant ',
     &            'not the same for for retrofit ID ',rtrftid(j),
     &            'Pollutant','Frac-or-N Retro',
     &            rtrftplltnt(expctplltntidx),
     &            plltntfracretro(expctplltntidx),
     &            rtrftplltnt(k),plltntfracretro(k)
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c  Format statements:
c-----------------------------------------------------------------------
c
 8000 format(/,1X,A,/,6X,2A,/,11X,A,2X,A,2X,A,9X,A,3X,A,2X,A,2X,A,
     &2X,A,2(/,11X,I6,2X,I8,2X,A,2X,A,2X,F6.0,2X,F6.0,2X,I8,2X,I6))
 8001 format(/,1X,A,/,6X,2A,/,11X,A,2X,A,2X,A,9X,A,3X,A,2X,A,2X,A,
     &2X,A,2(/,11X,I6,2X,I8,2X,A,2X,A,2X,F6.0,2X,F6.0,2X,I8,2X,I6))
 8002 format(/,1X,A,/,6X,2A,/,11X,A,2X,A,2X,A,3X,A,9X,A,3X,A,2X,A,
     &2X,A,2(/,11X,I6,2X,I8,2X,A,2X,A,2X,A,2X,F6.0,2X,F6.0,2X,F13.3))
 8003 format(/,1X,A,/,6X,2A,I5,/,11X,A,3X,A,2(/,11X,A,2X,F15.3))
 8100 format(/,1X,A,/,6X,4A,I5,/)
c
c-----------------------------------------------------------------------
c  Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
