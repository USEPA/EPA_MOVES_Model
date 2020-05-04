C**** FNDRTRFT
c
      subroutine fndrtrft( ierr, fltrtyp, scc, hpavg, mdlyr, techtype )
c
c-----------------------------------------------------------------------
c
c     Filters the retrofit-record arrays down to the records that could
c     possibly affect the current iteration of SCC-HP combination, model
c     year, and tech type.  This is done in phases:  SCC-HP combination,
c     then model year, then tech type.
c
c     Does not actually modify the retrofit-record arrays.  Fills the
c     appropriate (based on fltrtyp) rtrftfltr# array and sets
c     rtrftfltr#cnt instead.  The indexes of the filtered arrays are
c     stored in the same order as they are in the retrofit arrays, so
c     any sorting that has been done on the retrofit arrays applies to
c     the filtered arrays.
c
c     The filtering that is done depends on fltrtyp:
c         1 = filter entire set of retrofit records down to the retrofits
c             that match the current scc and hpavg; ignores mdlyr
c             and techtype
c         2 = filter the previously filtered retrofits from type 1 down
c             to the retrofits that match the current model year;
c             assumes type 1 filtering has already been done, and that
c             subsequent calls for type 2 filtering will have a higher
c             mdlyr; ignores scc, hpavg, and techtype
c         3 = filter the previously filtered retrofits from type 2 down
c             to the retrofits that match the current tech type;
c             assumes type 2 filtering has already been done; ignores
c             scc, hpavg, and mdlyr
c
c     Argument declaration
c       Outputs:
c             ierr  I  error flag
c       Inputs:
c             fltrtyp   I  the filter type that is being executed (see
c                          notes above)
c             scc       C  the SCC to filter by
c             hpavg     R  the HP average to filter by
c             mdlyr     I  the model year to filter by
c             techtype  C  the tech type to filter by
c
c-----------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------
c
c      05/25/05  -cimulus-  original development
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
      integer*4    ierr
      integer*4    fltrtyp
      character*10 scc
      real*4       hpavg
      integer*4    mdlyr
      character*10 techtype
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c  Local variables:
c-----------------------------------------------------------------------
c
      integer*4    i
      character*10 iterscc ! SCC of iteration retrofit record
      logical*4    foundnewfltr1mnidx
      character*10 itertechtype ! tech type of iteration retrofit record
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- filter by SCC and HP ---
c
      if( fltrtyp .EQ. 1 ) then
c
c       --- filter entire set of retrofit records down to the
c           retrofits that match the SCC and HP average ---
c
          rtrftfltr1cnt = 0
          rtrftfltr1mnidx = 1 ! start type 2 filtering at the first type 1 index
          rtrftfltr2cnt = 0
          rtrftfltr3cnt = 0
          do i = 1, rtrftcount ! for all retrofit records
c
c           --- if the SCC doesn't match, skip this record ---
c
              iterscc = rtrftscc(i)
              if( .NOT. ( iterscc .EQ. RTRFTSCCALL
     &                .OR. scc .EQ. iterscc
     &                .OR. ( iterscc(5:10) .EQ. '000000'
     &                .AND. scc(1:4) .EQ. iterscc(1:4) )
     &                .OR. ( iterscc(8:10) .EQ. '000'
     &                .AND. scc(1:7) .EQ. iterscc(1:7) ) ) ) then
                  cycle
              endif
c
c           --- if the HP doesn't match, skip this record ---
c
              if( hpavg .LE. rtrfthpmn(i)
     &                .OR. hpavg .GT. rtrfthpmx(i) ) then
                  cycle
              endif
c
c           --- the record matches, so add it to the filter array ---
c
              rtrftfltr1cnt = rtrftfltr1cnt + 1
              rtrftfltr1(rtrftfltr1cnt) = i
c
          end do
c
c   --- filter by model year ---
c
      else if( fltrtyp .EQ. 2 ) then
c
c       --- filter the previously filtered retrofits from type 1 down
c           to the retrofits that match the model year ---
c
          foundnewfltr1mnidx = .FALSE.
          rtrftfltr2cnt = 0
          rtrftfltr3cnt = 0
          do i = rtrftfltr1mnidx, rtrftfltr1cnt ! for all retrofit records filtered in the type 1 phase with model-year end >= mdlyr
c
c           --- if haven't found new minimum type 1 index to search
c               and this index would fit the subsequent model year,
c               update the minimum type 1 index ---
c           --- note:  this optimization only works because the first
c                      comparison field used for sorting the retrofit
c                      arrays is model year end; if that every changes
c                      (in cmprrtrft()), this must be removed ---
c
              if ( .NOT. foundnewfltr1mnidx .AND.
     &                rtrftmyen(rtrftfltr1(i)) .GE. (mdlyr + 1) ) then
                  foundnewfltr1mnidx = .TRUE.
                  rtrftfltr1mnidx = i
              endif
c
c           --- if the model year doesn't match, skip this record ---
c
              if( mdlyr .LT. rtrftmyst(rtrftfltr1(i))
     &                .OR. mdlyr .GT. rtrftmyen(rtrftfltr1(i)) ) then
                  cycle
              endif
c
c           --- the record matches, so add it to the filter array ---
c
              rtrftfltr2cnt = rtrftfltr2cnt + 1
              rtrftfltr2(rtrftfltr2cnt) = rtrftfltr1(i)
c
          end do
c
c   --- filter by tech type ---
c
      else if( fltrtyp .EQ. 3 ) then
c
c       --- filter the previously filtered retrofits from type 2 down
c           to the retrofits that match the tech type ---
c
          rtrftfltr3cnt = 0
          do i = 1, rtrftfltr2cnt ! for all retrofit records filtered in the type 2 phase
c
c           --- if the tech type doesn't match, skip this record ---
c
              itertechtype = rtrfttechtype(rtrftfltr2(i))
              if( .NOT. ( itertechtype .EQ. RTRFTTCHTYPALL
     &                .OR. techtype .EQ. itertechtype ) ) then
                  cycle
              endif
c
c           --- the record matches, so add it to the filter array ---
c
              rtrftfltr3cnt = rtrftfltr3cnt + 1
              rtrftfltr3(rtrftfltr3cnt) = rtrftfltr2(i)
c
          end do
c
c   --- invalid filter type ---
c
      else
c
          goto 7000
c
      endif
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
     &        'ERROR: Finding matching retrofits; ',
     &        'invalid filter type: ',fltrtyp
      if( lmsgfl ) then
          write(IOWMSG,8000,ERR=9999)
     &            'ERROR: Finding matching retrofits; ',
     &            'invalid filter type: ',fltrtyp
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c  Format statements:
c-----------------------------------------------------------------------
c
 8000 format(/,1X,2A,I5)
c
c-----------------------------------------------------------------------
c  Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
