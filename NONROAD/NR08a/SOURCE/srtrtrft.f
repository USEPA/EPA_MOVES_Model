C**** SRTRTRFT
c
      recursive subroutine srtrtrft( cmprtype, startindex, stopindex )
c
c-----------------------------------------------------------------------
c
c     Sorts the retrofit-record arrays based on the comparison done in
c     cmprrtrft() (the comparison done depends on cmprtype), using a
c     quick-sort algorithm.
c
c     Argument declaration
c       Outputs:
c             (none)
c       Inputs:
c             cmprtype    I  comparison type, passed to cmprrtrft() (see
c                            comments in cmprrtrft() for details)
c             startindex  I  start index into retrofit-record arrays
c                            of the current partition to be sorted
c             stopindex   I  stop index into retrofit-record arrays
c                            of the current partition to be sorted
c
c-----------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------
c
c      05/24/05  -cimulus-  original development
c
c-----------------------------------------------------------------------
c  Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdrtrft.inc'
c
c-----------------------------------------------------------------------
c  Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 cmprtype
      integer*4 startindex
      integer*4 stopindex
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c   cmprrtrft  I  compares two retrofit records
c                 (< 0 if a < b, 0 if a = b, > 0 if a > b)
c
      integer*4 cmprrtrft
c
c-----------------------------------------------------------------------
c  Local variables:
c-----------------------------------------------------------------------
c
      integer*4 l
      integer*4 r
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
      if( stopindex - startindex .GT. 1 ) then
          l = startindex + 1
          r = stopindex

          do while ( l .LT. r )
              do while ( l .LE. stopindex )
                  if( cmprrtrft(cmprtype, l, startindex) .LE. 0 ) then
                      l = l + 1
                  else
                      exit
                  endif
              end do
              do while ( r .GT. startindex )
                  if( cmprrtrft(cmprtype, r, startindex) .GE. 0 ) then
                      r = r - 1
                  else
                      exit
                  endif
              end do

              if( l .LT. r ) then
                  call swaprtrft(l, r)
              else
                  call swaprtrft(startindex, r)
              endif
          end do

          call srtrtrft(cmprtype, startindex, r - 1)
          call srtrtrft(cmprtype, r + 1, stopindex)
      else if( stopindex - startindex .EQ. 1 ) then
          if( cmprrtrft(cmprtype, startindex, stopindex) .GT. 0 ) then
              call swaprtrft(stopindex, startindex)
          endif
      endif
c
c-----------------------------------------------------------------------
c  Error messages:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c  Format statements:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c  Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
