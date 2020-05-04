C**** CMPRRTRFT
c
      integer*4 function cmprrtrft( cmprtype, a, b )
c
c-----------------------------------------------------------------------
c
c     This function compares two retrofit records, first by model year
c     end and then by maximum HP.
c
c     The comparison that is made depends on cmprtype:
c         1 = compare retrofit ID, then pollutant, then record number
c         2 = compare model year end, then maximum HP
c
c     Return value:
c         < 0 =  retrofit a is less than retrofit b
c           0 =  retrofit a is equal to retrofit b
c         > 0 =  retrofit a is greater than retrofit b
c
c     Argument declaration
c       Inputs:
c             cmprtype  I  comparison type
c             a         I  index of first retrofit record to compare
c             b         I  index of second retrofit record to compare
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
      include 'nonrdrtrft.inc'
c
c-----------------------------------------------------------------------
c  Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 cmprtype
      integer*4 a
      integer*4 b
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
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c   --- assume equal ---
c
      cmprrtrft = 0
c
c
c   --- compare retrofit ID, then pollutant, then record number ---
c
      if( cmprtype .EQ. 1 ) then
c
c       --- compare retrofit ID values ---
c
          if( rtrftid(a) .LT. rtrftid(b) ) then
              cmprrtrft = -1
          else if( rtrftid(a) .GT. rtrftid(b) ) then
              cmprrtrft = 1
          else
c
c           --- retrofit ID values are equal,
c               so compare pollutant values ---
c
              if( rtrftplltntidx(a) .LT. rtrftplltntidx(b) ) then
                  cmprrtrft = -1
              else if( rtrftplltntidx(a) .GT. rtrftplltntidx(b) ) then
                  cmprrtrft = 1
              else
c
c               --- retrofit ID and pollutant values are equal,
c                   so compare record number ---
c
                  if( rtrftrec(a) .LT. rtrftrec(b) ) then
                      cmprrtrft = -1
                  else if( rtrftrec(a) .GT. rtrftrec(b) ) then
                      cmprrtrft = 1
                  else
c
c                   --- retrofit ID, pollutant, and record number
c                       values are equal, so compare ... ---
c
                  endif
              endif
          endif
c
c   --- compare model year end, then maximum HP ---
c
      else if( cmprtype .EQ. 2 ) then
c
c       --- compare model year end values ---
c       --- note:  an optimization in fndrtrft() depends on the fact
c                  that model year end is the first comparison
c                  criterion; if this ever changes, fndrtrft() will
c                  need to be modified ---
c
          if( rtrftmyen(a) .LT. rtrftmyen(b) ) then
              cmprrtrft = -1
          else if( rtrftmyen(a) .GT. rtrftmyen(b) ) then
              cmprrtrft = 1
          else
c
c           --- model year end values are equal,
c               so compare maximum HP values ---
c
              if( rtrfthpmx(a) .LT. rtrfthpmx(b) ) then
                  cmprrtrft = -1
              else if( rtrfthpmx(a) .GT. rtrfthpmx(b) ) then
                  cmprrtrft = 1
              else
c
c               --- model year end and maximum HP values are equal,
c                   so compare ... ---
c
              endif
          endif
c
c   --- invalid comparison type ---
c
      else
c
          goto 9999
c
      endif
c
c   --- return ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c  Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
