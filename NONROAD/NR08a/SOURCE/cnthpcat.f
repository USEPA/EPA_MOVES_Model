C**** CNTHPCAT
c
      integer*4 function cnthpcat( hpmn, hpmx )
c
c-----------------------------------------------------------------------
c
c     This function counts the number of HP categories defined by the
c     specified HP range.  Assumes that hpmn and hpmx are each 0, 9999,
c     or one of the values specified in hpclev, and that hpmn < hpmx.
c
c     Return value:
c         count of HP categories represented by the HP range
c
c     Argument declaration
c       Inputs:
c             hpmn  R  minimum HP
c             hpmx  R  maximum HP
c
c-----------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------
c
c      05/19/05  -cimulus-  original development
c      07/20/05  -cimulus-  floating-point comparison for equality okay;
c                           hpmn and hpmx values are read from file and
c                           must be 0, 9999, or one of the values in
c                           hpclev, which are hard-coded, rather than
c                           being calculated at runtime
c
c-----------------------------------------------------------------------
c  Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdeqp.inc'
c
c-----------------------------------------------------------------------
c  Argument declarations:
c-----------------------------------------------------------------------
c
      real*4 hpmn
      real*4 hpmx
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c   fndhpc  L  returns index into hpclev for specified HP
c
      integer*4 fndhpc
c
c-----------------------------------------------------------------------
c  Local variables:
c-----------------------------------------------------------------------
c
      integer*4 hpmnidx
      integer*4 hpmxidx
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c   --- assume no HP categories ---
c
      cnthpcat = 0
c
c   --- determine the category index of the minimum HP ---
c
      if( hpmn .EQ. 0. ) then ! floating-point comparison for equality okay; hpmn values are read from file and must be 0, 9999, or one of the values in hpclev, which are hard-coded, rather than being calculated at runtime
          hpmnidx = 0
      else if( hpmn .EQ. 9999. ) then ! floating-point comparison for equality okay; hpmn values are read from file and must be 0, 9999, or one of the values in hpclev, which are hard-coded, rather than being calculated at runtime
          hpmnidx = MXHPC + 1
      else
c
c       --- find matching HP category ---
c
          hpmnidx = fndhpc(hpmn)
          if( hpmnidx .EQ. 0 ) then
c
c           --- no matching category found, return 0 ---
c
              goto 9999
          endif
      endif
c
c   --- determine the category index of the maximum HP ---
c
      if( hpmx .EQ. 0. ) then ! floating-point comparison for equality okay; hpmx values are read from file and must be 0, 9999, or one of the values in hpclev, which are hard-coded, rather than being calculated at runtime
          hpmxidx = 0
      else if( hpmx .EQ. 9999. ) then ! floating-point comparison for equality okay; hpmx values are read from file and must be 0, 9999, or one of the values in hpclev, which are hard-coded, rather than being calculated at runtime
          hpmxidx = MXHPC + 1
      else
c
c       --- find matching HP category ---
c
          hpmxidx = fndhpc(hpmx)
          if( hpmxidx .EQ. 0 ) then
c
c           --- no matching category found, return 0 ---
c
              goto 9999
          endif
      endif
c
c   --- the count of categories is the difference
c       between the category indexes ---
c
      cnthpcat = hpmxidx - hpmnidx
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
