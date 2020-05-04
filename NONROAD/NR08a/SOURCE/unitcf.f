C****UNITCF
c
      function unitcf(index,hpavg,indlod,densit,bsfc)
c
      IMPLICIT NONE
c
      real*4 unitcf
c
c-----------------------------------------------------------------------
c
c   This function returns the unit conversion factor for the
c   corresponding unit index.  Used by subroutine emfclc.
c    Argument description:
c      Input:
c         index   I  index specifiying unit to be converted
c         hpavg   R  average HP for equipment
c         indlod  I  index of equipment type in acticity arrays
c         densit  R  density of fuel
c         bsfc    R  BSFC of equipment
c      Output:
c         unitcf  R  conversion factor
c
c----------------------------------------------------------------------
c  LOG:
c----------------------------------------------------------------------
c     03/05/96  --djk--  original development
c     07/19/96  --jlf--  removed old writes
c----------------------------------------------------------------------
c  Include files:
c----------------------------------------------------------------------
c
      include 'nonrdprm.inc'
      include 'nonrdefc.inc'
      include 'nonrdact.inc'
c
c----------------------------------------------------------------------
c  Argument declaration:
c----------------------------------------------------------------------
c
      integer*4 index
      integer*4 indlod
      real*4    hpavg
      real*4    densit
      real*4    bsfc
c
c----------------------------------------------------------------------
c  Local variables:
c----------------------------------------------------------------------
c
      real*4 cvttmp
c
c----------------------------------------------------------------------
c  Entry point:
c----------------------------------------------------------------------
c
      cvttmp = 1.0
c
      if( index .EQ. IDXGHP ) then
         cvttmp = hpavg * faclod(indlod)
      else if( index .EQ. IDXGAL ) then
         if( iactun(indlod) .EQ. IDXGLY .OR. 
     &                            iactun(indlod) .EQ. IDXGLD ) then
            cvttmp = 1.0 
         else
            if( densit .EQ. 0. ) then
               cvttmp = 0.
            else
               cvttmp = ( bsfc * faclod(indlod) * hpavg ) / densit
            endif
         endif
      else if( index .EQ. IDXGDY ) then
         cvttmp = FACGDY
      endif
      if( index .EQ. IDXMLT ) cvttmp = 1.0
c
c ---  return conversion factor  ---
c
      unitcf = cvttmp
      return
      end
