C**** INITRTRFT
c
      subroutine initrtrft()
c
c-----------------------------------------------------------------------
c
c     Initializes the fixed arrays that are used for retrofit
c     functionality, such as input-validation lookup arrays.
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
      include 'nonrdrtrft.inc'
c
c-----------------------------------------------------------------------
c  Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c   --- Initialize retrofit-record arrays ---
c
      rtrftcount = 0
      do i = 1, MXRTRFT
          rtrftryst(i) = 0
          rtrftryen(i) = 0
          rtrftmyst(i) = 0
          rtrftmyen(i) = 0
          rtrftscc(i) = ' '
          rtrfttechtype(i) = ' '
          rtrfthpmn(i) = 0.
          rtrfthpmx(i) = 0.
          rtrftannualfracorn(i) = 0.
          rtrfteffect(i) = 0.
          rtrftpollutant(i) = ' '
          rtrftplltntidx(i) = 0
          rtrftid(i) = 0
      end do
c
c   --- Pollutants that are allowed for retrofit records ---
c
      rtrftplltnt(1) = 'HC'
      rtrftplltnt(2) = 'CO'
      rtrftplltnt(3) = 'NOX'
      rtrftplltnt(4) = 'PM'
c
c   --- Map from index into rtrftplltnt to pollutant index
c       for the pollutants that are allowed for retrofit records ---
c
      rtrftplltntidxmp(1) = IDXTHC
      rtrftplltntidxmp(2) = IDXCO
      rtrftplltntidxmp(3) = IDXNOX
      rtrftplltntidxmp(4) = IDXPM
c
c   --- Intitialize pollutant reduction fractions to 0 ---
c
      do i = 1, MXPOL
          rtrftplltntrdfrc(i) = 0.
      end do
c
c-----------------------------------------------------------------------
c  Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
