C**** SWAPRTRFT
c
      subroutine swaprtrft( a, b )
c
c-----------------------------------------------------------------------
c
c     This routine swaps two retrofit records.
c
c     Argument declaration
c       Outputs:
c             (none)
c       Inputs:
c             a  I  index of first retrofit record to be swapped
c             b  I  index of second retrofit record to be swapped
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
      integer*4     rec
      integer*4     ryst
      integer*4     ryen
      integer*4     myst
      integer*4     myen
      character*10  scc
      character*10  techtype
      real*4        hpmn
      real*4        hpmx
      real*4        annualfracorn
      real*4        effect
      character*10  pollutant
      integer*4     plltntidx
      integer*4     id
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c   --- store a in temp variables ---
c
      rec = rtrftrec(a)
      ryst = rtrftryst(a)
      ryen = rtrftryen(a)
      myst = rtrftmyst(a)
      myen = rtrftmyen(a)
      scc = rtrftscc(a)
      techtype = rtrfttechtype(a)
      hpmn = rtrfthpmn(a)
      hpmx = rtrfthpmx(a)
      annualfracorn = rtrftannualfracorn(a)
      effect = rtrfteffect(a)
      pollutant = rtrftpollutant(a)
      plltntidx = rtrftplltntidx(a)
      id = rtrftid(a)
c
c   --- set a to b ---
c
      rtrftrec(a) = rtrftrec(b)
      rtrftryst(a) = rtrftryst(b)
      rtrftryen(a) = rtrftryen(b)
      rtrftmyst(a) = rtrftmyst(b)
      rtrftmyen(a) = rtrftmyen(b)
      rtrftscc(a) = rtrftscc(b)
      rtrfttechtype(a) = rtrfttechtype(b)
      rtrfthpmn(a) = rtrfthpmn(b)
      rtrfthpmx(a) = rtrfthpmx(b)
      rtrftannualfracorn(a) = rtrftannualfracorn(b)
      rtrfteffect(a) = rtrfteffect(b)
      rtrftpollutant(a) = rtrftpollutant(b)
      rtrftplltntidx(a) = rtrftplltntidx(b)
      rtrftid(a) = rtrftid(b)
c
c   --- set b to original a ---
c
      rtrftrec(b) = rec
      rtrftryst(b) = ryst
      rtrftryen(b) = ryen
      rtrftmyst(b) = myst
      rtrftmyen(b) = myen
      rtrftscc(b) = scc
      rtrfttechtype(b) = techtype
      rtrfthpmn(b) = hpmn
      rtrfthpmx(b) = hpmx
      rtrftannualfracorn(b) = annualfracorn
      rtrfteffect(b) = effect
      rtrftpollutant(b) = pollutant
      rtrftplltntidx(b) = plltntidx
      rtrftid(b) = id
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
