C**** INTADJ
c
      subroutine intadj( )
c
c-----------------------------------------------------------------------
c  
c    This routine intializes the common variables used for the various 
c    emission adjustment factors.
c     Argument description:
c       Outputs:
c       Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      06/10/97  --gmw--  original development
c      09/22/04  --dfk--  SOXFUL(IDXDSM) initialization (commented out
c                         with NR04n2
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdusr.inc'
      include 'nonrdefc.inc'
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 idxbeg, idxend
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      idxbeg = 1
      idxend = 2
c
c  --- initialize the local arrays for sulfur content ---
c
      soxbas(IDXGS2) = SWTGS2
      soxbas(IDXGS4) = SWTGS4
      soxbas(IDXLPG) = SWTLPG
      soxbas(IDXCNG) = SWTCNG
      soxbas(IDXDSL) = SWTDSL
c
      soxfrc(IDXGS2) = SFCGS2
      soxfrc(IDXGS4) = SFCGS4
      soxfrc(IDXLPG) = SFCLPG
      soxfrc(IDXCNG) = SFCCNG
      soxfrc(IDXDSL) = SFCDSL
c
      soxful(IDXGS2) = soxgas 
      soxful(IDXGS4) = soxgas 
      soxful(IDXLPG) = soxcng 
      soxful(IDXCNG) = soxcng 
      soxful(IDXDSL) = soxdsl 
c     soxful(IDXDSM) = soxdsm 
c
      altfac(IDXGS2) = ALTGS2
      altfac(IDXGS4) = ALTGS4
      altfac(IDXLPG) = ALTLPG
      altfac(IDXCNG) = ALTCNG
      altfac(IDXDSL) = ALTDSL
c
      iyrbin(IDXSUM,1,idxbeg) = 1995
      iyrbin(IDXSUM,1,idxend) = 1996
      rfggs2(IDXSUM,1,IDXTHC) = 1.0
      rfggs2(IDXSUM,1,IDXCO) = 1.0
      rfggs2(IDXSUM,1,IDXNOX) = 1.0
      rfggs2(IDXSUM,1,IDXSOX) = 1.0
      rfggs2(IDXSUM,1,IDXPM) = 1.0
      rfggs4(IDXSUM,1,IDXTHC) = 1.0
      rfggs4(IDXSUM,1,IDXCO) = 1.0
      rfggs4(IDXSUM,1,IDXNOX) = 1.0
      rfggs4(IDXSUM,1,IDXSOX) = 1.0
      rfggs4(IDXSUM,1,IDXPM) = 1.0
c
      iyrbin(IDXSUM,2,idxbeg) = 1997
      iyrbin(IDXSUM,2,idxend) = 1999
      rfggs2(IDXSUM,2,IDXTHC) = 1.0
      rfggs2(IDXSUM,2,IDXCO) = 1.0
      rfggs2(IDXSUM,2,IDXNOX) = 1.0
      rfggs2(IDXSUM,2,IDXSOX) = 1.0
      rfggs2(IDXSUM,2,IDXPM) = 1.0
      rfggs4(IDXSUM,2,IDXTHC) = 1.0
      rfggs4(IDXSUM,2,IDXCO) = 1.0
      rfggs4(IDXSUM,2,IDXNOX) = 1.0
      rfggs4(IDXSUM,2,IDXSOX) = 1.0
      rfggs4(IDXSUM,2,IDXPM) = 1.0
c
      iyrbin(IDXSUM,3,idxbeg) = 2000
      iyrbin(IDXSUM,3,idxend) = 9999
      rfggs2(IDXSUM,3,IDXTHC) = 1.0
      rfggs2(IDXSUM,3,IDXCO) = 1.0
      rfggs2(IDXSUM,3,IDXNOX) = 1.0
      rfggs2(IDXSUM,3,IDXSOX) = 1.0
      rfggs2(IDXSUM,3,IDXPM) = 1.0
      rfggs4(IDXSUM,3,IDXTHC) = 1.0
      rfggs4(IDXSUM,3,IDXCO) = 1.0
      rfggs4(IDXSUM,3,IDXNOX) = 1.0
      rfggs4(IDXSUM,3,IDXSOX) = 1.0
      rfggs4(IDXSUM,3,IDXPM) = 1.0
c
      iyrbin(IDXWTR,1,idxbeg) = 1995
      iyrbin(IDXWTR,1,idxend) = 1996
      rfggs2(IDXWTR,3,IDXTHC) = 1.0
      rfggs2(IDXWTR,3,IDXCO) = 1.0
      rfggs2(IDXWTR,3,IDXNOX) = 1.0
      rfggs2(IDXWTR,3,IDXSOX) = 1.0
      rfggs2(IDXWTR,3,IDXPM) = 1.0
      rfggs4(IDXWTR,3,IDXTHC) = 1.0
      rfggs4(IDXWTR,3,IDXCO) = 1.0
      rfggs4(IDXWTR,3,IDXNOX) = 1.0
      rfggs4(IDXWTR,3,IDXSOX) = 1.0
      rfggs4(IDXWTR,3,IDXPM) = 1.0
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
