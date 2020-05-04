C**** CHKASC 
c
      function chkasc( ascin, skipunreq )
C      
      IMPLICIT NONE
C
      logical*4 chkasc
c
c-----------------------------------------------------------------------
c
c     This routine searches through the array of equipment codes and
c     compares the codes to the code in the argument list.  It will
c     skip any SCC codes that were not requested.  The comparison is
c     done to make sure that the code in the argument list is needed.
c     Since the code in the argument list may be a global code it is
c     not as simple as just check for an exact match.  To do this,
c     pass false for skipunreq.
c
c     This can also be used to do a straight validation of the input
c     code (i.e., whether or not the SCC is valid, as opposed to
c     whether or not the code was requested).  To do this, pass true
c     for skipunreq.
c
c   Arguments:
c     Inputs:
c       ascin      C  code to match
c       skipunreq  L  whether or not to skip unrequested SCCs
c     Return Value:
c       .TRUE.   = code will be used
c       .FALSE.  = code will not be used
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     11/10/95  -gmw-  Original development
c     05/19/05  -cimulus-  Added skipunreq parameter to allow alternate
c                          usage of this function for SCC validation (in
c                          addition to being able to use if for
c                          determining whether or not an SCC or global
c                          was requested)
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(10) ascin
      logical*4      skipunreq
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'nonrdprm.inc'
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      chkasc = .FALSE.
c
c   ---- loop through the array ---
c
      do 10 i=1,NEQNAM
c
c   ---- skip the equipment code if not requested
c        and skipping is desired ----
c
         if( (.NOT. lascat(i)) .AND. skipunreq ) goto 10
c
c   --- if exact match set return value and return ---
c
         if( ascin .EQ. eqpcod(i) ) then
             chkasc = .TRUE.
             goto 9999
         endif
c
c   --- if input SCC is global check for a global match ---
c
         if( ascin(5:10) .EQ. '000000' ) then
             if( ascin(1:4) .EQ. eqpcod(i)(1:4) ) then
                 chkasc = .TRUE.
                 goto 9999
              endif
         else if( ascin(8:10) .EQ. '000' ) then
             if( ascin(1:7) .EQ. eqpcod(i)(1:7) ) then
                 chkasc = .TRUE.
                 goto 9999
             endif
         endif
c
c   --- no match yet, check the next code ---
c
   10 continue
c
c   ---- no match, return false ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
