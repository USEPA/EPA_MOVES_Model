C**** FNDSCRP
c
      function fndscrp( frcmlusd, scrpbin, scrppct )
c
      IMPLICIT NONE
c
      real*4 fndscrp
c---------------------------------------------------------------------
c
c     Finds the percent scrapped from the provided array based on
c     the provided fraction of median life used
c   
c     Return value:
c         <  0.  =  no match found
c         >= 0.  =  percent scrapped for best match
c
c     Argument declaration
c       Inputs:
c             frcmlusd  R  fraction of median life used
c             scrpbin   R  array of scrappage bin values
c             scrppct   R  percent of equipment left in each bin
c
c---------------------------------------------------------------------
c  LOG:
c---------------------------------------------------------------------
c
c        03-23-05  -cimulus-  original development
c        04-05-05  -cimulus-  for efficiency, pass in scrpbin and
c                             scrppct instead of finding them for each
c                             call to this function
c        07/20/05  -cimulus-  removed unused function
c        07/20/05  -cimulus-  floating-point comparison for equality
c                             okay; fndscrp value is hard-coded to -1
c                             by default
c
c---------------------------------------------------------------------
c  Include files:
c---------------------------------------------------------------------
c
      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdeqp.inc'
c
c---------------------------------------------------------------------
c  Argument declarations:
c---------------------------------------------------------------------
c
      real*4 frcmlusd
      real*4 scrpbin(MXSCRP)
      real*4 scrppct(MXSCRP)
c
c---------------------------------------------------------------------
c  External functions:
c---------------------------------------------------------------------
c
c
c---------------------------------------------------------------------
c  Local variables:
c---------------------------------------------------------------------
c
      integer*4 i
c
c---------------------------------------------------------------------
c  Entry point:
c---------------------------------------------------------------------
c
c   --- assume no match
c
      fndscrp = -1.
c
c   --- if fraction of median life used falls below valid
c       range, return first value ---
c
      if( frcmlusd .LT. scrpbin(1) ) then
          fndscrp = scrppct(1)
          goto 9999
      endif
c
c   --- search the scrappage curve for the best match ---
c
      do i=1,MXSCRP-1
          if( scrpbin(i + 1) .GT. frcmlusd ) then
              fndscrp = scrppct(i)
              exit
          endif
      end do
c
c   --- if no match found, fraction of median life used falls above
c       valid range, so return maximum value ---
c
      if( fndscrp .EQ. -1. ) then ! floating-point comparison for equality okay; fndscrp value is hard-coded to -1 by default
          fndscrp = scrppct(MXSCRP)
      endif
c
c   --- return to calling routine ---
c
      goto 9999
c
c---------------------------------------------------------------------
c  Return point:
c---------------------------------------------------------------------
c
 9999 continue
      return
      end
