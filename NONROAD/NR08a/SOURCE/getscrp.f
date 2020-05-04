C**** GETSCRP
c
      subroutine getscrp( ierr, scrpbin, scrppct, scrpnam )
c
c---------------------------------------------------------------------
c
c     Returns the scrappage curve specified by scrpnam.  Uses the
c     default scrappage curve if the name specified is 'DEFAULT'
c     or is not found in the alternate scrappage curve arrays.
c   
c     Argument declaration
c       Outputs:
c             ierr     I  error indicator flag
c             scrpbin  R  array of scrappage bin values
c             scrppct  R  percent of equipment left in each bin
c       Inputs:
c             scrpnam  C  name of the scrappage curve to search
c
c---------------------------------------------------------------------
c  LOG:
c---------------------------------------------------------------------
c
c        04-05-05  -cimulus-  original development
c
c---------------------------------------------------------------------
c  Include files:
c---------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdeqp.inc'
c
c---------------------------------------------------------------------
c  Argument declarations:
c---------------------------------------------------------------------
c
      integer*4    ierr
      real*4       scrpbin(MXSCRP)
      real*4       scrppct(MXSCRP)
      character*10 scrpnam
c
c---------------------------------------------------------------------
c  External functions:
c---------------------------------------------------------------------
c
c   fndchr  I   returns the index of string in array of strings
c
      integer*4 fndchr
c
c---------------------------------------------------------------------
c  Local variables:
c---------------------------------------------------------------------
c
      integer*4 i              ! loop iterator
      integer*4 idxalt         ! index of alternate scrappage curve, if any
c
c---------------------------------------------------------------------
c  Entry point:
c---------------------------------------------------------------------
c
      ierr = IFAIL
c
c   --- determine which scrappage curve to search ---
c
      do i=1,MXSCRP
         scrpbin(i) = scpbin(i)
         scrppct(i) = scppct(i)
      end do
      if( scrpnam .NE. 'DEFAULT' ) then
          idxalt = fndchr( scrpnam, 10, altnam, naltnm )
          if( idxalt .GT. 0 ) then
              do i=1,MXSCRP
                  scrpbin(i) = altbin(i)
                  scrppct(i) = altpct(idxalt,i)
              end do
          else
              write(IOWMSG,'(/,1X,4A,/)',ERR=9999) 'WARNING:  Cannot ',
     &                'find /ALTERNATE SCRAPPAGE/ curve ',scrpnam,
     &                '   Using DEFAULT curve.'
              nwarn = nwarn + 1
          endif
      endif
c
c   --- set error code to sucess and return ---
c
      ierr = ISUCES
      goto 9999
c
c---------------------------------------------------------------------
c  Error messages:
c---------------------------------------------------------------------
c
c
c---------------------------------------------------------------------
c  Format statements:
c---------------------------------------------------------------------
c
c
c---------------------------------------------------------------------
c  Return point:
c---------------------------------------------------------------------
c
 9999 continue
      return
      end
