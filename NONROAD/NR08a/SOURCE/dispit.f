C**** DISPIT()
c
      subroutine dispit()
c
c-----------------------------------------------------------------------
c
c    displays a count of records processed 
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 ipct
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
      nrecds = nrecds + 1
      if( ntotrc .GT. 0 ) then
         ipct = INT( 100.0 * FLOAT( nrecds ) / FLOAT( ntotrc ) )
      else
         ipct = 100
      endif
cgwilson      IF(MOD(FLOAT(IPCT),5.).EQ.0.)write(IOWSTD,9000,ERR=9999) ipct
CDFK      write(IOWSTD,9000,ERR=9999,advance='no') ipct
CDFK      call flush(IOWSTD)
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format('+','Processing...',I3,'%')
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
