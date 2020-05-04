C**** CLSNON 
c
      subroutine clsnon( )
c
c-----------------------------------------------------------------------
c
c    closes all of the file used in the NONROAD program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      06/10/97  --gwilson--  original development
c      05/10/04  --dfk---     added closure of evap tech group file
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
c    Argument declarations:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
      close( IORSTD )
c
      close( IOWSTD )
c
      close( IORUSR )
c
      close( IORPOP )
c
      close( IORALO )
c
      close( IORIND )
c
      close( IORACT )
c
      close( IORSES )
c
      close( IORFAC )
c
      close( IORDAC )
c
      close( IORGRW )
c
      close( IORTCH )
c
      close( IOREVTCH )
c
      close( IORREG )
c
      close( IOWMSG )
c
      close( IOWDAT )
c
      close( IOWAMS )
c
      close( IOSDIR, status='DELETE' )
c
      close( IOSPOP, status='DELETE' )
c
      close( IOSIND, status='DELETE' )
c
      close( IOSGRW, status='DELETE' )
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
