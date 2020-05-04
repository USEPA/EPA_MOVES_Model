C**** GETIME 
c
      subroutine getime( cdate )
c
c-----------------------------------------------------------------------
c
c   Description:
c
c     This routine returns the actual length of a string, i.e. with no
c     trailing blanks.
c
c   Arguments:
c
c     Outputs:
c       cdate    C   date as character string
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     11/10/91  -gmw-  Original development
c     07/20/05  -cimulus-  removed unused variables
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      character*(*) cdate
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'nonrdprm.inc'
      include 'nonrdio.inc'
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      character*3  months(12)
      integer*4    idatvl(8), i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data months /'Jan','Feb','Mar','Apr','May','Jun',
     &             'Jul','Aug','Sep','Oct','Nov','Dec'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize the string ---
c
      do 10 i=1,LEN(cdate)
           cdate(i:i) = ' '
   10 continue 
c
c   ---- call routine to get date ---
c
      call date_and_time( values=idatvl )
c
c   --- create the date string ---
c
      if( idatvl(2) .GT. 0 .AND. idatvl(2) .LE. 12 ) then
          write(cdate,9000,ERR=9999) months(idatvl(2)), idatvl(3),
     &                    idatvl(5), idatvl(6), idatvl(7), idatvl(1)
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(A,1X,I2.2,1X,I2.2,':',I2.2,':',I2.2,':',1X,I4)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
