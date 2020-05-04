C**** WRTSUM
c
      subroutine wrtsum( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the summary of population records processed.
c
c    Argument description:
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/01/97  --gwilson--  original development
c      05/16/05  --cimulus--  increased size of strings in cntynm to 50
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdreg.inc'
      include 'nonrdeqp.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c 
      integer*4 ibegj, iendj, i, j
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      write(IOWMSG,'(/,A,/)',ERR=7000) 
     &            '   **** Number of Population Records Found ****'
c
c   --- if natinal records used echo such ---
c
      if( nnatrc .GT. 0 ) then
        write(IOWMSG,'(20X,3A,T45,A,I5)',ERR=7000) 'Entire U.S.',' ',
     &                                   'National Record',':',nnatrc
      endif
c
c   --- loop over the states ---
c
      do 10 i=1,NSTATE
         if( lstacd(i) .AND. lstlev(i) ) then
           write(IOWMSG,'(20X,3A,T77,A,I5)',ERR=7000) statcd(i),' ',
     &                                        statnm(i),':',nstarc(i)
           ibegj = idxcty(i)
           if( i .LT. NSTATE ) then
              iendj = idxcty(i+1) - 1
           else
              iendj = NCNTY
           endif
           do 20 j=ibegj,iendj
              if( lfipcd(j) .AND. lctlev(j) ) then
                  write(IOWMSG,'(20X,3A,T77,A,I5)',ERR=7000) 
     &                         fipcod(j),' ',cntynm(j),':',nctyrc(j)
              endif
   20      continue
         endif
   10  continue
c
c   --- set error code to succes and return ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,3A)') 
     &                 'ERROR:  Writing to the message data file ',
     &                                           datfl(:strmin(datfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(6A,I5,A)
 9001 format(A,E15.8)
 9002 format(A)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
