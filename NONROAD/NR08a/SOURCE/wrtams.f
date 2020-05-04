C**** WRTAMS
c
      subroutine wrtams( ierr, emsams, asccod )
c
c-----------------------------------------------------------------------
c
c    writes the records for the EPS2 AMS file
c
c    Argument description:
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c       emsams  R  array of emissions
c       asccod  C  SCC code
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      06/01/97  --gmw--  original development
c      06/14/04  --dfk--  commented out resting loss
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdeqp.inc'
      include 'nonrdreg.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      real*4       emsams(NCNTY,MXPOL)
      character*10 asccod
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c  strmin   I   returns the length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*5 isbrg
      integer*4   idxfip, i
      real*4      crtpol
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      isbrg = ' '
c
c   --- loop over counties ---
c
      do 10 idxfip=1,NCNTY
         if( .NOT. lfipcd(idxfip) ) goto 10
c
c   --- HC species ---
c
         crtpol = emsams(idxfip,IDXTHC) + emsams(idxfip,IDXCRA) +
     &             emsams(idxfip,IDXDIU) + emsams(idxfip,IDXDIS) +
     &               emsams(idxfip,IDXSPL) + emsams(idxfip,IDXRLS)  
c     &                                          emsams(idxfip,IDXRST)
         crtpol = crtpol * cvtams
         if( crtpol .GT. 0. ) write(IOWAMS,9000) itype, irefyr, 
     &                          ibasyr, inetyp,  fipcod(idxfip), 
     &                             isbrg, asccod(1:4), asccod, iperod,
     &                                ibegdt, ienddt, ISCTHC, crtpol,
     &                                                 ' ',(-9,i=1,36)
c
c   --- NOx species ---
c
         crtpol = emsams(idxfip,IDXNOX)
         crtpol = crtpol * cvtams
         if( crtpol .GT. 0. ) write(IOWAMS,9000,ERR=7000) itype, irefyr, 
     &                                 ibasyr, inetyp,  fipcod(idxfip), 
     &                             isbrg, asccod(1:4), asccod, iperod,
     &                                ibegdt, ienddt, ISCNOX, crtpol,
     &                                                 ' ',(-9,i=1,36)
c
c   --- CO species ---
c
         crtpol = emsams(idxfip,IDXCO)
         crtpol = crtpol * cvtams
         if( crtpol .GT. 0. ) write(IOWAMS,9000,ERR=7000) itype, irefyr, 
     &                                 ibasyr, inetyp,  fipcod(idxfip), 
     &                             isbrg, asccod(1:4), asccod, iperod,
     &                                ibegdt, ienddt, ISCCO, crtpol,
     &                                                 ' ',(-9,i=1,36)
c
c   --- SOx species ---
c
         crtpol = emsams(idxfip,IDXSOX)
         crtpol = crtpol * cvtams
         if( crtpol .GT. 0. ) write(IOWAMS,9000,ERR=7000) itype, irefyr, 
     &                                 ibasyr, inetyp,  fipcod(idxfip), 
     &                             isbrg, asccod(1:4), asccod, iperod,
     &                                ibegdt, ienddt, ISCSOX, crtpol,
     &                                                 ' ',(-9,i=1,36)
c
c   --- HC species ---
c
         crtpol = emsams(idxfip,IDXPM)
         crtpol = crtpol * cvtams
         if( crtpol .GT. 0. ) write(IOWAMS,9000,ERR=7000) itype, irefyr, 
     &                                 ibasyr, inetyp,  fipcod(idxfip), 
     &                             isbrg, asccod(1:4), asccod, iperod,
     &                                ibegdt, ienddt, ISCPM, crtpol,
     &                                                 ' ',(-9,i=1,36)
c
c   --- set error code to succes and return ----
c
   10 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,3A)') 
     &                 'ERROR:  Writing to the EPS2 AMS file ',
     &                                           amsfl(:strmin(amsfl))
      write(IOWMSG,'(/,1X,3A)') 
     &                 'ERROR:  Writing to the EPS2 AMS file ',
     &                                           amsfl(:strmin(amsfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(A1,I2,1X,I2,A2,A5,1X,A5,1X,A4,1X,A10,1X,A2,1X,I8,1X,I8,
     &       1X,I5,1X,E10.4,1X,A3,3(1X,I5),1X,I3,1X,28(I5,1X),5(1X,I10))
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
