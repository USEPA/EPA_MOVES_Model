C**** FNDGXF
c
      function fndgxf( fipin, asccin, hpin )
C
      IMPLICIT NONE
C
      integer*4 fndgxf
c
c-----------------------------------------------------------------------
c
c     This routine searches through the array of SCC codes read in the
c     seasonality file and looks for the best match.
c
c   Arguments:
c     Inputs:
c       fipin    C   FIPS code (needed for state code)
c       asccin   C   SCC code to match
c       hpcat    R   HP category to match
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     01/10/94  -gmw-  Original development
c     12/11/01  cah  Allow exact SCC match with global FIPS match.
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'nonrdprm.inc'
      include 'nonrdgrw.inc'
ccc
cc      include 'nonrdio.inc'
ccc
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*10 asccin
      character*5  fipin
      real*4       hpin
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c   MXGLB  I  maximum number of global checks
c
      integer*4 MXGLB
c
      parameter( MXGLB = 3 )
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   ascglb   C   variable for storing global code
c
      character*10 ascglb(MXGLB)
      character*5  fipglb(MXGLB)
      integer*4    idxglb(MXGLB,MXGLB), idxfip, idx, i, j
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c  --- initialize the global codes ---
c
      fndgxf = 0
      do 10 i=1,MXGLB
         ascglb(i) = '0000000000'
         idxglb(1,i) = -9
         idxglb(2,i) = -9
         idxglb(3,i) = -9
   10 continue
      ascglb(1)(1:4) = asccin(1:4)
      ascglb(2)(1:7) = asccin(1:7)
      ascglb(3) = asccin
      fipglb(1) = '00000'
      fipglb(2) = fipin(1:2)//'000'
      fipglb(3) = fipin
c
c   ---- loop through the array of codes ---
c
      do 20 i=1,nrcgrx
c
c   ---- check if this is the correct FIPS state code ----
c
         idxfip = 0
         if( fipgrx(i).EQ. fipglb(1) ) idxfip = 1
         if( fipgrx(i).EQ. fipglb(2) ) idxfip = 2
         if( fipgrx(i).EQ. fipglb(3) ) idxfip = 3
         if( idxfip .LE. 0 ) goto 20
c
c   ---- check if the HP category is within the range ----
c
         if( hpin .LT. hpgrx(i,1) .OR. hpin .GT. hpgrx(i,2) ) goto 20
c
c   ---- if exact match, check for better HP category ---
c
         if( asccin .EQ. ascgrx(i) .AND. fipin .EQ. fipgrx(i) ) then
ccc
cc           write(IOWMSG,'(12X,A,2X,A,2X,A)',ERR=9999)
cc     &        asccin,ascgrx(i),fipglb(2)
ccc
             if( fndgxf .LE. 0 ) then
                 fndgxf = i
             else 
                 if( hpgrx(i,1) .GT. hpgrx(fndgxf,1) .OR. 
     &                           hpgrx(i,2) .LT. hpgrx(fndgxf,2) ) then
                     fndgxf = i
                 endif
             endif
         endif
c
c   ---- loop over global codes, setting indexes ---
c
         do 30 j=1,MXGLB
ccc
cc           write(IOWMSG,'(8X,A,2X,A,2X,A)',ERR=9999)
cc     &        asccin,ascgrx(i),ascglb(j)
ccc
c
             if( ascglb(j) .EQ. ascgrx(i) ) then
ccc
cc             if( ascglb(j) .EQ. ascgrx(i) .OR. 
cc     &         asccin .EQ. ascgrx(i) ) then
ccc
                 if( idxglb(idxfip,j) .LE. 0 ) then
                     idxglb(idxfip,j) = i
                 else
                     idx = idxglb(idxfip,j)
                     if( hpgrx(i,1) .GT. hpgrx(idx,1) .OR. 
     &                           hpgrx(i,2) .LT. hpgrx(idx,2) ) then
                        idxglb(idxfip,j) = i
                     endif
                 endif
             endif
   30    continue
c
c   ---- next seasonality code ---
c
   20 continue
c
c   --- if no exact matches found, loop over global indexes, 
c       backwards for best match ---
c
      if( fndgxf .LE. 0 ) then
          do 40 i=MXGLB,1,-1
              do 50 j=MXGLB,1,-1
ccc
cc           write(IOWMSG,'(14X,A,2X,A,2X,A)',ERR=9999)
cc     &        asccin,fipglb(i),ascglb(j)
ccc
                  if( idxglb(i,j) .GT. 0 ) then
                     fndgxf = idxglb(i,j)
ccc
cc           write(IOWMSG,'(14X,A10,I5)',ERR=9999)
cc     &        'Found GXF',fndgxf
ccc
                     goto 9999
                   endif
   50          continue
   40     continue
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
