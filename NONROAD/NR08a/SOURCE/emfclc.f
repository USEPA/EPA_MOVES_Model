C**** EMFCLC
c
      subroutine emfclc(ierr,emsfac,bsfc,idxunt,adetcf,bdetcf,detcap,
     &                   asccod,tecnam,ntch,tecfrc,iyrin,idxyr,icurec)
c
c-----------------------------------------------------------------------
c  
c    This routine calculates the emission factors for the specified
c    equipment type.  Wherever necessary, the original emission factors
c    are converted to grams/hour.  This routine also retrieves the 
c    deterioration data and applies a deterioration factor to the 
c    emission factor based on the age of the equipment.
c    Since the BSFC data is in the same format as the emission factor
c    data, this routine also retieves the BSFC data.
c    Before a call is made to the routine that searches for emission
c    factor data, a check is made to determine if the search is 
c    necessary.   That is, if it is certain that a new search will yield
c    the same data record, no search is made.
c
c     Argument description:
c       Outputs:
c          ierr      I   error code
c          emsfac    R   array of caluclated exhaust emission factors
c          bsfc      R   BSFC value for this equipment type
c          idxunt    I   index in the units array for the emission factors
c          adetcf    R   A-coefficient of the deterioration factor equation
c          bdetcf    R   B-coefficient of the deterioration factor equation
c          detcap    R   cap on age of equipment for deterioration calc
c       Inputs:
c          asccod    C   SCC code of equipment
c          tecnam    C   exhaust technology type of equipment
c          ntch      I   number of exhaust technology types for this equipemnt
c          tecfrc    I   the exhaust tech type fractions 
c          iyrin     I   current model year 
c          idxyr     I   index of year in emission factor arrays
c          icurec    I   index in array of current record
c          idxyr     I   index of model year being evaluated
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/10/95  --gmw--  original development
c      07/21/96  --jlf--  added deterioration
c      05/13/04  --dfk--  added seperate evap tech group by species processing
c      07/20/05  --cimulus--  floating-point comparison for equality
c                             okay; hpavga and emhpc values are read
c                             from file, rather than being calculated
c                             at runtime
c      04/03/06  --epa--  added BSFC read check warning for missing BSFCs.
c      04/24/06  --epa--  enhanced BSFC warning check to only care for
c                           tech fractions > 0.0 (like other EF inputs).
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdefc.inc'
      include 'nonrdeqp.inc'
      include 'nonrdact.inc'
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   fndefc   I   returns the index in the emission factor arrays
c   fnddet   I   returns the index in the deterioration factor arrays
c
      integer*4 fndefc
      integer*4 fnddet
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      real*4       emsfac(MXAGYR,MXPOL,MXTECH)
      real*4       bsfc(MXAGYR,MXTECH)
      integer*4    idxunt(MXPOL,MXTECH)
      real*4       adetcf(MXPOL,MXTECH)
      real*4       bdetcf(MXPOL,MXTECH)
      real*4       detcap(MXPOL,MXTECH)
      character*10 asccod
      character*10 tecnam(MXTECH)
      integer*4    ntch
      real*4       tecfrc(MXTECH)
      integer*4    iyrin
      integer*4    idxyr
      integer*4    icurec
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4    idxspc, idxefc, idxdet, idxbsf, idxall, i, j
      integer*4    jerr
      real*4       hpavga
      logical*4    lcall(0:MXPOL,0:MXTECH)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- set error code ---
c
      ierr = IFAIL
c
c  --- intialize the exhaust emission factors to missing ---
c
      do 10 i=1,MXPOL
      if(i.GE.IDXDIU .AND. i.LE.IDXRLS) cycle
        do 20 j=1,MXTECH
           if( tecfrc(j) .NE. 0. ) then
              emsfac(idxyr,i,j) = RMISS
           else
              emsfac(idxyr,i,j) = 0.
           endif
           adetcf(i,j) = 0.0
           bdetcf(i,j) = 1.0
           detcap(i,j) = 0.
   20   continue
   10 continue
c
c   --- set the HP category ----
c
      hpavga = avghpc(icurec)
c
c   --- BSFC DATA ---
c
c   --- check the identification data to determine if a new
c       search for BSFC data is necessary,
c       position 0 of the last match arrays is BSFC ----
c
      do 25 i=0,ntch
         lcall(0,i) = .FALSE.
         if( asccod .NE. emasc(0) ) lcall(0,i) = .TRUE.
         if( hpavga .NE. emhpc(0) ) lcall(0,i) = .TRUE. ! floating-point comparison for equality okay; hpavga and emhpc values are read from file, rather than being calculated at runtime
         if( iyrin .LT. iemyr(0,i,1) .OR. 
     &                   iyrin .GE. iemyr(0,i,2) ) lcall(0,i) = .TRUE.
         if( i .GT. 0) then
         if( tecnam(i) .NE. emtech(0,i) ) lcall(0,i)=.TRUE.
         end if
   25 continue
c
c   --- if needed, get the global match ---
c       reinitialize the upper and lower bound on years
c       for last record found ----
c
      if( lcall(0,0) ) then
         iemyr(0,0,1) = 0
         iemyr(0,0,2) = 9999
chmm fix ifrst to = 0
chmm     idxeal(0) = fndefc( asccod, ascbsf, 0, TECDEF, tecbsf,
chmm &           hpavga, bsfpcb, bsfpce, iyrin, iyrbsf, bsffac, 1,
chmm &                                                   nbsffc, 0)
         idxeal(0) = fndefc( asccod, ascbsf, 0, TECDEF, tecbsf,
     &           hpavga, bsfpcb, bsfpce, iyrin, iyrbsf, bsffac, 0,
     &                                                   nbsffc, 0)
      endif
c
c   --- the information may give a different best match, call
c       routine to find the best match in the BSFC data arrays ----
c       reinitialize the upper and lower bound on years
c       for last record found ----
c
      do 26 i=1,ntch
         if( lcall(0,i) .AND. tecnam(i) .NE. TECDEF ) then 
            iemyr(0,i,1) = 0
            iemyr(0,i,2) = 9999
chmm fix ifrst to = 0
chmm        idxbsf = fndefc( asccod, ascbsf, i, tecnam(i), tecbsf,
chmm &      hpavga, bsfpcb, bsfpce, iyrin, iyrbsf, bsffac, 1,
chmm &                                               nbsffc, 0)
            idxbsf = fndefc( asccod, ascbsf, i, tecnam(i), tecbsf,
     &      hpavga, bsfpcb, bsfpce, iyrin, iyrbsf, bsffac, 0,
     &                                               nbsffc, 0)
         else
             idxbsf = idxems(0,i)
         endif
         if( idxbsf .EQ. 0 ) idxbsf = idxeal(0)
         if( idxbsf .GT. 0 ) then
             bsfc(idxyr,i) = bsffac(idxbsf,0)
         else if( idxbsf .LE. 0 .AND. tecfrc(i) .GT. 0.0 ) then
cah          bsfc(idxyr,i) = 1.0
             write(IOWMSG,9000,ERR=9999) 
     &           'WARNING:  No BSFC factors found for: ',
     &            asccod, tecnam(i), hpavga, iyrin, ' '
             call chkwrn(jerr,IDXWEM)
             if( jerr .NE. ISUCES ) goto 9999
             goto 26
         endif
   26 continue
c
c   --- EXHAUST SPECIES ----
c
c  --- Loop over exhaust species ----
c
      do 90 idxspc=IDXTHC,IDXCRA
c
c  --- since SOx is calculated when doing the HC emissions, 
c      skip it this is SOx ---
c
         if( idxspc .EQ. IDXSOX ) goto 90
c
c  --- skip if there is no emission factor file for this species ----
c
         if( .NOT. lfacfl(idxspc) ) goto 90
c
c  --- check which tech types need new emission factors ---
c
         do 21 i=0,ntch
c
c   --- check the identification data to determine if a new
c       search for emission factor data is necessary,
c
           lcall(idxspc,i) = .FALSE.
           if( asccod .NE. emasc(idxspc) ) lcall(idxspc,i) = .TRUE.
           if( hpavga .NE. emhpc(idxspc) ) lcall(idxspc,i) = .TRUE. ! floating-point comparison for equality okay; hpavga and emhpc values are read from file, rather than being calculated at runtime
           if( iyrin .LT. iemyr(idxspc,i,1) .OR. 
     &          iyrin .GE. iemyr(idxspc,i,2) ) lcall(idxspc,i) = .TRUE.
           if( i .GT. 0 ) then
               if( tecnam(i) .NE. emtech(idxspc,i) ) 
     &                                         lcall(idxspc,i) = .TRUE.
               if( tecfrc(i) .GT. 0. ) emsfac(idxyr,idxspc,i) = RMISS
            endif
   21    continue
c
c  --- call to get the emission factor for global tech types ---
c
         if( lcall(idxspc,0) ) then
            iemyr(idxspc,0,1) = 0
            iemyr(idxspc,0,2) = 9999
            idxeal(idxspc) = fndefc( asccod, ascexh, 0, TECDEF, tecexh,
     &           hpavga, exhpcb, exhpce, iyrin, iyrexh, exhfac, IDXTHC,
     &                                                  nexhfc, idxspc)
         endif
c
c   --- the information may give a different best match, call
c       routine to find the best match in the exhaust 
c       emission factor data arrays ----
c
          do 31 i=1,ntch
             if( lcall(idxspc,i) .AND. tecnam(i) .NE. TECDEF ) then
               iemyr(idxspc,i,1) = 0
               iemyr(idxspc,i,2) = 9999
               idxefc = fndefc( asccod, ascexh, i, tecnam(i), tecexh,
     &           hpavga, exhpcb, exhpce, iyrin, iyrexh, exhfac, IDXTHC,
     &                                                  nexhfc, idxspc)
               idxems(idxspc,i) = idxefc
               emtech(idxspc,i) = tecnam(i)
             else
               idxefc = idxems(idxspc,i)
             endif
c
c   --- if data was found, fill the local variables with the
c       data from the global arrays ----
c
             if( idxefc .EQ. 0 ) idxefc = idxeal(idxspc)
             if( idxefc .GT. 0 ) then
               emsfac(idxyr,idxspc,i) = exhfac(idxefc,idxspc)
               idxunt(idxspc,i) = iexhun(idxefc,idxspc)
c
c   --- no data was found, write a warning message ----
c
             else if( idxefc .LE. 0 .AND. tecfrc(i) .GT. 0.0 ) then
               write(IOWMSG,9000,ERR=9999) 
     &             'WARNING:  No emissions factors found for: ',
     &              asccod, tecnam(i), hpavga, iyrin, polnam(idxspc)
               call chkwrn(jerr,IDXWEM)
               if( jerr .NE. ISUCES ) goto 9999
               goto 31
             endif
c
c   ---- call routine to find the best match in the 
c        exhaust deterioration rate data arrays ----
c
             idxall = 0
             idxdet = 0
             if( ldetfl(idxspc) ) then
               idxall = fnddet( idxspc, TECDEF )
               idxdet = fnddet( idxspc, tecnam(i) )
               if( idxdet .EQ. 0 ) idxdet = idxall
               if( idxdet .GT. 0 ) then
                  adetcf(idxspc,i) = detavl(idxdet,idxspc)
                  bdetcf(idxspc,i) = detbvl(idxdet,idxspc)
                  detcap(idxspc,i) = capdet(idxdet,idxspc)
               endif
             endif
   31     continue
   90 continue
c
c   ---- return to calling routine ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(1X,3(A,1X),F5.0,I5,1X,A) 
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
