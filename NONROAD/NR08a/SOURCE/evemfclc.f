C**** EVEMFCLC
c
      subroutine evemfclc(ierr,evemsfac,idxevunt,aevdetcf,bevdetcf,
     &    evdetcap,asccod,evtecnam,nevtch,evtecfrc,iyrin,idxyr,icurec)
c
c-----------------------------------------------------------------------
c  
c    This routine calculates the evap emission factors for the specified
c    equipment type.  Wherever necessary, the original emission factors
c    are converted to grams/hour.  This routine also retrieves the 
c    deterioration data and applies a deterioration factor to the 
c    emission factor based on the age of the equipment.
c    Before a call is made to the routine that searches for emission
c    factor data, a check is made to determine if the search is 
c    necessary.   That is, if it is certain that a new search will yield
c    the same data record, no search is made.
c
c     Argument description:
c       Outputs:
c          ierr      I   error code
c          evemsfac  R   array of caluclated evap emission factors
c          idxevunt  I   index in the units array for the emission factors
c          aevdetcf  R   A-coefficient of the deterioration factor equation
c          bevdetcf  R   B-coefficient of the deterioration factor equation
c          evdetcap  R   cap on age of equipment for deterioration calc
c       Inputs:
c          asccod    C   SCC code of equipment
c          evtecnam  C   evap technology type of equipment
c          nevtch    I   number of evap technology types for this equipemnt
c          evtecfrc  I   the evap tech type fractions 
c          iyrin     I   current model year 
c          idxyr     I   index of year in emission factor arrays
c          icurec    I   index in array of current record
c          idxyr     I   index of model year being evaluated
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      05/19/04  --dfk--  original development (from emfclc)
c      07/20/05  --cimulus--  removed unused variable
c      07/20/05  --cimulus--  floating-point comparison for equality
c                             okay; hpavga and emhpc values are read
c                             from file, rather than being calculated
c                             at runtime
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
c   fndevefc I   returns the index in the evap emission factor arrays
c   fnddet   I   returns the index in the deterioration factor arrays
c
      integer*4 fnddet
      integer*4 fndevefc
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      real*4       evemsfac(MXAGYR,MXPOL,MXEVTECH)
      integer*4    idxevunt(MXPOL,MXEVTECH)
      real*4       aevdetcf(MXPOL,MXEVTECH)
      real*4       bevdetcf(MXPOL,MXEVTECH)
      real*4       evdetcap(MXPOL,MXEVTECH)
      character*10 asccod
      character*10 evtecnam(MXEVTECH)
      integer*4    nevtch
      real*4       evtecfrc(MXEVTECH)
      integer*4    iyrin
      integer*4    idxyr
      integer*4    icurec
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4    idxspc, idxefc, idxdet, idxall, i, j
      integer*4    jerr
      real*4       hpavga
      logical*4    lcall(0:MXPOL,0:MXEVTECH)
      character*10 tname
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- set error code ---
c
      ierr = IFAIL
c
c  --- intialize the emission factors to missing ---
c
      do 10 i=IDXDIU, IDXRLS
        do 20 j=1,MXEVTECH
           if( evtecfrc(j) .NE. 0. ) then
              evemsfac(idxyr,i,j) = RMISS
           else
              evemsfac(idxyr,i,j) = 0.
           endif
           aevdetcf(i,j) = 0.0
           bevdetcf(i,j) = 1.0
           evdetcap(i,j) = 0.
   20   continue
   10 continue
c
c   --- set the HP category ----
c
      hpavga = avghpc(icurec)
c
c   --- EVAP SPECIES ----
c
c  --- Loop over evap species ----
c
      do 30 idxspc=IDXDIU, IDXRLS
c
c  --- skip if diesel, LPG or CNG type ---
c
         if( ifuel .EQ. IDXDSL .OR.
     &       ifuel .EQ. IDXCNG .OR.
     &       ifuel .EQ. IDXLPG) then
             do 40 i=1,nevtch
                evemsfac(idxyr,idxspc,i) = 0.
   40        continue
             goto 30
         endif
c
c  --- skip if there is no emission factor file for this species ---
c
         if( .NOT. lfacfl(idxspc) .OR. idxspc.EQ.IDXSPL ) goto 30
c
c  ---- an evap tech group consists of an E followed by 8 numbers.
c       each number corresponds to a tech group name for each evap
c       species, as follows:
c
c        12345678
c       E00000000
c
c       E: Evap Tech Group
c       1: Diurnal
c       2: Tank Permeation
c       3: Hose Permeation (applies to non-rec-marine hose and 3 rec-marine hoses)
c       4: Hot Soak
c       5: Displacement
c       6: Spillage
c       7: Running Loss
c       8: Resting Loss
c
c       the tech type name format for each evap species is En, where n=0-9
c
c   --- check the identification data to determine if a new
c       search for emission factor data is necessary,
c       (parse the species tech group name from the evap tech group ----
c        the species tech group name format is En, where n=0-9.)
c
         do 50 i=0,nevtch
            lcall(idxspc,i) = .FALSE.
            if( asccod .NE. emasc(idxspc) ) lcall(idxspc,i) = .TRUE.
            if( hpavga .NE. emhpc(idxspc) ) lcall(idxspc,i) = .TRUE. ! floating-point comparison for equality okay; hpavga and emhpc values are read from file, rather than being calculated at runtime
            if( iyrin .LT. iemyr(idxspc,i,1) .OR. 
     &          iyrin .GE. iemyr(idxspc,i,2) ) lcall(idxspc,i) = .TRUE.
            if( i .GT. 0 ) then
                if(idxspc.le.IDXHOS) then
                 j=idxspc-IDXDIU+2  ! ID starts in column 2 of evap tech name
                else if (idxspc.ge.IDXNCK.and.idxspc.le.IDXVNT) then
                 j=IDXHOS-IDXDIU+2  ! map all hoses into hose
                else
                 j=idxspc-IDXDIU+2-3 ! shift 3 slots per 3-rec-marine hose
                end if
                tname='E'//evtecnam(i)(j:j)
                if( tname .NE. evemtech(idxspc,i) ) 
     &                                         lcall(idxspc,i) = .TRUE.
                if( evtecfrc(i) .GT. 0. )
     &                             evemsfac(idxyr,idxspc,i) = RMISS
            endif
   50    continue
c
c   --- if needed, get the global match ---
c       reinitialize the upper and lower bound on years
c       for last record found ----
c
         if( lcall(idxspc,0) ) then
            iemyr(idxspc,0,1) = 0
            iemyr(idxspc,0,2) = 9999
            idxeal(idxspc) = fndevefc( asccod, ascevp, 0, TECDEF,
     &          tecevp, hpavga, evhpcb, evhpce, iyrin, iyrevp, evpfac,
     &          IDXTHC, nevpfc, idxspc)
         endif
c
c   --- the information may give a different best match, call
c       routine to find the best match in the non-exhaust 
c       emission factor data arrays ----
c       reinitialize the upper and lower bound on years
c       for last record found ----
c
         do 60 i=1,nevtch
            j=idxspc-IDXDIU+2
              if(idxspc.le.IDXHOS) then
               j=idxspc-IDXDIU+2  ! ID starts in column 2 of evap tech name
              else if (idxspc.ge.IDXNCK.and.idxspc.le.IDXVNT) then
               j=IDXHOS-IDXDIU+2  ! map all hoses into hose
              else
               j=idxspc-IDXDIU+2-3 ! shift 3 slots per 3-rec-marine hose
              end if
            tname='E'//evtecnam(i)(j:j)
            if(lcall(idxspc,i) .AND. tname .NE. TECDEF) then
               iemyr(idxspc,i,1) = 0
               iemyr(idxspc,i,2) = 9999
               idxefc = fndevefc( asccod, ascevp, i, tname,  
     &                           tecevp, hpavga, evhpcb, evhpce, iyrin, 
     &                           iyrevp, evpfac, IDXTHC, nevpfc, idxspc)
               idxems(idxspc,i) = idxefc 
               evemtech(idxspc,i) = tname
            else
               idxefc = idxems(idxspc,i)
            endif
c
c   --- if data was found, fill the local variables with the
c       data from the global arrays ----
c
            if( idxefc .EQ. 0 ) idxefc = idxeal(idxspc)
            if( idxefc .GT. 0 .AND. evtecfrc(i) .GT. 0.0 ) then
               evemsfac(idxyr,idxspc,i) = evpfac(idxefc,idxspc)
               idxevunt(idxspc,i) = ievpun(idxefc,idxspc)
            else if( evtecfrc(i) .GT. 0.0 ) then
c
c   --- no data was found, write a warning message ----
c
               write(IOWMSG,9000,ERR=9999) 
     &               'WARNING:  No evap emission factors found for: ',
     &              asccod, evtecnam(i), tname, hpavga, iyrin,
     &              polnam(idxspc)
               call chkwrn(jerr,IDXWEM)
               if( jerr .NE. ISUCES ) goto 9999
               goto 60
            endif
c
c   ---- call routine to find the best match in the 
c        non-exhaust deterioration rate data arrays ----
c
            idxall = 0
            idxdet = 0
            if( ldetfl(idxspc) ) then
               idxall = fnddet( idxspc, TECDEF )
               idxdet = fnddet( idxspc, tname )
               if( idxdet .EQ. 0 ) idxdet = idxall
               if( idxdet .GT. 0 ) then
                  aevdetcf(idxspc,i) = detavl(idxdet,idxspc)
                  bevdetcf(idxspc,i) = detbvl(idxdet,idxspc)
                  evdetcap(idxspc,i) = capdet(idxdet,idxspc)
               endif
            endif
   60    continue
   30 continue
cc
cc   --- HOT SOAKS handled differently ---
cc
cc   --- check the identification data to determine if a new
cc       search for emission factor data for Hot SoaK is necessary,
cc
c      do 70 i=0,nevtch
c        if( asccod .NE. emasc(IDXSOK) ) lcall(IDXSOK,i) = .TRUE.
c        if( hpavga .NE. emhpc(IDXSOK) ) lcall(IDXSOK,i) = .TRUE. ! floating-point comparison for equality okay; hpavga and emhpc values are read from file, rather than being calculated at runtime
c        if( iyrin .LT. iemyr(IDXSOK,i,1) .OR. 
c     &        iyrin .GE. iemyr(IDXSOK,i,2) ) lcall(IDXSOK,i) = .TRUE.
c        if( i .GT. 0 ) then
c            if( evtecnam(i) .NE. emtech(IDXSOK,i) ) 
c     &                                       lcall(IDXSOK,i) = .TRUE.
c            if( evtecfrc(i) .GT. 0. ) evemsfac(idxyr,IDXSOK,i) = RMISS
c        endif
c   70 continue
cc
c      if( lfacfl(IDXSOK) ) then
cc
cc   --- the information may give a different best match, call
cc       routine to find the best match in the Hot Soak
cc       emission factor data array ----
cc       reinitialize the upper and lower bound on years
cc       for last record found ----
cc
c         if( lcall(IDXSOK,0) ) then
c            iemyr(IDXSOK,0,1) = 0
c            iemyr(IDXSOK,0,2) = 9999
c            idxeal(IDXSOK) = fndefc( asccod, ascevp, 0, TECDEF, tecevp,
c     &         hpavga, evhpcb, evhpce, iyrin, iyrevp, evpfac, IDXSOK,
c     &                                                nevpfc, IDXSOK)
c         endif
cc
c         do 80 i=1,nevtch
c            iemyr(IDXSOK,i,1) = 0
c            iemyr(IDXSOK,i,2) = 9999
c            if( lcall(IDXSOK,i) .AND. tecnam(i) .NE. TECDEF ) then
c               idxefc = fndefc( asccod, ascevp, i, tecnam(i), tecevp,
c     &            hpavga, evhpcb, evhpce, iyrin, iyrevp, evpfac, IDXSOK,
c     &                                                   nevpfc, IDXSOK)
c               idxems(IDXSOK,i) = idxefc
c               emtech(IDXSOK,i) = evtecnam(i)
c            else
c               idxefc = idxems(IDXSOK,i)
c            endif
cc
cc   ---- call routine to find the best match in the 
cc        Hot Soak deterioration rate data arrays ----
cc       reinitialize the upper and lower bound on years
cc       for last record found ----
cc
c            idxall = 0
c            idxdet = 0
c            if( ldetfl(IDXSOK) ) then
c               idxall = fnddet( IDXSOK, TECDEF )
c               idxdet = fnddet( IDXSOK, tecnam(i) )
c               if( idxdet .EQ. 0 ) idxdet = idxall
c               if( idxdet .GT. 0 ) then
c                  aevdetcf(idxspc,i) = detavl(idxdet,idxspc)
c                  bevdetcf(idxspc,i) = detbvl(idxdet,idxspc)
c                  evdetcap(idxspc,i) = capdet(idxdet,idxspc)
c               endif
c            endif
cc
cc   --- if data is available, calculate the hot soak factors
cc        this should not need conversion ---
cc
c            if( idxefc .GT. 0 ) then
c                evemsfac(idxyr,IDXSOK,i) = exhfac(idxefc,idxspc)
c                idxevunt(IDXSOK,i) = iexhun(idxefc,idxspc)
cc
cc   --- no data was found, write a warning message ----
cc
c            else if( idxefc .LE. 0 .AND. tecfrc(i) .GT. 0.0 ) then
c                write(IOWMSG,9000,ERR=9999) 
c     &               'WARNING:  No emissions factors found for: ',
c     &              asccod, tecnam(i), hpavga, iyrin, polnam(IDXSOK)
c                call chkwrn(jerr,IDXWEM)
c                if( jerr .NE. ISUCES ) goto 9999
c            endif
c   80    continue
c      endif
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
 9000 format(1X,4(A,1X),F5.0,I5,1X,A) 
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
