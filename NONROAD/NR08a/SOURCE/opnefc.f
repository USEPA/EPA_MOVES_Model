C**** OPNEFC 
c
      subroutine opnefc( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the filenames to be used in the NONROAD program for all
c    emission factors files and deterioration factors files
c
c    Argument description.
c     Inputs:
c     Outputs:
c       ierr    I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/10/93  --gmw--  original development
c      03/03/96  --djk--  adapted for enees
c      07/19/96  --jlf--  added deterioration
c      02/10/98  --gwilson-- took out CO2 as an input file.  CO2 
c                            emissions are a function of BSFC.
c      05/21/98  --gwilson-- took out SOx as an input file.  SOx 
c                            emissions are a function of BSFC and EXHTHC.
c      06/14/04  --dfk--     removed resting loss
c      09/22/04  --dfk--     changed SOx to SO2 per version NR04
c      10/06/04  --dfk--     changed Refueling to Displacement
c      11/15/04  --dfk--     separate input of rec-marine neck, 
c                            supply/return and vent
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4         ierr
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strlen  I   returns the actual length of a string
c   strmin  I   returns the actual length of a string (minimum of 1)
c   fndchr  I   returns the index of a string in array of strings
c
      integer*4 strlen
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   fname   C   character string for temporary storage of filenames
c
      character*(MXSTR) fname, line
      character*20      keywrd, keyfil, keyin(MXPOL), keybsf
      integer*4         jerr, idxpol, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- set the keywords for each pollutant ---
c
      keybsf = 'BSFC'
      keyin(IDXTHC) = 'THC EXHAUST'
      keyin(IDXNOX) = 'NOX EXHAUST'
      keyin(IDXCO)  = 'CO EXHAUST'
      keyin(IDXPM)  = 'PM EXHAUST'
      keyin(IDXCRA) = 'CRANKCASE'
      keyin(IDXDIU) = 'DIURNAL'
      keyin(IDXDIS) = 'DISPLACEMENT'
      keyin(IDXSPL) = 'SPILLAGE'
      keyin(IDXSOK) = 'HOT SOAKS'
      keyin(IDXTKP) = 'TANK PERM'
      keyin(IDXHOS) = 'NON-RM HOSE PERM'
      keyin(IDXNCK) = 'RM FILL NECK PERM'
      keyin(IDXSR) = 'RM SUPPLY/RETURN'
      keyin(IDXVNT) = 'RM VENT PERM'
      keyin(IDXRLS) = 'RUNINGLOSS'
C      keyin(IDXRST) = 'RESTNGLOSS'
      keyin(IDSTHC) = 'THC STARTS'
      keyin(IDSNOX) = 'NOX STARTS'
      keyin(IDSCO)  = 'CO STARTS'
      keyin(IDSPM)  = 'PM STARTS'
      keyin(IDXSOX) = 'SO2 EXHAUST'
      keyin(IDXCO2) = 'CO2 EXHAUST'
      keyin(IDSSOX) = 'SO2 STARTS'
      keyin(IDSCO2) = 'CO2 STARTS'
c
c   --- search for the packet header ----
c
      keywrd = '/EMFAC FILES/'
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .NE. ISUCES ) goto 7000
c
  111 continue
      read(IORUSR,8000,ERR=7001,END=7002) line
      call spinit()
      keyfil = line(1:19)
      call lftjst( keyfil )
      call low2up( keyfil )
c
c   --- check for /END/ keyword ----
c
      if( keyfil .EQ. KEYEND ) goto 222 
c
c   --- get the filename ---
c
      fname = line(21:)
      call lftjst( fname )
c
c    ---- check for the BSFC file ---
c
      if( keyfil .EQ. keybsf ) then
         bsffl = fname
         lbsffl = .TRUE. 
      else
c
c    ---- check if file type is valid ---
c
         idxpol = fndchr( keyfil, 19, keyin, MXPOL )
c
c   ---- if it is a CO2 file or SOx file, add output a warning ---
c
         if( idxpol .EQ. IDXCO2 .OR. idxpol .EQ. IDXSOX) then
             write(IOWMSG,'(/,1X,5A)',ERR=9999) 
     &           'WARNING:  Emission factor file not needed for ',
     &                     keyin(idxpol)(:strmin(keyin(idxpol))),'.'
             write(IOWMSG,'(11X,3A)',ERR=9999) 
     &          keyin(idxpol)(:strmin(keyin(idxpol))),' emissions are ',
     &         'calculated as a function of BSFC.  Ignoring this file.'
             nwarn = nwarn + 1
c
c   ---- if it is a refueling (vapor displacemnt) file, output a warning ---
c
         elseif( idxpol .EQ. IDXDIS ) then
             write(IOWMSG,'(/,1X,5A)',ERR=9999) 
     &           'WARNING:  Emission factor file not needed for ',
     &                     keyin(idxpol)(:strmin(keyin(idxpol))),'.'
             write(IOWMSG,'(11X,3A,/11X,A)',ERR=9999) 
     &          keyin(idxpol)(:strmin(keyin(idxpol))),' emissions are ',
     &         'calculated as a function of temperature and RVP.',
     &         'Ignoring this file.'
             nwarn = nwarn + 1
         else if( idxpol .GT. 0 ) then
            if( strlen( fname ) .GT. 0 ) then
               facfl(idxpol) = fname
               lfacfl(idxpol) = .TRUE.
            endif
c
c   ---- unrecognized keyword, skip it ---
c
         else
            goto 7003
         endif
      endif
c
c  --- get the next record ---
c
      goto 111 
c
c  ---- read the entire file, make sure all files provided ---
c       Missing BSFC file is an error ---
c
 222  continue
      if( .NOT. lbsffl ) goto 7005
cgmw      do 10 i=1,MXPOL
      do 10 i=1,IDSTHC-1
         if( .NOT. lfacfl(i) .AND. i .NE. IDXCO2 
     &                       .AND. i .NE. IDXSOX 
     &                       .AND. i .NE. IDXSOK 
     &                       .AND. i .NE. IDXDIS 
C     &                       .AND. i .NE. IDXRST
     &                       .AND. i .NE. IDXRLS )  then
             if ( i .EQ. IDXSPL )  then
                write(IOWMSG,'(/,1X,5A)',ERR=9999) 
     &           'WARNING:  Cannot find ',keyin(i)(:strmin(keyin(i))),
     &                      ' filename in ',keywrd(:strmin(keywrd)),
     &                                       ' packet of options file.'
                write(IOWMSG,'(11X,3A)',ERR=9999) 'All factors for ',
     &                              'spillage and vapor displacement',
     &                              ' will be set to missing.'
                nwarn = nwarn + 1
             else
                write(IOWMSG,'(/,1X,5A)',ERR=9999) 
     &           'WARNING:  Cannot find ',keyin(i)(:strmin(keyin(i))),
     &                      ' filename in ',keywrd(:strmin(keywrd)),
     &                                       ' packet of options file.'
                write(IOWMSG,'(11X,2A)',ERR=9999) 'All factors for ',
     &                         'this species will be set to missing.'
                nwarn = nwarn + 1
             endif
         endif
   10 continue
c
c   ---- search for the deterioration factors files ---
c
      keywrd = '/DETERIORATE FILES/'
      call fndkey( jerr, IORUSR, keywrd )
      if( jerr .EQ. IRDERR ) goto 7004
      if( jerr .EQ. IEOF ) then
         write(IOWMSG,'(/,1X,3A)',ERR=9999) 'WARNING:  Cannot find ',
     &       keywrd(:strmin(keywrd)),' packet of the options file.'
         write(IOWMSG,'(11X,2A)',ERR=9999) 'Factors for all species ',
     &                         'will be set to 1.0 (no deterioration).'
         nwarn = nwarn + 1
         goto 555
      endif
c
  333 continue
      read(IORUSR,8000,ERR=7001,END=7002) line
      call spinit()
      keyfil = line(1:19)
      call lftjst( keyfil )
      call low2up( keyfil )
c
c   --- check for /END/ keyword ----
c
      if( keyfil .EQ. KEYEND ) goto 444
c
c   --- get the filename ---
c
      fname = line(21:)
      call lftjst( fname )
c
c    ---- check if file type is valid ---
c
      idxpol = fndchr( keyfil, 19, keyin, MXPOL )
c
c   ---- if it is a CO2 file or SOx file, add output a warning ---
c
         if( idxpol .EQ. IDXCO2 .OR. idxpol .EQ. IDXSOX) then
             write(IOWMSG,'(/,1X,5A)',ERR=9999) 
     &           'WARNING:  Deterioration factor file not needed for ',
     &                     keyin(idxpol)(:strmin(keyin(idxpol))),'.'
             write(IOWMSG,'(11X,3A)',ERR=9999) 
     &          keyin(idxpol)(:strmin(keyin(idxpol))),' emissions are ',
     &         'calculated as a function of BSFC.  Ignoring this file.'
             nwarn = nwarn + 1
c
c   ---- if it is a refueling (vapor displacement) file, output a warning ---
c
         elseif( idxpol .EQ. IDXDIS ) then
             write(IOWMSG,'(/,1X,5A)',ERR=9999) 
     &           'WARNING:  Deterioration factor file not needed for ',
     &                     keyin(idxpol)(:strmin(keyin(idxpol))),'.'
             write(IOWMSG,'(11X,3A,/11X,A)',ERR=9999) 
     &          keyin(idxpol)(:strmin(keyin(idxpol))),' emissions are ',
     &         'calculated as a function of temperature and RVP.',
     &         'Ignoring this file.'
             nwarn = nwarn + 1
c
c   ---- if it is a spillage file, output a warning ---
c
         elseif( idxpol .EQ. IDXSPL ) then
             write(IOWMSG,'(/,1X,5A)',ERR=9999) 
     &           'WARNING:  Deterioration factor file not needed for ',
     &                     keyin(idxpol)(:strmin(keyin(idxpol))),'.'
             write(IOWMSG,'(11X,3A,/11X,A)',ERR=9999) 
     &          keyin(idxpol)(:strmin(keyin(idxpol))),' emissions are ',
     &         'calculated as a function of tank volume.',
     &         'Ignoring this file.'
             nwarn = nwarn + 1
c
      else if( idxpol .GT. 0 ) then
         if( strlen( fname ) .GT. 0 ) then
            detfl(idxpol) = fname
            ldetfl(idxpol) = .TRUE.
         endif
c
c   ---- unrecognized keyword, skip it ---
c
      else
         goto 7003
      endif
c
c  --- get the next record ---
c
      goto 333
c
c  --- check to make sure all necessary files were provided
c
 444  continue
      ldetfl(IDXCO2) = .FALSE.
      ldetfl(IDXSOX) = .FALSE.
      ldetfl(IDXDIS) = .FALSE.
      ldetfl(IDXSPL) = .FALSE.
cgmw      do 20 i=1,MXPOL
      do 20 i=1,IDSTHC-1
         if( .NOT. ldetfl(i) .AND. (i .EQ. IDXTHC .OR. i .EQ. IDXCO 
     &                      .OR. i .EQ. IDXNOX .OR. i .EQ. IDXPM) ) then
             write(IOWMSG,'(/,1X,5A)',ERR=9999) 
     &           'WARNING:  Cannot find ',keyin(i)(:strmin(keyin(i))),
     &                      ' filename in ',keywrd(:strmin(keywrd)),
     &                                        ' packet of options file.'
             write(IOWMSG,'(11X,2A)',ERR=9999) 'All factors for this',
     &                 ' species will be set to 1.0 (no deterioration).'
             nwarn = nwarn + 1
         endif
   20 continue
c
c   --- echo filenames ---
c
  555 continue
      write(IOWMSG,9001,ERR=9999)
      write(IOWMSG,9003,ERR=9999) '*** Emission Factors Files ***'
      write(IOWMSG,9001,ERR=9999)
c
      write(IOWMSG,9002,ERR=9999) keybsf(:strmin(keybsf))//
     &                             ' file',bsffl(:strmin( bsffl ))
cgmw      do 30 idxpol=1,MXPOL
      do 30 idxpol=1,IDSTHC-1
         if( lfacfl(idxpol) ) then
             write(IOWMSG,9002,ERR=9999) 
     &             keyin(idxpol)(:strmin(keyin(idxpol)))//
     &                 ' file',facfl(idxpol)(:strmin( facfl(idxpol)))
         else 
             write(IOWMSG,9002,ERR=9999) 
     &             keyin(idxpol)(:strmin(keyin(idxpol)))//
     &                                      ' file',' Not Supplied.'
         endif
  30  continue
c
      write(IOWMSG,9001,ERR=9999)
      write(IOWMSG,9003,ERR=9999) '*** Deterioration Factors Files ***'
      write(IOWMSG,9001,ERR=9999)
c
cgmw      do 40 idxpol=1,MXPOL
      do 40 idxpol=1,IDSTHC-1
         if( ldetfl(idxpol) ) then
             write(IOWMSG,9002,ERR=9999)
     &             keyin(idxpol)(:strmin(keyin(idxpol)))//
     &                 ' file',detfl(idxpol)(:strmin( detfl(idxpol)))
         else
             write(IOWMSG,9002,ERR=9999) 
     &             keyin(idxpol)(:strmin(keyin(idxpol)))//
     &                                      ' file',' Not Supplied.'
         endif
  40  continue
      write(IOWMSG,'(A)',ERR=9999) 
     

c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  This program ',
     &                'requires the ',keywrd(:strmin( keywrd )),
     &                                  ' packet of the options file.'
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  This program ',
     &                'requires the ',keywrd(:strmin( keywrd )),
     &                                  ' packet of the options file.'
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR: reading ',
     &               keywrd(:strmin(keywrd)),' packet of options file.'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR: reading ',
     &               keywrd(:strmin(keywrd)),' packet of options file.'
      goto 9999
c
 7002 continue
      write(IOWSTD,'(/,1X,2A,/9X,2A)',ERR=9999) 'ERROR:  Unexpected ',
     &        'end-of-file reached reading ',keywrd(:strmin(keywrd)), 
     &                                    ' packet of the options file.'
      write(IOWMSG,'(/,1X,2A,/9X,2A)',ERR=9999) 'ERROR:  Unexpected ',
     &        'end-of-file reached reading ',keywrd(:strmin(keywrd)), 
     &                                    ' packet of the options file.'
      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,4A,/9X,3A)',ERR=9999) 'ERROR:  Invalid file ',
     &                   'identifier in ',keywrd(:strmin(keywrd)),
     &                 ' packet','of options file: -->',line(1:19),'<--'
      write(IOWMSG,'(/,1X,4A,/9X,3A)',ERR=9999) 'ERROR:  Invalid file ',
     &                   'identifier in ',keywrd(:strmin(keywrd)),
     &                 ' packet','of options file: -->',line(1:19),'<--'
      goto 9999 
c
 7004 continue
      write(IOWSTD,'(/,1X,4A)',ERR=9999) 'ERROR:  Reading the ',
     &                       'options file while searching for the ',
     &                               keywrd(:strmin(keywrd)),' packet.'
      write(IOWMSG,'(/,1X,4A)',ERR=9999) 'ERROR:  Reading the ',
     &                       'options file while searching for the ',
     &                               keywrd(:strmin(keywrd)),' packet.'
      goto 9999
c
 7005 continue
      write(IOWSTD,'(/,1X,5A)',ERR=9999) 
     &           'ERROR:  Cannot find ',keybsf(:strmin(keybsf)),
     &                      ' filename in ',keywrd(:strmin(keywrd)),
     &                                       ' packet of options file.'
      write(IOWMSG,'(/,1X,5A)',ERR=9999) 
     &           'ERROR:  Cannot find ',keybsf(:strmin(keybsf)),
     &                      ' filename in ',keywrd(:strmin(keywrd)),
     &                                       ' packet of options file.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(A)
 9001 format(1X,A)
 9002 format(T10,A,T30,:,':',A)
 9003 format(T20,A)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
