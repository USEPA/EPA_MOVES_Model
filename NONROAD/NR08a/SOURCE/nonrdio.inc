C**** IOOFFRD.INC
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for OFFROAD program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     09/01/93  --gmw--  Original development
c     07/18/96  --jlf--  added deterioration rates
c     07/22/96  --jlf--  added new file of technology fractions by model
c                        year by equipment type
c     05/10/04  --dfk--  added evap tech groups
c     06/04/04  --dfk--  added evap bmy output I/O
c     06/15/04  --dfk--  added daily ambient temperatures I/O
c     04/26/05  --cimulus-- added IOFIPS, fipsfl and lfipsfl variables
c                           all related to the FIPS.DAT file holding
c                           county names and FIPS codes
c     05/13/05  --cimulus-- added IORTRFT, rtrftfl, and lrtrftfl
c                           variables, all related to the diesel
c                           retrofit input file
c     05/24/05  --cimulus-- updated version
c     07/15/05  --cimulus-- removed commercial marine related constants
c                           and variables
c
c-----------------------------------------------------------------------
c    Program version parameter:
c-----------------------------------------------------------------------
c
c   PROGNM   C   character string paramater containing program name
c   VERSON   C   character string paramater containing version of the
c                OFFROAD program
c
      character*30 PROGNM
      character*30 VERSON
c
      parameter( PROGNM = ' EPA`s NONROAD Emissions Model')
      parameter( VERSON = 'Core Model ver 2008a, 07/06/09')
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORSTD   I   unit number for standard input
c   IOWSTD   I   unit number for standard output
c   IORUSR   I   unit number for user input file
c   IORPOP   I   unit number for base population data file
c   IORALO   I   unit number for allocation data file
c   IORIND   I   unit number for spatial indicator file
c   IORACT   I   unit number for base activity levels data file
c   IORSES   I   unit number for seasonality data file
c   IORFAC   I   base unit number for emission factors data files
c   IORDAC   I   base unit number for deterioration factors data files
c   IORBSF   I   unit number for BSFC file
c   IORGRW   I   unit number for growth factors file
c   IORTCH   I   unit number for exhaust technology fractions file
c   IOREVTCH I   unit number for evap technology fractions file
c   IORREG   I   unit number for regions definitions file
c   IOWMSG   I   unit number for output message file
c   IOWDAT   I   unit number for output data file
c   IOWAMS   I   unit number for output AMS workfile
c   IOWBMY   I   unit number for output exhaust by-model-year
c   IOWEVBMY I   unit number for output evap by-model-year
c   IOWSI    I   unit number for output SI report
c   IOSDIR   I   unit number for scratch direct-access file
c   IOSPOP   I   unit number for scratch population file
c   IOSIND   I   unit number for scratch allocation file
c   IOSGRW   I   unit number for scratch growth indicator data
c   IORDAY   I   unit number for daily temperatures/RVPs file
c   IOFIPS   I   unit number for FIPS.DAT file containing county codes
c   IORTRFT  I   unit number for retrofit input file
c
      integer*4 IORSTD
      integer*4 IOWSTD
      integer*4 IORUSR
      integer*4 IORPOP
      integer*4 IORALO
      integer*4 IORIND
      integer*4 IORACT
      integer*4 IORSES
      integer*4 IORFAC
      integer*4 IORDAC
      integer*4 IORBSF
      integer*4 IORGRW
      integer*4 IORTCH
      integer*4 IOREVTCH
      integer*4 IORREG
      integer*4 IOWMSG
      integer*4 IOWDAT
      integer*4 IOWAMS
      integer*4 IOWBMY
      integer*4 IOWEVBMY
      integer*4 IOWSI 
      integer*4 IOSDIR
      integer*4 IOSPOP
      integer*4 IOSIND
      integer*4 IOSGRW
      integer*4 IORDAY
      integer*4 IOFIPS
      integer*4 IORTRFT
c
      parameter( IORSTD =   5  )
      parameter( IOWSTD =   6  )
      parameter( IORUSR =   7  )
      parameter( IORPOP =   8  )
      parameter( IORALO =   9  )
      parameter( IORTCH =   10 )
      parameter( IORIND =   11 )
      parameter( IORREG =   12 )
      parameter( IORSES =   13 )
      parameter( IORGRW =   14 )
      parameter( IORACT =   15 )
      parameter( IORFAC =   18 )
      parameter( IORDAC =   18 )
      parameter( IORBSF =   18 )
      parameter( IOREVTCH = 19 )
      parameter( IOWMSG =   45 )
      parameter( IOWDAT =   46 )
      parameter( IOWAMS =   47 )
      parameter( IOWBMY =   48 )
      parameter( IOWEVBMY = 53 )
      parameter( IOWSI  =   49 )
      parameter( IOSDIR =   38 )
      parameter( IOSPOP =   39 )
      parameter( IOSIND =   40 )
      parameter( IOSGRW =   42 )
      parameter( IORDAY =   54 )
      parameter( IOFIPS =   55 )
      parameter( IORTRFT =  56 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   popfl    C   array of base population data files
c   alofl    C   allocation coefficients data file
c   indfl    C   filename of file with spatial indicator data
c   actfl    C   activity data file
c   sesfl    C   seasonality data file
c   facfl    C   emissions factors files (one for each pollutant)
c   detfl    C   deterioration factors files (one for each pollutant)
c   bsffl    C   BSFC file 
c   tchfl    C   exhaust technology fractions file
c   evtchfl  C   evap technology fractions file
c   grwfl    C   growth factors filename
c   ctlfl    C   emissions control factors file
c   regfl    C   file with definitions of regions 
c   msgfl    C   output tables filename
c   datfl    C   output data file
c   amsfl    C   output AMS workfile name
c   bmyfl    C   output exhaust by-model-year name
c   evbmyfl  C   output evap by-model-year name
c   sifl     C   output SI report filename
c   sysfl    C   user control file combining options and system file functions
c   spopfl   C   output sorted population file
c   npopfl   I   number of population files provided
c   dayfl    C   input daily ambient temperatures file
c   fipsfl   C   location of the FIPS.DAT file or equivalent that names all counties
c   rtrftfl  C   location of the retrofit input file
c
      character*(MXSTR) popfl(MXPFIL)
      character*(MXSTR) alofl
      character*(MXSTR) indfl
      character*(MXSTR) actfl
      character*(MXSTR) sesfl
      character*(MXSTR) tchfl
      character*(MXSTR) evtchfl
      character*(MXSTR) facfl(MXPOL)
      character*(MXSTR) detfl(MXPOL)
      character*(MXSTR) bsffl
      character*(MXSTR) grwfl
      character*(MXSTR) ctlfl
      character*(MXSTR) msgfl 
      character*(MXSTR) regfl 
      character*(MXSTR) datfl
      character*(MXSTR) amsfl
      character*(MXSTR) bmyfl
      character*(MXSTR) evbmyfl
      character*(MXSTR) sifl
      character*(MXSTR) sysfl
      character*(MXSTR) spopfl
      character*(MXSTR) dayfl
      character*(MXSTR) fipsfl
      character*(MXSTR) rtrftfl
      integer*4         npopfl
c
      common /iochr/ popfl, alofl, indfl, 
     &               actfl, sesfl, tchfl, facfl, detfl, bsffl, datfl,
     &               amsfl, bmyfl, sifl, grwfl, ctlfl, regfl, msgfl,
     &               sysfl, spopfl, evtchfl, evbmyfl, dayfl, fipsfl,
     &               rtrftfl
      common /iodat/ npopfl
c
c-----------------------------------------------------------------------
c   Flags for processing certain input files:
c-----------------------------------------------------------------------
c
c   lalofl    L   flag for determining if using allocation data file
c   lactfl    L   flag for determining if using activity data file
c   lsesfl    L   flag for determining if using seasonality data file
c   lfacfl    L   flag for determining if using emissions factors file
c   lbsffl    L   flag for determining if using BSFC file
c   ldetfl    L   flag for determining if using deterioration factors file
c   ltchfl    L   flag for determining if using exhaust technology fractions file
c   levtchfl  L   flag for determining if using evap technology fractions file
c   lgrwfl    L   flag for determining if using growth factors filename
c   lctlfl    L   flag for determining if using emissions control factors file
c   lregfl    L   flag for determining if region definition file is supplied
c   lmsgfl    L   flag for determining if using output tables filename
c   ldatfl    L   flag for determining if output data file was supplied
c   lamsfl    L   flag for determining if creating AMS workfile
c   lbmyfl    L   flag for determining if creating exhasut by-model-year output file
c   levbmyfl  L   flag for determining if creating evap by-model-year output file
c   lsifl     L   flag for determining if creating SI report output file
c   ldayfl    L   flag for determining if using daily ambient temperatures file
c   lfipsfl   L   flag for determining if using a FIPS.DAT file listing all counties
c   lrtrftfl  L   flag for determining if using a retrofit input file
c
      logical*4 lalofl
      logical*4 lactfl
      logical*4 lsesfl
      logical*4 ltchfl
      logical*4 levtchfl
      logical*4 lfacfl(MXPOL)
      logical*4 ldetfl(MXPOL)
      logical*4 lbsffl
      logical*4 lgrwfl
      logical*4 lctlfl
      logical*4 lregfl
      logical*4 lmsgfl
      logical*4 ldatfl
      logical*4 lamsfl
      logical*4 lbmyfl
      logical*4 levbmyfl
      logical*4 lsifl
      logical*4 ldayfl
      logical*4 lfipsfl
      logical*4 lrtrftfl
c
      common /flgdat/  lalofl, lactfl, lsesfl, lfacfl, lbsffl, 
     &                 ldetfl, ltchfl, lgrwfl, lctlfl, lregfl,
     &                 lmsgfl, ldatfl, lamsfl, lbmyfl, lsifl,
     &                 levtchfl, levbmyfl, ldayfl, lfipsfl, lrtrftfl
c
c-----------------------------------------------------------------------
c   Varables for displaying progress.
c-----------------------------------------------------------------------
c
c  spin   C   characters that simulate a spinning thing
c  icall  I   number of subroutines called
c  nwarn  I   number of WARNING messages.
c  nwrnct I   array for keeping track of warnings for error stop features
c  nrecds I   number of records processed (for display purposes)
c  ntotrc I   number of total population records to process
c  nctyrc I   number of population records for each county
c  nstarc I   number of population records for each state
c  nnatrc I   number of population records for entire nation
c
      character*1 spin(0:11)
      integer*4   icall
      integer*4   nwarn
      integer*4   nwrnct(IDXWSE)
      integer*4   nrecds
      integer*4   ntotrc
      integer*4   nctyrc(NCNTY)
      integer*4   nstarc(NSTATE)
      integer*4   nnatrc
c
      common /spnchr/ spin
      common /spndat/ icall, nwarn, nwrnct, nrecds, ntotrc, 
     &                nctyrc, nstarc, nnatrc
