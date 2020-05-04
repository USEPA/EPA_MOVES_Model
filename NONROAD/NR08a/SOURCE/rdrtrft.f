C**** RDRTRFT
c
      subroutine rdrtrft( ierr )
c
c-----------------------------------------------------------------------
c
c     If necessary, reads the retrofit input file, into the global
c     arrays of retrofit parameters.
c
c     Skips any records that would have no effect due to retrofit-year
c     or model-year range (i.e., if evaluation year < retrofit year
c     start or evaluation year < model year start), or SCC (i.e., if
c     SCC not requested for the model run. Assumes the global iepyr
c     variable has been set to be able to do this.
c
c     Executes field level, record level, and record-set level
c     validation on the retrofit input. If an error condition occurs,
c     writes an error message to standard out and the message file, and
c     returns IFAIL.  If a warning condition occurs, writes a warning
c     message to standard out and the message file.
c
c     Argument declaration
c       Outputs:
c             ierr  I  error flag
c       Inputs:
c             (none)
c
c-----------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------
c
c      05/26/05  -cimulus-  original development
c      05/27/05  -cimulus-  more specific floating point format
c                           specifications
c
c-----------------------------------------------------------------------
c  Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdusr.inc'
      include 'nonrdrtrft.inc'
c
c-----------------------------------------------------------------------
c  Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
c   strmin          I  returns the actual length of a string (minimum of 1)
c   fndchr          I  returns the index of string in array of strings
c   chkasc          L  returns true if SCC code is needed for current run
c   vldrtrftscc     L  returns true if valid retrofit scc
c   vldrtrfttchtyp  L  returns true if valid retrofit tech type
c   vldrtrfthp      L  returns true if valid retrofit HP
c   cnthpcat        I  returns count of HP categories defined by HP range
c   vldrtrftrecs    L  returns true if valid retrofit record-set
c
      integer*4 strmin
      integer*4 fndchr
      logical*4 chkasc
      logical*4 vldrtrftscc
      logical*4 vldrtrfttchtyp
      logical*4 vldrtrfthp
      integer*4 cnthpcat
      logical*4 vldrtrftrecs
c
c-----------------------------------------------------------------------
c  Local variables:
c-----------------------------------------------------------------------
c
      character*300 errprefix ! common prefix for error messages
      character*300 wrnprefix ! common prefix for warning messages
      logical*4     didopenfile
      character*20  keywrd
      character*20  keytmp
      integer*4     jerr
      logical*4     done
      integer*4     irec
      character*100 line
      character*20  fldnm ! field name for error messages
      integer*4     ryst
      integer*4     ryen
      integer*4     myst
      integer*4     myen
      character*10  scc
      character*10  techtype
      real*4        hpmn
      real*4        hpmx
      real*4        annualfracorn
      real*4        effect
      character*10  pollutant
      integer*4     plltntidx
      integer*4     id
      real*4        hp ! HP for error messages
      character*20  hpnm ! HP name for error messages
      integer*4     yr1 ! year 1 for error messages
      integer*4     yr2 ! year 2 for error messages
      character*20  yrnm1 ! year name 1 for error messages
      character*20  yrnm2 ! year name 2 for error messages
      integer*4     mnyr ! minimum year for error messages
      integer*4     mxyr ! maximum year for error messages
      logical*4     shouldskip
      character*500 skipreason
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- common prefixes for messages ---
c
      write(errprefix,'(3A)',ERR=9999)
     &        'ERROR:  Reading retrofit data file: ',
     &        rtrftfl(:strmin(rtrftfl)),' at record: '
      write(wrnprefix,'(3A)',ERR=9999)
     &        'WARNING:  Reading retrofit data file: ',
     &        rtrftfl(:strmin(rtrftfl)),' at record: '
c
c   --- initialize retrofit variables ---
c
      call initrtrft()
c
c   --- skip if there is no retrofit file ---
c
      didopenfile = .FALSE.
      if( .NOT. lrtrftfl ) then
          goto 999 ! go to successful end
      endif
c
c   --- open the retrofit input file ---
c
      open(IORTRFT,file=rtrftfl,ERR=7000,status='UNKNOWN')
      rewind(IORTRFT)
      didopenfile = .TRUE.
c
c   --- call routine to find the /RETROFIT/ keyword ---
c
      keywrd = '/RETROFIT/'
      call fndkey(jerr, IORTRFT, keywrd )
      if( jerr .NE. ISUCES ) goto 7001
c
c   --- read each retrofit record from the packet ---
c
      done = .FALSE.
      irec = 0
      do while ( .NOT. done )
c
c       --- read record as a character string ---
c
          irec = irec + 1
          read(IORTRFT,'(A)',ERR=7002,END=7003) line
          call spinit()
c
c       --- look for /END/ keyword ---
c
          keytmp = line(1:strmin(KEYEND))
          call lftjst( keytmp )
          call low2up( keytmp )
          if( keytmp .EQ. KEYEND ) then
              done = .TRUE.
              cycle
          endif
c
c       --- check for maximum records exceeded ---
c
          if( irec .GT. MXRTRFT ) then
              goto 7015
          endif
c
c       --- parse the character string into the retrofit fields ---
c
          fldnm = 'Retrofit Year Start'
          read(line(1:4),'(I4)',ERR=7004) ryst
c
          fldnm = 'Retrofit Year End'
          read(line(6:9),'(I4)',ERR=7004) ryen
c
          fldnm = 'Model Year Start'
          read(line(11:14),'(I4)',ERR=7004) myst
c
          fldnm = 'Model Year End'
          read(line(16:19),'(I4)',ERR=7004) myen
c
          fldnm = 'SCC'
          scc = line(21:30)
          call lftjst(scc)
          call low2up(scc)
c
          fldnm = 'Tech Type'
          techtype = line(32:41)
          call lftjst(techtype)
          call low2up(techtype)
c
          fldnm = 'Minimum HP'
          read(line(43:47),'(F5.0)',ERR=7004) hpmn
c
          fldnm = 'Maximum HP'
          read(line(48:52),'(F5.0)',ERR=7004) hpmx
c
          fldnm = 'Annual Fraction or N'
          read(line(54:71),'(F18.0)',ERR=7004) annualfracorn
c
          fldnm = 'Effectiveness'
          read(line(73:78),'(F6.0)',ERR=7004) effect
c
          fldnm = 'Pollutant'
          pollutant = line(80:89)
          call lftjst(pollutant)
          call low2up(pollutant)
c
          fldnm = 'ID'
          read(line(91:95),'(I5)',ERR=7004) id
c
c       --- validate retrofit year start ---
c
          if( ryst .LT. MINRTRFTYEAR .OR. ryst .GT. MAXRTRFTYEAR ) then
              yrnm1 = 'retrofit year start'
              yr1 = ryst
              mnyr = MINRTRFTYEAR
              mxyr = MAXRTRFTYEAR
              goto 7005
          endif
c
c       --- validate retrofit year end ---
c
          if( ryen .LT. MINRTRFTYEAR .OR. ryen .GT. MAXRTRFTYEAR ) then
              yrnm1 = 'retrofit year end'
              yr1 = ryen
              mnyr = MINRTRFTYEAR
              mxyr = MAXRTRFTYEAR
              goto 7005
          endif
c
c       --- validate retrofit year range ---
c
          if( ryst .GT. ryen ) then
              yrnm1 = 'retrofit year start'
              yr1 = ryst
              yrnm2 = 'retrofit year end'
              yr2 = ryen
              goto 7006
          endif
c
c       --- validate model year start ---
c
          if( myst .LT. MINRTRFTMDLYEAR
     &            .OR. myst .GT. MAXRTRFTMDLYEAR ) then
              yrnm1 = 'model year start'
              yr1 = myst
              mnyr = MINRTRFTMDLYEAR
              mxyr = MAXRTRFTMDLYEAR
              goto 7005
          endif
c
c       --- validate model year end ---
c
          if( myen .LT. MINRTRFTMDLYEAR
     &            .OR. myen .GT. MAXRTRFTMDLYEAR ) then
              yrnm1 = 'model year end'
              yr1 = myen
              mnyr = MINRTRFTMDLYEAR
              mxyr = MAXRTRFTMDLYEAR
              goto 7005
          endif
c
c       --- validate model year range ---
c
          if( myst .GT. myen ) then
              yrnm1 = 'model year start'
              yr1 = myst
              yrnm2 = 'model year end'
              yr2 = myen
              goto 7006
          endif
c
c       --- validate SCC ---
c
          if( .NOT. vldrtrftscc(scc) ) then
              goto 7007
          endif
c
c       --- validate tech type ---
c
          if( .NOT. vldrtrfttchtyp(techtype) ) then
              goto 7008
          endif
c
c       --- validate minimum HP (non-inclusive) ---
c
          if( .NOT. vldrtrfthp(hpmn) ) then
              hp = hpmn
              hpnm = 'minimum HP'
              goto 7009
          endif
c
c       --- validate maximum HP (inclusive) ---
c
          if( .NOT. vldrtrfthp(hpmx) ) then
              hp = hpmx
              hpnm = 'maximum HP'
              goto 7009
          endif
c
c       --- validate HP range ---
c
          if( hpmn .GE. hpmx ) then
              goto 7010
          endif
c
c       --- validate annual fraction or N ---
c
          if( annualfracorn < 0. ) then
              goto 7011
          endif
c
c       --- validate effectiveness ---
c
          if( effect .LT. 0. .OR. effect .GT. 1. ) then
              goto 7012
          endif
c
c       --- validate pollutant ---
c
          plltntidx = fndchr(pollutant, 10, rtrftplltnt, NRTRFTPLLTNT)
          if( plltntidx .EQ. 0 ) then
              goto 7013
          endif
c
c       --- validate ID ---
c
          if( id .LE. 0 ) then
              goto 7014
          endif
c
c       --- skip records that would have no effect (evaluation year
c           < retrofit year start, evaluation year < model year start,
c           or SCC not requested for this run) ---
c
          shouldskip = .FALSE.
          if( iepyr .LT. ryst ) then
              shouldskip = .TRUE.
              write(skipreason,'(2(A,I4))',ERR=9999)
     &                'Skipping retrofit record: Evaluation year ',
     &                iepyr,' less than retrofit year start ',ryst
          else if( iepyr .LT. myst ) then
              shouldskip = .TRUE.
              write(skipreason,'(2(A,I4))',ERR=9999)
     &                'Skipping retrofit record: Evaluation year ',
     &                iepyr,' less than model year start ',myst
          else if( scc .NE. RTRFTSCCALL
     &            .AND. .NOT. chkasc(scc, .TRUE.) ) then
              shouldskip = .TRUE.
              write(skipreason,'(3A)',ERR=9999)
     &                'Skipping retrofit record: SCC ',
     &                scc(:strmin(scc)),' not requested for this run'
          endif
c
c       --- if skipping the record ---
c
          if( shouldskip ) then
c
c           --- log a warning ---
c
              write(IOWSTD,8100,ERR=9999)
     &                wrnprefix(:strmin(wrnprefix)),irec,
     &                skipreason(:strmin(skipreason))
              if( lmsgfl ) then
                  write(IOWMSG,8100,ERR=9999)
     &                    wrnprefix(:strmin(wrnprefix)),irec,
     &                    skipreason(:strmin(skipreason))
              endif
c
c       --- else not skipping the record ---
c
          else
c
c           --- if retrofitting N units instead of a fraction and the
c               retrofit affects more than one retrofit year, model
c               year, SCC (i.e., "ALL", 4-digit or 7-digit global),
c               tech type (i.e., "ALL"), and/or HP category,
c               log a warning ---
c
              if( annualfracorn .GT. 1.
     &                .AND. ( ryst .NE. ryen .OR. myst .NE. myen
     &                .OR. scc .EQ. RTRFTSCCALL
     &                .OR. scc(5:10) .EQ. '000000'
     &                .OR. scc(8:10) .EQ. '000'
     &                .OR. techtype .EQ. RTRFTTCHTYPALL
     &                .OR. cnthpcat(hpmn, hpmx) .GT. 1 ) ) then
                  write(IOWSTD,8102,ERR=9999)
     &                    wrnprefix(:strmin(wrnprefix)),irec,
     &                    'N units retrofitted for more than one ',
     &                    'retrofit year, model year, SCC, ',
     &                    'tech type, and/or HP category'
                  if( lmsgfl ) then
                      write(IOWMSG,8102,ERR=9999)
     &                        wrnprefix(:strmin(wrnprefix)),irec,
     &                        'N units retrofitted for more than one ',
     &                        'retrofit year, model year, SCC, ',
     &                        'tech type, and/or HP category'
                  endif
              endif
c
c           --- increment count of retrofit records and
c               populate new index in the retrofit-record arrays ---
c
              rtrftcount = rtrftcount + 1
              rtrftrec(rtrftcount) = irec
              rtrftryst(rtrftcount) = ryst
              rtrftryen(rtrftcount) = ryen
              rtrftmyst(rtrftcount) = myst
              rtrftmyen(rtrftcount) = myen
              rtrftscc(rtrftcount) = scc
              rtrfttechtype(rtrftcount) = techtype
              rtrfthpmn(rtrftcount) = hpmn
              rtrfthpmx(rtrftcount) = hpmx
              rtrftannualfracorn(rtrftcount) = annualfracorn
              rtrfteffect(rtrftcount) = effect
              rtrftpollutant(rtrftcount) = pollutant
              rtrftplltntidx(rtrftcount) = plltntidx
              rtrftid(rtrftcount) = id
          endif
      end do
c
c   --- if no retrofit records read, log warning and exit successfully ---
c
      if( rtrftcount .EQ. 0 ) then
          write(IOWSTD,8101,ERR=9999)
     &            wrnprefix(:strmin(wrnprefix)),irec,
     &            'No retrofit records read'
          if( lmsgfl ) then
              write(IOWMSG,8101,ERR=9999)
     &                wrnprefix(:strmin(wrnprefix)),irec,
     &                'No retrofit records read'
          endif
          goto 999 ! go to successful end
      endif
c
c   --- record-set-level validation ---
c
      if( .NOT. vldrtrftrecs() ) then
          goto 9999 ! go to end without success
      endif
c
c   --- sort the record-set ---
c
      call srtrtrft(2, 1, rtrftcount)
c
c   --- set error code to sucess and return ---
c
  999 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c  Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,8000,ERR=9999)
     &        'ERROR: Opening retrofit data file: ',
     &        rtrftfl(:strmin(rtrftfl))
      if( lmsgfl ) then
          write(IOWMSG,8000,ERR=9999)
     &            'ERROR: Opening retrofit data file: ',
     &            rtrftfl(:strmin(rtrftfl))
      endif
      goto 9999
c
 7001 continue
      write(IOWSTD,8001,ERR=9999) 'ERROR:  Cannot find ',
     &        keywrd(:strmin(keywrd)),' packet of retrofit data file ',
     &        rtrftfl(:strmin(rtrftfl))
      if( lmsgfl ) then
          write(IOWMSG,8001,ERR=9999) 'ERROR:  Cannot find ',
     &            keywrd(:strmin(keywrd)),
     &            ' packet of retrofit data file ',
     &            rtrftfl(:strmin(rtrftfl))
      endif
      goto 9999
c
 7002 continue
      write(IOWSTD,8002,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec
      if( lmsgfl ) then
          write(IOWMSG,8002,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec
      endif
      goto 9999
c
 7003 continue
      write(IOWSTD,8003,ERR=9999)
     &        'ERROR:  Unexpected end-of-file reached reading ',
     &        'retrofit data file ',rtrftfl(:strmin(rtrftfl))
      if( lmsgfl ) then
          write(IOWMSG,8003,ERR=9999)
     &            'ERROR:  Unexpected end-of-file reached reading ',
     &            'retrofit data file ',rtrftfl(:strmin(rtrftfl))
      endif
      goto 9999
c
 7004 continue
      write(IOWSTD,8004,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Field: ',fldnm(:strmin(fldnm)),
     &        'Line read: ',line(:strmin(line))
      if( lmsgfl ) then
          write(IOWMSG,8004,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Field: ',fldnm(:strmin(fldnm)),
     &            'Line read: ',line(:strmin(line))
      endif
      goto 9999
c
 7005 continue
      write(IOWSTD,8005,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Invalid ',yrnm1(:strmin(yrnm1)),' value ',yr1,
     &        'Valid range is: ',mnyr,' to ',mxyr
      if( lmsgfl ) then
          write(IOWMSG,8005,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Invalid ',yrnm1(:strmin(yrnm1)),' value ',yr1,
     &            'Valid range is: ',mnyr,' to ',mxyr
      endif
      goto 9999
c
 7006 continue
      write(IOWSTD,8006,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Invalid ',yrnm1(:strmin(yrnm1)),' value ',yr1,
     &        'Must be less than or equal to ',
     &        yrnm2(:strmin(yrnm2)),' value ',yr2
      if( lmsgfl ) then
          write(IOWMSG,8006,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Invalid ',yrnm1(:strmin(yrnm1)),' value ',yr1,
     &            'Must be less than or equal to ',
     &            yrnm2(:strmin(yrnm2)),' value ',yr2
      endif
      goto 9999
c
 7007 continue
      write(IOWSTD,8007,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Invalid SCC value ',scc(:strmin(scc)),
     &        'Unknown SCC'
      if( lmsgfl ) then
          write(IOWMSG,8007,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Invalid SCC value ',scc(:strmin(scc)),
     &            'Unknown SCC'
      endif
      goto 9999
c
 7008 continue
      write(IOWSTD,8008,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Invalid tech type value ',
     &        techtype(:strmin(techtype)),
     &        'Unknown tech type'
      if( lmsgfl ) then
          write(IOWMSG,8008,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Invalid tech type value ',
     &            techtype(:strmin(techtype)),
     &            'Unknown tech type'
      endif
      goto 9999
c
 7009 continue
      write(IOWSTD,8009,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Invalid ',hpnm(:strmin(hpnm)),' value ',hp,
     &        'Unknown HP level'
      if( lmsgfl ) then
          write(IOWMSG,8009,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Invalid ',hpnm(:strmin(hpnm)),' value ',hp,
     &            'Unknown HP level'
      endif
      goto 9999
c
 7010 continue
      write(IOWSTD,8010,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Invalid minimum HP value ',hpmn,
     &        'Must be less than maximum HP value ',hpmx
      if( lmsgfl ) then
          write(IOWMSG,8010,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Invalid minimum HP value ',hpmn,
     &            'Must be less than maximum HP value ',hpmx
      endif
      goto 9999
c
 7011 continue
      write(IOWSTD,8011,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Invalid annual fraction or N value ',
     &        annualfracorn,
     &        'Must be greater than or equal to 0'
      if( lmsgfl ) then
          write(IOWMSG,8011,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Invalid annual fraction or N value ',
     &            annualfracorn,
     &            'Must be greater than or equal to 0'
      endif
      goto 9999
c
 7012 continue
      write(IOWSTD,8012,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Invalid effectiveness value ',effect,
     &        'Must be between 0 and 1'
      if( lmsgfl ) then
          write(IOWMSG,8012,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Invalid effectiveness value ',effect,
     &            'Must be between 0 and 1'
      endif
      goto 9999
c
 7013 continue
      write(IOWSTD,8013,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Invalid pollutant value ',
     &        pollutant(:strmin(pollutant)),
     &        'Not a valid retrofit pollutant'
      if( lmsgfl ) then
          write(IOWMSG,8013,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Invalid pollutant value ',
     &            pollutant(:strmin(pollutant)),
     &            'Not a valid retrofit pollutant'
      endif
      goto 9999
c
 7014 continue
      write(IOWSTD,8014,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Invalid ID value ',id,
     &        'Must be greater than 0'
      if( lmsgfl ) then
          write(IOWMSG,8014,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Invalid ID value ',id,
     &            'Must be greater than 0'
      endif
      goto 9999
c
 7015 continue
      write(IOWSTD,8015,ERR=9999)
     &        errprefix(:strmin(errprefix)),irec,
     &        'Maximum retrofit record count of ',
     &        MXRTRFT,' exceeded'
      if( lmsgfl ) then
          write(IOWMSG,8015,ERR=9999)
     &            errprefix(:strmin(errprefix)),irec,
     &            'Maximum retrofit record count of ',
     &            MXRTRFT,' exceeded'
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c  Format statements:
c-----------------------------------------------------------------------
c
 8000 format(/,1X,2A)
 8001 format(/,1X,4A)
 8002 format(/,1X,A,I6)
 8003 format(/,1X,4A)
 8004 format(/,1X,A,I6,/,6X,2A,/,6X,2A)
 8005 format(/,1X,A,I6,/,6X,3A,I4,/,6X,A,I4,A,I4)
 8006 format(/,1X,A,I6,/,6X,3A,I4,/,6X,3A,I4)
 8007 format(/,1X,A,I6,/,6X,2A,/,6X,A)
 8008 format(/,1X,A,I6,/,6X,2A,/,6X,A)
 8009 format(/,1X,A,I6,/,6X,3A,F5.0,/,6X,A)
 8010 format(/,1X,A,I6,/,6X,A,F5.0,/,6X,A,F5.0)
 8011 format(/,1X,A,I6,/,6X,A,F18.3,/,6X,A)
 8012 format(/,1X,A,I6,/,6X,A,F6.3,/,6X,A)
 8013 format(/,1X,A,I6,/,6X,2A,/,6X,A)
 8014 format(/,1X,A,I6,/,6X,A,I5,/,6X,A)
 8015 format(/,1X,A,I6,/,6X,A,I5,A)
 8100 format(/,1X,A,I6,/,6X,A,/)
 8101 format(/,1X,A,I6,/,6X,3A,/)
 8102 format(/,1X,A,I6,/,6X,3A,/)
c
c-----------------------------------------------------------------------
c  Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      if( didopenfile ) then
          close(IORTRFT)
      endif
      return
      end
