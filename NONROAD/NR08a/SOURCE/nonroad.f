C**** NONROAD
c
      program nonroad
c
c-----------------------------------------------------------------------
c  
c   This is the main driving routine for the EPA NONROAD model. It calls
c   all of the routines to read and store the data needed to process
c   the nonroad emisisons.  It reads populations for all equipment types
c   and states/counties requested and then allocates population to 
c   county/subregion level.  Then population is then grown to future
c   year predictions.  Activity levels and episode specific adjustment
c   factors are applied to the population to get hours of use for the
c   requested period.  Emission factors are then retrieved and applied 
c   to the activity to get emission estimates.  The emission estimates 
c   are written to a scratch file which will be read and processed for
c   purposes of creating output tables.
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/10/95  --gmw--  began development
c      07/22/96  --jlf-- changes for technology fractions
c      09/22/04  --dfk-- added initialization of yrsav per NR04n2
c      07/15/05  --cimulus-- removed commercial marine related handling
c      07/20/05  --cimulus-- floating-point comparison for equality
c                            okay; avghpc values are read from file,
c                            rather than being calculated at runtime
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdreg.inc'
      include 'nonrdusr.inc'
      include 'nonrdefc.inc'
      include 'nonrdeqp.inc'
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   fndchr   I   returns the index of a string in an array of strings
c   strmin   I   returns length of string (minumum of 1)
c
      integer*4 fndchr
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*80 fname
      character*20 tmpstr
      character*10 asccod
      integer*4    jerr, idxasc, icurec, idxsta, idxfip, iyear
      integer*4    idxreg, i, j
      real*4       growth, emsams(NCNTY,MXPOL)
      logical*4    lskip, lexist
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- initialize the spin characters ---
c
      spin(0) = '|'
      spin(1) = '|'
      spin(2) = '/'
      spin(3) = '-'
      spin(4) = '-'
      spin(5) = '\\'
      spin(6) = '|'
      spin(7) = '|'
      spin(8) = '/'
      spin(9) = '-'
      spin(10) = '-'
      spin(11) = '\\'
      nrecds = 0
c
c  --- call routine to get the options file ---
c
      call getsys( jerr, fname )
      if( jerr .NE. ISUCES ) goto 9999
      sysfl = fname
      inquire(file=sysfl,exist=lexist)
      if(.NOT. lexist) goto 7002
c
c  --- call routines to read and initialize some of the arrays --
c
      write(IOWSTD,'(A,$)') 'Initializing...'
      call iniasc()
      call spinit()
      call in1fip()
      call spinit()
      call in2fip()
      call spinit()
      call in3fip()
      call spinit()
      call in4fip()
      call spinit()
      call in5fip()
      call spinit()
c
c  --- call routines to read filenames from the user options file 
c      and open some of the files ---
c
      call opnnon( jerr )
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to read all data files and initialize the 
c      data structures ---
c
      call intnon( jerr )
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to write the header of the data file ---
c
      call wrthdr( jerr )
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- if doing EPS2 AMS file, call routine to initialize variables ---
c
      if( lamsfl ) call intams()
c
c  --- main processing loop, call routine to read the population
c      records until there are no more population records to process ----
c
      iyear = iepyr
      write(IOWSTD,'(2A)') 'Done'
      write(IOWSTD,'(1X,A,$)') 'Processing...'
      namsrc = 0
ccc   ascsav = '          '
      yrsav = 1900
  111 continue
      call getpop(jerr, asccod, iyear )
      if( jerr .EQ. IEOF ) goto 222
      if( jerr .NE. ISUCES ) goto 7000
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- initialize the emissions for AMS file ---
c
      do 10 j=1,MXPOL
        do 20 i=1,NCNTY
           emsams(i,j) = 0.
   20   continue
   10 continue
c
c  --- if equipment type is not requested skip it, should not happen --- 
c
      idxasc = fndchr( asccod, 10, eqpcod, NEQNAM )
      if( idxasc .LE. 0 ) goto 111
      if( .NOT. lascat(idxasc) ) goto 111
c
c  --- if region is not requested skip it, also shouldn't happen ---
c
      idxsta = 0
      idxfip = 0
      if( regncd(1)(1:5) .NE. '00000' ) then
         if( regncd(1)(3:5) .EQ. '000' ) then
             idxsta = fndchr( regncd(1)(1:5), 5, statcd, NSTATE )
             if( idxsta .LE. 0 ) goto 111
             if( .NOT. lstacd(idxsta) ) goto 111
         else 
             idxfip = fndchr( regncd(1)(1:5), 5, fipcod, NCNTY )
             if( idxfip .LE. 0 ) goto 111
             if( .NOT. lfipcd(idxfip) ) goto 111
         endif
      endif
c
c  --- process all of the records read for this SCC ---
c
      icurec = 0
      lskip = .FALSE.
  333 continue
c
c   --- if the last record was a "growth record" skip it ---
c
      if( lskip ) then
        icurec = icurec + 1
      endif
c
c   --- increment to get the next record
c
      icurec = icurec + 1
      if( icurec .GT. npoprc ) then
          if( lamsfl ) then
             call wrtams( jerr, emsams, asccod )
             if( jerr .NE. ISUCES ) goto 9999
          endif
          goto 111
      endif
      call dispit()
c
c   --- if the next record is the same except for year, then we
c       should use it as a growth factor to project base year ---
c
      growth = -9
      lskip = .FALSE.
      if( icurec .LT. npoprc ) then
         if( regncd(icurec) .EQ. regncd(icurec+1) .AND.
     &         avghpc(icurec) .EQ. avghpc(icurec+1) .AND. ! floating-point comparison for equality okay; avghpc values are read from file, rather than being calculated at runtime
     &                   popeqp(icurec) .GT. 0 .AND.
     &                       ipopyr(icurec) .NE. ipopyr(icurec+1) ) then
cc           growth = EXP( 1/(ipopyr(icurec+1) - ipopyr(icurec)) *
cc   &                          LOG(popeqp(icurec+1) / popeqp(icurec)) )
             growth = ( popeqp(icurec+1) - popeqp(icurec) ) / 
     &         ( popeqp(icurec) * (ipopyr(icurec+1) - ipopyr(icurec)) )
             lskip = .TRUE.
             nrecds = nrecds + 1
         endif
      endif
c
c  --- if region is not requested skip it, also shouldn't happen ---
c
      idxsta = 0
      idxfip = 0
      if( regncd(icurec)(1:5) .NE. '00000' ) then
         if( regncd(icurec)(3:5) .EQ. '000' ) then
             idxsta = fndchr( regncd(icurec)(1:5), 5, statcd, NSTATE )
             if( idxsta .LE. 0 ) goto 333
             if( .NOT. lstacd(idxsta) ) goto 333
         else 
             idxfip = fndchr( regncd(icurec)(1:5), 5, fipcod, NCNTY )
             if( idxfip .LE. 0 ) goto 333
             if( .NOT. lfipcd(idxfip) ) goto 333
         endif
      endif
c
c   --- set the current fuel type ---
c
      ifuel = 0
      if( asccod(1:4) .EQ. '2260' .OR. 
     &            asccod(1:7) .EQ. '2282005' .OR. 
     &                              asccod(1:7) .EQ. '2285003') then
         ifuel = IDXGS2
      else if( asccod(1:4) .EQ. '2265' .OR. 
     &            asccod(1:7) .EQ. '2282010' .OR.
     &                              asccod(1:7) .EQ. '2285004') then
         ifuel = IDXGS4
      else if( asccod(1:4) .EQ. '2268' .OR. 
     &                              asccod(1:7) .EQ. '2285008') then
         ifuel = IDXCNG
      else if( asccod(1:4) .EQ. '2267' .OR. 
     &                              asccod(1:7) .EQ. '2285006') then
         ifuel = IDXLPG
      else if( asccod(1:4) .EQ. '2270' .OR. 
     &            asccod(1:7) .EQ. '2280002' .OR. 
     &                  asccod(1:7) .EQ. '2282020' .OR. 
     &                              asccod(1:7) .EQ. '2285002') then
         ifuel = IDXDSL
      endif
c
c   --- if doing national to state allocation, call the
c       routine to process the states ---
c
      if( regncd(icurec) .EQ. '00000' ) then
         if( reglvl .EQ. USTOT ) then
              nnatrc = nnatrc + 1
              call prcus(jerr,icurec,asccod,growth)
              if( jerr .EQ. ISKIP ) goto 333
              if( jerr .NE. ISUCES ) goto 9999
         else if( reglvl .EQ. STATE .OR. reglvl .EQ. NATION ) then
              nnatrc = nnatrc + 1
              call prcnat(jerr,icurec,asccod,idxsta,growth)
              if( jerr .EQ. ISKIP ) goto 333
              if( jerr .NE. ISUCES ) goto 9999
              goto 333
          endif
c
c   --- if doing state to county allocation, call the
c       routine to process the counties ---
c
      else if( regncd(icurec)(3:5) .EQ. '000' ) then
         if( reglvl .EQ. COUNTY ) then
              call prcsta(jerr,emsams,icurec,asccod,idxsta,growth)
              if( jerr .EQ. ISKIP ) goto 333
              if( jerr .NE. ISUCES ) goto 9999
              goto 333
         else if( reglvl .EQ. STATE .OR. reglvl .EQ. NATION ) then
              call prc1st(jerr,icurec,asccod,growth)
              if( jerr .EQ. ISKIP ) goto 333
              if( jerr .NE. ISUCES ) goto 9999
              goto 333
         endif 
c
c   --- FIPS code is a county code, call routine to process the subregion ---
c
      else
         if( reglvl .EQ. COUNTY ) then
             idxfip = fndchr( regncd(icurec)(1:5), 5, fipcod, NCNTY )
             if( idxfip .LE. 0 ) goto 333
             call prccty(jerr,emsams,icurec,asccod,growth)
             if( jerr .EQ. ISKIP ) goto 333
             if( jerr .NE. ISUCES ) goto 9999
             goto 333
c
c  --- could be full county record or partial county record ---
c
          else if( reglvl .EQ. SUBCTY ) then
             idxreg = fndchr( regncd(icurec)(1:5)//'      ', 
     &                                            10, reglst, nregin ) 
             if( idxreg .GT. 0 ) then
                call prccty(jerr,emsams,icurec,asccod,growth)
                if( jerr .EQ. ISKIP ) goto 333
                if( jerr .NE. ISUCES ) goto 9999
             endif
             idxfip = fndchr( regncd(icurec)(1:5), 5, reglst, nregin )
             if( idxfip .GT. 0 ) then
                call prcsub(jerr,emsams,icurec,asccod,growth)
                if( jerr .EQ. ISKIP ) goto 333
                if( jerr .NE. ISUCES ) goto 9999
              endif  
          endif
      endif
c
c   --- get next population record in this set ---
c
      goto 333
  222 continue
c
c  --- if doing the SI report, call routine to write the data ---
c
      if( lsifl ) then
          call wrtsi(jerr)
          if( jerr .NE. ISUCES ) goto 9999
      endif

c  --- all of the records processed, write summary ---
c
      nrecds = ntotrc - 1
      call dispit()
      call wrtsum(jerr) 
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- Display completion banner ----
c
      write(IOWSTD,'(2A)') 'Done'
      if( nwarn .EQ. 0 ) then
         write(IOWSTD,'(/,1X,4A)') 
     &                'Successful completion of ',PROGNM,', ',VERSON
      else
         write(tmpstr,'(I20)') nwarn
         call lftjst( tmpstr )
         write(IOWSTD,'(/,1X,4A)') 'Completion of ',PROGNM,', ',VERSON
         write(IOWSTD,'(10X,4A)') 'There were ',
     &      tmpstr(:strmin(tmpstr)) ,' warnings.  Review message file.'
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,2A,/,1X,A)',ERR=9999) 'ERROR:  Could not ',
     &       'find any population data for the regions ',
     &                     '        and equipment types requested.'
      write(IOWSTD,'(1X,3A)',ERR=9999) 'Make sure the population files',
     &           ' are for the area specified in the /REGION/ packet.'
      write(IOWMSG,'(/,1X,2A,/,1X,A)',ERR=9999) 'ERROR:  Could not ',
     &       'find any population data for the regions ',
     &                     '        and equipment types requested.'
      write(IOWMSG,'(1X,3A)',ERR=9999) 'Make sure the population files',
     &           ' are for the area specified in the /REGION/ packet.'
      goto 9999
c
 7002 continue
      write(IOWSTD,'(1X,3A)',ERR=9999) 'ERROR:  Could not find file ',
     &                                           sysfl(:strmin(sysfl))
      goto 9999
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format('+',A,/)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
c
c  --- call the routine to close the open files ---
c
      call clsnon()
C      PAUSE
      end
