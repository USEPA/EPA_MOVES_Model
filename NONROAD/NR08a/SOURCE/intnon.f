C**** INTNON
      subroutine intnon( ierr )
c
c-----------------------------------------------------------------------
c  
c   Subroutine to perform initializaton for the nonroad model.
c
c    Argument description:
c        Outputs:
c           ierr   I   error code
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      12/07/95  --djk--  original development
c      07/22/96  --jlf--  added reading of technology fractions
c      10/29/01 2  --charvey--  added reading of alt base sulfur
c      05/10/04  --dfk--  added reading of evap tech groups
c      06/15/04  --dfk--  added reading of optional daily temperatures/RVPs
c      01/12/05  --dfk--  removed restriction that annual period can
c                         only be used with optional daily temp/RVP
c      04/25/05  --cimulus-- added call to rdfips to read the FIPS.dat
c                            list of counties just after the PERIOD packet
c                            is read but before the REGION packet is read.
c      05/16/05  --cimulus-- added call to rdrtrft to read the retrofit
c                            input data, after all other input files are
c                            read, due to retrofit input validation
c                            dependencies
c      07/21/05  --cimulus-- commented error block with label that was
c                            only referenced in commented code
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdalo.inc'
      include 'nonrdusr.inc'
c
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4    jerr
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- set the error code ----
c
      ierr =  IFAIL
c      
c  --- call routine to read the /OPTIONS/ packet ----
c
      call rdnropt(jerr )
      if( jerr .NE. ISUCES ) goto 9999 
      call spinit()
c      
c  --- call routine to read the /PERIOD/ packet ----
c
      call rdnrper(jerr )
      if( jerr .NE. ISUCES ) goto 9999 
      call spinit()
c
c  --- read the list of counties from a default file or the file
c      specified by the options file ---
c
      call rdfips( jerr )
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to read the /REGION/ packet ----
c
      call rdnrreg(jerr)
      if( jerr .NE. ISUCES ) goto 9999 
      call spinit()
c
c  --- call routine to read the /SOURCE CATEGORY/ packet ----
c
      call rdnrsrc(jerr)
      if( jerr .NE. ISUCES ) goto 9999 
      call spinit()
c
c  --- call routine to open the emission factor files ---
c
      call opnefc( jerr ) 
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the spatial allocation coefficients ---
c
      call rdalo(jerr)
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to read the spatial indicator data and
c      create the sorted file ---
c
      call rdind( jerr )
      if( jerr .EQ. IEOF ) goto 7001
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to read the growth indicator data and create
c      the sorted growth indicator file ---
c
      call rdgrow( jerr )
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to read the /STAGE II/ packet ----
c
      call rdstg2(jerr)
      if( jerr .NE. ISUCES ) goto 9999 
      call spinit()
c
c  --- call routine to read the /PM BASE SULFUR/ packet ----
c
      call rdsulf(jerr)
      if( jerr .NE. ISUCES ) goto 9999 
      call spinit()
c
c  --- call routine to echo the inputs to message file --- 
c
      call wrtmsg( jerr )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the regions definition file ----
c
      call rdrgndf(jerr)
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to read the activity data ---
c
      call rdact( jerr )
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to read all of the population data and create the
c      sorted file ----
c
      call rdpop(jerr)
      if( jerr .EQ. IEOF ) goto 7000
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to read the emission factor files ---
c
      call rdefls( jerr ) 
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to read the BSFC file ---
c
      call rdbsfc( jerr ) 
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to read the seasonality files ----
c
      call rdseas( jerr )
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to read the exhaust technology fraction file ----
c      could be a MOVES style file ---
c
      call rdtech( jerr )
      if( jerr .NE. ISUCES .AND. jerr .NE. ISKIP ) goto 9999
      if( jerr .EQ. ISKIP ) then
         call rdtech_moves( jerr )
         if( jerr .EQ. IFAIL ) goto 9999
         if( jerr .EQ. ISKIP ) goto 7003
      endif
      call spinit()
c
c  --- call routine to read the evap technology fraction file ----
c
      call rdevtech( jerr )
      if( jerr .NE. ISUCES .AND. jerr .NE. ISKIP ) goto 9999
      if( jerr .EQ. ISKIP ) then
         call rdevtech_moves( jerr )
         if( jerr .EQ. IFAIL ) goto 9999
         if( jerr .EQ. ISKIP ) goto 7004
      endif
      call spinit()
c
c  --- call routine to initialize the adjustment factor variables ---
c
      call intadj()
      if( jerr .NE. ISUCES ) goto 9999
      call spinit()
c
c  --- call routine to initialize the optional daily temperatures/RVPs ---
c  --- option only allowed if annual period is selected ---
c
      if(ldayfl) then
c        if(iprtyp .NE. IDXANN) goto 7002
        call rdday( jerr, IORDAY, dayfl)
        if( jerr .NE. ISUCES ) goto 9999
        call spinit()
      end if
c
c  --- call routine to read the optional retrofit file
c      done after all other input files read, since the retrofit input
c      validation depends on some of the data read from other files ----
c
      if( lrtrftfl ) then
          call rdrtrft( jerr )
          if( jerr .NE. ISUCES ) goto 9999
          call spinit()
      endif
c
c  --- set error code and return successuflly ---
c
      ierr = ISUCES 
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,1X,2A,/,1X,A)',ERR=9999) 'ERROR:  Could not ',
     &            'find any population data for the regions ',
     &                      '        and equipment types requested.'
      write(IOWSTD,'(9X,A,/9X,A)',ERR=9999) 
     &            'Make sure the population files are for the region',
     &            'specified in the /REGION/ packet.'
      write(IOWMSG,'(/,1X,2A,/,1X,A)',ERR=9999) 'ERROR:  Could not ',
     &            'find any population data for the regions ',
     &                      '        and equipment types requested.'
      write(IOWMSG,'(9X,A,/9X,A)',ERR=9999) 
     &            'Make sure the population files are for the region',
     &            'specified in the /REGION/ packet.'
      goto 9999
c
 7001 continue
      write(IOWSTD,'(/,1X,3A)',ERR=9999) 'ERROR:  Could not find any ',
     &             'spatial allocation indicator data for the regions ',
     &                                  'and equipment types requested.'
      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Could not find any ',
     &             'spatial allocation indicator data for the regions ',
     &                                  'and equipment types requested.'
      goto 9999
c
c 7002 continue
c      write(IOWSTD,'(/,1X,2A)',ERR=9999) 'ERROR:  Optional annual',
c     &             ' temperatures specified for non-annual period.'
c      write(IOWMSG,'(/,1X,3A)',ERR=9999) 'ERROR:  Optional annual',
c     &             ' temperatures specified for non-annual period.'
c      goto 9999
c
 7003 continue
      write(IOWSTD,'(/,1X,2A,/,9X,A)',ERR=9999)
     &       'ERROR:  Cannot find packet identifier in',
     &                   ' technology fractions file ',
     &                                   tchfl(:strmin(tchfl))
      write(IOWMSG,'(/,1X,3A,/,9X,3A)',ERR=9999)
     &       'ERROR:  Cannot find packet identifier in',
     &                   ' technology fractions file ',
     &                                   tchfl(:strmin(tchfl))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(/,1X,2A,/,9X,A)',ERR=9999)
     &       'ERROR:  Cannot find packet identifier in',
     &                   ' evap technology fractions file ',
     &                                   evtchfl(:strmin(evtchfl))
      write(IOWMSG,'(/,1X,3A,/,9X,3A)',ERR=9999)
     &       'ERROR:  Cannot find packet identifier in',
     &                   ' evap technology fractions file ',
     &                                   evtchfl(:strmin(evtchfl))
      goto 9999
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

