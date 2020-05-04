C***** DAYLOOP
C
      subroutine dayloop(jbday,jeday,jbskip,jeskip,lskip)
c
c   --- set looping: if optional daily temperatures/RVPs were read
c       in, then use those values according to the month, season, or
c       annual period selected by the user, otherwise, only use the
c       single temperatures and RVP found in the /OPTIONS/ packet.
c
c
c-----------------------------------------------------------------------
c  
c   This routine calculates emissions
c      Argument descriptions:
c        Outputs:
c          jbday     I   beginning day
c          jeday     I   ending day
c          jbskip    I   beginning day of winter skip over
c          jeskip    I   ending day of winter skip over
c          lskip     L   flag to involk winter skip over
c        Inputs:
c          NONE
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      01/12/05  --dfk-- original development
c      01/20/05  --dfk-- corrected summer index error
c      02/03/05  --dfk-- commented out leap year correction, all years
c                        will now have 365 days, per EPA
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE
c
      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4   jbday
      integer*4   jeday
      integer*4   jbskip
      integer*4   jeskip
      logical     lskip
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
cleap      logical     leapyr
      integer, dimension(13), save :: daynum=(/1,32,60,91,121,152,182,
     &                                       213,244,274,305,335,366/)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      lskip=.false.
      jbday=1
      jeday=1
      jbskip=0
      jeskip=0
c
c  if not using daily temps/RVPs, return
c
      if(.NOT.ldayfl) goto 999
c
c  assign looping indexes      
c
        if( iprtyp .EQ. IDXANN ) then
           jbday=1
           jeday=MXDAYS
        else if( iprtyp .EQ. IDXSES ) then
           if( iseasn .EQ. IDXWTR ) then
               jbday=1
               jeday=MXDAYS
               lskip=.TRUE.
               jbskip=daynum(3)
               jeskip=daynum(12)-1
           else if( iseasn .EQ. IDXSPR ) then
               jbday=daynum(3)
               jeday=daynum(6)-1
           else if( iseasn .EQ. IDXSUM ) then
               jbday=daynum(6)
               jeday=daynum(9)-1
           else if( iseasn .EQ. IDXFAL ) then
               jbday=daynum(9)
               jeday=daynum(12)-1
           endif
        else if( iprtyp .EQ. IDXMTH) then
               jbday=daynum(imonth)
               jeday=daynum(imonth+1)-1
        endif
cleapc
cleapc  check and adjust for leap year
cleapc
cleap        if ((iepyr-(iepyr/400)*400).eq.0) then
cleap          leapyr = .TRUE.
cleap        elseif ((iepyr-(iepyr/100)*100).eq.0) then
cleap          leapyr = .FALSE.
cleap        elseif ((iepyr-(iepyr/4)*4).eq.0) then
cleap          leapyr = .TRUE.
cleap        else
cleap          leapyr = .FALSE.
cleap        endif
cleapc
cleap        if(leapyr) then
cleap          if(jbday.ge.daynum(3)) jbday=jbday+1
cleap          if(jeday.ge.daynum(3)-1) jeday=jeday+1
cleap          if(jeday.gt.daynum(13)) jeday=daynum(13)
cleap          if(lskip) then
cleap            if(jbskip.ge.daynum(3)) jbskip=jbskip+1
cleap            if(jeskip.ge.daynum(3)-1) jeskip=jeskip+1
cleap          endif
cleap        endif
c
 999  return
      end
      
