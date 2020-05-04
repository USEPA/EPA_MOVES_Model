C**** WRTMSG
c
      subroutine wrtmsg( ierr )
c
c-----------------------------------------------------------------------
c
c    echos the input parameters to the message file 
c
c    Argument description:
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      09/01/97  --gmw--  original development
c      09/22/04  --dfk--  added rec marine diesel sulfur
c      03/15/05  --cimulus--  added growth year
c      03/15/05  --cimulus--  added technology year
c      09/20/06  --epa--  added ethanol blend mkt share & volume percent
c      01/24/07  --epa-- add mthnam(IDXDEC) = MONDEC
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
      include 'nonrdio.inc'
      include 'nonrdact.inc'
      include 'nonrdalo.inc'
      include 'nonrdefc.inc'
      include 'nonrdeqp.inc'
      include 'nonrdgrw.inc'
      include 'nonrdreg.inc'
      include 'nonrdtpl.inc'
      include 'nonrdusr.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4    ierr
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c  strmin   I   returns the length of a string (min of 1)
c  fndchr   I   returns the index of a string in an array of strings
c
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
      character*40 name, pernam(IDXSES), sumnam(IDXTOT), mthnam(12)
      character*40 sesnam(4), daynam(2)
      integer*4    i, idx
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- echo the /OPTIONS/ packet parameters ---
c
      write(IOWMSG,9000,ERR=7000)
      write(IOWMSG,9000,ERR=7000) '*** Scenario Specific Parameters ***'
      write(IOWMSG,9000,ERR=7000)
      write(IOWMSG,9001,ERR=7000) 'First Title line',
     &                                    title1(:strmin(title1))
      write(IOWMSG,9001,ERR=7000) 'Second Title line',
     &                                    title2(:strmin(title2))
      write(IOWMSG,9002,ERR=7000) 'Fuel RVP (psi)',fulrvp
      write(IOWMSG,9002,ERR=7000) 'Fuel Oxygen weight %',oxypct
      write(IOWMSG,9004,ERR=7000) 'Gasoline Sulfur %',soxgas
      write(IOWMSG,9004,ERR=7000) 'Diesel Sulfur %',soxdsl
      write(IOWMSG,9004,ERR=7000) 'Marine Diesel Sulfur %',soxdsm
      write(IOWMSG,9004,ERR=7000) 'LPG/CNG Sulfur %',soxcng
      write(IOWMSG,9002,ERR=7000) 'Minimum Temperature',tempmn
      write(IOWMSG,9002,ERR=7000) 'Maximum Temperature',tempmx
      write(IOWMSG,9002,ERR=7000) 'Average Ambient Temp',amtemp
      if( lhigh ) then
          write(IOWMSG,9001,ERR=7000) 'Altitude of region',FLAGHI
      else
          write(IOWMSG,9001,ERR=7000) 'Altitude of region',FLAGLO
      endif
      write(IOWMSG,9002,ERR=7000) 'Stage II Control %',100 - stg2fac*100
      write(IOWMSG,9002,ERR=7000) 'EtOH Blend % Mkt',ethmkt
      write(IOWMSG,9002,ERR=7000) 'EtOH Vol %',ethvpct
c
c   --- echo the /PERIOD/ packet parameters ---
c
      write(IOWMSG,9000,ERR=7000)
      write(IOWMSG,9000,ERR=7000) '*** Period Parameters ***'
      write(IOWMSG,9000,ERR=7000)
      write(IOWMSG,9003,ERR=7000) 'Year of Inventory',iepyr
      pernam(IDXANN) = PERANN
      pernam(IDXSES) = PERSES
      pernam(IDXMTH) = PERMTH
      write(IOWMSG,9001,ERR=7000) 'Inventory for',
     &               pernam(iprtyp)(:strmin(pernam(iprtyp)))//' period'
      sumnam(IDXTYP) = SUMTYP
      sumnam(IDXTOT) = SUMTOT
      write(IOWMSG,9001,ERR=7000) 'Emissions summed for',
     &                         sumnam(ismtyp)(:strmin(sumnam(ismtyp)))
      if( iprtyp .EQ. IDXSES ) then
         sesnam(IDXWTR) = SESWTR
         sesnam(IDXSPR) = SESSPR
         sesnam(IDXSUM) = SESSUM
         sesnam(IDXFAL) = SESFAL
         write(IOWMSG,9001,ERR=7000) 'Season',
     &                         sesnam(iseasn)(:strmin(sesnam(iseasn)))
      else if( iprtyp .EQ. IDXMTH ) then
         mthnam(IDXJAN) = MONJAN
         mthnam(IDXFEB) = MONFEB
         mthnam(IDXMAR) = MONMAR
         mthnam(IDXAPR) = MONAPR
         mthnam(IDXMAY) = MONMAY
         mthnam(IDXJUN) = MONJUN
         mthnam(IDXJUL) = MONJUL
         mthnam(IDXAUG) = MONAUG
         mthnam(IDXSEP) = MONSEP
         mthnam(IDXOCT) = MONOCT
         mthnam(IDXNOV) = MONNOV
         mthnam(IDXDEC) = MONDEC
         write(IOWMSG,9001,ERR=7000) 'Month',
     &                         mthnam(imonth)(:strmin(mthnam(imonth)))
      endif
      daynam(IDXWKD) = WEEKDY
      daynam(IDXWKE) = WEEKND
      if( ismtyp .EQ. IDXTYP ) then
         write(IOWMSG,9001,ERR=7000) 'Day of week',
     &                         daynam(iday)(:strmin(daynam(iday)))
      endif
      write(IOWMSG,9003,ERR=7000) 'Year of Growth Calc',igryr
      write(IOWMSG,9003,ERR=7000) 'Year of Tech Sel',itchyr
c
c   --- echo the /REGION/ packet parameters ---
c
      write(IOWMSG,9000,ERR=7000)
      write(IOWMSG,9000,ERR=7000) '*** Region of Interest ***'
      write(IOWMSG,9000,ERR=7000)
      if( reglvl .EQ. NATION ) then
          write(IOWMSG,9001,ERR=7000) 'Region level',
     &                                       ' State-level estimates'
          write(IOWMSG,9001,ERR=7000) 'States of Interest',
     &                                              'All 50 states'
      else if( reglvl .EQ. STATE ) then
          write(IOWMSG,9001,ERR=7000) 'Region level',
     &                                       ' State-level estimates'
          if( reglst(1) .EQ. '00000' ) then
               write(IOWMSG,9001,ERR=7000) 'States of Interest',
     &                                              'All 50 states'
          else
              write(IOWMSG,9001,ERR=7000) 'States of Interest'
              do 10 i=1,NSTATE
               if( lstacd(i) )  write(IOWMSG,9001,ERR=7000) ' ',
     &                     statcd(i),' - ',statnm(i)(:strmin(statnm(i)))
   10         continue
          endif
      else if( reglvl .EQ. COUNTY ) then
          write(IOWMSG,9001,ERR=7000) 'Region level',
     &                                       ' County-level estimates'
          write(IOWMSG,9001,ERR=7000) 'Counties of Interest'
          do 20 i=1,NCNTY
             if( lfipcd(i) ) then
                name = ' '
                idx = fndchr( fipcod(i)(1:2)//'000', 5, statcd, NSTATE)
                if( idx .GT. 0 ) name = ', '//statnm(idx)
                write(IOWMSG,9001,ERR=7000) ' ',fipcod(i),
     &           ' - ',cntynm(i)(:strmin(cntynm(i))),name(:strmin(name))
             endif
   20     continue
      else if( reglvl .EQ. SUBCTY ) then
          write(IOWMSG,9001,ERR=7000) 'Region level',
     &                                     ' Sub-County-level estimates'
          write(IOWMSG,9001,ERR=7000) 'Areas of Interest'
          do 30 i=1,nregin
              write(IOWMSG,9001,ERR=7000) ' ',
     &                                  reglst(i)(:strmin(reglst(i)))
   30     continue
      endif
c
c   --- echo the /SOURCE CATEGORY/ packet parameters ---
c
      write(IOWMSG,9000,ERR=7000)
      write(IOWMSG,9000,ERR=7000) '*** Equipment Types ***'
      write(IOWMSG,9000,ERR=7000)
      if( lascal ) then
          write(IOWMSG,9001,ERR=7000) 'All equipment types.'
      else
          write(IOWMSG,9001,ERR=7000) 'SCC codes Selected'
          do 40 i=1,NEQNAM
            if( lascat(i) ) write(IOWMSG,9001,ERR=7000) ' ',eqpcod(i)
   40     continue
      endif
c
c   --- set error code to succes and return ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(1X,2A)',ERR=9999) 'ERROR: Writing output ',
     &                                                  'message file.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(T20,A)
 9001 format(T10,A,T30,:,':',7A)
 9002 format(T10,A,T30,:,':',F6.2)
 9003 format(T10,A,T30,:,':',I4)
 9004 format(T10,A,T30,:,':',F8.4)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
