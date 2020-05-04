c*** GROWCNTL.INC
c
c-----------------------------------------------------------------------
c
c   Include file for the growth and control factors for the OFFROAD
c   program
c
c
c-----------------------------------------------------------------------
c   Parameters for Growth data:
c-----------------------------------------------------------------------
c
c    MXGRXF   I  maximum number of growth cross reference records
c    MXGROW   I  maximum number of growth records 
c              
      integer*4 MXGXRF
      integer*4 MXGROW
c
      parameter( MXGXRF =   12000 )
      parameter( MXGROW =   300   )
c
c-----------------------------------------------------------------------
c   Growth indicator cross reference data arrays:
c-----------------------------------------------------------------------
c
c    fipgrx   C   state code for record
c    ascgrx   C   SCC code for the growth record (could be global) 
c    hpgrx    R   range of HP categories for growth recrod
c    tecgrx   C   technology type for the growth record
c    indgrx   C   growth indicator code
c    nrcgrx   I   number of growth records
c
      character*10 ascgrx(MXGXRF)
      character*10 tecgrx(MXGXRF)
      character*5  fipgrx(MXGXRF)
      character*4  indgrx(MXGXRF)
      integer*4    nrcgrx
      real*4       hpgrx(MXGXRF,2)
c
      common /gxfchr/ fipgrx, ascgrx, tecgrx, indgrx
      common /gxfdat/ nrcgrx, hpgrx       
c
c-----------------------------------------------------------------------
c   Growth indicator data arrays:
c-----------------------------------------------------------------------
c
c    fipgrw  C   FIPS code for the growth data
c    subgrw  C   subregion code for the growth data
c    iyrgrw  I   year for the growth indicator
c    valgrw  R   growth indicator value
c    nrcgrw  I   number of growth records 
c
      character*5 fipgrw(MXGROW)
      character*5 subgrw(MXGROW)
      integer*4   iyrgrw(MXGROW)
      real*4      valgrw(MXGROW)
      integer*4   nrcgrw
c
      common /grwchr/ fipgrw, subgrw
      common /grwdat/ iyrgrw, valgrw, nrcgrw
