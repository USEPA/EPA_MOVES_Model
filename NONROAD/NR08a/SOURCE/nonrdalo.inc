c*** ALLOCATE.INC
c
c-----------------------------------------------------------------------
c
c   Include for the allocation surrogate data for the OFFROAD program.
c
c-----------------------------------------------------------------------
c   Parameters for array bounds:
c-----------------------------------------------------------------------
c
c   MXALO   I  maximum number of allocation indicator assignments
c   MXCOEF  I  maximum number of coefficients in regression equation
c   MXCODE  I  maximum number of indicator codes used 
c
      integer*4 MXALO
      integer*4 MXCOEF
      integer*4 MXCODE
c
      parameter( MXALO  =  500 )
      parameter( MXCOEF =    3 )
      parameter( MXCODE =  100 )
c
c-----------------------------------------------------------------------
c   Allocation indicator assignment data arrays:
c-----------------------------------------------------------------------
c
c    ascalo  C  asc code for assignment of allocation indicator
c    coeffs  R  coefficients for regression equations
c    indcod  C  indicator code for allocation
c    alocod  C  list of spatial indicator codes
c    nalorc  I  number of allocation assignment records found 
c    nalocd  I  number of unigue indicator codes used
c
      character*10 ascalo(MXALO)
      character*3  indcod(MXALO,MXCOEF)
      character*3  alocod(MXCODE)
      real*4       coeffs(MXALO,MXCOEF)
      integer*4    nalorc
      integer*4    nalocd
c
      common /alochr/ ascalo, indcod, alocod
      common /alodat/ coeffs, nalorc, nalocd
