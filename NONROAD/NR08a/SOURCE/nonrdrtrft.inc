c*** NONRDRTRFT.INC
c
c-----------------------------------------------------------------------
c
c   Include file for the retrofit parameters used by the NONROAD program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c   05/25/05 --cimulus-- Original development
c   05/31/05 --cimulus-- Added rtrftfltr1mnidx to the rtrftrtdat common block
c
c-----------------------------------------------------------------------
c   Parameters for array bounds:
c-----------------------------------------------------------------------
c
c   MXRTRFT        I  maximum number of retrofit records in retrofit file
c   NRTRFTPLLTNT   I  number of pollutants that are allowed as possible
c                     value in the pollutant field of a retrofit record
c
      integer*4 MXRTRFT
      integer*4 NRTRFTPLLTNT
c
      parameter( MXRTRFT       = 500 )
      parameter( NRTRFTPLLTNT  =   4 )
c
c
c-----------------------------------------------------------------------
c   Variables for retrofit parameters
c-----------------------------------------------------------------------
c
c   rtrftcount          I  number of records in the retrofit arrays
c   rtrftrec            I  original record number from input file
c   rtrftryst           I  retrofit year start (first calendar year
c                          when retrofits are done)
c   rtrftryen           I  retrofit year end (last calendar year
c                          when retrofits are done)
c   rtrftmyst           I  model year start for the retrofit (first model
c                          year equipment that is receiving retrofit)
c   rtrftmyen           I  model year end for the retrofit (last model
c                          year equipment that is receiving retrofit)
c   rtrftscc            C  SCC code for the retrofit
c   rtrfttechtype       C  exhaust technology type for the retrofit
c                          (e.g., Base, T0, T1, T2, T2M)
c   rtrfthpmn           R  minimum HP for the retrofit
c   rtrfthpmx           R  maximum HP for the retrofit
c   rtrftannualfracorn  R  annual retrofit fraction (0.0 - 1.0) OR actual
c                          total number retrofitted (greater than 1.0)
c   rtrfteffect         R  retrofit effectiveness; range = 0.0 - 1.0
c                          (zero is no benefit)
c   rtrftpollutant      C  exhaust pollutant affected by retrofit
c                          (HC, CO, NOx, or PM)
c   rtrftplltntidx      I  index into rtrftplltnt for the pollutant that
c                          corresponds to rtrftpollutant (HC = IDXTHC,
c                          CO = IDXCO, etc); this is calculated based on
c                          rtrftpollutant rather than read from file
c   rtrftid             I  retrofit ID (same arbitrary number for
c                          different pollutants being affected by
c                          same retrofit)
c
      integer*4    rtrftcount
      integer*4    rtrftrec(MXRTRFT)
      integer*4    rtrftryst(MXRTRFT)
      integer*4    rtrftryen(MXRTRFT)
      integer*4    rtrftmyst(MXRTRFT)
      integer*4    rtrftmyen(MXRTRFT)
      character*10 rtrftscc(MXRTRFT)
      character*10 rtrfttechtype(MXRTRFT)
      real*4       rtrfthpmn(MXRTRFT)
      real*4       rtrfthpmx(MXRTRFT)
      real*4       rtrftannualfracorn(MXRTRFT)
      real*4       rtrfteffect(MXRTRFT)
      character*10 rtrftpollutant(MXRTRFT)
      integer*4    rtrftplltntidx(MXRTRFT)
      integer*4    rtrftid(MXRTRFT)
c
      common /rtrftchr/ rtrftscc, rtrfttechtype, rtrftpollutant
      common /rtrftdat/ rtrftcount, rtrftrec, rtrftryst, rtrftryen,
     &                  rtrftmyst, rtrftmyen, rtrfthpmn, rtrfthpmx,
     &                  rtrftannualfracorn, rtrfteffect,
     &                  rtrftplltntidx, rtrftid
c
c-----------------------------------------------------------------------
c   Parameters for input validation:
c-----------------------------------------------------------------------
c
c   MINRTRFTYEAR     I  minimum retrofit year
c   MAXRTRFTYEAR     I  maximum retrofit year
c   MINRTRFTMDLYEAR  I  minimum retrofit model year
c   MAXRTRFTMDLYEAR  I  maximum retrofit model year
c   RTRFTSCCALL      C  the "ALL" option for SCC
c   RTRFTTCHTYPALL   C  the "ALL" option for tech type
c
      integer*4    MINRTRFTYEAR
      integer*4    MAXRTRFTYEAR
      integer*4    MINRTRFTMDLYEAR
      integer*4    MAXRTRFTMDLYEAR
      character*10 RTRFTSCCALL
      character*10 RTRFTTCHTYPALL
c
      parameter( MINRTRFTYEAR =      MINYEAR )
      parameter( MAXRTRFTYEAR =      MAXYEAR )
      parameter( MINRTRFTMDLYEAR =      1900 )
      parameter( MAXRTRFTMDLYEAR =   MAXYEAR )
      parameter( RTRFTSCCALL =         'ALL' )
      parameter( RTRFTTCHTYPALL =      'ALL' )
c
c-----------------------------------------------------------------------
c   Variables for retrofit input validation
c-----------------------------------------------------------------------
c
c   rtrftplltnt        C  pollutants allowed in retrofit records
c   rtrftplltntidxmp   I  map from index into rtrftplltnt to
c                         pollutant index
c
      character*10 rtrftplltnt(NRTRFTPLLTNT)
      integer*4    rtrftplltntidxmp(NRTRFTPLLTNT)
c
      common /rtrftvldchr/ rtrftplltnt, rtrftplltntidxmp
c
c-----------------------------------------------------------------------
c   Variables for retrofit runtime, calculated values
c-----------------------------------------------------------------------
c
c   rtrftfltr1cnt     I  number of indexes in the retrofit filter
c                        type 1 array
c   rtrftfltr1        I  the indexes of a filtered set of retrofit
c                        records for filter type 1
c   rtrftfltr1mnidx   I  the minimum index into rtrftfltr1 that could
c                        possibly affect a subsequent call to fndrtrft
c                        (i.e., the minimum index into rtrftfltr1 that
c                        has a rtrftmyen value that is >= the next
c                        model year that will be passed to fndrtrft)
c   rtrftfltr2cnt     I  number of indexes in the retrofit filter
c                        type 2 array
c   rtrftfltr2        I  the indexes of a filtered set of retrofit
c                        records for filter type 2
c   rtrftfltr3cnt     I  number of indexes in the retrofit filter
c                        type 3 array
c   rtrftfltr3        I  the indexes of a filtered set of retrofit
c                        records for filter type 3
c   numrtrft          I  number of retrofits applicable to the current
c                        iteration (this is different than rtrftfltr3cnt
c                        in that numrtrft is the number of unique
c                        retrofit IDs, whereas rtrftfltr3cnt can include
c                        multiple records per retrofit ID)
c   rtrftplltntrdfrc  R  pollutant reduction fraction for the current
c                        iteration (SCC, HP, model year, tech type);
c                        large enough for all pollutants, although
c                        only some are used
c
      integer*4 rtrftfltr1cnt
      integer*4 rtrftfltr1(MXRTRFT)
      integer*4 rtrftfltr1mnidx
      integer*4 rtrftfltr2cnt
      integer*4 rtrftfltr2(MXRTRFT)
      integer*4 rtrftfltr3cnt
      integer*4 rtrftfltr3(MXRTRFT)
      integer*4 numrtrft
      real*4    rtrftplltntrdfrc(MXPOL)
c
      common /rtrftrtdat/ rtrftfltr1cnt, rtrftfltr1, rtrftfltr1mnidx,
     &                    rtrftfltr2cnt, rtrftfltr2,
     &                    rtrftfltr3cnt, rtrftfltr3, numrtrft,
     &                    rtrftplltntrdfrc
