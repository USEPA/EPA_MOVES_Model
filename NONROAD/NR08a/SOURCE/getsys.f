C**** GETSYS
c
      subroutine getsys( ierr, fname )
      
      implicit none
c
c-----------------------------------------------------------------------
c
c   This routine gets the name of the system file from the user.  It
c   is done in a seperate routine so that platform dependant code
c   can easily be implimented to take advantage of certain compiler
c   dependant extensions such as the GETARG function of MS Fortran.
c
c   Arguments:
c      Outputs:
c         ierr  I   error code
c         fname C   name of system file
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      01/10/94  --gmw--  original development
c      4/6/96    --gwl--  modified for MS Fortran compiler
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'nonrdprm.inc'
      include 'nonrdio.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4         ierr
      character*(MXSTR) fname
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strlen
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) cline
      integer*4         iend
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- set error code ----
c
      ierr = IFAIL
c
c  --- get number of arguments, if more than 1 get control filename ---
c      note if running on unix or compiling with MS Fortran, use getarg 
c      instead of getcl ---
c
      call getarg( 1, cline )
clahey      call GETCL( cline )
      if( strlen(cline) .GT. 0 ) then  
        call lftjst( cline )
         iend = INDEX(cline,' ')
         if( iend .EQ. 0 ) iend = LEN( cline )
         fname = cline(:iend) 
c
c  --- only one argument, prompt user for system file ----
c
      else
         write(IOWSTD,9000,ERR=9999) 'Enter options filename: '
         read(IORSTD,'(A)') fname
         ierr = ISKIP
      endif
c
c  --- set error code to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format('+',A)
c
c-----------------------------------------------------------------------
c   Exit point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
