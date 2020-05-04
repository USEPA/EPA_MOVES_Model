C**** FNDKEY version 1.0 fndkey.f 1.5 5/20/92
c
      subroutine fndkey( ierr,
     &                       iounit, keyin )
c
c-----------------------------------------------------------------------
c
c   Description:
c
c     This routine searches the file attached to logical unit number
c     "iounit" for a string matching "keyin".  The first 20 cahracters
c     of each line are read and converted to upper case.  It is assumed
c     that the "keyin" string is upper case.  An initial search is made,
c     and if not successful, the file is rewound and a second search is
c     made.  If a match is still not found, the error flag is set to
c     failure.
c
c   Arguments:
c
c     Outputs:
c       ierr     I   error flag (either ISUCES or IFAIL)
c     Inputs:
c       iounit   I   logical unit number of file to read
c       keyin    C   string to match (all upper case)
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     11/10/91  -gmw-  Original development
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      IMPLICIT NONE

      include 'nonrdprm.inc'
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
c       strlen   I   returns actual length of string (no trailing blanks)
c
      integer*4 strlen
c
c-----------------------------------------------------------------------
c   Argument decleration:
c-----------------------------------------------------------------------
c
      integer*4      ierr
      integer*4      iounit
      character*(*)  keyin
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c       keywrd   C   keyword to check against argument for match
c       lpass2   L   flag to determine if currently on second pass
c
      character*(MXSTR) keywrd
      logical           lpass2
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
      lpass2 = .FALSE.
c
c   ---- read next string from file ----
c
 111  continue
      read( iounit, 8000, ERR=7000, END=7001 ) keywrd
c
c   ---- convert ot upper case ---
c
      call low2up( keywrd )
c
c   ---- get rid of leading blanks ----
c
      call lftjst( keywrd )
c
c   ---- If match return success; otherwise read next record ---
c
      if( keywrd(:strlen(keyin)) .EQ. keyin(:strlen(keyin)) ) then
          ierr = ISUCES
          goto 9999
      else
          goto 111
      endif
c
c----------------------------------------------------------------------
c   Error statements:
c----------------------------------------------------------------------
c
 7000 continue
      ierr = IRDERR
      goto 9999
c
 7001 continue
      rewind( iounit, ERR=7000 )
      if( lpass2 ) then
         ierr = IEOF
         goto 9999
      else
         lpass2 = .TRUE.
         goto 111
      endif
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(A20)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
