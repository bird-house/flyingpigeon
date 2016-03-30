FUNCTION SLAMCH_F95( PRECISION, CMACH, INFO )
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. "Use Statements" ..
   USE LA_PRECISION, ONLY: WP => SP
   USE LA_AUXMOD, ONLY: ERINFO, LSAME
   USE F77_LAPACK, ONLY: SLAMCH
!  .. "Implicit Statement" ..
   IMPLICIT NONE
!  .. "Scalar Arguments" ..
   CHARACTER(LEN=1), INTENT(IN) :: CMACH
   REAL(WP), INTENT(IN) :: PRECISION
   INTEGER, INTENT(OUT), OPTIONAL :: INFO
   REAL(WP) :: SLAMCH_F95
!--------------------------------------------------
! .. "Parameters" ..
   CHARACTER( LEN=10 ), PARAMETER :: CHR='ESBPNRMULO'
   CHARACTER( LEN=10 ), PARAMETER :: SRNAME = 'LA_LAMCH'
! .. "Local Scalars" ..
   INTEGER :: I, LINFO
!  .. "Executable Statements" ..
   LINFO = 2; SLAMCH_F95 = PRECISION
   DO I = 1, 10
     IF( LSAME(CMACH, CHR(I:I)) ) LINFO = 0
   END DO
   SLAMCH_F95 = SLAMCH(CMACH)
   CALL ERINFO( LINFO, SRNAME, INFO )
END FUNCTION SLAMCH_F95
