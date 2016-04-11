      MODULE F77_LAPACK
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
 
      INTERFACE LA_LANGB

       FUNCTION SLANGB( NORM, N, KL, KU, AB, LDAB, WORK )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP) :: SLANGB
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDAB, N, KL, KU
         REAL(WP), INTENT(IN) :: AB( LDAB, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION SLANGB

       FUNCTION DLANGB( NORM, N, KL, KU, AB, LDAB, WORK )
         USE LA_PRECISION, ONLY: WP => DP
         REAL(WP) :: DLANGB
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDAB, N, KL, KU
         REAL(WP), INTENT(IN) :: AB( LDAB, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION DLANGB

       END INTERFACE
       
       INTERFACE LA_TGSEN

      SUBROUTINE STGSEN( IJOB, WANTQ, WANTZ, SELECT, N, A, LDA, B, LDB, &
     &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, M, PL,   &
     &                   PR, DIF, WORK, LWORK, IWORK, LIWORK, INFO )
      USE LA_PRECISION, ONLY: WP => SP
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDQ, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
      REAL(WP), INTENT(OUT) :: PL, PR
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*), Z(LDZ,*)
      REAL(WP), INTENT(OUT) :: ALPHAI(*), ALPHAR(*), BETA(*), DIF(2),   &
     &                         WORK(LWORK)
      END SUBROUTINE STGSEN

      SUBROUTINE DTGSEN( IJOB, WANTQ, WANTZ, SELECT, N, A, LDA, B, LDB, &
     &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, M, PL,   &
     &                   PR, DIF, WORK, LWORK, IWORK, LIWORK, INFO )
      USE LA_PRECISION, ONLY: WP => DP
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDQ, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
      REAL(WP), INTENT(OUT) :: PL, PR
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*), Z(LDZ,*)
      REAL(WP), INTENT(OUT) :: ALPHAI(*), ALPHAR(*), BETA(*), DIF(2),   &
     &                         WORK(LWORK)
      END SUBROUTINE DTGSEN


       END INTERFACE

       
       INTERFACE LA_TGSNA

      SUBROUTINE STGSNA( JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,    &
     &                   LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,    &
     &                   IWORK, INFO )
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
      INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, LWORK, MM, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(OUT) :: DIF(*), S(*)
      REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), VL(LDVL,*),           &
     &                        VR(LDVR,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE STGSNA

      SUBROUTINE DTGSNA( JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,    &
     &                   LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,    &
     &                   IWORK, INFO )
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
      INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, LWORK, MM, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(OUT) :: DIF(*), S(*)
      REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), VL(LDVL,*),           &
     &                        VR(LDVR,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DTGSNA

       END INTERFACE
       
       INTERFACE LA_TGSYL

      SUBROUTINE STGSYL( TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,  &
     &                   LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,  &
     &                   IWORK, INFO )
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDC, LDD, LDE, LDF, LWORK, &
     &                       M, N
      INTEGER, INTENT(OUT) :: INFO, IWORK(*)
      REAL(WP), INTENT(OUT) :: DIF, SCALE
      REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), D(LDD,*), E(LDF,*)
      REAL(WP), INTENT(INOUT) :: C(LDC,*), F(LDF,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE STGSYL

      SUBROUTINE DTGSYL( TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,  &
     &                   LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,  &
     &                   IWORK, INFO )
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDC, LDD, LDE, LDF, LWORK, &
     &                       M, N
      INTEGER, INTENT(OUT) :: INFO, IWORK(*)
      REAL(WP), INTENT(OUT) :: DIF, SCALE
      REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), D(LDD,*), E(LDF,*)
      REAL(WP), INTENT(INOUT) :: C(LDC,*), F(LDF,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DTGSYL

       END INTERFACE
       
       INTERFACE LA_TGEXC

         SUBROUTINE STGEXC( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, &
     &                      LDZ, IFST, ILST, WORK, LWORK, INFO )
      USE LA_PRECISION, ONLY: WP => SP
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDZ, LWORK, N
      INTEGER, INTENT(INOUT) :: IFST, ILST
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*), Z(LDZ,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE STGEXC

         SUBROUTINE DTGEXC( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, &
     &                      LDZ, IFST, ILST, WORK, LWORK, INFO )
      USE LA_PRECISION, ONLY: WP => DP
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDZ, LWORK, N
      INTEGER, INTENT(INOUT) :: IFST, ILST
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*), Z(LDZ,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DTGEXC


       END INTERFACE

       INTERFACE LA_BDSDC

         SUBROUTINE SBDSDC( UPLO, COMPQ, N, D, E, U, LDU, VT, LDVT, Q,  &
     &                      IQ, WORK, IWORK, INFO )
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: COMPQ, UPLO
      INTEGER, INTENT(IN) :: LDU, LDVT, N
      INTEGER, INTENT(OUT) :: INFO, IQ( * ), IWORK( * )
      REAL(WP), INTENT(INOUT) :: D( * ), E( * )
      REAL(WP), INTENT(OUT) :: Q(*), U(LDU,*), VT(LDVT,*), WORK(*)
      END SUBROUTINE SBDSDC

         SUBROUTINE DBDSDC( UPLO, COMPQ, N, D, E, U, LDU, VT, LDVT, Q,  &
     &                      IQ, WORK, IWORK, INFO )
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: COMPQ, UPLO
      INTEGER, INTENT(IN) :: LDU, LDVT, N
      INTEGER, INTENT(OUT) :: INFO, IQ( * ), IWORK( * )
      REAL(WP), INTENT(INOUT) :: D( * ), E( * )
      REAL(WP), INTENT(OUT) :: Q(*), U(LDU,*), VT(LDVT,*), WORK(*)
      END SUBROUTINE DBDSDC

       END INTERFACE

       INTERFACE LA_STEGR

         SUBROUTINE SSTEGR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU,       &
     &                      ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,  &
     &                      IWORK, LIWORK, INFO )
      USE LA_PRECISION, ONLY: WP => SP  
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
      INTEGER, INTENT(IN) :: IL, IU, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M
      INTEGER, INTENT(OUT) :: ISUPPZ( * ), IWORK(LIWORK)
      REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
      REAL(WP), INTENT(INOUT) :: D( * ), E( * )
      REAL(WP), INTENT(IN) :: W( * )
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      REAL(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE SSTEGR
        
         SUBROUTINE DSTEGR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU,       &
     &                      ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,  &
     &                      IWORK, LIWORK, INFO )
      USE LA_PRECISION, ONLY: WP => DP  
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
      INTEGER, INTENT(IN) :: IL, IU, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M
      INTEGER, INTENT(OUT) :: ISUPPZ( * ), IWORK(LIWORK)
      REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
      REAL(WP), INTENT(INOUT) :: D( * ), E( * )
      REAL(WP), INTENT(IN) :: W( * )
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      REAL(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE DSTEGR
        
       END INTERFACE

       INTERFACE LA_ORMRZ

         SUBROUTINE SORMRZ( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C,    &
     &                      LDC, WORK, LWORK, INFO )
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
      INTEGER, INTENT(IN) :: K, L, LDA, LDC, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(IN) :: A( LDA, * ), TAU( * )
      REAL(WP), INTENT(INOUT) :: C( LDC, * )
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMRZ

         SUBROUTINE DORMRZ( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C,    &
     &                      LDC, WORK, LWORK, INFO )
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
      INTEGER, INTENT(IN) :: K, L, LDA, LDC, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(IN) :: A( LDA, * ), TAU( * )
      REAL(WP), INTENT(INOUT) :: C( LDC, * )
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMRZ

       END INTERFACE



       INTERFACE LA_TZRZF

         SUBROUTINE STZRZF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
      USE LA_PRECISION, ONLY: WP => SP
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: A( LDA, * )
      REAL(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      END SUBROUTINE STZRZF

         SUBROUTINE DTZRZF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
      USE LA_PRECISION, ONLY: WP => DP
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: A( LDA, * )
      REAL(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      END SUBROUTINE DTZRZF

       END INTERFACE

       INTERFACE LA_GEQP3

         SUBROUTINE SGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK,       &
     &                      INFO )
      USE LA_PRECISION, ONLY: WP => SP
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(INOUT) :: JPVT( * )
      REAL(WP), INTENT(INOUT) :: A( LDA, * )
      REAL(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      END SUBROUTINE SGEQP3

         SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK,       &
     &                      INFO )
      USE LA_PRECISION, ONLY: WP => DP
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(INOUT) :: JPVT( * )
      REAL(WP), INTENT(INOUT) :: A( LDA, * )
      REAL(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      END SUBROUTINE DGEQP3


       END INTERFACE

       INTERFACE LA_GESDD


         SUBROUTINE SGESDD( JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT,    &
     &                      WORK, LWORK, IWORK, INFO )
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ
      INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: S(*)
      REAL(WP), INTENT(INOUT) :: A(LDA,*)
      REAL(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      INTEGER :: IWORK(*)
      END SUBROUTINE SGESDD


         SUBROUTINE DGESDD( JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT,    &
     &                      WORK, LWORK, IWORK, INFO )
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ
      INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: S(*)
      REAL(WP), INTENT(INOUT) :: A(LDA,*)
      REAL(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      INTEGER :: IWORK(*)
      END SUBROUTINE DGESDD


      END INTERFACE       


      INTERFACE LA_GGRQF

      SUBROUTINE SGGRQF( M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE SGGRQF

      SUBROUTINE DGGRQF( M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE DGGRQF

      END INTERFACE

      INTERFACE LA_GGQRF

      SUBROUTINE SGGQRF( N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE SGGQRF

      SUBROUTINE DGGQRF( N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE DGGQRF

      END INTERFACE

      INTERFACE LA_DISNA

      SUBROUTINE SDISNA( JOB, M, N, D, SEP, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(OUT) :: SEP(*)
      END SUBROUTINE SDISNA

      SUBROUTINE DDISNA( JOB, M, N, D, SEP, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(OUT) :: SEP(*)
      END SUBROUTINE DDISNA

      END INTERFACE

      INTERFACE LA_TGSJA

      SUBROUTINE STGSJA( JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B,    &
     &                   LDB, TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV,  &
     &                   Q, LDQ, WORK, NCYCLE, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: K, L, LDA, LDB, LDQ, LDU, LDV, M, N,    &
     &                          NCYCLE, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              U(LDU,*), V(LDV,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STGSJA

      SUBROUTINE DTGSJA( JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B,    &
     &                   LDB, TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV,  &
     &                   Q, LDQ, WORK, NCYCLE, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: K, L, LDA, LDB, LDQ, LDU, LDV, M, N,    &
     &                          NCYCLE, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              U(LDU,*), V(LDV,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTGSJA

      END INTERFACE

      INTERFACE LA_GGSVP

      SUBROUTINE SGGSVP( JOBU, JOBV, JOBQ, M, P, N, A, LDA, B, LDB,     &
     &                   TOLA, TOLB, K, L, U, LDU, V, LDV, Q, LDQ,      &
     &                   IWORK, TAU, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDU, LDV, M, N, P
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), TAU(*), U(LDU,*), V(LDV,*), &
     &                            WORK(*)
      END SUBROUTINE SGGSVP

      SUBROUTINE DGGSVP( JOBU, JOBV, JOBQ, M, P, N, A, LDA, B, LDB,     &
     &                   TOLA, TOLB, K, L, U, LDU, V, LDV, Q, LDQ,      &
     &                   IWORK, TAU, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDU, LDV, M, N, P
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), TAU(*), U(LDU,*), V(LDV,*), &
     &                            WORK(*)
      END SUBROUTINE DGGSVP

      END INTERFACE

      INTERFACE LA_TGEVC

      SUBROUTINE STGEVC( SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,   &
     &                   LDVL, VR, LDVR, MM, M, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STGEVC

      SUBROUTINE DTGEVC( SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,   &
     &                   LDVL, VR, LDVR, MM, M, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTGEVC

      END INTERFACE

      INTERFACE LA_HGEQZ

      SUBROUTINE SHGEQZ( JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,&
     &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, WORK,    &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*), WORK(LWORK)
      END SUBROUTINE SHGEQZ

      SUBROUTINE DHGEQZ( JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,&
     &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, WORK,    &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*), WORK(LWORK)
      END SUBROUTINE DHGEQZ

      END INTERFACE

      INTERFACE LA_GGBAK

      SUBROUTINE SGGBAK( JOB, SIDE, N, ILO, IHI, LSCALE, RSCALE, M, V,  &
     &                   LDV, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: LSCALE(*), RSCALE(*)
         REAL(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE SGGBAK

      SUBROUTINE DGGBAK( JOB, SIDE, N, ILO, IHI, LSCALE, RSCALE, M, V,  &
     &                   LDV, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: LSCALE(*), RSCALE(*)
         REAL(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE DGGBAK

      END INTERFACE

      INTERFACE LA_GGBAL

      SUBROUTINE SGGBAL( JOB, N, A, LDA, B, LDB, ILO, IHI, LSCALE,      &
     &                   RSCALE, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, LDB, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: LSCALE(*), RSCALE(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE SGGBAL

      SUBROUTINE DGGBAL( JOB, N, A, LDA, B, LDB, ILO, IHI, LSCALE,      &
     &                   RSCALE, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, LDB, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: LSCALE(*), RSCALE(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE DGGBAL

      END INTERFACE

      INTERFACE LA_GGHRD

      SUBROUTINE SGGHRD( COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q,  &
     &                   LDQ, Z, LDZ, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              Z(LDZ,*)
      END SUBROUTINE SGGHRD

      SUBROUTINE DGGHRD( COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q,  &
     &                   LDQ, Z, LDZ, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              Z(LDZ,*)
      END SUBROUTINE DGGHRD

      END INTERFACE

      INTERFACE LA_PBSTF

      SUBROUTINE SPBSTF( UPLO, N, KD, AB, LDAB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) ::UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB( LDAB, * )
      END SUBROUTINE SPBSTF

      SUBROUTINE DPBSTF( UPLO, N, KD, AB, LDAB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) ::UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB( LDAB, * )
      END SUBROUTINE DPBSTF

      END INTERFACE

      INTERFACE LA_SBGST

      SUBROUTINE SSBGST( VECT, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, X,  &
     &                   LDX, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, LDX, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: BB(LDBB,*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(LDX,*)
      END SUBROUTINE SSBGST

      SUBROUTINE DSBGST( VECT, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, X,  &
     &                   LDX, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, LDX, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: BB(LDBB,*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(LDX,*)
      END SUBROUTINE DSBGST

      END INTERFACE


      INTERFACE LA_SPGST

      SUBROUTINE SSPGST( ITYPE, UPLO, N, AP, BP, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: BP(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE SSPGST

      SUBROUTINE DSPGST( ITYPE, UPLO, N, AP, BP, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: BP(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE DSPGST

      END INTERFACE


      INTERFACE LA_BDSQR

      SUBROUTINE SBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,    &
     &                   LDU, C, LDC, RWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDC, LDU, LDVT, N, NCC, NCVT, NRU
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(INOUT) :: D(*), E(*)
         REAL, INTENT(OUT) :: RWORK(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*), U(LDU,*), VT(LDVT,*)
      END SUBROUTINE SBDSQR

      SUBROUTINE DBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,    &
     &                   LDU, C, LDC, RWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDC, LDU, LDVT, N, NCC, NCVT, NRU
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(INOUT) :: D(*), E(*)
         REAL, INTENT(OUT) :: RWORK(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*), U(LDU,*), VT(LDVT,*)
      END SUBROUTINE DBDSQR

      END INTERFACE

      INTERFACE LA_ORMBR

      SUBROUTINE SORMBR( VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,    &
     &                   LDC, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, VECT
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMBR

      SUBROUTINE DORMBR( VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,    &
     &                   LDC, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, VECT
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMBR

      END INTERFACE


      INTERFACE LA_ORGBR

      SUBROUTINE SORGBR( VECT, M, N, K, A, LDA, TAU, WORK, LWORK,       &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGBR

      SUBROUTINE DORGBR( VECT, M, N, K, A, LDA, TAU, WORK, LWORK,       &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGBR

      END INTERFACE


      INTERFACE LA_GBBRD

      SUBROUTINE SGBBRD( VECT, M, N, NCC, KL, KU, AB, LDAB, D, E, Q,    &
     &                   LDQ, PT, LDPT, C, LDC, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDC, LDPT, LDQ, M, N, NCC
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), C(LDC,*)
         REAL(WP), INTENT(OUT) :: PT(LDPT,*), Q(LDQ,*), WORK(*)
      END SUBROUTINE SGBBRD

      SUBROUTINE DGBBRD( VECT, M, N, NCC, KL, KU, AB, LDAB, D, E, Q,    &
     &                   LDQ, PT, LDPT, C, LDC, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDC, LDPT, LDQ, M, N, NCC
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), C(LDC,*)
         REAL(WP), INTENT(OUT) :: PT(LDPT,*), Q(LDQ,*), WORK(*)
      END SUBROUTINE DGBBRD

      END INTERFACE

      INTERFACE LA_GEBRD

      SUBROUTINE SGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,   &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAUP(*), TAUQ(*), WORK(LWORK)
      END SUBROUTINE SGEBRD

      SUBROUTINE DGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,   &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAUP(*), TAUQ(*), WORK(LWORK)
      END SUBROUTINE DGEBRD

      END INTERFACE

      INTERFACE LA_TRSEN

      SUBROUTINE STRSEN( JOB, COMPQ, SELECT, N, T, LDT, Q, LDQ, WR, WI, &
     &                   M, S, SEP, WORK, LWORK, IWORK, LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, JOB
         INTEGER, INTENT(IN) :: LDQ, LDT, LWORK, N, LIWORK
         INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
         REAL(WP), INTENT(OUT) :: S, SEP
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*)
         REAL(WP), INTENT(IN) :: WR(*), WI(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE STRSEN

      SUBROUTINE DTRSEN( JOB, COMPQ, SELECT, N, T, LDT, Q, LDQ, WR, WI, &
     &                   M, S, SEP, WORK, LWORK, IWORK, LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, JOB
         INTEGER, INTENT(IN) :: LDQ, LDT, LWORK, N, LIWORK
         INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
         REAL(WP), INTENT(OUT) :: S, SEP
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*)
         REAL(WP), INTENT(IN) :: WR(*), WI(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DTRSEN

      END INTERFACE

      INTERFACE LA_TRSNA

      SUBROUTINE STRSNA( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,  &
     &                   LDVR, S, SEP, MM, M, WORK, LDWORK, IWORK,      &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, LDWORK, MM, N
         INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: S(*), SEP(*)
         REAL(WP), INTENT(IN) :: T(LDT,*), VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(LDWORK,*)
      END SUBROUTINE STRSNA

      SUBROUTINE DTRSNA( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,  &
     &                   LDVR, S, SEP, MM, M, WORK, LDWORK, IWORK,      &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, LDWORK, MM, N
         INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: S(*), SEP(*)
         REAL(WP), INTENT(IN) :: T(LDT,*), VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(LDWORK,*)
      END SUBROUTINE DTRSNA

      END INTERFACE

      INTERFACE LA_TRSYL

      SUBROUTINE STRSYL( TRANA, TRANB, ISGN, M, N, A, LDA, B, LDB, C,   &
     &                   LDC, SCALE, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANA, TRANB
         INTEGER, INTENT(IN) :: ISGN, LDA, LDB, LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: SCALE
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE STRSYL

      SUBROUTINE DTRSYL( TRANA, TRANB, ISGN, M, N, A, LDA, B, LDB, C,   &
     &                   LDC, SCALE, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANA, TRANB
         INTEGER, INTENT(IN) :: ISGN, LDA, LDB, LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: SCALE
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE DTRSYL

      END INTERFACE

      INTERFACE LA_TREXC

      SUBROUTINE STREXC( COMPQ, N, T, LDT, Q, LDQ, IFST, ILST, WORK,    &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ
         INTEGER, INTENT(IN) :: IFST, ILST, LDQ, LDT, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*), WORK(*)
      END SUBROUTINE STREXC

      SUBROUTINE DTREXC( COMPQ, N, T, LDT, Q, LDQ, IFST, ILST, WORK,    &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ
         INTEGER, INTENT(IN) :: IFST, ILST, LDQ, LDT, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*), WORK(*)
      END SUBROUTINE DTREXC

      END INTERFACE

      INTERFACE LA_TREVC

      SUBROUTINE STREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR, &
     &                   LDVR, MM, M, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(INOUT) :: SELECT(*)
         REAL(WP), INTENT(IN) :: T(LDT,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STREVC

      SUBROUTINE DTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR, &
     &                   LDVR, MM, M, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(INOUT) :: SELECT(*)
         REAL(WP), INTENT(IN) :: T(LDT,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTREVC

      END INTERFACE

      INTERFACE LA_HSEIN

      SUBROUTINE SHSEIN( SIDE, EIGSRC, INITV, SELECT, N, H, LDH, WR, WI,&
     &                   VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL,       &
     &                   IFAILR, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: EIGSRC, INITV, SIDE
         INTEGER, INTENT(IN) :: LDH, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M, IFAILL(*), IFAILR(*)
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(INOUT) :: WR(*), WI(*)
         REAL(WP), INTENT(IN) :: H(LDH,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SHSEIN

      SUBROUTINE DHSEIN( SIDE, EIGSRC, INITV, SELECT, N, H, LDH, WR, WI,&
     &                   VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL,       &
     &                   IFAILR, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: EIGSRC, INITV, SIDE
         INTEGER, INTENT(IN) :: LDH, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M, IFAILL(*), IFAILR(*)
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(INOUT) :: WR(*), WI(*)
         REAL(WP), INTENT(IN) :: H(LDH,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DHSEIN

      END INTERFACE

      INTERFACE LA_HSEQR

      SUBROUTINE SHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, WR, WI, Z,    &
     &                   LDZ, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDH, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: WR(*), WI(*)
         REAL(WP), INTENT(INOUT) :: H(LDH,*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SHSEQR

      SUBROUTINE DHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, WR, WI, Z,    &
     &                   LDZ, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDH, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: WR(*), WI(*)
         REAL(WP), INTENT(INOUT) :: H(LDH,*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DHSEQR

      END INTERFACE

      INTERFACE LA_ORMHR

      SUBROUTINE SORMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,   &
     &                   LDC, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1),  INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMHR

      SUBROUTINE DORMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,   &
     &                   LDC, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1),  INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMHR

      END INTERFACE


      INTERFACE LA_ORGHR

      SUBROUTINE SORGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGHR

      SUBROUTINE DORGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGHR

      END INTERFACE


      INTERFACE LA_GEBAK

      SUBROUTINE SGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,      &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: SCALE(*)
         REAL(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE SGEBAK

      SUBROUTINE DGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,      &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: SCALE(*)
         REAL(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE DGEBAK

      END INTERFACE

      INTERFACE LA_GEBAL

      SUBROUTINE SGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: SCALE(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE SGEBAL

      SUBROUTINE DGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: SCALE(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE DGEBAL

      END INTERFACE

      INTERFACE LA_GEHRD

      SUBROUTINE SGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SGEHRD

      SUBROUTINE DGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DGEHRD

      END INTERFACE

      INTERFACE LA_PTEQR

      SUBROUTINE SPTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: INFO, LDZ, N
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE SPTEQR

      SUBROUTINE DPTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: INFO, LDZ, N
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE DPTEQR

      END INTERFACE

      INTERFACE LA_STEIN

      SUBROUTINE SSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,   &
     &                   IWORK, IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDZ, M, N, IBLOCK(*), ISPLIT(*)
         INTEGER, INTENT(OUT) :: INFO, IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: D(*), E(*), W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE SSTEIN

      SUBROUTINE DSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,   &
     &                   IWORK, IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDZ, M, N, IBLOCK(*), ISPLIT(*)
         INTEGER, INTENT(OUT) :: INFO, IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: D(*), E(*), W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE DSTEIN

      END INTERFACE

      INTERFACE LA_STEBZ

      SUBROUTINE SSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E, &
     &                   M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK,     &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: ORDER, RANGE
         INTEGER, INTENT(IN) :: IL, IU, M, N
         INTEGER, INTENT(OUT) :: INFO, NSPLIT, IBLOCK(*), ISPLIT(*),    &
     &                           IWORK(*) 
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU, D(*), E(*)
         REAL(WP), INTENT(OUT) :: W(*), WORK(*)
      END SUBROUTINE SSTEBZ

      SUBROUTINE DSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E, &
     &                   M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK,     &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: ORDER, RANGE
         INTEGER, INTENT(IN) :: IL, IU, M, N
         INTEGER, INTENT(OUT) :: INFO, NSPLIT, IBLOCK(*), ISPLIT(*),    &
     &                           IWORK(*) 
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU, D(*), E(*)
         REAL(WP), INTENT(OUT) :: W(*), WORK(*)
      END SUBROUTINE DSTEBZ

      END INTERFACE

      INTERFACE LA_STEDC

      SUBROUTINE SSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
     &                   LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, LIWORK, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(LIWORK)
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SSTEDC

      SUBROUTINE DSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
     &                   LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, LIWORK, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(LIWORK)
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DSTEDC

      END INTERFACE

      INTERFACE LA_STERF

      SUBROUTINE SSTERF( N, D, E, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
      END SUBROUTINE SSTERF

      SUBROUTINE DSTERF( N, D, E, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
      END SUBROUTINE DSTERF

      END INTERFACE

      INTERFACE LA_STEQR

      SUBROUTINE SSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE SSTEQR

      SUBROUTINE DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE DSTEQR

      END INTERFACE

      INTERFACE LA_OPMTR

      SUBROUTINE SOPMTR( SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,&
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SOPMTR

      SUBROUTINE DOPMTR( SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,&
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DOPMTR

      END INTERFACE


      INTERFACE LA_OPGTR

      SUBROUTINE SOPGTR( UPLO, N, AP, TAU, Q, LDQ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*), TAU(*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), WORK(*)
      END SUBROUTINE SOPGTR

      SUBROUTINE DOPGTR( UPLO, N, AP, TAU, Q, LDQ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*), TAU(*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), WORK(*)
      END SUBROUTINE DOPGTR

      END INTERFACE


      INTERFACE LA_ORMTR

      SUBROUTINE SORMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,  &
     &                   WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE SORMTR

      SUBROUTINE DORMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,  &
     &                   WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE DORMTR

      END INTERFACE


      INTERFACE LA_SBTRD

      SUBROUTINE SSBTRD( VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ,     &
     &                   WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KD, LDAB, LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), Q(LDQ,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSBTRD

      SUBROUTINE DSBTRD( VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ,     &
     &                   WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KD, LDAB, LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), Q(LDQ,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSBTRD

      END INTERFACE


      INTERFACE LA_SPTRD

      SUBROUTINE SSPTRD( UPLO, N, AP, D, E, TAU, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: TAU(*)
      END SUBROUTINE SSPTRD

      SUBROUTINE DSPTRD( UPLO, N, AP, D, E, TAU, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: TAU(*)
      END SUBROUTINE DSPTRD

      END INTERFACE


      INTERFACE LA_TZRQF

      SUBROUTINE STZRQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE STZRQF

      SUBROUTINE DTZRQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DTZRQF

      END INTERFACE

      INTERFACE LA_ORMRQ

      SUBROUTINE SORMRQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMRQ

      SUBROUTINE DORMRQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMRQ

      END INTERFACE


      INTERFACE LA_ORGRQ

      SUBROUTINE SORGRQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGRQ

      SUBROUTINE DORGRQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGRQ

      END INTERFACE


      INTERFACE LA_GERQF

      SUBROUTINE SGERQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SGERQF

      SUBROUTINE DGERQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DGERQF

      END INTERFACE

      INTERFACE LA_ORMQL

      SUBROUTINE SORMQL( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMQL

      SUBROUTINE DORMQL( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMQL

      END INTERFACE


      INTERFACE LA_ORGQL

      SUBROUTINE SORGQL( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGQL

      SUBROUTINE DORGQL( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGQL

      END INTERFACE


      INTERFACE LA_GEQLF

      SUBROUTINE SGEQLF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SGEQLF

      SUBROUTINE DGEQLF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DGEQLF

      END INTERFACE

      INTERFACE LA_ORMLQ

      SUBROUTINE SORMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMLQ

      SUBROUTINE DORMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMLQ

      END INTERFACE


      INTERFACE LA_ORGLQ

      SUBROUTINE SORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGLQ

      SUBROUTINE DORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGLQ

      END INTERFACE


      INTERFACE LA_GELQF

      SUBROUTINE SGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SGELQF

      SUBROUTINE DGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DGELQF

      END INTERFACE

      INTERFACE LA_ORMQR

      SUBROUTINE SORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMQR

      SUBROUTINE DORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMQR

      END INTERFACE


      INTERFACE LA_ORGQR

      SUBROUTINE SORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGQR

      SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGQR

      END INTERFACE


      INTERFACE LA_GEQRF

      SUBROUTINE SGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SGEQRF

      SUBROUTINE DGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DGEQRF

      END INTERFACE

      INTERFACE LA_GEQPF

      SUBROUTINE SGEQPF( M, N, A, LDA, JPVT, TAU, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(*)
      END SUBROUTINE SGEQPF

      SUBROUTINE DGEQPF( M, N, A, LDA, JPVT, TAU, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(*)
      END SUBROUTINE DGEQPF

      END INTERFACE

      INTERFACE LA_TBRFS

      SUBROUTINE STBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STBRFS

      SUBROUTINE DTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTBRFS

      MODULE PROCEDURE STBRFS1
      MODULE PROCEDURE DTBRFS1

      END INTERFACE

      INTERFACE LA_TBCON

      SUBROUTINE STBCON( NORM, UPLO, DIAG, N, KD, AB, LDAB, RCOND, WORK,&
     &                   IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STBCON

      SUBROUTINE DTBCON( NORM, UPLO, DIAG, N, KD, AB, LDAB, RCOND, WORK,&
     &                   IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTBCON

      END INTERFACE

      INTERFACE LA_TBTRS

      SUBROUTINE STBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE STBTRS

      SUBROUTINE DTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DTBTRS

      MODULE PROCEDURE STBTRS1
      MODULE PROCEDURE DTBTRS1

      END INTERFACE

      INTERFACE LA_TPTRI

      SUBROUTINE STPTRI( UPLO, DIAG, N, AP, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP( * )
      END SUBROUTINE STPTRI

      SUBROUTINE DTPTRI( UPLO, DIAG, N, AP, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP( * )
      END SUBROUTINE DTPTRI

      END INTERFACE

      INTERFACE LA_TPRFS

      SUBROUTINE STPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,&
     &                   FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STPRFS

      SUBROUTINE DTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,&
     &                   FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTPRFS

      MODULE PROCEDURE STPRFS1
      MODULE PROCEDURE DTPRFS1

      END INTERFACE

      INTERFACE LA_TPCON

      SUBROUTINE STPCON( NORM, UPLO, DIAG, N, AP, RCOND, WORK, IWORK,   &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STPCON

      SUBROUTINE DTPCON( NORM, UPLO, DIAG, N, AP, RCOND, WORK, IWORK,   &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTPCON

      END INTERFACE

      INTERFACE LA_TPTRS

      SUBROUTINE STPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE STPTRS

      SUBROUTINE DTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DTPTRS

      MODULE PROCEDURE STPTRS1
      MODULE PROCEDURE DTPTRS1

      END INTERFACE

      INTERFACE LA_TRTRI

      SUBROUTINE STRTRI( UPLO, DIAG, N, A, LDA, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A( LDA, * )
      END SUBROUTINE STRTRI

      SUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A( LDA, * )
      END SUBROUTINE DTRTRI

      END INTERFACE

      INTERFACE LA_TRRFS

      SUBROUTINE STRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X, &
     &                   LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(IN) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STRRFS

      SUBROUTINE DTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X, &
     &                   LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(IN) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTRRFS

      MODULE PROCEDURE STRRFS1
      MODULE PROCEDURE DTRRFS1

      END INTERFACE

      INTERFACE LA_TRCON

      SUBROUTINE STRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK,      &
     &                   IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STRCON

      SUBROUTINE DTRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK,      &
     &                   IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTRCON

      END INTERFACE

      INTERFACE LA_TRTRS

      SUBROUTINE STRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,    &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE STRTRS

      SUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,    &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DTRTRS

      MODULE PROCEDURE STRTRS1
      MODULE PROCEDURE DTRTRS1

      END INTERFACE

      INTERFACE LA_SPTRI

      SUBROUTINE SSPTRI( UPLO, N, AP, IPIV, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSPTRI

      SUBROUTINE DSPTRI( UPLO, N, AP, IPIV, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSPTRI

      END INTERFACE


      INTERFACE LA_SPRFS

      SUBROUTINE SSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,  &
     &                   FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSPRFS

      SUBROUTINE DSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,  &
     &                   FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSPRFS

      MODULE PROCEDURE SSPRFS1
      MODULE PROCEDURE DSPRFS1

      END INTERFACE



      INTERFACE LA_SPCON

      SUBROUTINE SSPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, IWORK,  &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSPCON

      SUBROUTINE DSPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, IWORK,  &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSPCON

      END INTERFACE

      INTERFACE LA_SPTRS

      SUBROUTINE SSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE SSPTRS

      SUBROUTINE DSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DSPTRS

      MODULE PROCEDURE SSPTRS1
      MODULE PROCEDURE DSPTRS1

      END INTERFACE



      INTERFACE LA_SPTRF

      SUBROUTINE SSPTRF( UPLO, N, AP, IPIV, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE SSPTRF

      SUBROUTINE DSPTRF( UPLO, N, AP, IPIV, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE DSPTRF

      END INTERFACE

      INTERFACE LA_SYTRI

      SUBROUTINE SSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYTRI

      SUBROUTINE DSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYTRI

      END INTERFACE


      INTERFACE LA_SYRFS

      SUBROUTINE SSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, &
     &                   X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*) 
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYRFS

      SUBROUTINE DSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, &
     &                   X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*) 
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYRFS

      MODULE PROCEDURE SSYRFS1
      MODULE PROCEDURE DSYRFS1

      END INTERFACE


      INTERFACE LA_SYCON

      SUBROUTINE SSYCON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK,     &
     &                   IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYCON

      SUBROUTINE DSYCON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK,     &
     &                   IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYCON

      END INTERFACE



      INTERFACE LA_SYTRS

      SUBROUTINE SSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SSYTRS

      SUBROUTINE DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DSYTRS

      MODULE PROCEDURE SSYTRS1
      MODULE PROCEDURE DSYTRS1

      END INTERFACE


      INTERFACE LA_SYTRF

      SUBROUTINE SSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK( LWORK )
      END SUBROUTINE SSYTRF

      SUBROUTINE DSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK( LWORK )
      END SUBROUTINE DSYTRF

      END INTERFACE

      INTERFACE LA_PTRFS

      SUBROUTINE SPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR,   &
     &                   BERR, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPTRFS

      SUBROUTINE DPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR,   &
     &                   BERR, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPTRFS

      MODULE PROCEDURE SPTRFS1
      MODULE PROCEDURE DPTRFS1

      END INTERFACE

      INTERFACE LA_PTCON

      SUBROUTINE SPTCON( N, D, E, ANORM, RCOND, RWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM, D(*)
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         REAL(WP), INTENT(IN) :: E(*)
      END SUBROUTINE SPTCON

      SUBROUTINE DPTCON( N, D, E, ANORM, RCOND, RWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM, D(*)
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         REAL(WP), INTENT(IN) :: E(*)
      END SUBROUTINE DPTCON

      END INTERFACE

      INTERFACE LA_PTTRS

      SUBROUTINE SPTTRS( N, NRHS, D, E, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SPTTRS

      SUBROUTINE DPTTRS( N, NRHS, D, E, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DPTTRS

      MODULE PROCEDURE SPTTRS1
      MODULE PROCEDURE DPTTRS1

      END INTERFACE

      INTERFACE LA_PTTRF

      SUBROUTINE SPTTRF( N, D, E, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D( * )
         REAL(WP), INTENT(INOUT) :: E( * )
      END SUBROUTINE SPTTRF

      SUBROUTINE DPTTRF( N, D, E, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D( * )
         REAL(WP), INTENT(INOUT) :: E( * )
      END SUBROUTINE DPTTRF

      END INTERFACE

      INTERFACE LA_PBEQU

      SUBROUTINE SPBEQU( UPLO, N, KD, AB, LDAB, S, SCOND, AMAX, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE SPBEQU

      SUBROUTINE DPBEQU( UPLO, N, KD, AB, LDAB, S, SCOND, AMAX, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE DPBEQU

      END INTERFACE

      INTERFACE LA_PBRFS

      SUBROUTINE SPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,    &
     &                   LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPBRFS

      SUBROUTINE DPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,    &
     &                   LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPBRFS

      MODULE PROCEDURE SPBRFS1
      MODULE PROCEDURE DPBRFS1

      END INTERFACE

      INTERFACE LA_PBCON

      SUBROUTINE SPBCON( UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK,     &
     &                   IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPBCON

      SUBROUTINE DPBCON( UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK,     &
     &                   IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPBCON

      END INTERFACE

      INTERFACE LA_PBTRS

      SUBROUTINE SPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SPBTRS

      SUBROUTINE DPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DPBTRS

      MODULE PROCEDURE SPBTRS1
      MODULE PROCEDURE DPBTRS1

      END INTERFACE

      INTERFACE LA_PBTRF

      SUBROUTINE SPBTRF( UPLO, N, KD, AB, LDAB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB( LDAB,*)
      END SUBROUTINE SPBTRF

      SUBROUTINE DPBTRF( UPLO, N, KD, AB, LDAB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB( LDAB,*)
      END SUBROUTINE DPBTRF

      END INTERFACE

      INTERFACE LA_PPEQU

      SUBROUTINE SPPEQU( UPLO, N, AP, S, SCOND, AMAX, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: AP(*)
      END SUBROUTINE SPPEQU

      SUBROUTINE DPPEQU( UPLO, N, AP, S, SCOND, AMAX, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: AP(*)
      END SUBROUTINE DPPEQU

      END INTERFACE

      INTERFACE LA_PPTRI

      SUBROUTINE SPPTRI( UPLO, N, AP, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE SPPTRI

      SUBROUTINE DPPTRI( UPLO, N, AP, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE DPPTRI

      END INTERFACE

      INTERFACE LA_PPRFS

      SUBROUTINE SPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR,  &
     &                   BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPPRFS

      SUBROUTINE DPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR,  &
     &                   BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPPRFS

      MODULE PROCEDURE SPPRFS1
      MODULE PROCEDURE DPPRFS1

      END INTERFACE

      INTERFACE LA_PPCON

      SUBROUTINE SPPCON( UPLO, N, AP, ANORM, RCOND, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPPCON

      SUBROUTINE DPPCON( UPLO, N, AP, ANORM, RCOND, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPPCON

      END INTERFACE

      INTERFACE LA_PPTRS

      SUBROUTINE SPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SPPTRS

      SUBROUTINE DPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DPPTRS

      MODULE PROCEDURE SPPTRS1
      MODULE PROCEDURE DPPTRS1

      END INTERFACE

      INTERFACE LA_PPTRF

      SUBROUTINE SPPTRF( UPLO, N, AP, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE SPPTRF

      SUBROUTINE DPPTRF( UPLO, N, AP, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE DPPTRF

      END INTERFACE

      INTERFACE LA_POEQU

      SUBROUTINE SPOEQU( N, A, LDA, S, SCOND, AMAX, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
      END SUBROUTINE SPOEQU

      SUBROUTINE DPOEQU( N, A, LDA, S, SCOND, AMAX, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
      END SUBROUTINE DPOEQU

      END INTERFACE

      INTERFACE LA_POTRI

      SUBROUTINE SPOTRI( UPLO, N, A, LDA, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
      END SUBROUTINE SPOTRI

      SUBROUTINE DPOTRI( UPLO, N, A, LDA, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
      END SUBROUTINE DPOTRI

      END INTERFACE

      INTERFACE LA_PORFS

      SUBROUTINE SPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,    &
     &                   LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPORFS

      SUBROUTINE DPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,    &
     &                   LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPORFS

      MODULE PROCEDURE SPORFS1
      MODULE PROCEDURE DPORFS1

      END INTERFACE

      INTERFACE LA_POTRS

      SUBROUTINE SPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SPOTRS

      SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DPOTRS

      MODULE PROCEDURE SPOTRS1
      MODULE PROCEDURE DPOTRS1

      END INTERFACE

      INTERFACE LA_GTRFS

      SUBROUTINE SGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*), DLF(*), &
     &                           DU(*), DU2(*), DUF(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SGTRFS

      SUBROUTINE DGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*), DLF(*), &
     &                           DU(*), DU2(*), DUF(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DGTRFS

      MODULE PROCEDURE SGTRFS1
      MODULE PROCEDURE DGTRFS1

      END INTERFACE

      INTERFACE LA_GTCON

      SUBROUTINE SGTCON( NORM, N, DL, D, DU, DU2, IPIV, ANORM, RCOND,   &
     &                   WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SGTCON

      SUBROUTINE DGTCON( NORM, N, DL, D, DU, DU2, IPIV, ANORM, RCOND,   &
     &                   WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DGTCON

      END INTERFACE

      INTERFACE LA_GTTRS

      SUBROUTINE SGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,  &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SGTTRS

      SUBROUTINE DGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,  &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DGTTRS

      MODULE PROCEDURE SGTTRS1
      MODULE PROCEDURE DGTTRS1

      END INTERFACE

      INTERFACE LA_GTTRF

      SUBROUTINE SGTTRF( N, DL, D, DU, DU2, IPIV, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: D(*), DL(*), DU(*)
         REAL(WP), INTENT(OUT) :: DU2(*)
      END SUBROUTINE SGTTRF

      SUBROUTINE DGTTRF( N, DL, D, DU, DU2, IPIV, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: D(*), DL(*), DU(*)
         REAL(WP), INTENT(OUT) :: DU2(*)
      END SUBROUTINE DGTTRF

      END INTERFACE

      INTERFACE LA_GBEQU

      SUBROUTINE SGBEQU( M, N, KL, KU, AB, LDAB, R, C, ROWCND, COLCND,  &
     &                   AMAX, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         REAL(WP), INTENT(OUT) :: C(*), R(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE SGBEQU

      SUBROUTINE DGBEQU( M, N, KL, KU, AB, LDAB, R, C, ROWCND, COLCND,  &
     &                   AMAX, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         REAL(WP), INTENT(OUT) :: C(*), R(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE DGBEQU

      END INTERFACE

      INTERFACE LA_GBRFS

      SUBROUTINE SGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*), B( LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
      END SUBROUTINE SGBRFS

      SUBROUTINE DGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*), B( LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
      END SUBROUTINE DGBRFS

      MODULE PROCEDURE SGBRFS1
      MODULE PROCEDURE DGBRFS1
      END INTERFACE

      INTERFACE LA_GBCON

      SUBROUTINE SGBCON( NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND, &
     &                   WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: KL, KU, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: AB( LDAB, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE SGBCON

      SUBROUTINE DGBCON( NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND, &
     &                   WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: KL, KU, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: AB( LDAB, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE DGBCON

      END INTERFACE

      INTERFACE LA_GBTRS

      SUBROUTINE SGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,&
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE SGBTRS

      SUBROUTINE DGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,&
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DGBTRS

      MODULE PROCEDURE SGBTRS1
      MODULE PROCEDURE DGBTRS1
      END INTERFACE

      INTERFACE LA_GBTRF

      SUBROUTINE SGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
      END SUBROUTINE SGBTRF

      SUBROUTINE DGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
      END SUBROUTINE DGBTRF

      END INTERFACE

      INTERFACE

      FUNCTION SLAMCH( CMACH )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP) :: SLAMCH
         CHARACTER(LEN=1), INTENT(IN) :: CMACH
      END FUNCTION SLAMCH

      FUNCTION DLAMCH( CMACH )
         USE LA_PRECISION, ONLY: WP => DP
         REAL(WP) :: DLAMCH
         CHARACTER(LEN=1), INTENT(IN) :: CMACH
      END FUNCTION DLAMCH

      END INTERFACE

      INTERFACE LA_GGSVD

       SUBROUTINE SGGSVD( JOBU, JOBV, JOBQ, M, N, P, K, L, A, LDA, B,   &
     &                    LDB, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ,     &
     &                    WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBV, JOBQ
         INTEGER, INTENT(IN) :: M, N, P, LDA, LDB, LDU, LDV, LDQ
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: U(LDU,*), V(LDV,*), Q(LDQ,*), WORK(*)
      END SUBROUTINE SGGSVD

       SUBROUTINE DGGSVD( JOBU, JOBV, JOBQ, M, N, P, K, L, A, LDA, B,   &
     &                    LDB, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ,     &
     &                    WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBV, JOBQ
         INTEGER, INTENT(IN) :: M, N, P, LDA, LDB, LDU, LDV, LDQ
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: U(LDU,*), V(LDV,*), Q(LDQ,*), WORK(*)
      END SUBROUTINE DGGSVD

       END INTERFACE

      INTERFACE LA_GEGV

       SUBROUTINE SGEGV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR,       &
     &                   ALPHAI, BETA, VL, LDVL, VR, LDVR, WORK, LWORK, &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*)
      END SUBROUTINE SGEGV

       SUBROUTINE DGEGV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR,       &
     &                   ALPHAI, BETA, VL, LDVL, VR, LDVR, WORK, LWORK, &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*)
      END SUBROUTINE DGEGV

       END INTERFACE

      INTERFACE LA_GEGS

       SUBROUTINE SGEGS( JOBVSL, JOBVSR, N, A, LDA, B, LDB, ALPHAR,     &
     &                   ALPHAI, BETA, VSL, LDVSL, VSR, LDVSR, WORK,    &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
      END SUBROUTINE SGEGS

       SUBROUTINE DGEGS( JOBVSL, JOBVSR, N, A, LDA, B, LDB, ALPHAR,     &
     &                   ALPHAI, BETA, VSL, LDVSL, VSR, LDVSR, WORK,    &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
      END SUBROUTINE DGEGS

       END INTERFACE

        INTERFACE LA_SBGVX
	

       SUBROUTINE SSBGVX( JOBZ, RANGE, UPLO, N, KAB, KBB, AB, LDAB, BB, &
     &                    LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z,&
     &                    LDZ, WORK, IWORK, IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, KAB, KBB, LDAB, LDBB, LDQ, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         REAL(WP), INTENT(OUT) :: WORK(*), Q(LDQ,*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE SSBGVX
	

       SUBROUTINE DSBGVX( JOBZ, RANGE, UPLO, N, KAB, KBB, AB, LDAB, BB, &
     &                    LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z,&
     &                    LDZ, WORK, IWORK, IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, KAB, KBB, LDAB, LDBB, LDQ, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         REAL(WP), INTENT(OUT) :: WORK(*), Q(LDQ,*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE DSBGVX
	
        END INTERFACE
	    

        INTERFACE LA_SBGVD
	

        SUBROUTINE SSBGVD( JOBZ, UPLO, N, KAB, KBB, AB, LDAB, BB, LDBB, &
     &                     W, Z, LDZ, WORK, LWORK, IWORK, LIWORK,       &
     &                     INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO 
           INTEGER, INTENT(IN) :: N, KAB, KBB, LDAB, LDBB, LDZ
           INTEGER, INTENT(IN) :: LWORK, LIWORK
           INTEGER, INTENT(OUT) :: INFO, IWORK(*)
           REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
           REAL(WP), INTENT(OUT) :: W(*)
           REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
	 END SUBROUTINE SSBGVD
	 

        SUBROUTINE DSBGVD( JOBZ, UPLO, N, KAB, KBB, AB, LDAB, BB, LDBB, &
     &                     W, Z, LDZ, WORK, LWORK, IWORK, LIWORK,       &
     &                     INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO 
           INTEGER, INTENT(IN) :: N, KAB, KBB, LDAB, LDBB, LDZ
           INTEGER, INTENT(IN) :: LWORK, LIWORK
           INTEGER, INTENT(OUT) :: INFO, IWORK(*)
           REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
           REAL(WP), INTENT(OUT) :: W(*)
           REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
	 END SUBROUTINE DSBGVD
	 
         END INTERFACE
		

      INTERFACE LA_SBGV

       SUBROUTINE SSBGV( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W,  &
     &                   Z, LDZ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE SSBGV

       SUBROUTINE DSBGV( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W,  &
     &                   Z, LDZ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE DSBGV

       END INTERFACE


       INTERFACE LA_SPGVX


       SUBROUTINE SSPGVX( ITYPE, JOBZ, RANGE, UPLO, N, AP, BP, VL, VU,  &
     &                    IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK,    &
     &                    IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE SSPGVX


       SUBROUTINE DSPGVX( ITYPE, JOBZ, RANGE, UPLO, N, AP, BP, VL, VU,  &
     &                    IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK,    &
     &                    IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE DSPGVX

        END INTERFACE
		

       INTERFACE LA_SPGVD
       

        SUBROUTINE SSPGVD( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ,     &
     &                     WORK, LWORK, IWORK, LIWORK, INFO )
        USE LA_PRECISION, ONLY: WP => SP
        CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
        INTEGER, INTENT(IN) :: ITYPE, N, LDZ
        INTEGER, INTENT(IN) :: LWORK, LIWORK
        INTEGER, INTENT(OUT) :: INFO, IWORK(*)
        REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
        REAL(WP), INTENT(OUT) :: W(*)
        REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
       END SUBROUTINE SSPGVD


        SUBROUTINE DSPGVD( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ,     &
     &                     WORK, LWORK, IWORK, LIWORK, INFO )
        USE LA_PRECISION, ONLY: WP => DP
        CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
        INTEGER, INTENT(IN) :: ITYPE, N, LDZ
        INTEGER, INTENT(IN) :: LWORK, LIWORK
        INTEGER, INTENT(OUT) :: INFO, IWORK(*)
        REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
        REAL(WP), INTENT(OUT) :: W(*)
        REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
       END SUBROUTINE DSPGVD

        END INTERFACE
		

      INTERFACE LA_SPGV

       SUBROUTINE SSPGV( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE SSPGV

       SUBROUTINE DSPGV( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE DSPGV

       END INTERFACE


      INTERFACE LA_GESVD

       SUBROUTINE SGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT,     &
     &                    LDVT, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBVT
         INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      END SUBROUTINE SGESVD

       SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT,     &
     &                    LDVT, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBVT
         INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      END SUBROUTINE DGESVD

       END INTERFACE

      INTERFACE LA_GEEVX

       SUBROUTINE SGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, WR,   &
     &                    WI, VL, LDVL, VR, LDVR, ILO, IHI, SCALE,      &
     &                    ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK,    &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO, ILO, IHI, IWORK(*)
         REAL(WP), INTENT(OUT) :: ABNRM
         REAL(WP), INTENT(OUT) :: SCALE(*), RCONDE(*), RCONDV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), WR(*), WI(*), &
     &                            WORK(*)
      END SUBROUTINE SGEEVX

       SUBROUTINE DGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, WR,   &
     &                    WI, VL, LDVL, VR, LDVR, ILO, IHI, SCALE,      &
     &                    ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK,    &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO, ILO, IHI, IWORK(*)
         REAL(WP), INTENT(OUT) :: ABNRM
         REAL(WP), INTENT(OUT) :: SCALE(*), RCONDE(*), RCONDV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), WR(*), WI(*), &
     &                            WORK(*)
      END SUBROUTINE DGEEVX

       END INTERFACE

        INTERFACE LA_GGEVX

       SUBROUTINE SGGEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, B,    &
     &                    LDB, ALPHAR, ALPHAI, BETA, VL, LDVL, VR, LDVR,&
     &                    ILO, IHI, LSCALE, RSCALE, ABNRM, BBNRM,       &
     &                    RCONDE, RCONDV, WORK, LWORK, IWORK, BWORK,    &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT):: ILO, IHI
         REAL(WP), INTENT(OUT) :: ABNRM, BBNRM
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*),      &
     &                            LSCALE(*), RSCALE(*), RCONDE(*),      &
     &                            RCONDV(*)
         INTEGER :: IWORK(*)
         LOGICAL :: BWORK(*) 
        END SUBROUTINE SGGEVX

       SUBROUTINE DGGEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, B,    &
     &                    LDB, ALPHAR, ALPHAI, BETA, VL, LDVL, VR, LDVR,&
     &                    ILO, IHI, LSCALE, RSCALE, ABNRM, BBNRM,       &
     &                    RCONDE, RCONDV, WORK, LWORK, IWORK, BWORK,    &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT):: ILO, IHI
         REAL(WP), INTENT(OUT) :: ABNRM, BBNRM
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*),      &
     &                            LSCALE(*), RSCALE(*), RCONDE(*),      &
     &                            RCONDV(*)
         INTEGER :: IWORK(*)
         LOGICAL :: BWORK(*) 
        END SUBROUTINE DGGEVX

        END INTERFACE

        INTERFACE LA_GGEV
      
       SUBROUTINE SGGEV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR,       &
     &                   ALPHAI, BETA, VL, LDVL, VR, LDVR, WORK, LWORK, &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*)
       END SUBROUTINE SGGEV

       SUBROUTINE DGGEV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR,       &
     &                   ALPHAI, BETA, VL, LDVL, VR, LDVR, WORK, LWORK, &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*)
       END SUBROUTINE DGGEV

       END INTERFACE

      INTERFACE LA_GEEV

       SUBROUTINE SGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR, &
     &                   LDVR, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), WR(*), WI(*), &
     &                            WORK(*)
      END SUBROUTINE SGEEV

       SUBROUTINE DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR, &
     &                   LDVR, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), WR(*), WI(*), &
     &                            WORK(*)
      END SUBROUTINE DGEEV

       END INTERFACE

      INTERFACE LA_GEESX

       SUBROUTINE SGEESX( JOBVS, SORT, SELECT, SENSE, N, A, LDA, SDIM,  &
     &                    WR, WI, VS, LDVS, RCONDE, RCONDV, WORK, LWORK,&
     &                    IWORK, LIWORK, BWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTERFACE
            LOGICAL FUNCTION SELECT(WR, WI)
               USE LA_PRECISION, ONLY: WP => SP
               REAL(WP), INTENT(IN) :: WR, WI
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM, IWORK(*)
         LOGICAL, INTENT(OUT) :: BWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: RCONDV, RCONDE
         REAL(WP), INTENT(OUT) :: VS(LDVS,*), WR(*), WI(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE SGEESX

       SUBROUTINE DGEESX( JOBVS, SORT, SELECT, SENSE, N, A, LDA, SDIM,  &
     &                    WR, WI, VS, LDVS, RCONDE, RCONDV, WORK, LWORK,&
     &                    IWORK, LIWORK, BWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTERFACE
            LOGICAL FUNCTION SELECT(WR, WI)
               USE LA_PRECISION, ONLY: WP => DP
               REAL(WP), INTENT(IN) :: WR, WI
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM, IWORK(*)
         LOGICAL, INTENT(OUT) :: BWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: RCONDV, RCONDE
         REAL(WP), INTENT(OUT) :: VS(LDVS,*), WR(*), WI(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE DGEESX

       END INTERFACE
       
       INTERFACE LA_GGESX

       SUBROUTINE SGGESX( JOBVSL, JOBVSR, SORT, SELCTG, SENSE, N, A,    &
     &                    LDA, B, LDB, SDIM, ALPHAR, ALPHAI, BETA, VSL, &
     &                    LDVSL, VSR, LDVSR, RCONDE, RCONDV, WORK,      &
     &                    LWORK, IWORK, LIWORK, BWORK, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT, SENSE
          INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK, LIWORK
          INTEGER, INTENT(INOUT) :: INFO
          INTEGER, INTENT(OUT) :: SDIM
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
          REAL(WP), INTENT(OUT) :: RCONDE(2), RCONDV(2)
          LOGICAL :: BWORK(*)
          INTEGER :: IWORK (*)
          INTERFACE
           LOGICAL FUNCTION SELCTG(ALPHAR, ALPHAI, BETA)
              USE LA_PRECISION, ONLY: WP => SP
              REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
           END FUNCTION SELCTG
          END INTERFACE
          OPTIONAL :: SELCTG
         END SUBROUTINE SGGESX
	 
       SUBROUTINE DGGESX( JOBVSL, JOBVSR, SORT, DELCTG, SENSE, N, A,    &
     &                    LDA, B, LDB, SDIM, ALPHAR, ALPHAI, BETA, VSL, &
     &                    LDVSL, VSR, LDVSR, RCONDE, RCONDV, WORK,      &
     &                    LWORK, IWORK, LIWORK, BWORK, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT, SENSE
          INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK, LIWORK
          INTEGER, INTENT(INOUT) :: INFO
          INTEGER, INTENT(OUT) :: SDIM
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
          REAL(WP), INTENT(OUT) :: RCONDE(2), RCONDV(2)
          LOGICAL :: BWORK(*)
          INTEGER :: IWORK (*)
          INTERFACE
           LOGICAL FUNCTION DELCTG(ALPHAR, ALPHAI, BETA)
              USE LA_PRECISION, ONLY: WP => DP
              REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
           END FUNCTION DELCTG
          END INTERFACE
          OPTIONAL :: DELCTG
         END SUBROUTINE DGGESX
	 
      END INTERFACE 

       
      INTERFACE LA_GGES
      
       SUBROUTINE SGGES( JOBVSL, JOBVSR, SORT, SELCTG, N, A, LDA, B,    &
     &                   LDB, SDIM, ALPHAR, ALPHAI, BETA, VSL, LDVSL,   &
     &                   VSR, LDVSR, WORK, LWORK, BWORK, INFO )
       USE LA_PRECISION, ONLY: WP => SP
       CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT
       INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
       INTEGER, INTENT(INOUT) :: INFO
       INTEGER, INTENT(OUT) :: SDIM
       REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
       LOGICAL :: BWORK(*)
       INTERFACE
         LOGICAL FUNCTION SELCTG(ALPHAR, ALPHAI, BETA)
          USE LA_PRECISION, ONLY: WP => SP
          REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
         END FUNCTION SELCTG
       END INTERFACE
       OPTIONAL :: SELCTG
      END SUBROUTINE SGGES
      
       SUBROUTINE DGGES( JOBVSL, JOBVSR, SORT, DELCTG, N, A, LDA, B,    &
     &                   LDB, SDIM, ALPHAR, ALPHAI, BETA, VSL, LDVSL,   &
     &                   VSR, LDVSR, WORK, LWORK, BWORK, INFO )
       USE LA_PRECISION, ONLY: WP => DP
       CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT
       INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
       INTEGER, INTENT(INOUT) :: INFO
       INTEGER, INTENT(OUT) :: SDIM
       REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
       LOGICAL :: BWORK(*)
       INTERFACE
         LOGICAL FUNCTION DELCTG(ALPHAR, ALPHAI, BETA)
          USE LA_PRECISION, ONLY: WP => DP
          REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
         END FUNCTION DELCTG
       END INTERFACE
       OPTIONAL :: DELCTG
      END SUBROUTINE DGGES
      
        END INTERFACE

      INTERFACE LA_GEES

       SUBROUTINE SGEES( JOBVS, SORT, SELECT, N, A, LDA, SDIM, WR, WI,  &
     &                   VS, LDVS, WORK, LWORK, BWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTERFACE
            LOGICAL FUNCTION SELECT(WR, WI)
               USE LA_PRECISION, ONLY: WP => SP
               REAL(WP), INTENT(IN) :: WR, WI
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM
         LOGICAL, INTENT(OUT) :: BWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VS(LDVS,*), WR(*), WI(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE SGEES

       SUBROUTINE DGEES( JOBVS, SORT, SELECT, N, A, LDA, SDIM, WR, WI,  &
     &                   VS, LDVS, WORK, LWORK, BWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTERFACE
            LOGICAL FUNCTION SELECT(WR, WI)
               USE LA_PRECISION, ONLY: WP => DP
               REAL(WP), INTENT(IN) :: WR, WI
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM
         LOGICAL, INTENT(OUT) :: BWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VS(LDVS,*), WR(*), WI(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE DGEES

       END INTERFACE

        INTERFACE LA_STEVR

       SUBROUTINE SSTEVR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
     &                    M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK,     &
     &                    LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
         INTEGER, INTENT(IN) :: N, IL, IU, LDZ,  LWORK, LIWORK
         INTEGER, INTENT(OUT) :: M
         INTEGER, INTENT(OUT), TARGET :: ISUPPZ(*)
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*), W(*)
         REAL(WP), INTENT(OUT), TARGET :: Z(LDZ,*)
       END SUBROUTINE SSTEVR

       SUBROUTINE DSTEVR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
     &                    M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK,     &
     &                    LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
         INTEGER, INTENT(IN) :: N, IL, IU, LDZ,  LWORK, LIWORK
         INTEGER, INTENT(OUT) :: M
         INTEGER, INTENT(OUT), TARGET :: ISUPPZ(*)
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*), W(*)
         REAL(WP), INTENT(OUT), TARGET :: Z(LDZ,*)
       END SUBROUTINE DSTEVR

       END INTERFACE

      INTERFACE LA_STEVX

       SUBROUTINE SSTEVX( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
     &                    M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSTEVX

       SUBROUTINE DSTEVX( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
     &                    M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSTEVX

       END INTERFACE

      INTERFACE LA_STEVD

       SUBROUTINE SSTEVD( JOBZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
     &                    LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ
         INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE SSTEVD

       SUBROUTINE DSTEVD( JOBZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
     &                    LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ
         INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE DSTEVD

       END INTERFACE

      INTERFACE LA_STEV

       SUBROUTINE SSTEV( JOBZ, N, D, E, Z, LDZ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE SSTEV

       SUBROUTINE DSTEV( JOBZ, N, D, E, Z, LDZ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE DSTEV

       END INTERFACE

      INTERFACE LA_SBEVX

       SUBROUTINE SSBEVX( JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ,   &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    IWORK, IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU, LDQ, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSBEVX

       SUBROUTINE DSBEVX( JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ,   &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    IWORK, IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU, LDQ, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSBEVX

       END INTERFACE


      INTERFACE LA_SBEVD

       SUBROUTINE SSBEVD( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
     &                    LWORK, IWORK, LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE SSBEVD

       SUBROUTINE DSBEVD( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
     &                    LWORK, IWORK, LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE DSBEVD

       END INTERFACE


      INTERFACE LA_SBEV

       SUBROUTINE SSBEV( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,  &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSBEV

       SUBROUTINE DSBEV( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,  &
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSBEV

       END INTERFACE


      INTERFACE LA_SPEVX

       SUBROUTINE SSPEVX( JOBZ, RANGE, UPLO, N, AP, VL, VU, IL, IU,     &
     &                    ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL,     &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSPEVX

       SUBROUTINE DSPEVX( JOBZ, RANGE, UPLO, N, AP, VL, VU, IL, IU,     &
     &                    ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL,     &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSPEVX

       END INTERFACE


      INTERFACE LA_SPEVD

       SUBROUTINE SSPEVD( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, LWORK,    &
     &                    IWORK, LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSPEVD

       SUBROUTINE DSPEVD( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, LWORK,    &
     &                    IWORK, LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSPEVD

       END INTERFACE


      INTERFACE LA_SPEV

       SUBROUTINE SSPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSPEV

       SUBROUTINE DSPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSPEV

       END INTERFACE


      INTERFACE LA_GGGLM

       SUBROUTINE SGGGLM( N, M, P, A, LDA, B, LDB, D, X, Y, WORK, LWORK,&
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), D(*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(*), Y(*)
      END SUBROUTINE SGGGLM

       SUBROUTINE DGGGLM( N, M, P, A, LDA, B, LDB, D, X, Y, WORK, LWORK,&
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), D(*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(*), Y(*)
      END SUBROUTINE DGGGLM

       END INTERFACE

      INTERFACE LA_GGLSE

       SUBROUTINE SGGLSE( M, N, P, A, LDA, B, LDB, C, D, X, WORK, LWORK,&
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), C(*), D(*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(*)
      END SUBROUTINE SGGLSE

       SUBROUTINE DGGLSE( M, N, P, A, LDA, B, LDB, C, D, X, WORK, LWORK,&
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), C(*), D(*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(*)
      END SUBROUTINE DGGLSE

       END INTERFACE

       INTERFACE LA_GELSY

       SUBROUTINE SGELSY(  M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) ::  WORK(*)
       END SUBROUTINE SGELSY

       SUBROUTINE DGELSY(  M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) ::  WORK(*)
       END SUBROUTINE DGELSY

        MODULE PROCEDURE SGELSY1
        MODULE PROCEDURE DGELSY1
			
        END INTERFACE 

        INTERFACE LA_GELSD

       SUBROUTINE SGELSD(  M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, IWORK, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) ::  A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) :: S(*)
          REAL(WP), INTENT(OUT) :: WORK(*)
          INTEGER :: IWORK(*) 
        END SUBROUTINE SGELSD

       SUBROUTINE DGELSD(  M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, IWORK, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) ::  A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) :: S(*)
          REAL(WP), INTENT(OUT) :: WORK(*)
          INTEGER :: IWORK(*) 
        END SUBROUTINE DGELSD

          MODULE PROCEDURE SGELSD1
          MODULE PROCEDURE DGELSD1
		  
       END INTERFACE

      INTERFACE LA_GELSX

       SUBROUTINE SGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,&
     &                    WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SGELSX

       SUBROUTINE DGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,&
     &                    WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DGELSX

      MODULE PROCEDURE SGELSX1
      MODULE PROCEDURE DGELSX1

       END INTERFACE

      INTERFACE LA_GELSS

       SUBROUTINE SGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,   &
     &                    WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SGELSS

       SUBROUTINE DGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,   &
     &                    WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DGELSS

      MODULE PROCEDURE SGELSS1
      MODULE PROCEDURE DGELSS1

       END INTERFACE

      INTERFACE LA_GELS

       SUBROUTINE SGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,&
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SGELS

       SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,&
     &                   INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DGELS

      MODULE PROCEDURE SGELS1
      MODULE PROCEDURE DGELS1

       END INTERFACE

      INTERFACE LA_SPSV

       SUBROUTINE SSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE SSPSV

       SUBROUTINE DSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE DSPSV

      MODULE PROCEDURE SSPSV1
      MODULE PROCEDURE DSPSV1

       END INTERFACE


      INTERFACE LA_SYSV

       SUBROUTINE SSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,     &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYSV

       SUBROUTINE DSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,     &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYSV

      MODULE PROCEDURE SSYSV1
      MODULE PROCEDURE DSYSV1

       END INTERFACE


      INTERFACE LA_PTSV

       SUBROUTINE SPTSV( N, NRHS, D, E, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         REAL(WP), INTENT(INOUT) :: E(*), B(LDB,*)
      END SUBROUTINE SPTSV

       SUBROUTINE DPTSV( N, NRHS, D, E, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         REAL(WP), INTENT(INOUT) :: E(*), B(LDB,*)
      END SUBROUTINE DPTSV

      MODULE PROCEDURE SPTSV1
      MODULE PROCEDURE DPTSV1

       END INTERFACE

      INTERFACE LA_PBSV

       SUBROUTINE SPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE SPBSV

       SUBROUTINE DPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE DPBSV

      MODULE PROCEDURE SPBSV1
      MODULE PROCEDURE DPBSV1

       END INTERFACE

      INTERFACE LA_PPSV

       SUBROUTINE SPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE SPPSV

       SUBROUTINE DPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE DPPSV

      MODULE PROCEDURE SPPSV1
      MODULE PROCEDURE DPPSV1

       END INTERFACE

      INTERFACE LA_POSV

       SUBROUTINE SPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE SPOSV

       SUBROUTINE DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE DPOSV

      MODULE PROCEDURE SPOSV1
      MODULE PROCEDURE DPOSV1

       END INTERFACE

      INTERFACE LA_GTSV

       SUBROUTINE SGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
      END SUBROUTINE SGTSV

       SUBROUTINE DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
      END SUBROUTINE DGTSV

      MODULE PROCEDURE SGTSV1
      MODULE PROCEDURE DGTSV1

       END INTERFACE

      INTERFACE LA_GBSV

       SUBROUTINE SGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE SGBSV

       SUBROUTINE DGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE DGBSV

      MODULE PROCEDURE SGBSV1
      MODULE PROCEDURE DGBSV1

       END INTERFACE

      INTERFACE LA_GESV

       SUBROUTINE SGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE SGESV

       SUBROUTINE DGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE DGESV

      MODULE PROCEDURE SGESV1
      MODULE PROCEDURE DGESV1

       END INTERFACE

      INTERFACE LA_SPSVX

       SUBROUTINE SSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X,  &
     &                    LDX, RCOND, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: A(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: AF(*)
      END SUBROUTINE SSPSVX

       SUBROUTINE DSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X,  &
     &                    LDX, RCOND, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: A(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: AF(*)
      END SUBROUTINE DSPSVX

      MODULE PROCEDURE SSPSVX1
      MODULE PROCEDURE DSPSVX1

       END INTERFACE


      INTERFACE LA_SYSVX

       SUBROUTINE SSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV,  &
     &                    B, LDB, X, LDX, RCOND, FERR, BERR, WORK,      &
     &                    LWORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
      END SUBROUTINE SSYSVX

       SUBROUTINE DSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV,  &
     &                    B, LDB, X, LDX, RCOND, FERR, BERR, WORK,      &
     &                    LWORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
      END SUBROUTINE DSYSVX

      MODULE PROCEDURE SSYSVX1
      MODULE PROCEDURE DSYSVX1

       END INTERFACE


      INTERFACE LA_PTSVX

       SUBROUTINE SPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,  &
     &                    RCOND, FERR, BERR, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         REAL(WP), INTENT(INOUT) :: EF(*)
      END SUBROUTINE SPTSVX

       SUBROUTINE DPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,  &
     &                    RCOND, FERR, BERR, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         REAL(WP), INTENT(INOUT) :: EF(*)
      END SUBROUTINE DPTSVX

      MODULE PROCEDURE SPTSVX1
      MODULE PROCEDURE DPTSVX1

       END INTERFACE

      INTERFACE LA_PBSVX

       SUBROUTINE SPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,&
     &                    EQUED, S, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                    WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*), FERR(*), BERR(*),  &
     &                            RCOND
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*), B(LDB,*), &
     &                              S(*)
      END SUBROUTINE SPBSVX

       SUBROUTINE DPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,&
     &                    EQUED, S, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                    WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*), FERR(*), BERR(*),  &
     &                            RCOND
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*), B(LDB,*), &
     &                              S(*)
      END SUBROUTINE DPBSVX

      MODULE PROCEDURE SPBSVX1
      MODULE PROCEDURE DPBSVX1

       END INTERFACE

      INTERFACE LA_PPSVX

       SUBROUTINE SPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,    &
     &                    LDB, X, LDX, RCOND, FERR, BERR, WORK, IWORK,  &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*), S(*)
      END SUBROUTINE SPPSVX

       SUBROUTINE DPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,    &
     &                    LDB, X, LDX, RCOND, FERR, BERR, WORK, IWORK,  &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*), S(*)
      END SUBROUTINE DPPSVX

      MODULE PROCEDURE SPPSVX1
      MODULE PROCEDURE DPPSVX1

       END INTERFACE

      INTERFACE LA_POSVX

       SUBROUTINE SPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, &
     &                    S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,   &
     &                    IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE SPOSVX

       SUBROUTINE DPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, &
     &                    S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,   &
     &                    IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE DPOSVX

      MODULE PROCEDURE SPOSVX1
      MODULE PROCEDURE DPOSVX1

       END INTERFACE

      INTERFACE LA_GTSVX

       SUBROUTINE SGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,&
     &                    DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR, &
     &                    WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
         REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
      END SUBROUTINE SGTSVX

       SUBROUTINE DGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,&
     &                    DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR, &
     &                    WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
         REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
      END SUBROUTINE DGTSVX

      MODULE PROCEDURE SGTSVX1
      MODULE PROCEDURE DGTSVX1

       END INTERFACE

      INTERFACE LA_GBSVX

       SUBROUTINE SGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE SGBSVX

       SUBROUTINE DGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE DGBSVX

      MODULE PROCEDURE SGBSVX1
      MODULE PROCEDURE DGBSVX1

       END INTERFACE

      INTERFACE LA_GESVX

       SUBROUTINE SGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE SGESVX

       SUBROUTINE DGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE DGESVX

      MODULE PROCEDURE SGESVX1
      MODULE PROCEDURE DGESVX1

      END INTERFACE

      INTERFACE LA_GETRF

       SUBROUTINE SGETRF( M, N, A, LDA, PIV, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT( OUT ) :: PIV( * )
         REAL(WP), INTENT( INOUT ) :: A( LDA, * )
      END SUBROUTINE SGETRF

       SUBROUTINE DGETRF( M, N, A, LDA, PIV, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT( OUT ) :: PIV( * )
         REAL(WP), INTENT( INOUT ) :: A( LDA, * )
      END SUBROUTINE DGETRF


       END INTERFACE

      INTERFACE LA_GETRS

       SUBROUTINE SGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE SGETRS

       SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DGETRS

      MODULE PROCEDURE SGETRS1
      MODULE PROCEDURE DGETRS1

       END INTERFACE

      INTERFACE LA_GETRI

       SUBROUTINE SGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE SGETRI

       SUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE DGETRI

       END INTERFACE

      INTERFACE LA_GERFS

       SUBROUTINE SGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
      END SUBROUTINE SGERFS

       SUBROUTINE DGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
      END SUBROUTINE DGERFS

      MODULE PROCEDURE SGERFS1
      MODULE PROCEDURE DGERFS1

       END INTERFACE

      INTERFACE LA_GEEQU

       SUBROUTINE SGEEQU( M, N, A, LDA, R, C, ROWCND, COLCND, AMAX,     &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: C( * ), R( * )
      END SUBROUTINE SGEEQU

       SUBROUTINE DGEEQU( M, N, A, LDA, R, C, ROWCND, COLCND, AMAX,     &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: C( * ), R( * )
      END SUBROUTINE DGEEQU

       END INTERFACE

      INTERFACE LA_LANGE

       FUNCTION SLANGE( NORM, M, N, A, LDA, WORK )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP) :: SLANGE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION SLANGE

       FUNCTION DLANGE( NORM, M, N, A, LDA, WORK )
         USE LA_PRECISION, ONLY: WP => DP
         REAL(WP) :: DLANGE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION DLANGE

      MODULE PROCEDURE SLANGE1
      MODULE PROCEDURE DLANGE1

       END INTERFACE

      INTERFACE LA_GECON

       SUBROUTINE SGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, IWORK,   &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE SGECON

       SUBROUTINE DGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, IWORK,   &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE DGECON

       END INTERFACE

      INTERFACE LA_SYEV

       SUBROUTINE SSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYEV

       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYEV

       END INTERFACE


      INTERFACE LA_SYEVD

       SUBROUTINE SSYEVD( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, IWORK, &
     &                    LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LIWORK, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYEVD

       SUBROUTINE DSYEVD( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, IWORK, &
     &                    LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LIWORK, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYEVD

       END INTERFACE


       INTERFACE LA_SYEVR

       SUBROUTINE SSYEVR( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,    &
     &                    IWORK, LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, LDZ, LDA, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: M
         INTEGER, INTENT(OUT) :: ISUPPZ(*)
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
       END SUBROUTINE  SSYEVR    

       SUBROUTINE DSYEVR( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,    &
     &                    IWORK, LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, LDZ, LDA, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: M
         INTEGER, INTENT(OUT) :: ISUPPZ(*)
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
       END SUBROUTINE  DSYEVR    

      END INTERFACE
      

      INTERFACE LA_SYEVX

       SUBROUTINE SSYEVX( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK,     &
     &                    IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: IL, IU, LDA, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, M
         INTEGER, INTENT(OUT) :: IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
      END SUBROUTINE SSYEVX

       SUBROUTINE DSYEVX( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK,     &
     &                    IFAIL, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: IL, IU, LDA, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, M
         INTEGER, INTENT(OUT) :: IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
      END SUBROUTINE DSYEVX

       END INTERFACE


      INTERFACE LA_SYGST

       SUBROUTINE SSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: B(LDB,*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE SSYGST

       SUBROUTINE DSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: B(LDB,*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE DSYGST

       END INTERFACE


      INTERFACE LA_SYGV

       SUBROUTINE SSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYGV

       SUBROUTINE DSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, &
     &                   LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYGV

       END INTERFACE


        INTERFACE LA_SYGVX
	

       SUBROUTINE SSYGVX( ITYPE, JOBZ, RANGE, UPLO, N, A, LDA, B, LDB,  &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    LWORK, IWORK, IFAIL, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
          INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: M
          REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
          INTEGER, INTENT(OUT) ::  IWORK(*)
          INTEGER, INTENT(OUT) :: INFO
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
          REAL(WP), INTENT(OUT) :: W(*)	
          INTEGER, INTENT(IN) :: IFAIL(*)
         END SUBROUTINE SSYGVX
   

       SUBROUTINE DSYGVX( ITYPE, JOBZ, RANGE, UPLO, N, A, LDA, B, LDB,  &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    LWORK, IWORK, IFAIL, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
          INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: M
          REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
          INTEGER, INTENT(OUT) ::  IWORK(*)
          INTEGER, INTENT(OUT) :: INFO
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
          REAL(WP), INTENT(OUT) :: W(*)	
          INTEGER, INTENT(IN) :: IFAIL(*)
         END SUBROUTINE DSYGVX
   
        END INTERFACE
		


       INTERFACE LA_SYGVD


       SUBROUTINE SSYGVD( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,&
     &                    LWORK, IWORK, LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, LDA, LDB, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
        END SUBROUTINE SSYGVD
	

       SUBROUTINE DSYGVD( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,&
     &                    LWORK, IWORK, LIWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, LDA, LDB, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
        END SUBROUTINE DSYGVD
	
        END INTERFACE

            
      INTERFACE LA_SYTRD

       SUBROUTINE SSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK,      &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SSYTRD

       SUBROUTINE DSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK,      &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DSYTRD

       END INTERFACE


      INTERFACE LA_ORGTR

       SUBROUTINE SORGTR( UPLO, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGTR

       SUBROUTINE DORGTR( UPLO, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGTR

       END INTERFACE


      INTERFACE LA_LANSY

      FUNCTION SLANSY( NORM, UPLO, N, A, LDA, WORK )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP) :: SLANSY
         CHARACTER(LEN=1), INTENT(IN) :: NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION SLANSY

      FUNCTION DLANSY( NORM, UPLO, N, A, LDA, WORK )
         USE LA_PRECISION, ONLY: WP => DP
         REAL(WP) :: DLANSY
         CHARACTER(LEN=1), INTENT(IN) :: NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION DLANSY

       END INTERFACE

      INTERFACE LA_POTRF

       SUBROUTINE SPOTRF( UPLO, N, A, LDA, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE SPOTRF

       SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE DPOTRF

       END INTERFACE

      INTERFACE LA_POCON

       SUBROUTINE SPOCON( UPLO, N, A, LDA, ANORM, RCOND, WORK, IWORK,   &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE SPOCON

       SUBROUTINE DPOCON( UPLO, N, A, LDA, ANORM, RCOND, WORK, IWORK,   &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE DPOCON

      END INTERFACE

      INTERFACE LA_ILAENV

      FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
         INTEGER :: ILAENV
         CHARACTER(LEN=*), INTENT(IN) :: NAME, OPTS
         INTEGER, INTENT(IN) :: ISPEC, N1, N2, N3, N4
      END FUNCTION ILAENV

      END INTERFACE

      INTERFACE LA_LAGGE

       SUBROUTINE SLAGGE( M, N, KL, KU, D, A, LDA, ISEED, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: M, N, KL, KU, LDA
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: ISEED(4)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(OUT) :: A(LDA,*), WORK(*)
      END SUBROUTINE SLAGGE

       SUBROUTINE DLAGGE( M, N, KL, KU, D, A, LDA, ISEED, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: M, N, KL, KU, LDA
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: ISEED(4)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(OUT) :: A(LDA,*), WORK(*)
      END SUBROUTINE DLAGGE

      END INTERFACE

      CONTAINS

      SUBROUTINE SGESV1( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE SGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE SGESV
         END INTERFACE
         CALL SGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
      END SUBROUTINE SGESV1

      SUBROUTINE DGESV1( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE DGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE DGESV
         END INTERFACE
         CALL DGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
      END SUBROUTINE DGESV1



      SUBROUTINE SGESVX1( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE SGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF,   &
     &                        PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,  &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE SGESVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,      &
     &                EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR, LBERR, &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SGESVX1

      SUBROUTINE DGESVX1( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE DGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF,   &
     &                        PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,  &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE DGESVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,      &
     &                EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR, LBERR, &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DGESVX1


      SUBROUTINE SPOSV1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE SPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE SPOSV
         END INTERFACE
         CALL SPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE SPOSV1

      SUBROUTINE DPOSV1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE DPOSV
         END INTERFACE
         CALL DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE DPOSV1


      FUNCTION SLANGE1( NORM, M, N, A, LDA, WORK )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP) :: SLANGE1
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         REAL(WP), INTENT(IN) :: A( * )
         REAL(WP), INTENT(OUT) :: WORK( * )
         INTERFACE
           FUNCTION SLANGE( NORM, M, N, A, LDA, WORK )
             USE LA_PRECISION, ONLY: WP => SP
             REAL(WP) :: SLANGE
             CHARACTER(LEN=1), INTENT(IN) :: NORM
             INTEGER, INTENT(IN) :: LDA, M, N
             REAL(WP), INTENT(IN) :: A( LDA, * )
             REAL(WP), INTENT(OUT) :: WORK( * )
           END FUNCTION SLANGE
         END INTERFACE
        SLANGE1 = SLANGE( NORM, M, N, A, LDA, WORK )
      END FUNCTION SLANGE1

      FUNCTION DLANGE1( NORM, M, N, A, LDA, WORK )
         USE LA_PRECISION, ONLY: WP => DP
         REAL(WP) :: DLANGE1
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         REAL(WP), INTENT(IN) :: A( * )
         REAL(WP), INTENT(OUT) :: WORK( * )
         INTERFACE
           FUNCTION DLANGE( NORM, M, N, A, LDA, WORK )
             USE LA_PRECISION, ONLY: WP => DP
             REAL(WP) :: DLANGE
             CHARACTER(LEN=1), INTENT(IN) :: NORM
             INTEGER, INTENT(IN) :: LDA, M, N
             REAL(WP), INTENT(IN) :: A( LDA, * )
             REAL(WP), INTENT(OUT) :: WORK( * )
           END FUNCTION DLANGE
         END INTERFACE
        DLANGE1 = DLANGE( NORM, M, N, A, LDA, WORK )
      END FUNCTION DLANGE1


      SUBROUTINE SGBSV1( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE SGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB,    &
     &                       INFO )
             USE LA_PRECISION, ONLY: WP => SP
             INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE SGBSV
         END INTERFACE
         CALL SGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
      END SUBROUTINE SGBSV1

      SUBROUTINE DGBSV1( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE DGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB,    &
     &                       INFO )
             USE LA_PRECISION, ONLY: WP => DP
             INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE DGBSV
         END INTERFACE
         CALL DGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
      END SUBROUTINE DGBSV1


      SUBROUTINE SGBSVX1( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE SGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, &
     &                        LDAF, PIV, EQUED, R, C, B, LDB, X, LDX,   &
     &                        RCOND, FERR, BERR, WORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
             INTEGER, INTENT(OUT) :: INFO
!
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE SGBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, LDAF,   &
     &                PIV, EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR,   &
     &                LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SGBSVX1

      SUBROUTINE DGBSVX1( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE DGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, &
     &                        LDAF, PIV, EQUED, R, C, B, LDB, X, LDX,   &
     &                        RCOND, FERR, BERR, WORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
             INTEGER, INTENT(OUT) :: INFO
!
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE DGBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, LDAF,   &
     &                PIV, EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR,   &
     &                LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DGBSVX1


      SUBROUTINE SGTSV1( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(*)
         INTERFACE
           SUBROUTINE SGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
           END SUBROUTINE SGTSV
         END INTERFACE
         CALL SGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
      END SUBROUTINE SGTSV1

      SUBROUTINE DGTSV1( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(*)
         INTERFACE
           SUBROUTINE DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
           END SUBROUTINE DGTSV
         END INTERFACE
         CALL DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
      END SUBROUTINE DGTSV1


       SUBROUTINE SGTSVX1( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF,    &
     &                     DUF, DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, &
     &                     BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, X(*), WORK(*)
         REAL(WP), INTENT(IN) :: B(*), DL(*), D(*), DU(*) 
         REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
         INTERFACE
           SUBROUTINE SGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, &
     &                        DUF, DU2, IPIV, B, LDB, X, LDX, RCOND,    &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
             REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
           END SUBROUTINE SGTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,    &
     &                DU2, IPIV, B, LDB, X, LDX, RCOND, LFERR, LBERR,   &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SGTSVX1

       SUBROUTINE DGTSVX1( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF,    &
     &                     DUF, DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, &
     &                     BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, X(*), WORK(*)
         REAL(WP), INTENT(IN) :: B(*), DL(*), D(*), DU(*) 
         REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
         INTERFACE
           SUBROUTINE DGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, &
     &                        DUF, DU2, IPIV, B, LDB, X, LDX, RCOND,    &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
             REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
           END SUBROUTINE DGTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,    &
     &                DU2, IPIV, B, LDB, X, LDX, RCOND, LFERR, LBERR,   &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DGTSVX1


       SUBROUTINE SPOSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,&
     &                     S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,  &
     &                     IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE SPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        EQUED, S, B, LDB, X, LDX, RCOND, FERR,    &
     &                        BERR, WORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: S(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE SPOSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, S,  &
     &                B, LDB, X, LDX, RCOND, LFERR, LBERR, WORK, IWORK, &
     &                INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SPOSVX1

       SUBROUTINE DPOSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,&
     &                     S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,  &
     &                     IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE DPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        EQUED, S, B, LDB, X, LDX, RCOND, FERR,    &
     &                        BERR, WORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: S(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE DPOSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, S,  &
     &                B, LDB, X, LDX, RCOND, LFERR, LBERR, WORK, IWORK, &
     &                INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DPOSVX1


       SUBROUTINE SPPSV1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE SPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE SPPSV
         END INTERFACE
         CALL SPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE SPPSV1

       SUBROUTINE DPPSV1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE DPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE DPPSV
         END INTERFACE
         CALL DPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE DPPSV1


       SUBROUTINE SPPSVX1( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,   &
     &                     LDB, X, LDX, RCOND, FERR, BERR, WORK, IWORK, &
     &                     INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(*), S(*)
         INTERFACE
           SUBROUTINE SPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,&
     &                        LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                        IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*), S(*)
           END SUBROUTINE SPPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B, LDB, X,&
     &                LDX, RCOND, LFERR, LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SPPSVX1

       SUBROUTINE DPPSVX1( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,   &
     &                     LDB, X, LDX, RCOND, FERR, BERR, WORK, IWORK, &
     &                     INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(*), S(*)
         INTERFACE
           SUBROUTINE DPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,&
     &                        LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                        IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*), S(*)
           END SUBROUTINE DPPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B, LDB, X,&
     &                LDX, RCOND, LFERR, LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DPPSVX1


       SUBROUTINE SPBSV1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE SPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,       &
     &                       INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE SPBSV
         END INTERFACE
         CALL SPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE SPBSV1

       SUBROUTINE DPBSV1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE DPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,       &
     &                       INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE DPBSV
         END INTERFACE
         CALL DPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE DPBSV1


       SUBROUTINE SPBSVX1( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,      &
     &                     LDAFB, EQUED, S, B, LDB, X, LDX, RCOND, FERR,&
     &                     BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: X(*), WORK(*), FERR, BERR, RCOND
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*), B(*),     &
     &                              S(*)
         INTERFACE
           SUBROUTINE SPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,   &
     &                        LDAFB, EQUED, S, B, LDB, X, LDX, RCOND,   &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
             INTEGER, INTENT(OUT) :: INFO, IWORK(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*), FERR(*),       &
     &                                BERR(*), RCOND
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*),       &
     &                                  B(LDB,*), S(*)
           END SUBROUTINE SPBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,    &
     &                EQUED, S, B, LDB, X, LDX, RCOND, LFERR, LBERR,    &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SPBSVX1

       SUBROUTINE DPBSVX1( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,      &
     &                     LDAFB, EQUED, S, B, LDB, X, LDX, RCOND, FERR,&
     &                     BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: X(*), WORK(*), FERR, BERR, RCOND
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*), B(*),     &
     &                              S(*)
         INTERFACE
           SUBROUTINE DPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,   &
     &                        LDAFB, EQUED, S, B, LDB, X, LDX, RCOND,   &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
             INTEGER, INTENT(OUT) :: INFO, IWORK(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*), FERR(*),       &
     &                                BERR(*), RCOND
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*),       &
     &                                  B(LDB,*), S(*)
           END SUBROUTINE DPBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,    &
     &                EQUED, S, B, LDB, X, LDX, RCOND, LFERR, LBERR,    &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DPBSVX1


       SUBROUTINE SPTSV1( N, NRHS, D, E, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         REAL(WP), INTENT(INOUT) :: E(*), B(*)
         INTERFACE
           SUBROUTINE SPTSV( N, NRHS, D, E, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: D(*)
             REAL(WP), INTENT(INOUT) :: E(*), B(LDB,*)
           END SUBROUTINE SPTSV
         END INTERFACE
         CALL SPTSV( N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE SPTSV1

       SUBROUTINE DPTSV1( N, NRHS, D, E, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         REAL(WP), INTENT(INOUT) :: E(*), B(*)
         INTERFACE
           SUBROUTINE DPTSV( N, NRHS, D, E, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: D(*)
             REAL(WP), INTENT(INOUT) :: E(*), B(LDB,*)
           END SUBROUTINE DPTSV
         END INTERFACE
         CALL DPTSV( N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE DPTSV1


       SUBROUTINE SPTSVX1( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX, &
     &                     RCOND, FERR, BERR, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*), B(*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         REAL(WP), INTENT(INOUT) :: EF(*)
         INTERFACE
           SUBROUTINE SPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X,   &
     &                        LDX, RCOND, FERR, BERR, WORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: D(*)
             REAL(WP), INTENT(IN) :: E(*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: DF(*)
             REAL(WP), INTENT(INOUT) :: EF(*)
           END SUBROUTINE SPTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,      &
     &                RCOND, LFERR, LBERR, WORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SPTSVX1

       SUBROUTINE DPTSVX1( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX, &
     &                     RCOND, FERR, BERR, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*), B(*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         REAL(WP), INTENT(INOUT) :: EF(*)
         INTERFACE
           SUBROUTINE DPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X,   &
     &                        LDX, RCOND, FERR, BERR, WORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: D(*)
             REAL(WP), INTENT(IN) :: E(*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: DF(*)
             REAL(WP), INTENT(INOUT) :: EF(*)
           END SUBROUTINE DPTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,      &
     &                RCOND, LFERR, LBERR, WORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DPTSVX1


       SUBROUTINE SSYSV1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,    &
     &                    LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE SSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, &
     &                       LWORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE SSYSV
         END INTERFACE
         CALL SSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, LWORK,  &
     &               INFO )
      END SUBROUTINE SSYSV1

       SUBROUTINE DSYSV1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,    &
     &                    LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE DSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, &
     &                       LWORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE DSYSV
         END INTERFACE
         CALL DSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, LWORK,  &
     &               INFO )
      END SUBROUTINE DSYSV1



       SUBROUTINE SSYSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, &
     &                     B, LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                     LWORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(*)
         REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
         INTERFACE
           SUBROUTINE SSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        IPIV, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                        WORK, LWORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
           END SUBROUTINE SSYSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,   &
     &                LDB, X, LDX, RCOND, LFERR, LBERR, WORK, LWORK,    &
     &                IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SSYSVX1

       SUBROUTINE DSYSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, &
     &                     B, LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                     LWORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(*)
         REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
         INTERFACE
           SUBROUTINE DSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        IPIV, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                        WORK, LWORK, IWORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
           END SUBROUTINE DSYSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,   &
     &                LDB, X, LDX, RCOND, LFERR, LBERR, WORK, LWORK,    &
     &                IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DSYSVX1



       SUBROUTINE SSPSV1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE SSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE SSPSV
         END INTERFACE
         CALL SSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE SSPSV1

       SUBROUTINE DSPSV1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE DSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE DSPSV
         END INTERFACE
         CALL DSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE DSPSV1



       SUBROUTINE SSPSVX1( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
     &                     LDX, RCOND, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: A(*), B(*)
         REAL(WP), INTENT(INOUT) :: AF(*)
         INTERFACE
           SUBROUTINE SSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, &
     &                        X, LDX, RCOND, FERR, BERR, WORK, IWORK,   &
     &                        INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: A(*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: AF(*)
           END SUBROUTINE SSPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, LDX, &
     &                RCOND, LFERR, LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SSPSVX1

       SUBROUTINE DSPSVX1( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
     &                     LDX, RCOND, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: A(*), B(*)
         REAL(WP), INTENT(INOUT) :: AF(*)
         INTERFACE
           SUBROUTINE DSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, &
     &                        X, LDX, RCOND, FERR, BERR, WORK, IWORK,   &
     &                        INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: A(*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: AF(*)
           END SUBROUTINE DSPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, LDX, &
     &                RCOND, LFERR, LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DSPSVX1



       SUBROUTINE SGELS1( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,      &
     &                    LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE SGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,   &
     &                       LWORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             CHARACTER(LEN=1), INTENT(IN) :: TRANS
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE SGELS
         END INTERFACE
         CALL SGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,    &
     &               INFO )
      END SUBROUTINE SGELS1

       SUBROUTINE DGELS1( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,      &
     &                    LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,   &
     &                       LWORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             CHARACTER(LEN=1), INTENT(IN) :: TRANS
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE DGELS
         END INTERFACE
         CALL DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,    &
     &               INFO )
      END SUBROUTINE DGELS1


       SUBROUTINE SGELSY1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, LWORK, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          INTEGER, INTENT(INOUT) :: JPVT(*)
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
          REAL(WP), INTENT(OUT) ::  WORK(*)
          INTERFACE
           SUBROUTINE SGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, LWORK, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          INTEGER, INTENT(INOUT) :: JPVT(*)
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) ::  WORK(*)
         END SUBROUTINE SGELSY
        END INTERFACE
         CALL SGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, LWORK, INFO )
       END SUBROUTINE SGELSY1
       SUBROUTINE DGELSY1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, LWORK, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          INTEGER, INTENT(INOUT) :: JPVT(*)
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
          REAL(WP), INTENT(OUT) ::  WORK(*)
          INTERFACE
           SUBROUTINE DGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, LWORK, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          INTEGER, INTENT(INOUT) :: JPVT(*)
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) ::  WORK(*)
         END SUBROUTINE DGELSY
        END INTERFACE
         CALL DGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, LWORK, INFO )
       END SUBROUTINE DGELSY1

       SUBROUTINE SGELSD1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP =>  SP
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND 
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTEGER :: IWORK(*)  
         INTERFACE
           SUBROUTINE SGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, IWORK, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
           INTEGER, INTENT(OUT) :: INFO, RANK
           REAL(WP), INTENT(IN) :: RCOND
           REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           REAL(WP), INTENT(OUT) :: S(*)
           REAL(WP), INTENT(OUT) :: WORK(*)
           INTEGER :: IWORK(*)
         END SUBROUTINE SGELSD
       END INTERFACE
         CALL SGELSD ( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK,&
     &                 LWORK, IWORK, INFO )
       END SUBROUTINE SGELSD1
       SUBROUTINE DGELSD1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP =>  DP
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND 
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTEGER :: IWORK(*)  
         INTERFACE
           SUBROUTINE DGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, IWORK, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
           INTEGER, INTENT(OUT) :: INFO, RANK
           REAL(WP), INTENT(IN) :: RCOND
           REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           REAL(WP), INTENT(OUT) :: S(*)
           REAL(WP), INTENT(OUT) :: WORK(*)
           INTEGER :: IWORK(*)
         END SUBROUTINE DGELSD
       END INTERFACE
         CALL DGELSD ( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK,&
     &                 LWORK, IWORK, INFO )
       END SUBROUTINE DGELSD1


       SUBROUTINE SGELSX1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE SGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
             INTEGER, INTENT(OUT) :: INFO, RANK
             INTEGER, INTENT(INOUT) :: JPVT(*)
             REAL(WP), INTENT(IN) :: RCOND
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE SGELSX
         END INTERFACE
         CALL SGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, INFO )
      END SUBROUTINE SGELSX1

       SUBROUTINE DGELSX1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE DGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
             INTEGER, INTENT(OUT) :: INFO, RANK
             INTEGER, INTENT(INOUT) :: JPVT(*)
             REAL(WP), INTENT(IN) :: RCOND
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE DGELSX
         END INTERFACE
         CALL DGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, INFO )
      END SUBROUTINE DGELSX1


       SUBROUTINE SGELSS1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE SGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, INFO )
             USE LA_PRECISION, ONLY: WP => SP
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO, RANK
             REAL(WP), INTENT(IN) :: RCOND
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: S(*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE SGELSS
         END INTERFACE
         CALL SGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK, &
     &                LWORK, INFO )
      END SUBROUTINE SGELSS1

       SUBROUTINE DGELSS1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE DGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, INFO )
             USE LA_PRECISION, ONLY: WP => DP
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO, RANK
             REAL(WP), INTENT(IN) :: RCOND
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: S(*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE DGELSS
         END INTERFACE
         CALL DGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK, &
     &                LWORK, INFO )
      END SUBROUTINE DGELSS1

       SUBROUTINE SGETRS1( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          CHARACTER(LEN=1), INTENT(IN) :: TRANS
          INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
          INTEGER, INTENT(OUT) :: INFO
          INTEGER, INTENT(IN) :: PIV(*)
          REAL(WP), INTENT(IN) :: A(LDA,*)
          REAL(WP), INTENT(INOUT) :: B(*)
          INTERFACE
             SUBROUTINE SGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB,    &
     &                          INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: PIV(*)
               REAL(WP), INTENT(IN) :: A(LDA,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE SGETRS
          END INTERFACE
          CALL SGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
       END SUBROUTINE SGETRS1
       SUBROUTINE DGETRS1( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          CHARACTER(LEN=1), INTENT(IN) :: TRANS
          INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
          INTEGER, INTENT(OUT) :: INFO
          INTEGER, INTENT(IN) :: PIV(*)
          REAL(WP), INTENT(IN) :: A(LDA,*)
          REAL(WP), INTENT(INOUT) :: B(*)
          INTERFACE
             SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB,    &
     &                          INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: PIV(*)
               REAL(WP), INTENT(IN) :: A(LDA,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE DGETRS
          END INTERFACE
          CALL DGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
       END SUBROUTINE DGETRS1

       SUBROUTINE SGERFS1( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B,    &
     &                     LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN) :: TRANS
           INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
           INTEGER, INTENT(OUT) :: INFO
           INTEGER, INTENT(IN) :: PIV(*)
           INTEGER, INTENT(OUT) :: IWORK(*)
           REAL(WP), INTENT(OUT) :: FERR, BERR
           REAL(WP), INTENT(OUT) :: WORK(*)
           REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(*)
           REAL(WP), INTENT(INOUT) :: X(*)
           INTERFACE
              SUBROUTINE SGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, &
     &                           B, LDB, X, LDX, FERR, BERR, WORK,      &
     &                           IWORK, INFO )
                 USE LA_PRECISION, ONLY: WP => SP
                 CHARACTER(LEN=1), INTENT(IN) :: TRANS
                 INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
                 INTEGER, INTENT(OUT) :: INFO
                 INTEGER, INTENT(IN) :: PIV(*)
                 INTEGER, INTENT(OUT) :: IWORK(*)
                 REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
                 REAL(WP), INTENT(OUT) :: WORK(*)
                 REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
                 REAL(WP), INTENT(INOUT) :: X(LDX,*)
              END SUBROUTINE SGERFS
           END INTERFACE
           REAL(WP) FERR1(1), BERR1(1)
           CALL SGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB,  &
     &                  X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
           FERR = FERR1(1); BERR = BERR1(1)
        END SUBROUTINE SGERFS1

       SUBROUTINE DGERFS1( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B,    &
     &                     LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN) :: TRANS
           INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
           INTEGER, INTENT(OUT) :: INFO
           INTEGER, INTENT(IN) :: PIV(*)
           INTEGER, INTENT(OUT) :: IWORK(*)
           REAL(WP), INTENT(OUT) :: FERR, BERR
           REAL(WP), INTENT(OUT) :: WORK(*)
           REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(*)
           REAL(WP), INTENT(INOUT) :: X(*)
           INTERFACE
              SUBROUTINE DGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, &
     &                           B, LDB, X, LDX, FERR, BERR, WORK,      &
     &                           IWORK, INFO )
                 USE LA_PRECISION, ONLY: WP => DP
                 CHARACTER(LEN=1), INTENT(IN) :: TRANS
                 INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
                 INTEGER, INTENT(OUT) :: INFO
                 INTEGER, INTENT(IN) :: PIV(*)
                 INTEGER, INTENT(OUT) :: IWORK(*)
                 REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
                 REAL(WP), INTENT(OUT) :: WORK(*)
                 REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
                 REAL(WP), INTENT(INOUT) :: X(LDX,*)
              END SUBROUTINE DGERFS
           END INTERFACE
           REAL(WP) FERR1(1), BERR1(1)
           CALL DGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB,  &
     &                  X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
           FERR = FERR1(1); BERR = BERR1(1)
        END SUBROUTINE DGERFS1


      SUBROUTINE SGBTRS1( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B,    &
     &                    LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AB( LDAB,*), B(*)
         INTERFACE
            SUBROUTINE SGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV,  &
     &                         B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(INOUT) :: IPIV(*)
               REAL(WP), INTENT(INOUT) :: AB( LDAB,*), B(LDB,*)
            END SUBROUTINE SGBTRS
         END INTERFACE
         CALL SGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,   &
     &                INFO )
      END SUBROUTINE SGBTRS1
      SUBROUTINE DGBTRS1( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B,    &
     &                    LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AB( LDAB,*), B(*)
         INTERFACE
            SUBROUTINE DGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV,  &
     &                         B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(INOUT) :: IPIV(*)
               REAL(WP), INTENT(INOUT) :: AB( LDAB,*), B(LDB,*)
            END SUBROUTINE DGBTRS
         END INTERFACE
         CALL DGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,   &
     &                INFO )
      END SUBROUTINE DGBTRS1

      SUBROUTINE SGBRFS1( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK,&
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         INTERFACE
            SUBROUTINE SGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB,   &
     &                         LDAFB, IPIV, B, LDB, X, LDX, FERR, BERR, &
     &                         WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, &
     &                                NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               INTEGER, INTENT(OUT) :: IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*),      &
     &                                 B( LDB,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
            END SUBROUTINE SGBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL SGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, IWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SGBRFS1
      SUBROUTINE DGBRFS1( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK,&
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         INTERFACE
            SUBROUTINE DGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB,   &
     &                         LDAFB, IPIV, B, LDB, X, LDX, FERR, BERR, &
     &                         WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, &
     &                                NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               INTEGER, INTENT(OUT) :: IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*),      &
     &                                 B( LDB,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
            END SUBROUTINE DGBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL DGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, IWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DGBRFS1


      SUBROUTINE SGTTRS1( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB, &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, &
     &                         LDB, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(OUT) :: IPIV(*)
               REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE SGTTRS
         END INTERFACE
         CALL SGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,     &
     &                INFO )
      END SUBROUTINE SGTTRS1
      SUBROUTINE DGTTRS1( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB, &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, &
     &                         LDB, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(OUT) :: IPIV(*)
               REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE DGTTRS
         END INTERFACE
         CALL DGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,     &
     &                INFO )
      END SUBROUTINE DGTTRS1

      SUBROUTINE SGTRFS1( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK,&
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: B(*), D(*), DF(*), DL(*), DLF(*),      &
     &                           DU(*), DU2(*), DUF(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, &
     &                         DU2, IPIV, B, LDB, X, LDX, FERR, BERR,   &
     &                         WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*),   &
     &                                 DLF(*), DU(*), DU2(*), DUF(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SGTRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL SGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, IWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SGTRFS1

      SUBROUTINE DGTRFS1( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK,&
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: B(*), D(*), DF(*), DL(*), DLF(*),      &
     &                           DU(*), DU2(*), DUF(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, &
     &                         DU2, IPIV, B, LDB, X, LDX, FERR, BERR,   &
     &                         WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*),   &
     &                                 DLF(*), DU(*), DU2(*), DUF(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DGTRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL DGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, IWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DGTRFS1


      SUBROUTINE SPOTRS1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: A( LDA,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE SPOTRS
         END INTERFACE
         CALL SPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE SPOTRS1

      SUBROUTINE DPOTRS1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: A( LDA,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE DPOTRS
         END INTERFACE
         CALL DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE DPOTRS1


      SUBROUTINE SPORFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,   &
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: A(*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, &
     &                         X, LDX, FERR, BERR, WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*),          &
     &                                 B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SPORFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL SPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X, LDX,  &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE SPORFS1

      SUBROUTINE DPORFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,   &
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: A(*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, &
     &                         X, LDX, FERR, BERR, WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*),          &
     &                                 B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DPORFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL DPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X, LDX,  &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE DPORFS1


      SUBROUTINE SPPTRS1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE SPPTRS
         END INTERFACE
         CALL SPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE SPPTRS1

      SUBROUTINE DPPTRS1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE DPPTRS
         END INTERFACE
         CALL DPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE DPPTRS1


      SUBROUTINE SPPRFS1( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR, &
     &                    BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX,  &
     &                         FERR, BERR, WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SPPRFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL SPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR1,    &
     &                BERR1, WORK, IWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE SPPRFS1

      SUBROUTINE DPPRFS1( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR, &
     &                    BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX,  &
     &                         FERR, BERR, WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DPPRFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL DPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR1,    &
     &                BERR1, WORK, IWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE DPPRFS1

      SUBROUTINE SPBTRS1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,     &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AB( LDAB,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE SPBTRS
         END INTERFACE
         CALL SPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE SPBTRS1

      SUBROUTINE DPBTRS1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,     &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AB( LDAB,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE DPBTRS
         END INTERFACE
         CALL DPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE DPBTRS1


      SUBROUTINE SPBRFS1( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,   &
     &                    LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, &
     &                         B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N,    &
     &                                 NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*),     &
     &                                  B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SPBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1) 
         CALL SPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B, LDB,  &
     &                X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SPBRFS1

      SUBROUTINE DPBRFS1( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,   &
     &                    LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, &
     &                         B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N,    &
     &                                 NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*),     &
     &                                  B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DPBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1) 
         CALL DPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B, LDB,  &
     &                X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DPBRFS1


      SUBROUTINE SPTTRS1( N, NRHS, D, E, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*)
         REAL(WP), INTENT(OUT) :: B(*)
         INTERFACE
            SUBROUTINE SPTTRS( N, NRHS, D, E, B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*)
               REAL(WP), INTENT(IN) :: E(*)
               REAL(WP), INTENT(OUT) :: B( LDB,*)
            END SUBROUTINE SPTTRS
         END INTERFACE
         CALL SPTTRS( N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE SPTTRS1

      SUBROUTINE DPTTRS1( N, NRHS, D, E, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*)
         REAL(WP), INTENT(OUT) :: B(*)
         INTERFACE
            SUBROUTINE DPTTRS( N, NRHS, D, E, B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*)
               REAL(WP), INTENT(IN) :: E(*)
               REAL(WP), INTENT(OUT) :: B( LDB,*)
            END SUBROUTINE DPTTRS
         END INTERFACE
         CALL DPTTRS( N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE DPTTRS1


      SUBROUTINE SPTRFS1( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR,  &
     &                    BERR, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: B(*), E(*), EF(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX,   &
     &                         FERR, BERR, WORK, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*), DF(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SPTRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL SPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR1,     &
     &                BERR1, WORK, INFO )
      FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SPTRFS1

      SUBROUTINE DPTRFS1( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR,  &
     &                    BERR, WORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: B(*), E(*), EF(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX,   &
     &                         FERR, BERR, WORK, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*), DF(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DPTRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL DPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR1,     &
     &                BERR1, WORK, INFO )
      FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DPTRFS1

      SUBROUTINE SSYTRS1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,     &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER , INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(IN) :: A( LDA,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE SSYTRS
         END INTERFACE
         CALL SSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      END SUBROUTINE SSYTRS1

      SUBROUTINE DSYTRS1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,     &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER , INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(IN) :: A( LDA,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE DSYTRS
         END INTERFACE
         CALL DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      END SUBROUTINE DSYTRS1

      SUBROUTINE SSYRFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,&
     &                         LDB, X, LDX, FERR, BERR, WORK, IWORK,    &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*),         &
     &                                  B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SSYRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL SSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, X, &
     &                LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SSYRFS1

      SUBROUTINE DSYRFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,&
     &                         LDB, X, LDX, FERR, BERR, WORK, IWORK,    &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*),         &
     &                                  B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DSYRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL DSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, X, &
     &                LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DSYRFS1

      SUBROUTINE SSPTRS1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE SSPTRS
         ENDINTERFACE
         CALL SSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE SSPTRS1

      SUBROUTINE DSPTRS1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE DSPTRS
         ENDINTERFACE
         CALL DSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE DSPTRS1

      SUBROUTINE SSPRFS1( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, &
     &                         LDX, FERR, BERR, WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
               REAL(WP), INTENT(INOUT) :: X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SSPRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL SSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,     &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SSPRFS1

      SUBROUTINE DSPRFS1( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, &
     &                         LDX, FERR, BERR, WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
               REAL(WP), INTENT(INOUT) :: X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DSPRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL DSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,     &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DSPRFS1

      SUBROUTINE STRTRS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,   &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE STRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: A(LDA,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE STRTRS
         END INTERFACE
         CALL STRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,       &
     &                INFO )
      END SUBROUTINE STRTRS1

      SUBROUTINE DTRTRS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,   &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: A(LDA,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE DTRTRS
         END INTERFACE
         CALL DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,       &
     &                INFO )
      END SUBROUTINE DTRTRS1

      SUBROUTINE STRRFS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,&
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: A(LDA,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE STRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, X, LDX, FERR, BERR, WORK, IWORK,    &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
               REAL(WP), INTENT(INOUT) :: X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE STRRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL STRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,    &
     &                LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE STRRFS1

      SUBROUTINE DTRRFS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,&
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: A(LDA,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, X, LDX, FERR, BERR, WORK, IWORK,    &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
               REAL(WP), INTENT(INOUT) :: X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DTRRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL DTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,    &
     &                LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DTRRFS1

      SUBROUTINE STPTRS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,       &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE STPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE STPTRS
         END INTERFACE
         CALL STPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE STPTRS1

      SUBROUTINE DTPTRS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,       &
     &                    INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE DTPTRS
         END INTERFACE
         CALL DTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE DTPTRS1

      SUBROUTINE STPRFS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X,    &
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AP(*), B(*), X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE STPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         X, LDX, FERR, BERR, WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE STPRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL STPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,   &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE STPRFS1

      SUBROUTINE DTPRFS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X,    &
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AP(*), B(*), X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         X, LDX, FERR, BERR, WORK, IWORK, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DTPRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL DTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,   &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DTPRFS1

      SUBROUTINE STBTRS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE STBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AB(LDAB,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE STBTRS
         END INTERFACE
         CALL STBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                INFO )
      END SUBROUTINE STBTRS1

      SUBROUTINE DTBTRS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AB(LDAB,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE DTBTRS
         END INTERFACE
         CALL DTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                INFO )
      END SUBROUTINE DTBTRS1

      SUBROUTINE STBRFS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AB(LDAB,*), B(*), X(*)
         REAL(WP), INTENT(IN) :: WORK(*)
         INTERFACE
            SUBROUTINE STBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => SP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
               REAL(WP), INTENT(IN) :: WORK(*)
            END SUBROUTINE STBRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL STBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE STBRFS1

      SUBROUTINE DTBRFS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AB(LDAB,*), B(*), X(*)
         REAL(WP), INTENT(IN) :: WORK(*)
         INTERFACE
            SUBROUTINE DTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                         INFO )
               USE LA_PRECISION, ONLY: WP => DP
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
               REAL(WP), INTENT(IN) :: WORK(*)
            END SUBROUTINE DTBRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL DTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DTBRFS1



      END MODULE F77_LAPACK
