      MODULE F95_LAPACK
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!

      INTERFACE LA_LAMCH

       FUNCTION SLAMCH_F95( PRECISION, CMACH, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN) :: CMACH
           REAL(WP), INTENT(IN) :: PRECISION
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP) :: SLAMCH_F95
        END FUNCTION SLAMCH_F95

       FUNCTION DLAMCH_F95( PRECISION, CMACH, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN) :: CMACH
           REAL(WP), INTENT(IN) :: PRECISION
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP) :: DLAMCH_F95
        END FUNCTION DLAMCH_F95

      END INTERFACE

      INTERFACE LA_GGSVD

       SUBROUTINE SGGSVD_F95( A, B, ALPHA, BETA, K, L, U, V, Q, IWORK,  &
     &                        INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, K, L
          REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          REAL(WP), INTENT(OUT) :: ALPHA(:), BETA(:)
          REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: U(:,:), V(:,:),    &
     &                                               Q(:,:)
          INTEGER, INTENT(OUT), OPTIONAL :: IWORK(:)
        END SUBROUTINE SGGSVD_F95

       SUBROUTINE DGGSVD_F95( A, B, ALPHA, BETA, K, L, U, V, Q, IWORK,  &
     &                        INFO )
          USE LA_PRECISION, ONLY: WP => DP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, K, L
          REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          REAL(WP), INTENT(OUT) :: ALPHA(:), BETA(:)
          REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: U(:,:), V(:,:),    &
     &                                               Q(:,:)
          INTEGER, INTENT(OUT), OPTIONAL :: IWORK(:)
        END SUBROUTINE DGGSVD_F95

       SUBROUTINE CGGSVD_F95( A, B, ALPHA, BETA, K, L, U, V, Q, IWORK,  &
     &                        INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, K, L
          COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          REAL(WP), INTENT(OUT) :: ALPHA(:), BETA(:)
          COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: U(:,:), V(:,:), &
     &                                                  Q(:,:)
          INTEGER, INTENT(OUT), OPTIONAL :: IWORK(:)
        END SUBROUTINE CGGSVD_F95

      END INTERFACE

      INTERFACE LA_GEGV

       SUBROUTINE SGEGV_F95( A, B, ALPHAR, ALPHAI, BETA, VL, VR, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          REAL(WP), INTENT(OUT), OPTIONAL :: ALPHAR(:), ALPHAI(:),      &
     &                                       BETA(:)
          REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VL(:,:), VR(:,:)
        END SUBROUTINE SGEGV_F95

       SUBROUTINE DGEGV_F95( A, B, ALPHAR, ALPHAI, BETA, VL, VR, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          REAL(WP), INTENT(OUT), OPTIONAL :: ALPHAR(:), ALPHAI(:),      &
     &                                       BETA(:)
          REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VL(:,:), VR(:,:)
        END SUBROUTINE DGEGV_F95


       SUBROUTINE CGEGV_F95( A, B, ALPHA, BETA, VL, VR, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          COMPLEX(WP), INTENT(OUT), OPTIONAL :: ALPHA(:), BETA(:)
          COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: VL(:,:), VR(:,:)
        END SUBROUTINE CGEGV_F95

      END INTERFACE

      INTERFACE LA_GEGS

       SUBROUTINE SGEGS_F95( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR,      &
     &                       INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          REAL(WP), INTENT(OUT), OPTIONAL :: ALPHAR(:), ALPHAI(:),      &
     &                                       BETA(:)
          REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VSL(:,:), VSR(:,:)
        END SUBROUTINE SGEGS_F95

       SUBROUTINE DGEGS_F95( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR,      &
     &                       INFO )
          USE LA_PRECISION, ONLY: WP => DP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          REAL(WP), INTENT(OUT), OPTIONAL :: ALPHAR(:), ALPHAI(:),      &
     &                                       BETA(:)
          REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VSL(:,:), VSR(:,:)
        END SUBROUTINE DGEGS_F95


       SUBROUTINE CGEGS_F95( A, B, ALPHA, BETA, VSL, VSR, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          COMPLEX(WP), INTENT(OUT), OPTIONAL :: ALPHA(:), BETA(:)
          COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: VSL(:,:),       &
     &                                                  VSR(:,:)
        END SUBROUTINE CGEGS_F95

      END INTERFACE

        INTERFACE LA_SBGVX

        SUBROUTINE SSBGVX_F95( AB, BB, W, UPLO, Z, VL, VU, IL, IU, M,   &
     &                         IFAIL, Q, ABSTOL, INFO )
       USE LA_PRECISION, ONLY: WP => SP
       REAL(WP), INTENT(INOUT) :: AB(:,:), BB(:,:)
       REAL(WP), INTENT(OUT) :: W(:)
       CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  UPLO
       INTEGER, INTENT(OUT), OPTIONAL :: INFO
       REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: Z(:,:), Q(:,:)
       REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
       INTEGER, INTENT(IN), OPTIONAL :: IL, IU
       INTEGER, INTENT(OUT), OPTIONAL :: M
       INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
      END SUBROUTINE SSBGVX_F95

        SUBROUTINE DSBGVX_F95( AB, BB, W, UPLO, Z, VL, VU, IL, IU, M,   &
     &                         IFAIL, Q, ABSTOL, INFO )
       USE LA_PRECISION, ONLY: WP => DP
       REAL(WP), INTENT(INOUT) :: AB(:,:), BB(:,:)
       REAL(WP), INTENT(OUT) :: W(:)
       CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  UPLO
       INTEGER, INTENT(OUT), OPTIONAL :: INFO
       REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: Z(:,:), Q(:,:)
       REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
       INTEGER, INTENT(IN), OPTIONAL :: IL, IU
       INTEGER, INTENT(OUT), OPTIONAL :: M
       INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
      END SUBROUTINE DSBGVX_F95

       END INTERFACE
	     
       INTERFACE LA_HBGVX
       
       SUBROUTINE CHBGVX_F95( AB, BB, W, UPLO, Z, VL, VU, IL, IU, M,    &
     &                        IFAIL, Q, ABSTOL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         COMPLEX(WP), INTENT(INOUT) :: AB(:,:), BB(:,:)
         REAL(WP), INTENT(OUT) :: W(:)
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  UPLO
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: Z(:,:),  Q(:,:)
         REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
         INTEGER, INTENT(IN), OPTIONAL :: IL, IU
         INTEGER, INTENT(OUT), OPTIONAL :: M
         INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
        END SUBROUTINE CHBGVX_F95
	
        END INTERFACE

        INTERFACE LA_SBGVD
	

        SUBROUTINE SSBGVD_F95( AB, BB, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT(INOUT) :: AB(:,:), BB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: Z(:,:)
         END SUBROUTINE SSBGVD_F95
	  

        SUBROUTINE DSBGVD_F95( AB, BB, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           REAL(WP), INTENT(INOUT) :: AB(:,:), BB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: Z(:,:)
         END SUBROUTINE DSBGVD_F95
	  
         END INTERFACE
		
         INTERFACE LA_HBGVD
	 
       SUBROUTINE CHBGVD_F95( AB, BB, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           COMPLEX(WP), INTENT(INOUT) :: AB(:,:), BB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(OUT), OPTIONAL,TARGET :: Z(:,:)
          END SUBROUTINE CHBGVD_F95
	  
        
         END INTERFACE

      INTERFACE LA_SBGV

       SUBROUTINE SSBGV_F95( A, B, W, UPLO, Z, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          REAL(WP), INTENT(OUT) :: W(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE SSBGV_F95

       SUBROUTINE DSBGV_F95( A, B, W, UPLO, Z, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          REAL(WP), INTENT(OUT) :: W(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE DSBGV_F95

      END INTERFACE

      INTERFACE LA_HBGV

       SUBROUTINE CHBGV_F95( A, B, W, UPLO, Z, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          REAL(WP), INTENT(OUT) :: W(:)
          COMPLEX(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE CHBGV_F95

      END INTERFACE

        INTERFACE LA_SPGVX
 
        SUBROUTINE SSPGVX_F95( AP, BP, W, ITYPE, UPLO, Z, VL, VU, IL,   &
     &                         IU, M, IFAIL, ABSTOL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP), INTENT(INOUT) :: AP(:), BP(:)
         REAL(WP), INTENT(OUT) :: W(:)
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  UPLO
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: Z(:,:)
         REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
         INTEGER, INTENT(IN), OPTIONAL :: IL, IU, ITYPE
         INTEGER, INTENT(OUT), OPTIONAL :: M
         INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
       END SUBROUTINE SSPGVX_F95

        SUBROUTINE DSPGVX_F95( AP, BP, W, ITYPE, UPLO, Z, VL, VU, IL,   &
     &                         IU, M, IFAIL, ABSTOL, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         REAL(WP), INTENT(INOUT) :: AP(:), BP(:)
         REAL(WP), INTENT(OUT) :: W(:)
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  UPLO
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: Z(:,:)
         REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
         INTEGER, INTENT(IN), OPTIONAL :: IL, IU, ITYPE
         INTEGER, INTENT(OUT), OPTIONAL :: M
         INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
       END SUBROUTINE DSPGVX_F95

       END INTERFACE
	     
       INTERFACE LA_HPGVX
		    
       SUBROUTINE CHPGVX_F95( AP, BP, W, ITYPE, UPLO, Z, VL, VU, IL, IU,&
     &                        M, IFAIL, ABSTOL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         COMPLEX(WP), INTENT(INOUT) :: AP(:), BP(:)
         REAL(WP), INTENT(OUT) :: W(:)
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  UPLO
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: Z(:,:)
         REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
         INTEGER, INTENT(IN), OPTIONAL :: IL, IU, ITYPE
         INTEGER, INTENT(OUT), OPTIONAL :: M
         INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
       END SUBROUTINE CHPGVX_F95

        END INTERFACE

      INTERFACE LA_SPGVD

       SUBROUTINE SSPGVD_F95( AP, BP, W, ITYPE, UPLO, Z, INFO )
        USE LA_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(INOUT) :: AP(:), BP(:)
        REAL(WP), INTENT(OUT) :: W(:)
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        INTEGER, INTENT(IN), OPTIONAL :: ITYPE
        INTEGER, INTENT(OUT), OPTIONAL :: INFO
        REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: Z(:,:)
       END SUBROUTINE SSPGVD_F95
     
       SUBROUTINE DSPGVD_F95( AP, BP, W, ITYPE, UPLO, Z, INFO )
        USE LA_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(INOUT) :: AP(:), BP(:)
        REAL(WP), INTENT(OUT) :: W(:)
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
        INTEGER, INTENT(IN), OPTIONAL :: ITYPE
        INTEGER, INTENT(OUT), OPTIONAL :: INFO
        REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: Z(:,:)
       END SUBROUTINE DSPGVD_F95
     
        END INTERFACE
	    
        INTERFACE LA_HPGVD

       SUBROUTINE CHPGVD_F95( AP, BP, W, ITYPE, UPLO, Z, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         COMPLEX(WP), INTENT(INOUT) :: AP(:), BP(:)
         REAL(WP), INTENT(OUT) :: W(:)
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
         INTEGER, INTENT(IN), OPTIONAL :: ITYPE 
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: Z(:,:)
        END SUBROUTINE CHPGVD_F95

       END INTERFACE

      INTERFACE LA_SPGV

       SUBROUTINE SSPGV_F95( A, B, W, ITYPE, UPLO, Z, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
          INTEGER, INTENT(IN), OPTIONAL :: ITYPE
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:), B(:)
          REAL(WP), INTENT(OUT) :: W(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE SSPGV_F95

       SUBROUTINE DSPGV_F95( A, B, W, ITYPE, UPLO, Z, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
          INTEGER, INTENT(IN), OPTIONAL :: ITYPE
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:), B(:)
          REAL(WP), INTENT(OUT) :: W(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE DSPGV_F95

      END INTERFACE

      INTERFACE LA_HPGV

       SUBROUTINE CHPGV_F95( A, B, W, ITYPE, UPLO, Z, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
          INTEGER, INTENT(IN), OPTIONAL :: ITYPE
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          COMPLEX(WP), INTENT(INOUT) :: A(:), B(:)
          REAL(WP), INTENT(OUT) :: W(:)
          COMPLEX(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE CHPGV_F95

      END INTERFACE

      INTERFACE LA_GESVD

       SUBROUTINE SGESVD_F95( A, S, U, VT, WW, JOB, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          CHARACTER(LEN=1), OPTIONAL, INTENT(IN) :: JOB
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT) :: S(:)
          REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: WW(:)
         REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: U(:,:), VT(:,:)
        END SUBROUTINE SGESVD_F95

       SUBROUTINE DGESVD_F95( A, S, U, VT, WW, JOB, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          CHARACTER(LEN=1), OPTIONAL, INTENT(IN) :: JOB
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT) :: S(:)
          REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: WW(:)
         REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: U(:,:), VT(:,:)
        END SUBROUTINE DGESVD_F95

       SUBROUTINE CGESVD_F95( A, S, U, VT, WW, JOB, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          CHARACTER(LEN=1), OPTIONAL, INTENT(IN) :: JOB
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          COMPLEX(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT) :: S(:)
          REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: WW(:)
         COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: U(:,:), VT(:,:)
        END SUBROUTINE CGESVD_F95

      END INTERFACE

      INTERFACE LA_GEEVX

       SUBROUTINE SGEEVX_F95( A, WR, WI, VL, VR, BALANC, ILO, IHI,      &
     &                        SCALE, ABNRM, RCONDE, RCONDV, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: BALANC
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, ILO, IHI
          REAL(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT), OPTIONAL :: ABNRM
         REAL(WP), INTENT(OUT), OPTIONAL :: SCALE(:), RCONDE(:),        &
     &                                      RCONDV(:)
          REAL(WP), INTENT(OUT) :: WR(:), WI(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: VL(:,:), VR(:,:)
       END SUBROUTINE SGEEVX_F95

       SUBROUTINE DGEEVX_F95( A, WR, WI, VL, VR, BALANC, ILO, IHI,      &
     &                        SCALE, ABNRM, RCONDE, RCONDV, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: BALANC
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, ILO, IHI
          REAL(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT), OPTIONAL :: ABNRM
         REAL(WP), INTENT(OUT), OPTIONAL :: SCALE(:), RCONDE(:),        &
     &                                      RCONDV(:)
          REAL(WP), INTENT(OUT) :: WR(:), WI(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: VL(:,:), VR(:,:)
       END SUBROUTINE DGEEVX_F95

       SUBROUTINE CGEEVX_F95( A, W, VL, VR, BALANC, ILO, IHI, SCALE,    &
     &                        ABNRM, RCONDE, RCONDV, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: BALANC
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, ILO, IHI
          COMPLEX(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT), OPTIONAL :: ABNRM
         REAL(WP), INTENT(OUT), OPTIONAL :: SCALE(:), RCONDE(:),        &
     &                                      RCONDV(:)
          COMPLEX(WP), INTENT(OUT) :: W(:)
          COMPLEX(WP), INTENT(OUT), OPTIONAL :: VL(:,:), VR(:,:)
       END SUBROUTINE CGEEVX_F95

      END INTERFACE

         INTERFACE LA_GGEVX
      
       SUBROUTINE SGGEVX_F95( A, B, ALPHAR, ALPHAI, BETA, VL, VR,       &
     &                        BALANC, ILO, IHI, LSCALE, RSCALE, ABNRM,  &
     &                        BBNRM, RCONDE, RCONDV, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: BALANC
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         INTEGER, INTENT(OUT), OPTIONAL :: ILO,IHI
         REAL(WP), INTENT(OUT), OPTIONAL :: ABNRM, BBNRM
         REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
         REAL(WP), INTENT(OUT) :: ALPHAR(:), ALPHAI(:)
         REAL(WP), INTENT(OUT) :: BETA(:)
         REAL(WP), INTENT(OUT), OPTIONAL ::LSCALE(:), RSCALE(:),        &
     &                                     RCONDE(:), RCONDV(:)
         REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VL(:,:), VR(:,:)
        END SUBROUTINE SGGEVX_F95

       SUBROUTINE DGGEVX_F95( A, B, ALPHAR, ALPHAI, BETA, VL, VR,       &
     &                        BALANC, ILO, IHI, LSCALE, RSCALE, ABNRM,  &
     &                        BBNRM, RCONDE, RCONDV, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: BALANC
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         INTEGER, INTENT(OUT), OPTIONAL :: ILO,IHI
         REAL(WP), INTENT(OUT), OPTIONAL :: ABNRM, BBNRM
         REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
         REAL(WP), INTENT(OUT) :: ALPHAR(:), ALPHAI(:)
         REAL(WP), INTENT(OUT) :: BETA(:)
         REAL(WP), INTENT(OUT), OPTIONAL ::LSCALE(:), RSCALE(:),        &
     &                                     RCONDE(:), RCONDV(:)
         REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VL(:,:), VR(:,:)
        END SUBROUTINE DGGEVX_F95

       SUBROUTINE CGGEVX_F95( A, B, ALPHA, BETA, VL, VR, BALANC, ILO,   &
     &                        IHI, LSCALE, RSCALE, ABNRM, BBNRM, RCONDE,&
     &                        RCONDV, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: BALANC
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         INTEGER, INTENT(OUT), OPTIONAL :: ILO,IHI
         REAL(WP), INTENT(OUT), OPTIONAL :: ABNRM, BBNRM
         COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(:)
         COMPLEX(WP), INTENT(OUT) :: BETA(:)
         REAL(WP), INTENT(OUT), OPTIONAL :: LSCALE(:), RSCALE(:),       &
     &                                      RCONDE(:), RCONDV(:)
         COMPLEX(WP), INTENT(OUT), OPTIONAL :: VL(:,:), VR(:,:)
       END SUBROUTINE CGGEVX_F95

       END INTERFACE

       INTERFACE LA_GGEV
      
       SUBROUTINE SGGEV_F95( A, B, ALPHAR, ALPHAI, BETA, VL, VR, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
         REAL(WP), INTENT(OUT) :: ALPHAR(:), ALPHAI(:), BETA(:)
         REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VL(:,:), VR(:,:)
       END SUBROUTINE SGGEV_F95

       SUBROUTINE DGGEV_F95( A, B, ALPHAR, ALPHAI, BETA, VL, VR, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
         REAL(WP), INTENT(OUT) :: ALPHAR(:), ALPHAI(:), BETA(:)
         REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VL(:,:), VR(:,:)
       END SUBROUTINE DGGEV_F95

       SUBROUTINE CGGEV_F95( A, B, ALPHA, BETA, VL, VR, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
         COMPLEX(WP), INTENT(OUT) ::  ALPHA(:), BETA(:)
         COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: VL(:,:), VR(:,:)
        END SUBROUTINE CGGEV_F95

       END INTERFACE

      INTERFACE LA_GEEV

       SUBROUTINE SGEEV_F95( A, WR, WI, VL, VR, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT) :: WR(:), WI(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: VL(:,:), VR(:,:)
       END SUBROUTINE SGEEV_F95

       SUBROUTINE DGEEV_F95( A, WR, WI, VL, VR, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT) :: WR(:), WI(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: VL(:,:), VR(:,:)
       END SUBROUTINE DGEEV_F95

       SUBROUTINE CGEEV_F95( A, W, VL, VR, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          COMPLEX(WP), INTENT(INOUT) :: A(:,:)
          COMPLEX(WP), INTENT(OUT) :: W(:)
          COMPLEX(WP), INTENT(OUT), OPTIONAL :: VL(:,:), VR(:,:)
       END SUBROUTINE CGEEV_F95

      END INTERFACE

      INTERFACE LA_GEESX

       SUBROUTINE SGEESX_F95( A, WR, WI, VS, SELECT, SDIM, RCONDE,      &
     &                        RCONDV, INFO )
          USE LA_PRECISION, ONLY: WP => SP
!         USE LA_EXTERNAL
          INTERFACE
             LOGICAL FUNCTION SELECT(WR, WI)
                USE LA_PRECISION, ONLY: WP => SP
                REAL(WP), INTENT(IN) :: WR, WI
             END FUNCTION SELECT
          END INTERFACE
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, SDIM
          REAL(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT) :: WR(:), WI(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: RCONDE, RCONDV
          REAL(WP), INTENT(OUT), OPTIONAL :: VS(:,:)
          OPTIONAL :: SELECT
       END SUBROUTINE SGEESX_F95

       SUBROUTINE DGEESX_F95( A, WR, WI, VS, SELECT, SDIM, RCONDE,      &
     &                        RCONDV, INFO )
          USE LA_PRECISION, ONLY: WP => DP
!         USE LA_EXTERNAL
          INTERFACE
             LOGICAL FUNCTION SELECT(WR, WI)
                USE LA_PRECISION, ONLY: WP => DP
                REAL(WP), INTENT(IN) :: WR, WI
             END FUNCTION SELECT
          END INTERFACE
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, SDIM
          REAL(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT) :: WR(:), WI(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: RCONDE, RCONDV
          REAL(WP), INTENT(OUT), OPTIONAL :: VS(:,:)
          OPTIONAL :: SELECT
       END SUBROUTINE DGEESX_F95

       SUBROUTINE CGEESX_F95( A, W, VS, SELECT, SDIM, RCONDE, RCONDV,   &
     &                        INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTERFACE
             LOGICAL FUNCTION SELECT(W)
                USE LA_PRECISION, ONLY: WP => SP
                COMPLEX(WP), INTENT(IN) :: W
             END FUNCTION SELECT
          END INTERFACE
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, SDIM
          COMPLEX(WP), INTENT(INOUT) :: A(:,:)
          COMPLEX(WP), INTENT(OUT) :: W(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: RCONDE, RCONDV
          COMPLEX(WP), INTENT(OUT), OPTIONAL :: VS(:,:)
          OPTIONAL :: SELECT
       END SUBROUTINE CGEESX_F95

      END INTERFACE

        INTERFACE LA_GGESX

       SUBROUTINE SGGESX_F95( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR,     &
     &                        SELECT, SDIM, RCONDE, RCONDV, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          INTEGER, INTENT(OUT), OPTIONAL :: SDIM
          REAL(WP), INTENT(OUT) :: ALPHAR(:), ALPHAI(:),  BETA(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: VSL(:,:), VSR(:,:)
          INTERFACE
           LOGICAL FUNCTION SELECT(ALPHAR, ALPHAI, BETA)
            USE LA_PRECISION, ONLY: WP => SP
            REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
           END FUNCTION SELECT
          END INTERFACE
          OPTIONAL :: SELECT
          REAL(WP), INTENT(OUT), OPTIONAL :: RCONDE(2), RCONDV(2)
        END SUBROUTINE SGGESX_F95

       SUBROUTINE DGGESX_F95( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR,     &
     &                        SELECT, SDIM, RCONDE, RCONDV, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          INTEGER, INTENT(OUT), OPTIONAL :: SDIM
          REAL(WP), INTENT(OUT) :: ALPHAR(:), ALPHAI(:),  BETA(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: VSL(:,:), VSR(:,:)
          INTERFACE
           LOGICAL FUNCTION SELECT(ALPHAR, ALPHAI, BETA)
            USE LA_PRECISION, ONLY: WP => DP
            REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
           END FUNCTION SELECT
          END INTERFACE
          OPTIONAL :: SELECT
          REAL(WP), INTENT(OUT), OPTIONAL :: RCONDE(2), RCONDV(2)
        END SUBROUTINE DGGESX_F95

       SUBROUTINE CGGESX_F95( A, B, ALPHA, BETA, VSL, VSR, SELECT, SDIM,&
     &                        RCONDE, RCONDV, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           INTEGER, INTENT(OUT), OPTIONAL :: SDIM
           COMPLEX(WP), INTENT(OUT) :: ALPHA(:), BETA(:)
           COMPLEX(WP), INTENT(OUT), OPTIONAL :: VSL(:,:), VSR(:,:)
           INTERFACE
            LOGICAL FUNCTION SELECT(ALPHA, BETA)
              USE LA_PRECISION, ONLY: WP => SP
              COMPLEX(WP), INTENT(IN) :: ALPHA, BETA
           END FUNCTION SELECT
          END INTERFACE
          OPTIONAL :: SELECT
          REAL(WP), INTENT(OUT), OPTIONAL :: RCONDE(2), RCONDV(2)
         END SUBROUTINE  CGGESX_F95
	 
        END INTERFACE 


       INTERFACE LA_GGES
      
       SUBROUTINE SGGES_F95( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR,      &
     &                       SELECT, SDIM, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           INTEGER, INTENT(OUT), OPTIONAL :: SDIM
           REAL(WP), INTENT(OUT) :: ALPHAR(:), ALPHAI(:), BETA(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: VSL(:,:), VSR(:,:)
           INTERFACE
             LOGICAL FUNCTION SELECT(ALPHAR, ALPHAI, BETA)
               USE LA_PRECISION, ONLY: WP => SP
               REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
             END FUNCTION SELECT
           END INTERFACE
           OPTIONAL :: SELECT
          END SUBROUTINE SGGES_F95

       SUBROUTINE DGGES_F95( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR,      &
     &                       SELECT, SDIM, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           INTEGER, INTENT(OUT), OPTIONAL :: SDIM
           REAL(WP), INTENT(OUT) :: ALPHAR(:), ALPHAI(:), BETA(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: VSL(:,:), VSR(:,:)
           INTERFACE
             LOGICAL FUNCTION SELECT(ALPHAR, ALPHAI, BETA)
               USE LA_PRECISION, ONLY: WP => DP
               REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
             END FUNCTION SELECT
           END INTERFACE
           OPTIONAL :: SELECT
          END SUBROUTINE DGGES_F95

       SUBROUTINE CGGES_F95( A, B, ALPHA, BETA, VSL, VSR, SELECT, SDIM, &
     &                       INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
         INTEGER, INTENT(OUT), OPTIONAL :: SDIM
         COMPLEX(WP), INTENT(OUT) :: ALPHA(:),  BETA(:)
         COMPLEX(WP), INTENT(OUT), OPTIONAL :: VSL(:,:), VSR(:,:)
         INTERFACE
           LOGICAL FUNCTION SELECT(ALPHA, BETA)
             USE LA_PRECISION, ONLY: WP => SP
             COMPLEX(WP), INTENT(IN) :: ALPHA, BETA
           END FUNCTION SELECT
         END INTERFACE
         OPTIONAL :: SELECT
       END SUBROUTINE CGGES_F95

       END INTERFACE

      INTERFACE LA_GEES

       SUBROUTINE SGEES_F95( A, WR, WI, VS, SELECT, SDIM, INFO )
          USE LA_PRECISION, ONLY: WP => SP
!         USE LA_EXTERNAL, ONLY: SELECT
          INTERFACE
             LOGICAL FUNCTION SELECT(WR, WI)
                USE LA_PRECISION, ONLY: WP => SP
                REAL(WP), INTENT(IN) :: WR, WI
             END FUNCTION SELECT
          END INTERFACE

          INTEGER, INTENT(OUT), OPTIONAL :: INFO, SDIM
          REAL(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT) :: WR(:), WI(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: VS(:,:)
          OPTIONAL :: SELECT
       END SUBROUTINE SGEES_F95

       SUBROUTINE DGEES_F95( A, WR, WI, VS, SELECT, SDIM, INFO )
          USE LA_PRECISION, ONLY: WP => DP
!         USE LA_EXTERNAL, ONLY: SELECT
          INTERFACE
             LOGICAL FUNCTION SELECT(WR, WI)
                USE LA_PRECISION, ONLY: WP => DP
                REAL(WP), INTENT(IN) :: WR, WI
             END FUNCTION SELECT
          END INTERFACE

          INTEGER, INTENT(OUT), OPTIONAL :: INFO, SDIM
          REAL(WP), INTENT(INOUT) :: A(:,:)
          REAL(WP), INTENT(OUT) :: WR(:), WI(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: VS(:,:)
          OPTIONAL :: SELECT
       END SUBROUTINE DGEES_F95

       SUBROUTINE CGEES_F95( A, W, VS, SELECT, SDIM, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTERFACE
             LOGICAL FUNCTION SELECT(W)
                USE LA_PRECISION, ONLY: WP => SP
                COMPLEX(WP), INTENT(IN) :: W
             END FUNCTION SELECT
          END INTERFACE
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, SDIM
          COMPLEX(WP), INTENT(INOUT) :: A(:,:)
          COMPLEX(WP), INTENT(OUT) :: W(:)
          COMPLEX(WP), INTENT(OUT), OPTIONAL :: VS(:,:)
          OPTIONAL :: SELECT
       END SUBROUTINE CGEES_F95

      END INTERFACE

        INTERFACE LA_STEVR
 
       SUBROUTINE SSTEVR_F95( D, E, W, Z, VL, VU, IL, IU, M, ISUPPZ,    &
     &                        ABSTOL, INFO )
         USE LA_PRECISION, ONLY: WP =>  SP
          REAL(WP), INTENT(INOUT) :: D(:), E(:)
          REAL(WP), INTENT(OUT) :: W(:)
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(IN), OPTIONAL:: ABSTOL, VL, VU
          INTEGER, INTENT(IN), OPTIONAL :: IL, IU
          INTEGER, INTENT(OUT), OPTIONAL :: M
          REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
          INTEGER, INTENT(OUT), OPTIONAL, TARGET :: ISUPPZ(:)
         END SUBROUTINE SSTEVR_F95
 
       SUBROUTINE DSTEVR_F95( D, E, W, Z, VL, VU, IL, IU, M, ISUPPZ,    &
     &                        ABSTOL, INFO )
         USE LA_PRECISION, ONLY: WP =>  DP
          REAL(WP), INTENT(INOUT) :: D(:), E(:)
          REAL(WP), INTENT(OUT) :: W(:)
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          REAL(WP), INTENT(IN), OPTIONAL:: ABSTOL, VL, VU
          INTEGER, INTENT(IN), OPTIONAL :: IL, IU
          INTEGER, INTENT(OUT), OPTIONAL :: M
          REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
          INTEGER, INTENT(OUT), OPTIONAL, TARGET :: ISUPPZ(:)
         END SUBROUTINE DSTEVR_F95
 
        END INTERFACE
						
      INTERFACE LA_STEVX

       SUBROUTINE SSTEVX_F95( D, E, W, Z, VL, VU, IL, IU, M, IFAIL,     &
     &                        ABSTOL, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          INTEGER, INTENT(IN), OPTIONAL :: IL, IU
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, M
          REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
          INTEGER, INTENT(OUT), OPTIONAL :: IFAIL(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
          REAL(WP), INTENT(INOUT) :: D(:), E(:)
          REAL(WP), INTENT(OUT) :: W(:)
       END SUBROUTINE SSTEVX_F95

       SUBROUTINE DSTEVX_F95( D, E, W, Z, VL, VU, IL, IU, M, IFAIL,     &
     &                        ABSTOL, INFO )
          USE LA_PRECISION, ONLY: WP => DP
          INTEGER, INTENT(IN), OPTIONAL :: IL, IU
          INTEGER, INTENT(OUT), OPTIONAL :: INFO, M
          REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
          INTEGER, INTENT(OUT), OPTIONAL :: IFAIL(:)
          REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
          REAL(WP), INTENT(INOUT) :: D(:), E(:)
          REAL(WP), INTENT(OUT) :: W(:)
       END SUBROUTINE DSTEVX_F95

      END INTERFACE

      INTERFACE LA_STEVD

       SUBROUTINE SSTEVD_F95( D, E, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT(INOUT) :: D(:), E(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
      END SUBROUTINE SSTEVD_F95

       SUBROUTINE DSTEVD_F95( D, E, Z, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           REAL(WP), INTENT(INOUT) :: D(:), E(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
      END SUBROUTINE DSTEVD_F95

      END INTERFACE

      INTERFACE LA_STEV

       SUBROUTINE SSTEV_F95( D, E, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT(INOUT) :: D(:), E(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE SSTEV_F95

       SUBROUTINE DSTEV_F95( D, E, Z, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           REAL(WP), INTENT(INOUT) :: D(:), E(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE DSTEV_F95

      END INTERFACE

      INTERFACE LA_SBEVX

       SUBROUTINE SSBEVX_F95( AB, W, UPLO, Z, VL, VU, IL, IU, M, IFAIL, &
     &                        Q, ABSTOL, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(IN), OPTIONAL :: IL, IU
           INTEGER, INTENT(OUT), OPTIONAL :: INFO, M
           REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
           INTEGER, INTENT(OUT), OPTIONAL :: IFAIL(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:), Q(:,:)
           REAL(WP), INTENT(INOUT) :: AB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE SSBEVX_F95

       SUBROUTINE DSBEVX_F95( AB, W, UPLO, Z, VL, VU, IL, IU, M, IFAIL, &
     &                        Q, ABSTOL, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(IN), OPTIONAL :: IL, IU
           INTEGER, INTENT(OUT), OPTIONAL :: INFO, M
           REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
           INTEGER, INTENT(OUT), OPTIONAL :: IFAIL(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:), Q(:,:)
           REAL(WP), INTENT(INOUT) :: AB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE DSBEVX_F95

      END INTERFACE

      INTERFACE LA_HBEVX

       SUBROUTINE CHBEVX_F95( AB, W, UPLO, Z, VL, VU, IL, IU, M, IFAIL, &
     &                        Q, ABSTOL, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(IN), OPTIONAL :: IL, IU
           INTEGER, INTENT(OUT), OPTIONAL :: INFO, M
           REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
           INTEGER, INTENT(OUT), OPTIONAL :: IFAIL(:)
           COMPLEX(WP), INTENT(OUT), OPTIONAL :: Z(:,:), Q(:,:)
           COMPLEX(WP), INTENT(INOUT) :: AB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE CHBEVX_F95

      END INTERFACE

      INTERFACE LA_SBEVD

       SUBROUTINE SSBEVD_F95( AB, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: AB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE SSBEVD_F95

       SUBROUTINE DSBEVD_F95( AB, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: AB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE DSBEVD_F95

      END INTERFACE

      INTERFACE LA_HBEVD

       SUBROUTINE CHBEVD_F95( AB, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(INOUT) :: AB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
           COMPLEX(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE CHBEVD_F95

      END INTERFACE

      INTERFACE LA_SBEV

       SUBROUTINE SSBEV_F95( AB, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: AB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE SSBEV_F95

       SUBROUTINE DSBEV_F95( AB, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: AB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE DSBEV_F95

      END INTERFACE

      INTERFACE LA_HBEV

       SUBROUTINE CHBEV_F95( AB, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(INOUT) :: AB(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
           COMPLEX(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE CHBEV_F95

      END INTERFACE

      INTERFACE LA_SPEVX

       SUBROUTINE SSPEVX_F95( AP, W, UPLO, Z, VL, VU, IL, IU, M, IFAIL, &
     &                        ABSTOL, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(IN), OPTIONAL :: IL, IU
           INTEGER, INTENT(OUT), OPTIONAL :: INFO, M
           REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
           INTEGER, INTENT(OUT), OPTIONAL :: IFAIL(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
           REAL(WP), INTENT(INOUT) :: AP(:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE SSPEVX_F95

       SUBROUTINE DSPEVX_F95( AP, W, UPLO, Z, VL, VU, IL, IU, M, IFAIL, &
     &                        ABSTOL, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(IN), OPTIONAL :: IL, IU
           INTEGER, INTENT(OUT), OPTIONAL :: INFO, M
           REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
           INTEGER, INTENT(OUT), OPTIONAL :: IFAIL(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
           REAL(WP), INTENT(INOUT) :: AP(:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE DSPEVX_F95

      END INTERFACE

      INTERFACE LA_HPEVX

       SUBROUTINE CHPEVX_F95( AP, W, UPLO, Z, VL, VU, IL, IU, M, IFAIL, &
     &                        ABSTOL, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(IN), OPTIONAL :: IL, IU
           INTEGER, INTENT(OUT), OPTIONAL :: INFO, M
           REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
           INTEGER, INTENT(OUT), OPTIONAL :: IFAIL(:)
           COMPLEX(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
           COMPLEX(WP), INTENT(INOUT) :: AP(:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE CHPEVX_F95

      END INTERFACE

      INTERFACE LA_SPEVD

       SUBROUTINE SSPEVD_F95( AP, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: AP(:)
           REAL(WP), INTENT(OUT) :: W(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE SSPEVD_F95

       SUBROUTINE DSPEVD_F95( AP, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: AP(:)
           REAL(WP), INTENT(OUT) :: W(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE DSPEVD_F95

      END INTERFACE

      INTERFACE LA_HPEVD

       SUBROUTINE CHPEVD_F95( AP, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(INOUT) :: AP(:)
           REAL(WP), INTENT(OUT) :: W(:)
           COMPLEX(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE CHPEVD_F95

      END INTERFACE

      INTERFACE LA_SPEV

       SUBROUTINE SSPEV_F95( AP, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: AP(:)
           REAL(WP), INTENT(OUT) :: W(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE SSPEV_F95

       SUBROUTINE DSPEV_F95( AP, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: AP(:)
           REAL(WP), INTENT(OUT) :: W(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE DSPEV_F95

      END INTERFACE

      INTERFACE LA_HPEV

       SUBROUTINE CHPEV_F95( AP, W, UPLO, Z, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(INOUT) :: AP(:)
           REAL(WP), INTENT(OUT) :: W(:)
           COMPLEX(WP), INTENT(OUT), OPTIONAL :: Z(:,:)
        END SUBROUTINE CHPEV_F95

      END INTERFACE

      INTERFACE LA_GGGLM

       SUBROUTINE SGGGLM_F95( A, B, D, X, Y, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:), D(:)
           REAL(WP), INTENT( OUT ) :: X(:), Y(:)
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE SGGGLM_F95

       SUBROUTINE DGGGLM_F95( A, B, D, X, Y, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:), D(:)
           REAL(WP), INTENT( OUT ) :: X(:), Y(:)
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE DGGGLM_F95

       SUBROUTINE CGGGLM_F95( A, B, D, X, Y, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           COMPLEX(WP), INTENT( INOUT ) :: A( :, : ), B(:,:), D(:)
           COMPLEX(WP), INTENT( OUT ) :: X(:), Y(:)
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE CGGGLM_F95

      END INTERFACE

      INTERFACE LA_GGLSE

       SUBROUTINE SGGLSE_F95( A, B, C, D, X, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:), C(:), D(:)
           REAL(WP), INTENT( OUT ) :: X(:)
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE SGGLSE_F95

       SUBROUTINE DGGLSE_F95( A, B, C, D, X, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:), C(:), D(:)
           REAL(WP), INTENT( OUT ) :: X(:)
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE DGGLSE_F95

       SUBROUTINE CGGLSE_F95( A, B, C, D, X, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           COMPLEX(WP), INTENT( INOUT ) :: A( :, : ), B(:,:), C(:), D(:)
           COMPLEX(WP), INTENT( OUT ) :: X(:)
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE CGGLSE_F95

      END INTERFACE

         INTERFACE LA_GELSY

       SUBROUTINE SGELSY_F95( A, B, RANK, JPVT, RCOND, INFO )
       USE LA_PRECISION, ONLY: WP => SP
       REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
       INTEGER, INTENT(OUT), OPTIONAL :: RANK
       INTEGER, INTENT(INOUT), OPTIONAL, TARGET :: JPVT(:)
       REAL(WP), INTENT(IN), OPTIONAL :: RCOND
       INTEGER, INTENT(OUT), OPTIONAL :: INFO
      END SUBROUTINE SGELSY_F95   

       SUBROUTINE SGELSY1_F95( A, B, RANK, JPVT, RCOND, INFO )
        USE LA_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
        INTEGER, INTENT(OUT), OPTIONAL :: RANK
        INTEGER, INTENT(INOUT), OPTIONAL, TARGET :: JPVT(:)
        REAL(WP), INTENT(IN), OPTIONAL :: RCOND
        INTEGER, INTENT(OUT), OPTIONAL :: INFO
       END SUBROUTINE SGELSY1_F95
       
       SUBROUTINE DGELSY_F95( A, B, RANK, JPVT, RCOND, INFO )
       USE LA_PRECISION, ONLY: WP => DP
       REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
       INTEGER, INTENT(OUT), OPTIONAL :: RANK
       INTEGER, INTENT(INOUT), OPTIONAL, TARGET :: JPVT(:)
       REAL(WP), INTENT(IN), OPTIONAL :: RCOND
       INTEGER, INTENT(OUT), OPTIONAL :: INFO
      END SUBROUTINE DGELSY_F95   

       SUBROUTINE DGELSY1_F95( A, B, RANK, JPVT, RCOND, INFO )
        USE LA_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
        INTEGER, INTENT(OUT), OPTIONAL :: RANK
        INTEGER, INTENT(INOUT), OPTIONAL, TARGET :: JPVT(:)
        REAL(WP), INTENT(IN), OPTIONAL :: RCOND
        INTEGER, INTENT(OUT), OPTIONAL :: INFO
       END SUBROUTINE DGELSY1_F95
       
       SUBROUTINE CGELSY_F95( A, B, RANK, JPVT, RCOND, INFO )
       USE LA_PRECISION, ONLY: WP => SP
       COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
       INTEGER, INTENT(OUT), OPTIONAL :: RANK
       INTEGER, INTENT(INOUT), OPTIONAL, TARGET :: JPVT(:)
       REAL(WP), INTENT(IN), OPTIONAL :: RCOND
       INTEGER, INTENT(OUT), OPTIONAL :: INFO
      END SUBROUTINE CGELSY_F95   

       SUBROUTINE CGELSY1_F95( A, B, RANK, JPVT, RCOND, INFO )
        USE LA_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:)
        INTEGER, INTENT(OUT), OPTIONAL :: RANK
        INTEGER, INTENT(INOUT), OPTIONAL, TARGET :: JPVT(:)
        REAL(WP), INTENT(IN), OPTIONAL :: RCOND
        INTEGER, INTENT(OUT), OPTIONAL :: INFO
       END SUBROUTINE CGELSY1_F95
       
      END INTERFACE 

        INTERFACE LA_GELSD

       SUBROUTINE SGELSD_F95( A, B, RANK, S, RCOND, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
         INTEGER, INTENT(IN), OPTIONAL :: RANK
         REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
         REAL(WP), INTENT(IN), OPTIONAL :: RCOND
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
       END SUBROUTINE SGELSD_F95
       
       SUBROUTINE SGELSD1_F95( A, B, RANK, S, RCOND, INFO )
        USE LA_PRECISION, ONLY: WP => SP
        REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:)
        INTEGER, INTENT(IN), OPTIONAL :: RANK
        REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
        REAL(WP), INTENT(IN), OPTIONAL :: RCOND
        INTEGER, INTENT(OUT), OPTIONAL :: INFO
       END SUBROUTINE SGELSD1_F95

       SUBROUTINE DGELSD_F95( A, B, RANK, S, RCOND, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
         INTEGER, INTENT(IN), OPTIONAL :: RANK
         REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
         REAL(WP), INTENT(IN), OPTIONAL :: RCOND
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
       END SUBROUTINE DGELSD_F95
       
       SUBROUTINE DGELSD1_F95( A, B, RANK, S, RCOND, INFO )
        USE LA_PRECISION, ONLY: WP => DP
        REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:)
        INTEGER, INTENT(IN), OPTIONAL :: RANK
        REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
        REAL(WP), INTENT(IN), OPTIONAL :: RCOND
        INTEGER, INTENT(OUT), OPTIONAL :: INFO
       END SUBROUTINE DGELSD1_F95

       SUBROUTINE CGELSD_F95( A, B, RANK, S, RCOND, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         COMPLEX(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
         INTEGER, INTENT(IN), OPTIONAL :: RANK
         REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
         REAL(WP), INTENT(IN), OPTIONAL :: RCOND
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
       END SUBROUTINE CGELSD_F95
       
       SUBROUTINE CGELSD1_F95( A, B, RANK, S, RCOND, INFO )
        USE LA_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT( INOUT ) :: A( :, : ), B(:)
        INTEGER, INTENT(IN), OPTIONAL :: RANK
        REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
        REAL(WP), INTENT(IN), OPTIONAL :: RCOND
        INTEGER, INTENT(OUT), OPTIONAL :: INFO
       END SUBROUTINE CGELSD1_F95

      END INTERFACE

      INTERFACE LA_GELSX

       SUBROUTINE SGELSX_F95( A, B, RANK, JPVT, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           INTEGER, INTENT(OUT), OPTIONAL :: JPVT(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE SGELSX_F95

       SUBROUTINE SGELSX1_F95( A, B, RANK, JPVT, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           INTEGER, INTENT(OUT), OPTIONAL :: JPVT(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE SGELSX1_F95

       SUBROUTINE DGELSX_F95( A, B, RANK, JPVT, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           INTEGER, INTENT(OUT), OPTIONAL :: JPVT(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE DGELSX_F95

       SUBROUTINE DGELSX1_F95( A, B, RANK, JPVT, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           INTEGER, INTENT(OUT), OPTIONAL :: JPVT(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE DGELSX1_F95

       SUBROUTINE CGELSX_F95( A, B, RANK, JPVT, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           COMPLEX(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           INTEGER, INTENT(OUT), OPTIONAL :: JPVT(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE CGELSX_F95

       SUBROUTINE CGELSX1_F95( A, B, RANK, JPVT, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           COMPLEX(WP), INTENT( INOUT ) :: A( :, : ), B(:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           INTEGER, INTENT(OUT), OPTIONAL :: JPVT(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE CGELSX1_F95

      END INTERFACE

      INTERFACE LA_GELSS

       SUBROUTINE SGELSS_F95( A, B, RANK, S, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE SGELSS_F95

       SUBROUTINE SGELSS1_F95( A, B, RANK, S, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE SGELSS1_F95

       SUBROUTINE DGELSS_F95( A, B, RANK, S, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE DGELSS_F95

       SUBROUTINE DGELSS1_F95( A, B, RANK, S, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE DGELSS1_F95

       SUBROUTINE CGELSS_F95( A, B, RANK, S, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           COMPLEX(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE CGELSS_F95

       SUBROUTINE CGELSS1_F95( A, B, RANK, S, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           COMPLEX(WP), INTENT( INOUT ) :: A( :, : ), B(:)
           INTEGER, INTENT(IN), OPTIONAL :: RANK
           REAL(WP), INTENT(OUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(IN), OPTIONAL :: RCOND
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
        END SUBROUTINE CGELSS1_F95

      END INTERFACE

      INTERFACE LA_GELS

       SUBROUTINE SGELS_F95( A, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
        END SUBROUTINE SGELS_F95

       SUBROUTINE SGELS1_F95( A, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:)
        END SUBROUTINE SGELS1_F95

       SUBROUTINE DGELS_F95( A, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
        END SUBROUTINE DGELS_F95

       SUBROUTINE DGELS1_F95( A, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT( INOUT ) :: A( :, : ), B(:)
        END SUBROUTINE DGELS1_F95

       SUBROUTINE CGELS_F95( A, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT( INOUT ) :: A( :, : ), B(:,:)
        END SUBROUTINE CGELS_F95

       SUBROUTINE CGELS1_F95( A, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT( INOUT ) :: A( :, : ), B(:)
        END SUBROUTINE CGELS1_F95

      END INTERFACE

      INTERFACE LA_SPSVX

       SUBROUTINE SSPSVX_F95( AP, B, X, UPLO, AFP, IPIV, FACT, FERR,    &
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(IN) :: AP(:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE SSPSVX_F95

       SUBROUTINE SSPSVX1_F95( AP, B, X, UPLO, AFP, IPIV, FACT, FERR,   &
     &                         BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(IN) :: AP(:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
        END SUBROUTINE SSPSVX1_F95

       SUBROUTINE DSPSVX_F95( AP, B, X, UPLO, AFP, IPIV, FACT, FERR,    &
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(IN) :: AP(:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE DSPSVX_F95

       SUBROUTINE DSPSVX1_F95( AP, B, X, UPLO, AFP, IPIV, FACT, FERR,   &
     &                         BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(IN) :: AP(:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
        END SUBROUTINE DSPSVX1_F95

       SUBROUTINE CSPSVX_F95( AP, B, X, UPLO, AFP, IPIV, FACT, FERR,    &
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           COMPLEX(WP), INTENT(IN) :: AP(:), B(:,:)
           COMPLEX(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE CSPSVX_F95

       SUBROUTINE CSPSVX1_F95( AP, B, X, UPLO, AFP, IPIV, FACT, FERR,   &
     &                         BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           COMPLEX(WP), INTENT(IN) :: AP(:), B(:)
           COMPLEX(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
        END SUBROUTINE CSPSVX1_F95

      END INTERFACE

      INTERFACE LA_HPSVX

       SUBROUTINE CHPSVX_F95( A, B, X, UPLO, AFP, IPIV, FACT, FERR,     &
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           COMPLEX(WP), INTENT(IN) :: A(:), B(:,:)
           COMPLEX(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE CHPSVX_F95

       SUBROUTINE CHPSVX1_F95( A, B, X, UPLO, AFP, IPIV, FACT, FERR,    &
     &                         BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           COMPLEX(WP), INTENT(IN) :: A(:), B(:)
           COMPLEX(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
        END SUBROUTINE CHPSVX1_F95

      END INTERFACE

      INTERFACE LA_SYSVX

       SUBROUTINE SSYSVX_F95( A, B, X, UPLO, AF, IPIV, FACT, FERR, BERR,&
     &                        RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(IN) :: A(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE SSYSVX_F95

       SUBROUTINE SSYSVX1_F95( A, B, X, UPLO, AF, IPIV, FACT, FERR,     &
     &                         BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(IN) :: A(:,:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
        END SUBROUTINE SSYSVX1_F95

       SUBROUTINE DSYSVX_F95( A, B, X, UPLO, AF, IPIV, FACT, FERR, BERR,&
     &                        RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(IN) :: A(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE DSYSVX_F95

       SUBROUTINE DSYSVX1_F95( A, B, X, UPLO, AF, IPIV, FACT, FERR,     &
     &                         BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(IN) :: A(:,:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
        END SUBROUTINE DSYSVX1_F95

       SUBROUTINE CSYSVX_F95( A, B, X, UPLO, AF, IPIV, FACT, FERR, BERR,&
     &                        RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           COMPLEX(WP), INTENT(IN) :: A(:,:), B(:,:)
           COMPLEX(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE CSYSVX_F95

       SUBROUTINE CSYSVX1_F95( A, B, X, UPLO, AF, IPIV, FACT, FERR,     &
     &                         BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           COMPLEX(WP), INTENT(IN) :: A(:,:), B(:)
           COMPLEX(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
        END SUBROUTINE CSYSVX1_F95

      END INTERFACE

      INTERFACE LA_HESVX

       SUBROUTINE CHESVX_F95( A, B, X, UPLO, AF, IPIV, FACT, FERR, BERR,&
     &                        RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           COMPLEX(WP), INTENT(IN) :: A(:,:), B(:,:)
           COMPLEX(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE CHESVX_F95

       SUBROUTINE CHESVX1_F95( A, B, X, UPLO, AF, IPIV, FACT, FERR,     &
     &                         BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           COMPLEX(WP), INTENT(IN) :: A(:,:), B(:)
           COMPLEX(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
        END SUBROUTINE CHESVX1_F95

      END INTERFACE

      INTERFACE LA_PTSVX

       SUBROUTINE SPTSVX_F95( D, E, B, X, DF, EF, FACT, FERR, BERR,     &
     &                        RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(IN) :: D(:)
           REAL(WP), INTENT(IN) :: E(:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: DF(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: EF(:)
        END SUBROUTINE SPTSVX_F95

       SUBROUTINE SPTSVX1_F95( D, E, B, X, DF, EF, FACT, FERR, BERR,    &
     &                         RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(IN) :: D(:)
           REAL(WP), INTENT(IN) :: E(:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: DF(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: EF(:)
        END SUBROUTINE SPTSVX1_F95

       SUBROUTINE DPTSVX_F95( D, E, B, X, DF, EF, FACT, FERR, BERR,     &
     &                        RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(IN) :: D(:)
           REAL(WP), INTENT(IN) :: E(:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: DF(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: EF(:)
        END SUBROUTINE DPTSVX_F95

       SUBROUTINE DPTSVX1_F95( D, E, B, X, DF, EF, FACT, FERR, BERR,    &
     &                         RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(IN) :: D(:)
           REAL(WP), INTENT(IN) :: E(:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: DF(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: EF(:)
        END SUBROUTINE DPTSVX1_F95

       SUBROUTINE CPTSVX_F95( D, E, B, X, DF, EF, FACT, FERR, BERR,     &
     &                        RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(IN) :: D(:)
           COMPLEX(WP), INTENT(IN) :: E(:), B(:,:)
           COMPLEX(WP), INTENT(OUT) :: X(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: DF(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: EF(:)
        END SUBROUTINE CPTSVX_F95

       SUBROUTINE CPTSVX1_F95( D, E, B, X, DF, EF, FACT, FERR, BERR,    &
     &                         RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(IN) :: D(:)
           COMPLEX(WP), INTENT(IN) :: E(:), B(:)
           COMPLEX(WP), INTENT(OUT) :: X(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: DF(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: EF(:)
        END SUBROUTINE CPTSVX1_F95

      END INTERFACE

      INTERFACE LA_PBSVX

       SUBROUTINE SPBSVX_F95( AB, B, X, UPLO, AFB, FACT, EQUED, S, FERR,&
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(INOUT) :: AB(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
        END SUBROUTINE SPBSVX_F95

       SUBROUTINE SPBSVX1_F95( AB, B, X, UPLO, AFB, FACT, EQUED, S,     &
     &                         FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(INOUT) :: AB(:,:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
        END SUBROUTINE SPBSVX1_F95

       SUBROUTINE DPBSVX_F95( AB, B, X, UPLO, AFB, FACT, EQUED, S, FERR,&
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(INOUT) :: AB(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
        END SUBROUTINE DPBSVX_F95

       SUBROUTINE DPBSVX1_F95( AB, B, X, UPLO, AFB, FACT, EQUED, S,     &
     &                         FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(INOUT) :: AB(:,:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
        END SUBROUTINE DPBSVX1_F95

       SUBROUTINE CPBSVX_F95( AB, B, X, UPLO, AFB, FACT, EQUED, S, FERR,&
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           COMPLEX(WP), INTENT(INOUT) :: AB(:,:), B(:,:)
           COMPLEX(WP), INTENT(OUT) :: X(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
        END SUBROUTINE CPBSVX_F95

       SUBROUTINE CPBSVX1_F95( AB, B, X, UPLO, AFB, FACT, EQUED, S,     &
     &                         FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           COMPLEX(WP), INTENT(INOUT) :: AB(:,:), B(:)
           COMPLEX(WP), INTENT(OUT) :: X(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
        END SUBROUTINE CPBSVX1_F95

      END INTERFACE

      INTERFACE LA_PPSVX

       SUBROUTINE SPPSVX_F95( AP, B, X, UPLO, AFP, FACT, EQUED, S, FERR,&
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(INOUT) :: AP(:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE SPPSVX_F95

       SUBROUTINE SPPSVX1_F95( AP, B, X, UPLO, AFP, FACT, EQUED, S,     &
     &                         FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(INOUT) :: AP(:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
        END SUBROUTINE SPPSVX1_F95

       SUBROUTINE DPPSVX_F95( AP, B, X, UPLO, AFP, FACT, EQUED, S, FERR,&
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(INOUT) :: AP(:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE DPPSVX_F95

       SUBROUTINE DPPSVX1_F95( AP, B, X, UPLO, AFP, FACT, EQUED, S,     &
     &                         FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(INOUT) :: AP(:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
        END SUBROUTINE DPPSVX1_F95

       SUBROUTINE CPPSVX_F95( AP, B, X, UPLO, AFP, FACT, EQUED, S, FERR,&
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           COMPLEX(WP), INTENT(INOUT) :: AP(:), B(:,:)
           COMPLEX(WP), INTENT(OUT) :: X(:,:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE CPPSVX_F95

       SUBROUTINE CPPSVX1_F95( AP, B, X, UPLO, AFP, FACT, EQUED, S,     &
     &                         FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           COMPLEX(WP), INTENT(INOUT) :: AP(:), B(:)
           COMPLEX(WP), INTENT(OUT) :: X(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AFP(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
        END SUBROUTINE CPPSVX1_F95

      END INTERFACE

      INTERFACE LA_POSVX

       SUBROUTINE SPOSVX_F95( A, B, X, UPLO, AF, FACT, EQUED, S, FERR,  &
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE SPOSVX_F95

       SUBROUTINE SPOSVX1_F95( A, B, X, UPLO, AF, FACT, EQUED, S, FERR, &
     &                         BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
        END SUBROUTINE SPOSVX1_F95

       SUBROUTINE DPOSVX_F95( A, B, X, UPLO, AF, FACT, EQUED, S, FERR,  &
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE DPOSVX_F95

       SUBROUTINE DPOSVX1_F95( A, B, X, UPLO, AF, FACT, EQUED, S, FERR, &
     &                         BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
        END SUBROUTINE DPOSVX1_F95

       SUBROUTINE CPOSVX_F95( A, B, X, UPLO, AF, FACT, EQUED, S, FERR,  &
     &                        BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           COMPLEX(WP), INTENT(OUT) :: X(:,:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE CPOSVX_F95

       SUBROUTINE CPOSVX1_F95( A, B, X, UPLO, AF, FACT, EQUED, S, FERR, &
     &                         BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:)
           COMPLEX(WP), INTENT(OUT) :: X(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: S(:)
        END SUBROUTINE CPOSVX1_F95

      END INTERFACE

      INTERFACE LA_GTSVX

       SUBROUTINE SGTSVX_F95( DL, D, DU, B, X, DLF, DF, DUF, DU2, IPIV, &
     &                        FACT, TRANS, FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(IN) :: DL(:), D(:), DU(:), B(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: DLF(:), DF(:), DUF(:),  &
     &                                          DU2(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
           REAL(WP), INTENT(OUT) :: X(:,:)
        END SUBROUTINE SGTSVX_F95

       SUBROUTINE SGTSVX1_F95( DL, D, DU, B, X, DLF, DF, DUF, DU2, IPIV,&
     &                         FACT, TRANS, FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(IN) :: DL(:), D(:), DU(:), B(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: DLF(:), DF(:), DUF(:),  &
     &                                          DU2(:)
           REAL(WP), INTENT(OUT) :: X(:)
        END SUBROUTINE SGTSVX1_F95

       SUBROUTINE DGTSVX_F95( DL, D, DU, B, X, DLF, DF, DUF, DU2, IPIV, &
     &                        FACT, TRANS, FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(IN) :: DL(:), D(:), DU(:), B(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: DLF(:), DF(:), DUF(:),  &
     &                                          DU2(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
           REAL(WP), INTENT(OUT) :: X(:,:)
        END SUBROUTINE DGTSVX_F95

       SUBROUTINE DGTSVX1_F95( DL, D, DU, B, X, DLF, DF, DUF, DU2, IPIV,&
     &                         FACT, TRANS, FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           REAL(WP), INTENT(IN) :: DL(:), D(:), DU(:), B(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: DLF(:), DF(:), DUF(:),  &
     &                                          DU2(:)
           REAL(WP), INTENT(OUT) :: X(:)
        END SUBROUTINE DGTSVX1_F95

       SUBROUTINE CGTSVX_F95( DL, D, DU, B, X, DLF, DF, DUF, DU2, IPIV, &
     &                        FACT, TRANS, FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           COMPLEX(WP), INTENT(IN) :: DL(:), D(:), DU(:), B(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: DLF(:), DF(:),       &
     &                                             DUF(:), DU2(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
           COMPLEX(WP), INTENT(OUT) :: X(:,:)
        END SUBROUTINE CGTSVX_F95

       SUBROUTINE CGTSVX1_F95( DL, D, DU, B, X, DLF, DF, DUF, DU2, IPIV,&
     &                         FACT, TRANS, FERR, BERR, RCOND, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, FERR, BERR
           COMPLEX(WP), INTENT(IN) :: DL(:), D(:), DU(:), B(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: DLF(:), DF(:),       &
     &                                             DUF(:), DU2(:)
           COMPLEX(WP), INTENT(OUT) :: X(:)
        END SUBROUTINE CGTSVX1_F95

      END INTERFACE

      INTERFACE LA_GBSVX

       SUBROUTINE SGBSVX_F95( A, B, X, KL, AFB, IPIV, FACT, TRANS,      &
     &                        EQUED, R, C, FERR, BERR, RCOND, RPVGRW,   &
     &                        INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(IN), OPTIONAL :: KL
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE SGBSVX_F95

       SUBROUTINE SGBSVX1_F95( A, B, X, KL, AFB, IPIV, FACT, TRANS,     &
     &                         EQUED, R, C, FERR, BERR, RCOND, RPVGRW,  &
     &                         INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(IN), OPTIONAL :: KL
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW, FERR, BERR
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
        END SUBROUTINE SGBSVX1_F95

       SUBROUTINE DGBSVX_F95( A, B, X, KL, AFB, IPIV, FACT, TRANS,      &
     &                        EQUED, R, C, FERR, BERR, RCOND, RPVGRW,   &
     &                        INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(IN), OPTIONAL :: KL
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE DGBSVX_F95

       SUBROUTINE DGBSVX1_F95( A, B, X, KL, AFB, IPIV, FACT, TRANS,     &
     &                         EQUED, R, C, FERR, BERR, RCOND, RPVGRW,  &
     &                         INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(IN), OPTIONAL :: KL
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW, FERR, BERR
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
        END SUBROUTINE DGBSVX1_F95

       SUBROUTINE CGBSVX_F95( A, B, X, KL, AFB, IPIV, FACT, TRANS,      &
     &                        EQUED, R, C, FERR, BERR, RCOND, RPVGRW,   &
     &                        INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(IN), OPTIONAL :: KL
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW
           COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           COMPLEX(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE CGBSVX_F95

       SUBROUTINE CGBSVX1_F95( A, B, X, KL, AFB, IPIV, FACT, TRANS,     &
     &                         EQUED, R, C, FERR, BERR, RCOND, RPVGRW,  &
     &                         INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(IN), OPTIONAL :: KL
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW, FERR, BERR
           COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:)
           COMPLEX(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AFB(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
        END SUBROUTINE CGBSVX1_F95

      END INTERFACE

      INTERFACE LA_SPSV

       SUBROUTINE SSPSV_F95( AP, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: AP(:), B(:,:)
        END SUBROUTINE SSPSV_F95

       SUBROUTINE SSPSV1_F95( AP, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: AP(:), B(:)
        END SUBROUTINE SSPSV1_F95

       SUBROUTINE DSPSV_F95( AP, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: AP(:), B(:,:)
        END SUBROUTINE DSPSV_F95

       SUBROUTINE DSPSV1_F95( AP, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: AP(:), B(:)
        END SUBROUTINE DSPSV1_F95

       SUBROUTINE CSPSV_F95( AP, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: AP(:), B(:,:)
        END SUBROUTINE CSPSV_F95

       SUBROUTINE CSPSV1_F95( AP, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: AP(:), B(:)
        END SUBROUTINE CSPSV1_F95

      END INTERFACE

      INTERFACE LA_HPSV

       SUBROUTINE CHPSV_F95( AP, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: AP(:), B(:,:)
        END SUBROUTINE CHPSV_F95

       SUBROUTINE CHPSV1_F95( AP, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: AP(:), B(:)
        END SUBROUTINE CHPSV1_F95

      END INTERFACE

      INTERFACE LA_SYSV

       SUBROUTINE SSYSV_F95( A, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE SSYSV_F95

       SUBROUTINE SSYSV1_F95( A, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE SSYSV1_F95

       SUBROUTINE DSYSV_F95( A, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE DSYSV_F95

       SUBROUTINE DSYSV1_F95( A, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE DSYSV1_F95

       SUBROUTINE CSYSV_F95( A, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE CSYSV_F95

       SUBROUTINE CSYSV1_F95( A, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE CSYSV1_F95

      END INTERFACE

      INTERFACE LA_HESV

       SUBROUTINE CHESV_F95( A, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE CHESV_F95

       SUBROUTINE CHESV1_F95( A, B, UPLO, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE CHESV1_F95

      END INTERFACE

      INTERFACE LA_PTSV

       SUBROUTINE SPTSV_F95( D, E, B, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: D(:)
            REAL(WP), INTENT(INOUT) :: E(:), B(:,:)
        END SUBROUTINE SPTSV_F95

       SUBROUTINE SPTSV1_F95( D, E, B, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: D(:)
            REAL(WP), INTENT(INOUT) :: E(:), B(:)
        END SUBROUTINE SPTSV1_F95

       SUBROUTINE DPTSV_F95( D, E, B, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: D(:)
            REAL(WP), INTENT(INOUT) :: E(:), B(:,:)
        END SUBROUTINE DPTSV_F95

       SUBROUTINE DPTSV1_F95( D, E, B, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: D(:)
            REAL(WP), INTENT(INOUT) :: E(:), B(:)
        END SUBROUTINE DPTSV1_F95

       SUBROUTINE CPTSV_F95( D, E, B, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: D(:)
            COMPLEX(WP), INTENT(INOUT) :: E(:), B(:,:)
        END SUBROUTINE CPTSV_F95

       SUBROUTINE CPTSV1_F95( D, E, B, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: D(:)
            COMPLEX(WP), INTENT(INOUT) :: E(:), B(:)
        END SUBROUTINE CPTSV1_F95

      END INTERFACE

      INTERFACE LA_PBSV

       SUBROUTINE SPBSV_F95( AB, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: AB(:,:), B(:,:)
        END SUBROUTINE SPBSV_F95

       SUBROUTINE SPBSV1_F95( AB, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: AB(:,:), B(:)
        END SUBROUTINE SPBSV1_F95

       SUBROUTINE DPBSV_F95( AB, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: AB(:,:), B(:,:)
        END SUBROUTINE DPBSV_F95

       SUBROUTINE DPBSV1_F95( AB, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: AB(:,:), B(:)
        END SUBROUTINE DPBSV1_F95

       SUBROUTINE CPBSV_F95( AB, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            COMPLEX(WP), INTENT(INOUT) :: AB(:,:), B(:,:)
        END SUBROUTINE CPBSV_F95

       SUBROUTINE CPBSV1_F95( AB, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            COMPLEX(WP), INTENT(INOUT) :: AB(:,:), B(:)
        END SUBROUTINE CPBSV1_F95

      END INTERFACE

      INTERFACE LA_PPSV

       SUBROUTINE SPPSV_F95( AP, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: AP(:), B(:,:)
        END SUBROUTINE SPPSV_F95

       SUBROUTINE SPPSV1_F95( AP, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: AP(:), B(:)
        END SUBROUTINE SPPSV1_F95

       SUBROUTINE DPPSV_F95( AP, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: AP(:), B(:,:)
        END SUBROUTINE DPPSV_F95

       SUBROUTINE DPPSV1_F95( AP, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: AP(:), B(:)
        END SUBROUTINE DPPSV1_F95

       SUBROUTINE CPPSV_F95( AP, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            COMPLEX(WP), INTENT(INOUT) :: AP(:), B(:,:)
        END SUBROUTINE CPPSV_F95

       SUBROUTINE CPPSV1_F95( AP, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            COMPLEX(WP), INTENT(INOUT) :: AP(:), B(:)
        END SUBROUTINE CPPSV1_F95

      END INTERFACE

      INTERFACE LA_POSV

       SUBROUTINE SPOSV_F95( A, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE SPOSV_F95

       SUBROUTINE SPOSV1_F95( A, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE SPOSV1_F95

       SUBROUTINE DPOSV_F95( A, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE DPOSV_F95

       SUBROUTINE DPOSV1_F95( A, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE DPOSV1_F95

       SUBROUTINE CPOSV_F95( A, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE CPOSV_F95

       SUBROUTINE CPOSV1_F95( A, B, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE CPOSV1_F95

      END INTERFACE

      INTERFACE LA_GTSV

       SUBROUTINE SGTSV_F95( DL, D, DU, B, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: DL(:), D(:), DU(:), B(:,:)
        END SUBROUTINE SGTSV_F95

       SUBROUTINE SGTSV1_F95( DL, D, DU, B, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: DL(:), D(:), DU(:), B(:)
        END SUBROUTINE SGTSV1_F95

       SUBROUTINE DGTSV_F95( DL, D, DU, B, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: DL(:), D(:), DU(:), B(:,:)
        END SUBROUTINE DGTSV_F95

       SUBROUTINE DGTSV1_F95( DL, D, DU, B, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            REAL(WP), INTENT(INOUT) :: DL(:), D(:), DU(:), B(:)
        END SUBROUTINE DGTSV1_F95

       SUBROUTINE CGTSV_F95( DL, D, DU, B, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            COMPLEX(WP), INTENT(INOUT) :: DL(:), D(:), DU(:), B(:,:)
        END SUBROUTINE CGTSV_F95

       SUBROUTINE CGTSV1_F95( DL, D, DU, B, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            COMPLEX(WP), INTENT(INOUT) :: DL(:), D(:), DU(:), B(:)
        END SUBROUTINE CGTSV1_F95

      END INTERFACE

      INTERFACE LA_GBSV

       SUBROUTINE SGBSV_F95( AB, B, KL, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(IN), OPTIONAL :: KL
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: AB(:,:), B(:,:)
        END SUBROUTINE SGBSV_F95

       SUBROUTINE SGBSV1_F95( AB, B, KL, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(IN), OPTIONAL :: KL
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: AB(:,:), B(:)
        END SUBROUTINE SGBSV1_F95

       SUBROUTINE DGBSV_F95( AB, B, KL, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            INTEGER, INTENT(IN), OPTIONAL :: KL
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: AB(:,:), B(:,:)
        END SUBROUTINE DGBSV_F95

       SUBROUTINE DGBSV1_F95( AB, B, KL, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            INTEGER, INTENT(IN), OPTIONAL :: KL
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: AB(:,:), B(:)
        END SUBROUTINE DGBSV1_F95

       SUBROUTINE CGBSV_F95( AB, B, KL, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(IN), OPTIONAL :: KL
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: AB(:,:), B(:,:)
        END SUBROUTINE CGBSV_F95

       SUBROUTINE CGBSV1_F95( AB, B, KL, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(IN), OPTIONAL :: KL
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: AB(:,:), B(:)
        END SUBROUTINE CGBSV1_F95

      END INTERFACE

      INTERFACE LA_GESV

       SUBROUTINE SGESV_F95( A, B, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE SGESV_F95

       SUBROUTINE SGESV1_F95( A, B, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE SGESV1_F95

       SUBROUTINE DGESV_F95( A, B, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE DGESV_F95

       SUBROUTINE DGESV1_F95( A, B, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE DGESV1_F95

       SUBROUTINE CGESV_F95( A, B, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE CGESV_F95

       SUBROUTINE CGESV1_F95( A, B, IPIV, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
            INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
            COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE CGESV1_F95

      END INTERFACE

      INTERFACE LA_GESVX

       SUBROUTINE SGESVX_F95( A, B, X, AF, IPIV, FACT, TRANS, EQUED, R, &
     &                        C, FERR, BERR, RCOND, RPVGRW, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE SGESVX_F95

       SUBROUTINE SGESVX1_F95( A, B, X, AF, IPIV, FACT, TRANS, EQUED, R,&
     &                         C, FERR, BERR, RCOND, RPVGRW, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR, BERR
        END SUBROUTINE SGESVX1_F95

       SUBROUTINE DGESVX_F95( A, B, X, AF, IPIV, FACT, TRANS, EQUED, R, &
     &                        C, FERR, BERR, RCOND, RPVGRW, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE DGESVX_F95

       SUBROUTINE DGESVX1_F95( A, B, X, AF, IPIV, FACT, TRANS, EQUED, R,&
     &                         C, FERR, BERR, RCOND, RPVGRW, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
           REAL(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR, BERR
        END SUBROUTINE DGESVX1_F95

       SUBROUTINE CGESVX_F95( A, B, X, AF, IPIV, FACT, TRANS, EQUED, R, &
     &                        C, FERR, BERR, RCOND, RPVGRW, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW
           COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           COMPLEX(WP), INTENT(OUT) :: X(:,:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE CGESVX_F95

       SUBROUTINE CGESVX1_F95( A, B, X, AF, IPIV, FACT, TRANS, EQUED, R,&
     &                         C, FERR, BERR, RCOND, RPVGRW, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS, FACT
           CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND, RPVGRW
           COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:)
           COMPLEX(WP), INTENT(OUT) :: X(:)
           INTEGER, INTENT(INOUT), OPTIONAL :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT), OPTIONAL :: AF(:,:)
           REAL(WP), INTENT(INOUT), OPTIONAL :: C(:), R(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR, BERR
        END SUBROUTINE CGESVX1_F95

      END INTERFACE

      INTERFACE LA_GETRF

       SUBROUTINE SGETRF_F95( A, IPIV, RCOND, NORM, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT( OUT ), OPTIONAL :: RCOND
           INTEGER, INTENT( OUT ), OPTIONAL :: IPIV( : )
           REAL(WP), INTENT( INOUT ) :: A( :, : )
        END SUBROUTINE SGETRF_F95

       SUBROUTINE DGETRF_F95( A, IPIV, RCOND, NORM, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT( OUT ), OPTIONAL :: RCOND
           INTEGER, INTENT( OUT ), OPTIONAL :: IPIV( : )
           REAL(WP), INTENT( INOUT ) :: A( :, : )
        END SUBROUTINE DGETRF_F95

       SUBROUTINE CGETRF_F95( A, IPIV, RCOND, NORM, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT( OUT ), OPTIONAL :: RCOND
           INTEGER, INTENT( OUT ), OPTIONAL :: IPIV( : )
           COMPLEX(WP), INTENT( INOUT ) :: A( :, : )
        END SUBROUTINE CGETRF_F95

      END INTERFACE

      INTERFACE LA_GETRS

       SUBROUTINE SGETRS_F95( A, IPIV, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE SGETRS_F95

       SUBROUTINE SGETRS1_F95( A, IPIV, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE SGETRS1_F95

       SUBROUTINE DGETRS_F95( A, IPIV, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE DGETRS_F95

       SUBROUTINE DGETRS1_F95( A, IPIV, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE DGETRS1_F95

       SUBROUTINE CGETRS_F95( A, IPIV, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
        END SUBROUTINE CGETRS_F95

       SUBROUTINE CGETRS1_F95( A, IPIV, B, TRANS, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:)
        END SUBROUTINE CGETRS1_F95

      END INTERFACE

      INTERFACE LA_GETRI

       SUBROUTINE SGETRI_F95( A, IPIV, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           REAL(WP), INTENT(INOUT) :: A(:,:)
        END SUBROUTINE SGETRI_F95

       SUBROUTINE DGETRI_F95( A, IPIV, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           REAL(WP), INTENT(INOUT) :: A(:,:)
        END SUBROUTINE DGETRI_F95

       SUBROUTINE CGETRI_F95( A, IPIV, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        END SUBROUTINE CGETRI_F95

      END INTERFACE

      INTERFACE LA_GERFS

       SUBROUTINE SGERFS_F95( A, AF, IPIV, B, X, TRANS, FERR, BERR,     &
     &                        INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           REAL(WP), INTENT(IN) :: A(:,:), AF(:,:), B(:,:)
           REAL(WP), INTENT(INOUT) :: X(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE SGERFS_F95

       SUBROUTINE SGERFS1_F95( A, AF, IPIV, B, X, TRANS, FERR, BERR,    &
     &                         INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           REAL(WP), INTENT(IN) :: A(:,:), AF(:,:)
           REAL(WP), INTENT(IN) :: B(:)
           REAL(WP), INTENT(INOUT) :: X(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR, BERR
        END SUBROUTINE SGERFS1_F95

       SUBROUTINE DGERFS_F95( A, AF, IPIV, B, X, TRANS, FERR, BERR,     &
     &                        INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           REAL(WP), INTENT(IN) :: A(:,:), AF(:,:), B(:,:)
           REAL(WP), INTENT(INOUT) :: X(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE DGERFS_F95

       SUBROUTINE DGERFS1_F95( A, AF, IPIV, B, X, TRANS, FERR, BERR,    &
     &                         INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           REAL(WP), INTENT(IN) :: A(:,:), AF(:,:)
           REAL(WP), INTENT(IN) :: B(:)
           REAL(WP), INTENT(INOUT) :: X(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR, BERR
        END SUBROUTINE DGERFS1_F95

       SUBROUTINE CGERFS_F95( A, AF, IPIV, B, X, TRANS, FERR, BERR,     &
     &                        INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           COMPLEX(WP), INTENT(IN) :: A(:,:), AF(:,:), B(:,:)
           COMPLEX(WP), INTENT(INOUT) :: X(:,:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR(:), BERR(:)
        END SUBROUTINE CGERFS_F95

       SUBROUTINE CGERFS1_F95( A, AF, IPIV, B, X, TRANS, FERR, BERR,    &
     &                         INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           INTEGER, INTENT(IN) :: IPIV(:)
           COMPLEX(WP), INTENT(IN) :: A(:,:), AF(:,:)
           COMPLEX(WP), INTENT(IN) :: B(:)
           COMPLEX(WP), INTENT(INOUT) :: X(:)
           REAL(WP), INTENT(OUT), OPTIONAL :: FERR, BERR
        END SUBROUTINE CGERFS1_F95

      END INTERFACE

      INTERFACE LA_GEEQU

       SUBROUTINE SGEEQU_F95( A, R, C, ROWCND, COLCND, AMAX, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT( OUT ), OPTIONAL :: AMAX, COLCND, ROWCND
           REAL(WP), INTENT( IN ) :: A( :, : )
           REAL(WP), INTENT( OUT ) :: C( : ), R( : )
        END SUBROUTINE SGEEQU_F95

       SUBROUTINE DGEEQU_F95( A, R, C, ROWCND, COLCND, AMAX, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT( OUT ), OPTIONAL :: AMAX, COLCND, ROWCND
           REAL(WP), INTENT( IN ) :: A( :, : )
           REAL(WP), INTENT( OUT ) :: C( : ), R( : )
        END SUBROUTINE DGEEQU_F95

       SUBROUTINE CGEEQU_F95( A, R, C, ROWCND, COLCND, AMAX, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT( OUT ), OPTIONAL :: AMAX, COLCND, ROWCND
           COMPLEX(WP), INTENT( IN ) :: A( :, : )
           REAL(WP), INTENT( OUT ) :: C( : ), R( : )
        END SUBROUTINE CGEEQU_F95

      END INTERFACE

      INTERFACE LA_SYEV

       SUBROUTINE SSYEV_F95( A, W, JOBZ, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE SSYEV_F95

       SUBROUTINE DSYEV_F95( A, W, JOBZ, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE DSYEV_F95

      END INTERFACE

      INTERFACE LA_HEEV

       SUBROUTINE CHEEV_F95( A, W, JOBZ, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE CHEEV_F95

      END INTERFACE

      INTERFACE LA_SYEVD

       SUBROUTINE SSYEVD_F95( A, W, JOBZ, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE SSYEVD_F95

       SUBROUTINE DSYEVD_F95( A, W, JOBZ, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE DSYEVD_F95

      END INTERFACE

      INTERFACE LA_HEEVD

       SUBROUTINE CHEEVD_F95( A, W, JOBZ, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE CHEEVD_F95

      END INTERFACE

        INTERFACE LA_SYEVR

       SUBROUTINE SSYEVR_F95( A, W, JOBZ, UPLO, VL, VU, IL, IU, M,      &
     &                        ISUPPZ, ABSTOL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP), INTENT(INOUT) :: A(:,:)
         REAL(WP), INTENT(OUT) :: W(:)
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  JOBZ, UPLO
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
         INTEGER, INTENT(IN), OPTIONAL :: IL, IU
         INTEGER, INTENT(OUT), OPTIONAL :: M
         INTEGER, INTENT(OUT), OPTIONAL, TARGET :: ISUPPZ(:)
       END SUBROUTINE SSYEVR_F95   

       SUBROUTINE DSYEVR_F95( A, W, JOBZ, UPLO, VL, VU, IL, IU, M,      &
     &                        ISUPPZ, ABSTOL, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         REAL(WP), INTENT(INOUT) :: A(:,:)
         REAL(WP), INTENT(OUT) :: W(:)
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  JOBZ, UPLO
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
         INTEGER, INTENT(IN), OPTIONAL :: IL, IU
         INTEGER, INTENT(OUT), OPTIONAL :: M
         INTEGER, INTENT(OUT), OPTIONAL, TARGET :: ISUPPZ(:)
       END SUBROUTINE DSYEVR_F95   

       END INTERFACE
	   
       INTERFACE LA_HEEVR
		 
       SUBROUTINE CHEEVR_F95( A, W, JOBZ, UPLO, VL, VU, IL, IU, M,      &
     &                        ISUPPZ, ABSTOL, INFO )
        USE LA_PRECISION, ONLY: WP => SP
        COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        REAL(WP), INTENT(OUT) :: W(:)
        CHARACTER(LEN=1), INTENT(IN), OPTIONAL ::  JOBZ, UPLO
        INTEGER, INTENT(OUT), OPTIONAL :: INFO
        REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
        INTEGER, INTENT(IN), OPTIONAL :: IL, IU
        INTEGER, INTENT(OUT), OPTIONAL :: M
        INTEGER, INTENT(OUT), OPTIONAL, TARGET :: ISUPPZ(:) 
       END SUBROUTINE CHEEVR_F95
       
      END INTERFACE

      INTERFACE LA_SYEVX

       SUBROUTINE SSYEVX_F95( A, W, JOBZ, UPLO, VL, VU, IL, IU, M,      &
     &                        IFAIL, ABSTOL, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(IN), OPTIONAL :: IL, IU
           INTEGER, INTENT(OUT), OPTIONAL :: INFO, M
           REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
           INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
           REAL(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE SSYEVX_F95

       SUBROUTINE DSYEVX_F95( A, W, JOBZ, UPLO, VL, VU, IL, IU, M,      &
     &                        IFAIL, ABSTOL, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(IN), OPTIONAL :: IL, IU
           INTEGER, INTENT(OUT), OPTIONAL :: INFO, M
           REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
           INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
           REAL(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE DSYEVX_F95

      END INTERFACE

      INTERFACE LA_HEEVX

       SUBROUTINE CHEEVX_F95( A, W, JOBZ, UPLO, VL, VU, IL, IU, M,      &
     &                        IFAIL, ABSTOL, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(IN), OPTIONAL :: IL, IU
           INTEGER, INTENT(OUT), OPTIONAL :: INFO, M
           REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
           INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
           COMPLEX(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE CHEEVX_F95

      END INTERFACE

      INTERFACE LA_SYGST

       SUBROUTINE SSYGST_F95( A, B, ITYPE, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(IN), OPTIONAL :: ITYPE
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(IN) :: B(:,:)
        END SUBROUTINE SSYGST_F95

       SUBROUTINE DSYGST_F95( A, B, ITYPE, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(IN), OPTIONAL :: ITYPE
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(IN) :: B(:,:)
        END SUBROUTINE DSYGST_F95

      END INTERFACE

      INTERFACE LA_HEGST

       SUBROUTINE CHEGST_F95( A, B, ITYPE, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(IN), OPTIONAL :: ITYPE
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(INOUT) :: A(:,:)
           COMPLEX(WP), INTENT(IN) :: B(:,:)
        END SUBROUTINE CHEGST_F95

      END INTERFACE

      INTERFACE LA_SYGV

       SUBROUTINE SSYGV_F95( A, B, W, ITYPE, JOBZ, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(IN), OPTIONAL :: ITYPE
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE SSYGV_F95

       SUBROUTINE DSYGV_F95( A, B, W, ITYPE, JOBZ, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(IN), OPTIONAL :: ITYPE
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE DSYGV_F95

      END INTERFACE

      INTERFACE LA_HEGV

       SUBROUTINE CHEGV_F95( A, B, W, ITYPE, JOBZ, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
           INTEGER, INTENT(IN), OPTIONAL :: ITYPE
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
           REAL(WP), INTENT(OUT) :: W(:)
        END SUBROUTINE CHEGV_F95

      END INTERFACE

         INTERFACE LA_SYGVX

        SUBROUTINE SSYGVX_F95( A, B, W, ITYPE, JOBZ, UPLO, VL, VU, IL,  &
     &                         IU, M, IFAIL, ABSTOL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
         REAL(WP), INTENT(OUT) :: W(:)
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
         INTEGER, INTENT(IN), OPTIONAL :: IL, IU, ITYPE
         INTEGER, INTENT(OUT), OPTIONAL :: M
         INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
        END SUBROUTINE SSYGVX_F95
	
        SUBROUTINE DSYGVX_F95( A, B, W, ITYPE, JOBZ, UPLO, VL, VU, IL,  &
     &                         IU, M, IFAIL, ABSTOL, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
         REAL(WP), INTENT(OUT) :: W(:)
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
         INTEGER, INTENT(IN), OPTIONAL :: IL, IU, ITYPE
         INTEGER, INTENT(OUT), OPTIONAL :: M
         INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
        END SUBROUTINE DSYGVX_F95
	
       END INTERFACE
		  
       INTERFACE LA_HEGVX

       SUBROUTINE CHEGVX_F95( A, B, W, ITYPE, JOBZ, UPLO, VL, VU, IL,   &
     &                        IU, M, IFAIL, ABSTOL, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
         REAL(WP), INTENT(OUT) :: W(:)
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(IN), OPTIONAL :: ABSTOL, VL, VU
         INTEGER, INTENT(IN), OPTIONAL :: IL, IU, ITYPE
         INTEGER, INTENT(OUT), OPTIONAL :: M
         INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAIL(:)
        END SUBROUTINE  CHEGVX_F95

        END INTERFACE

         INTERFACE LA_SYGVD

        SUBROUTINE SSYGVD_F95( A, B, W, ITYPE, JOBZ, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => SP
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
            REAL(WP), INTENT(OUT) :: W(:)
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
            INTEGER, INTENT(IN), OPTIONAL :: ITYPE 
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
           END SUBROUTINE SSYGVD_F95

        SUBROUTINE DSYGVD_F95( A, B, W, ITYPE, JOBZ, UPLO, INFO )
            USE LA_PRECISION, ONLY: WP => DP
            REAL(WP), INTENT(INOUT) :: A(:,:), B(:,:)
            REAL(WP), INTENT(OUT) :: W(:)
            CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
            INTEGER, INTENT(IN), OPTIONAL :: ITYPE 
            INTEGER, INTENT(OUT), OPTIONAL :: INFO
           END SUBROUTINE DSYGVD_F95

            END INTERFACE
	    
          INTERFACE LA_HEGVD
	  
       SUBROUTINE CHEGVD_F95( A, B, W, ITYPE, JOBZ, UPLO, INFO )
          USE LA_PRECISION, ONLY: WP => SP
          COMPLEX(WP), INTENT(INOUT) :: A(:,:), B(:,:)
          REAL(WP), INTENT(OUT) :: W(:)
          CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
          INTEGER, INTENT(OUT), OPTIONAL :: INFO
          INTEGER, INTENT(IN), OPTIONAL :: ITYPE
         END SUBROUTINE CHEGVD_F95   


        END INTERFACE
      
      INTERFACE LA_SYTRD

       SUBROUTINE SSYTRD_F95( A, TAU, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(OUT) :: TAU(:)
        END SUBROUTINE SSYTRD_F95

       SUBROUTINE DSYTRD_F95( A, TAU, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(INOUT) :: A(:,:)
           REAL(WP), INTENT(OUT) :: TAU(:)
        END SUBROUTINE DSYTRD_F95

      END INTERFACE

      INTERFACE LA_HETRD

       SUBROUTINE CHETRD_F95( A, TAU, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(INOUT) :: A(:,:)
           COMPLEX(WP), INTENT(OUT) :: TAU(:)
        END SUBROUTINE CHETRD_F95

      END INTERFACE

      INTERFACE LA_ORGTR

       SUBROUTINE SORGTR_F95( A, TAU, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(IN) :: TAU(:)
           REAL(WP), INTENT(INOUT) :: A(:,:)
        END SUBROUTINE SORGTR_F95

       SUBROUTINE DORGTR_F95( A, TAU, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           REAL(WP), INTENT(IN) :: TAU(:)
           REAL(WP), INTENT(INOUT) :: A(:,:)
        END SUBROUTINE DORGTR_F95

      END INTERFACE

      INTERFACE LA_UNGTR

       SUBROUTINE CUNGTR_F95( A, TAU, UPLO, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
           INTEGER, INTENT(OUT), OPTIONAL :: INFO
           COMPLEX(WP), INTENT(IN) :: TAU(:)
           COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        END SUBROUTINE CUNGTR_F95

      END INTERFACE

      INTERFACE LA_POTRF

       SUBROUTINE SPOTRF_F95( A, UPLO, RCOND, NORM, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM, UPLO
           INTEGER, INTENT(OUT), OPTIONAL  :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(INOUT) :: A(:,:)
        END SUBROUTINE SPOTRF_F95

       SUBROUTINE DPOTRF_F95( A, UPLO, RCOND, NORM, INFO )
           USE LA_PRECISION, ONLY: WP => DP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM, UPLO
           INTEGER, INTENT(OUT), OPTIONAL  :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           REAL(WP), INTENT(INOUT) :: A(:,:)
        END SUBROUTINE DPOTRF_F95

       SUBROUTINE CPOTRF_F95( A, UPLO, RCOND, NORM, INFO )
           USE LA_PRECISION, ONLY: WP => SP
           CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM, UPLO
           INTEGER, INTENT(OUT), OPTIONAL  :: INFO
           REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
           COMPLEX(WP), INTENT(INOUT) :: A(:,:)
        END SUBROUTINE CPOTRF_F95

      END INTERFACE

      INTERFACE LA_LAGGE

      SUBROUTINE SLAGGE_F95( A, KL, KU, D, ISEED, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN), OPTIONAL :: KL, KU
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         INTEGER, INTENT(INOUT), OPTIONAL, TARGET :: ISEED(4)
         REAL(WP), INTENT(IN), OPTIONAL, TARGET :: D(:)
         REAL(WP), INTENT(OUT) :: A(:,:)
      END SUBROUTINE SLAGGE_F95

      SUBROUTINE DLAGGE_F95( A, KL, KU, D, ISEED, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         INTEGER, INTENT(IN), OPTIONAL :: KL, KU
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         INTEGER, INTENT(INOUT), OPTIONAL, TARGET :: ISEED(4)
         REAL(WP), INTENT(IN), OPTIONAL, TARGET :: D(:)
         REAL(WP), INTENT(OUT) :: A(:,:)
      END SUBROUTINE DLAGGE_F95

      SUBROUTINE CLAGGE_F95( A, KL, KU, D, ISEED, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         INTEGER, INTENT(IN), OPTIONAL :: KL, KU
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         INTEGER, INTENT(INOUT), OPTIONAL, TARGET :: ISEED(4)
         REAL(WP), INTENT(IN), OPTIONAL, TARGET :: D(:)
         COMPLEX(WP), INTENT(OUT) :: A(:,:)
      END SUBROUTINE CLAGGE_F95

      END INTERFACE

      INTERFACE LA_LANGE

      FUNCTION SLANGE_F95( A, NORM, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP) :: SLANGE_F95
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(IN) :: A(:,:)
      END FUNCTION SLANGE_F95

      FUNCTION SLANGE1_F95( A, NORM, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP) :: SLANGE1_F95
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(IN) :: A(:)
      END FUNCTION SLANGE1_F95

      FUNCTION DLANGE_F95( A, NORM, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         REAL(WP) :: DLANGE_F95
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(IN) :: A(:,:)
      END FUNCTION DLANGE_F95

      FUNCTION DLANGE1_F95( A, NORM, INFO )
         USE LA_PRECISION, ONLY: WP => DP
         REAL(WP) :: DLANGE1_F95
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         REAL(WP), INTENT(IN) :: A(:)
      END FUNCTION DLANGE1_F95

      FUNCTION CLANGE_F95( A, NORM, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP) :: CLANGE_F95
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         COMPLEX(WP), INTENT(IN) :: A(:,:)
      END FUNCTION CLANGE_F95

      FUNCTION CLANGE1_F95( A, NORM, INFO )
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP) :: CLANGE1_F95
         CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM
         INTEGER, INTENT(OUT), OPTIONAL :: INFO
         COMPLEX(WP), INTENT(IN) :: A(:)
      END FUNCTION CLANGE1_F95

      END INTERFACE

      INTERFACE LA_GESDD
      
       SUBROUTINE SGESDD_F95(A, S, U, VT, WW, JOB, INFO )
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), OPTIONAL, INTENT(IN) :: JOB
      INTEGER, INTENT(OUT), OPTIONAL :: INFO
      REAL(WP), INTENT(INOUT) :: A(:,:)
      REAL(WP), INTENT(OUT) :: S(:)
      REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: WW(:)
      REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: U(:,:), VT(:,:)
      END SUBROUTINE SGESDD_F95
      
       SUBROUTINE DGESDD_F95(A, S, U, VT, WW, JOB, INFO )
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), OPTIONAL, INTENT(IN) :: JOB
      INTEGER, INTENT(OUT), OPTIONAL :: INFO
      REAL(WP), INTENT(INOUT) :: A(:,:)
      REAL(WP), INTENT(OUT) :: S(:)
      REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: WW(:)
      REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: U(:,:), VT(:,:)
      END SUBROUTINE DGESDD_F95
      
       SUBROUTINE CGESDD_F95(A, S, U, VT, WW, JOB, INFO )
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), OPTIONAL, INTENT(IN) :: JOB
      INTEGER, INTENT(OUT), OPTIONAL :: INFO
      COMPLEX(WP), INTENT(INOUT) :: A(:,:)
      REAL(WP), INTENT(OUT) :: S(:)
      REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: WW(:)
      COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: U(:,:), VT(:,:)
      END SUBROUTINE CGESDD_F95
      
        END INTERFACE                         

      END MODULE F95_LAPACK
