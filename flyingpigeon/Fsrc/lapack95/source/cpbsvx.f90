!*******************************************************************************
!   Copyright(C) 2005-2013 Intel Corporation. All Rights Reserved.
!   
!   The source code, information  and  material ("Material") contained herein is
!   owned  by Intel Corporation or its suppliers or licensors, and title to such
!   Material remains  with Intel Corporation  or its suppliers or licensors. The
!   Material  contains proprietary information  of  Intel or  its  suppliers and
!   licensors. The  Material is protected by worldwide copyright laws and treaty
!   provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way  without Intel's  prior  express written  permission. No  license
!   under  any patent, copyright  or  other intellectual property rights  in the
!   Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
!   implication, inducement,  estoppel or  otherwise.  Any  license  under  such
!   intellectual  property  rights must  be express  and  approved  by  Intel in
!   writing.
!   
!   *Third Party trademarks are the property of their respective owners.
!   
!   Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
!   this  notice or  any other notice embedded  in Materials by Intel or Intel's
!   suppliers or licensors in any way.
!
!*******************************************************************************
!  Content:
!      F95 interface for LAPACK routines
!*******************************************************************************
! This file was generated automatically!
!*******************************************************************************

PURE SUBROUTINE CPBSVX_F95(AB,B,X,UPLO,AFB,FACT,EQUED,S,FERR,BERR,RCOND,&
     &                                                             INFO)
    ! Fortran77 call:
    ! CPBSVX(FACT,UPLO,N,KD,NRHS,AB,LDAB,AFB,LDAFB,EQUED,S,B,LDB,X,LDX,
    !   RCOND,FERR,BERR,WORK,IWORK,INFO)
    ! UPLO='U','L'; default: 'U'
    ! FACT='N','E','F'; default: 'N'
    ! EQUED='N','Y'; default: 'N'
    ! Default S(i)=1.0_WP
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_PBSVX, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY CPBSVX_MKL95(AB,B,X,UPLO,AFB,FACT,EQUED,S,FERR,BERR,RCOND,    &
     &                                                             INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: FACT
    CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
    REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(INOUT) :: AB(:,:)
    COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    COMPLEX(WP), INTENT(OUT) :: X(:,:)
    COMPLEX(WP), INTENT(INOUT), OPTIONAL, TARGET :: AFB(:,:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: S(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: FERR(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: BERR(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'PBSVX'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_UPLO
    CHARACTER(LEN=1) :: O_FACT
    CHARACTER(LEN=1) :: O_EQUED
    REAL(WP) :: O_RCOND
    INTEGER :: O_INFO
    INTEGER :: N
    INTEGER :: KD
    INTEGER :: NRHS
    INTEGER :: LDAB
    INTEGER :: LDAFB
    INTEGER :: LDB
    INTEGER :: LDX
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    COMPLEX(WP), POINTER :: O_AFB(:,:)
    REAL(WP), POINTER :: O_S(:)
    REAL(WP), POINTER :: O_FERR(:)
    REAL(WP), POINTER :: O_BERR(:)
    COMPLEX(WP), POINTER :: WORK(:)
    REAL(WP), POINTER :: IWORK(:)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(EQUED)) THEN
        O_EQUED = EQUED
    ELSE
        O_EQUED = 'N'
    ENDIF
    IF(PRESENT(FACT)) THEN
        O_FACT = FACT
    ELSE
        O_FACT = 'N'
    ENDIF
    IF(PRESENT(UPLO)) THEN
        O_UPLO = UPLO
    ELSE
        O_UPLO = 'U'
    ENDIF
    KD = SIZE(AB,1)-1
    LDAB = MAX(1,SIZE(AB,1))
    IF(PRESENT(AFB)) THEN
        LDAFB = MAX(1,SIZE(AFB,1))
    ELSE
        LDAFB = KD+1
    ENDIF
    LDB = MAX(1,SIZE(B,1))
    LDX = MAX(1,SIZE(X,1))
    N = SIZE(AB,2)
    NRHS = SIZE(B,2)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(AFB)) THEN
        O_AFB => AFB
    ELSE
        ALLOCATE(O_AFB(LDAFB,N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(BERR)) THEN
            O_BERR => BERR
        ELSE
            ALLOCATE(O_BERR(NRHS), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(FERR)) THEN
            O_FERR => FERR
        ELSE
            ALLOCATE(O_FERR(NRHS), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(S)) THEN
            O_S => S
        ELSE
            ALLOCATE(O_S(N), STAT=L_STAT_ALLOC); O_S = 1.0_WP
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(IWORK(N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(2*N), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_PBSVX(O_FACT,O_UPLO,N,KD,NRHS,AB,LDAB,O_AFB,LDAFB,     &
     &  O_EQUED,O_S,B,LDB,X,LDX,O_RCOND,O_FERR,O_BERR,WORK,IWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(EQUED)) THEN
        EQUED = O_EQUED
    ENDIF
    IF(PRESENT(RCOND)) THEN
        RCOND = O_RCOND
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(AFB)) THEN
        DEALLOCATE(O_AFB, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(BERR)) THEN
        DEALLOCATE(O_BERR, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(FERR)) THEN
        DEALLOCATE(O_FERR, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(S)) THEN
        DEALLOCATE(O_S, STAT=L_STAT_DEALLOC)
    ENDIF
    DEALLOCATE(IWORK, STAT=L_STAT_DEALLOC)
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE CPBSVX_F95
