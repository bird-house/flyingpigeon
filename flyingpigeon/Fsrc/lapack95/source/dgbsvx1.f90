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

PURE SUBROUTINE DGBSVX1_F95(AB,B,X,KL,AFB,IPIV,FACT,TRANS,EQUED,R,C,    &
     &                                      FERR,BERR,RCOND,RPVGRW,INFO)
    ! Fortran77 call:
    ! DGBSVX(FACT,TRANS,N,KL,KU,NRHS,AB,LDAB,AFB,LDAFB,IPIV,EQUED,R,C,B,
    !   LDB,X,LDX,RCOND,FERR,BERR,WORK,IWORK,INFO)
    ! FACT='N','E','F'; default: 'N'
    ! TRANS='N','C','T'; default: 'N'
    ! EQUED='N','B','C','R'; default: 'N'
    ! Default R(i)=1.0_WP
    ! Default C(i)=1.0_WP
    ! <<< Use statements >>>
    USE F77_LAPACK1, ONLY: F77_GBSVX
    USE F77_LAPACK, ONLY: F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY DGBSVX1_MKL95(AB,B,X,KL,AFB,IPIV,FACT,TRANS,EQUED,R,C,FERR,   &
     &                                           BERR,RCOND,RPVGRW,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(IN), OPTIONAL :: KL
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: FACT
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
    CHARACTER(LEN=1), INTENT(INOUT), OPTIONAL :: EQUED
    REAL(WP), INTENT(OUT), OPTIONAL :: FERR
    REAL(WP), INTENT(OUT), OPTIONAL :: BERR
    REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
    REAL(WP), INTENT(OUT), OPTIONAL :: RPVGRW
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: AB(:,:)
    REAL(WP), INTENT(INOUT) :: B(:)
    REAL(WP), INTENT(OUT) :: X(:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: AFB(:,:)
    INTEGER, INTENT(INOUT), OPTIONAL, TARGET :: IPIV(:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: R(:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: C(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GBSVX'
    ! <<< Local scalars >>>
    INTEGER :: O_KL
    CHARACTER(LEN=1) :: O_FACT
    CHARACTER(LEN=1) :: O_TRANS
    CHARACTER(LEN=1) :: O_EQUED
    REAL(WP) :: O_RCOND
    INTEGER :: O_INFO
    INTEGER :: N
    INTEGER :: KU
    INTEGER :: NRHS
    INTEGER :: LDAB
    INTEGER :: LDAFB
    INTEGER :: LDB
    INTEGER :: LDX
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_AFB(:,:)
    INTEGER, POINTER :: O_IPIV(:)
    REAL(WP), POINTER :: O_R(:)
    REAL(WP), POINTER :: O_C(:)
    REAL(WP), POINTER :: WORK(:)
    INTEGER, POINTER :: IWORK(:)
    REAL(WP) :: R_BERR(1)
    REAL(WP) :: R_FERR(1)
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
    IF(PRESENT(TRANS)) THEN
        O_TRANS = TRANS
    ELSE
        O_TRANS = 'N'
    ENDIF
    LDAB = MAX(1,SIZE(AB,1))
    LDB = MAX(1,SIZE(B,1))
    LDX = MAX(1,SIZE(X,1))
    N = SIZE(AB,2)
    NRHS = 1
    IF(PRESENT(KL)) THEN
        O_KL = KL
    ELSE
        O_KL = (LDAB-1)/2
    ENDIF
    KU = LDAB-O_KL-1
    IF(PRESENT(AFB)) THEN
        LDAFB = MAX(1,SIZE(AFB,1))
    ELSE
        LDAFB = 2*O_KL+KU+1
    ENDIF
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(AFB)) THEN
        O_AFB => AFB
    ELSE
        ALLOCATE(O_AFB(LDAFB,N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(C)) THEN
            O_C => C
        ELSE
            ALLOCATE(O_C(N), STAT=L_STAT_ALLOC); O_C = 1.0_WP
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(IPIV)) THEN
            O_IPIV => IPIV
        ELSE
            ALLOCATE(O_IPIV(N), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(R)) THEN
            O_R => R
        ELSE
            ALLOCATE(O_R(N), STAT=L_STAT_ALLOC); O_R = 1.0_WP
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(IWORK(N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(3*N), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_GBSVX(O_FACT,O_TRANS,N,O_KL,KU,NRHS,AB,LDAB,O_AFB,     &
     &  LDAFB,O_IPIV,O_EQUED,O_R,O_C,B,LDB,X,LDX,O_RCOND,R_FERR,R_BERR, &
     &                                                WORK,IWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalars for reduced arrays >>>
    IF(PRESENT(BERR)) THEN
        BERR = R_BERR(1)
    ENDIF
    IF(PRESENT(FERR)) THEN
        FERR = R_FERR(1)
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(EQUED)) THEN
        EQUED = O_EQUED
    ENDIF
    IF(PRESENT(RCOND)) THEN
        RCOND = O_RCOND
    ENDIF
    IF(PRESENT(RPVGRW)) THEN
        RPVGRW = WORK(1)
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(AFB)) THEN
        DEALLOCATE(O_AFB, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(C)) THEN
        DEALLOCATE(O_C, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(IPIV)) THEN
        DEALLOCATE(O_IPIV, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(R)) THEN
        DEALLOCATE(O_R, STAT=L_STAT_DEALLOC)
    ENDIF
    DEALLOCATE(IWORK, STAT=L_STAT_DEALLOC)
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE DGBSVX1_F95
