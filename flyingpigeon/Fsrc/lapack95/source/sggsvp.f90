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

PURE SUBROUTINE SGGSVP_F95(A,B,TOLA,TOLB,K,L,U,V,Q,INFO)
    ! Fortran77 call:
    ! SGGSVP(JOBU,JOBV,JOBQ,M,P,N,A,LDA,B,LDB,TOLA,TOLB,K,L,U,LDU,V,LDV,
    !   Q,LDQ,IWORK,TAU,WORK,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_GGSVP, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY SGGSVP_MKL95(A,B,TOLA,TOLB,K,L,U,V,Q,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(IN) :: TOLA
    REAL(WP), INTENT(IN) :: TOLB
    INTEGER, INTENT(OUT), OPTIONAL :: K
    INTEGER, INTENT(OUT), OPTIONAL :: L
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: A(:,:)
    REAL(WP), INTENT(INOUT) :: B(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: U(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: V(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: Q(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GGSVP'
    ! <<< Local scalars >>>
    INTEGER :: O_K
    INTEGER :: O_L
    INTEGER :: O_INFO
    CHARACTER(LEN=1) :: JOBU
    CHARACTER(LEN=1) :: JOBV
    CHARACTER(LEN=1) :: JOBQ
    INTEGER :: M
    INTEGER :: P
    INTEGER :: N
    INTEGER :: LDA
    INTEGER :: LDB
    INTEGER :: LDU
    INTEGER :: LDV
    INTEGER :: LDQ
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_U(:,:)
    REAL(WP), POINTER :: O_V(:,:)
    REAL(WP), POINTER :: O_Q(:,:)
    INTEGER, POINTER :: IWORK(:)
    REAL(WP), POINTER :: TAU(:)
    REAL(WP), POINTER :: WORK(:)
    ! <<< Stubs to "allocate" optional arrays >>>
    REAL(WP), TARGET :: L_A2_REAL(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(Q)) THEN
        JOBQ = 'Q'
    ELSE
        JOBQ = 'N'
    ENDIF
    IF(PRESENT(U)) THEN
        JOBU = 'U'
    ELSE
        JOBU = 'N'
    ENDIF
    IF(PRESENT(V)) THEN
        JOBV = 'V'
    ELSE
        JOBV = 'N'
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    LDB = MAX(1,SIZE(B,1))
    IF(PRESENT(Q)) THEN
        LDQ = MAX(1,SIZE(Q,1))
    ELSE
        LDQ = 1
    ENDIF
    IF(PRESENT(U)) THEN
        LDU = MAX(1,SIZE(U,1))
    ELSE
        LDU = 1
    ENDIF
    IF(PRESENT(V)) THEN
        LDV = MAX(1,SIZE(V,1))
    ELSE
        LDV = 1
    ENDIF
    M = SIZE(A,1)
    N = SIZE(A,2)
    P = SIZE(B,1)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(Q)) THEN
        O_Q => Q
    ELSE
        O_Q => L_A2_REAL
    ENDIF
    IF(PRESENT(U)) THEN
        O_U => U
    ELSE
        O_U => L_A2_REAL
    ENDIF
    IF(PRESENT(V)) THEN
        O_V => V
    ELSE
        O_V => L_A2_REAL
    ENDIF
    ALLOCATE(IWORK(N), STAT=L_STAT_ALLOC)
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(TAU(N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(MAX(3*N,M,P)), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_GGSVP(JOBU,JOBV,JOBQ,M,P,N,A,LDA,B,LDB,TOLA,TOLB,O_K,  &
     &                O_L,O_U,LDU,O_V,LDV,O_Q,LDQ,IWORK,TAU,WORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(K)) THEN
        K = O_K
    ENDIF
    IF(PRESENT(L)) THEN
        L = O_L
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    DEALLOCATE(IWORK, STAT=L_STAT_DEALLOC)
    DEALLOCATE(TAU, STAT=L_STAT_DEALLOC)
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE SGGSVP_F95
