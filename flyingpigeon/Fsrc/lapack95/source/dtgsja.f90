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

PURE SUBROUTINE DTGSJA_F95(A,B,TOLA,TOLB,K,L,U,V,Q,JOBU,JOBV,JOBQ,ALPHA,&
     &                                                 BETA,NCYCLE,INFO)
    ! Fortran77 call:
    ! DTGSJA(JOBU,JOBV,JOBQ,M,P,N,K,L,A,LDA,B,LDB,TOLA,TOLB,ALPHA,BETA,
    !   U,LDU,V,LDV,Q,LDQ,WORK,NCYCLE,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_TGSJA, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY DTGSJA_MKL95(A,B,TOLA,TOLB,K,L,U,V,Q,JOBU,JOBV,JOBQ,ALPHA,    &
     &                                                 BETA,NCYCLE,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(IN) :: TOLA
    REAL(WP), INTENT(IN) :: TOLB
    INTEGER, INTENT(IN) :: K
    INTEGER, INTENT(IN) :: L
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBU
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBV
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBQ
    INTEGER, INTENT(OUT), OPTIONAL :: NCYCLE
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: A(:,:)
    REAL(WP), INTENT(INOUT) :: B(:,:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: U(:,:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: V(:,:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: Q(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: ALPHA(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: BETA(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'TGSJA'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_JOBU
    CHARACTER(LEN=1) :: O_JOBV
    CHARACTER(LEN=1) :: O_JOBQ
    INTEGER :: O_NCYCLE
    INTEGER :: O_INFO
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
    REAL(WP), POINTER :: O_ALPHA(:)
    REAL(WP), POINTER :: O_BETA(:)
    REAL(WP), POINTER :: WORK(:)
    ! <<< Stubs to "allocate" optional arrays >>>
    REAL(WP), TARGET :: L_A2_REAL(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(Q).AND.PRESENT(JOBQ)) THEN
        IF((JOBQ.EQ.'I'.OR.JOBQ.EQ.'i').OR.                             &
     &    (JOBQ.EQ.'Q'.OR.JOBQ.EQ.'q')) THEN
            O_JOBQ = JOBQ
        ELSE
            O_INFO=-1001; GOTO 1001
        ENDIF
    ELSEIF(PRESENT(Q)) THEN
        O_JOBQ = 'Q'
    ELSEIF(PRESENT(JOBQ)) THEN
        O_INFO=-1001; GOTO 1001
    ELSE
        O_JOBQ = 'N'
    ENDIF
    IF(PRESENT(U).AND.PRESENT(JOBU)) THEN
        IF((JOBU.EQ.'I'.OR.JOBU.EQ.'i').OR.                             &
     &    (JOBU.EQ.'U'.OR.JOBU.EQ.'u')) THEN
            O_JOBU = JOBU
        ELSE
            O_INFO=-1001; GOTO 1001
        ENDIF
    ELSEIF(PRESENT(U)) THEN
        O_JOBU = 'U'
    ELSEIF(PRESENT(JOBU)) THEN
        O_INFO=-1001; GOTO 1001
    ELSE
        O_JOBU = 'N'
    ENDIF
    IF(PRESENT(V).AND.PRESENT(JOBV)) THEN
        IF((JOBV.EQ.'I'.OR.JOBV.EQ.'i').OR.                             &
     &    (JOBV.EQ.'V'.OR.JOBV.EQ.'v')) THEN
            O_JOBV = JOBV
        ELSE
            O_INFO=-1001; GOTO 1001
        ENDIF
    ELSEIF(PRESENT(V)) THEN
        O_JOBV = 'V'
    ELSEIF(PRESENT(JOBV)) THEN
        O_INFO=-1001; GOTO 1001
    ELSE
        O_JOBV = 'N'
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
    IF(PRESENT(ALPHA)) THEN
        O_ALPHA => ALPHA
    ELSE
        ALLOCATE(O_ALPHA(N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(BETA)) THEN
            O_BETA => BETA
        ELSE
            ALLOCATE(O_BETA(N), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
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
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(2*N), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_TGSJA(O_JOBU,O_JOBV,O_JOBQ,M,P,N,K,L,A,LDA,B,LDB,TOLA, &
     & TOLB,O_ALPHA,O_BETA,O_U,LDU,O_V,LDV,O_Q,LDQ,WORK,O_NCYCLE,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(NCYCLE)) THEN
        NCYCLE = O_NCYCLE
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(ALPHA)) THEN
        DEALLOCATE(O_ALPHA, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(BETA)) THEN
        DEALLOCATE(O_BETA, STAT=L_STAT_DEALLOC)
    ENDIF
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
1001    CONTINUE
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE DTGSJA_F95
