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

PURE SUBROUTINE DTRSEN_F95(T,SELECT,WR,WI,M,S,SEP,Q,INFO)
    ! Fortran77 call:
    ! DTRSEN(JOB,COMPQ,SELECT,N,T,LDT,Q,LDQ,WR,WI,M,S,SEP,WORK,LWORK,
    !   IWORK,LIWORK,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_TRSEN, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY DTRSEN_MKL95(T,SELECT,WR,WI,M,S,SEP,Q,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(OUT), OPTIONAL :: M
    REAL(WP), INTENT(OUT), OPTIONAL :: S
    REAL(WP), INTENT(OUT), OPTIONAL :: SEP
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: T(:,:)
    LOGICAL, INTENT(IN) :: SELECT(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: WR(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: WI(:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: Q(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'TRSEN'
    ! <<< Local scalars >>>
    INTEGER :: O_M
    REAL(WP) :: O_S
    REAL(WP) :: O_SEP
    INTEGER :: O_INFO
    CHARACTER(LEN=1) :: JOB
    CHARACTER(LEN=1) :: COMPQ
    INTEGER :: N
    INTEGER :: LDT
    INTEGER :: LDQ
    INTEGER :: LWORK
    INTEGER :: LIWORK
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_WR(:)
    REAL(WP), POINTER :: O_WI(:)
    REAL(WP), POINTER :: O_Q(:,:)
    REAL(WP), POINTER :: WORK(:)
    INTEGER, POINTER :: IWORK(:)
    ! <<< Arrays to request optimal sizes >>>
    INTEGER :: S_IWORK(1)
    REAL(WP) :: S_WORK(1)
    ! <<< Stubs to "allocate" optional arrays >>>
    REAL(WP), TARGET :: L_A2_REAL(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(Q)) THEN
        COMPQ = 'V'
    ELSE
        COMPQ = 'N'
    ENDIF
    IF(PRESENT(S).AND.PRESENT(SEP)) THEN
        JOB = 'B'
    ELSEIF(PRESENT(S)) THEN
        JOB = 'E'
    ELSEIF(PRESENT(SEP)) THEN
        JOB = 'V'
    ELSE
        JOB = 'N'
    ENDIF
    IF(PRESENT(Q)) THEN
        LDQ = MAX(1,SIZE(Q,1))
    ELSE
        LDQ = 1
    ENDIF
    LDT = MAX(1,SIZE(T,1))
    N = SIZE(T,2)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(Q)) THEN
        O_Q => Q
    ELSE
        O_Q => L_A2_REAL
    ENDIF
    IF(PRESENT(WI)) THEN
        O_WI => WI
    ELSE
        ALLOCATE(O_WI(N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(WR)) THEN
            O_WR => WR
        ELSE
            ALLOCATE(O_WR(N), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    ! <<< Request work array(s) size >>>
    LIWORK = -1
    LWORK = -1
    CALL F77_TRSEN(JOB,COMPQ,SELECT,N,T,LDT,O_Q,LDQ,O_WR,O_WI,O_M,O_S,  &
     &                         O_SEP,S_WORK,LWORK,S_IWORK,LIWORK,O_INFO)
    ! <<< Exit if error: bad parameters >>>
    IF(O_INFO /= 0) THEN
        GOTO 200
    ENDIF
    LIWORK = S_IWORK(1)
    LWORK = S_WORK(1)
    ! <<< Allocate work arrays with requested sizes >>>
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(IWORK(LIWORK), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(LWORK), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_TRSEN(JOB,COMPQ,SELECT,N,T,LDT,O_Q,LDQ,O_WR,O_WI,O_M,  &
     &                         O_S,O_SEP,WORK,LWORK,IWORK,LIWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(M)) THEN
        M = O_M
    ENDIF
    IF(PRESENT(S)) THEN
        S = O_S
    ENDIF
    IF(PRESENT(SEP)) THEN
        SEP = O_SEP
    ENDIF
    ! <<< Deallocate work arrays with requested sizes >>>
    DEALLOCATE(IWORK, STAT=L_STAT_DEALLOC)
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
200    CONTINUE
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(WI)) THEN
        DEALLOCATE(O_WI, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(WR)) THEN
        DEALLOCATE(O_WR, STAT=L_STAT_DEALLOC)
    ENDIF
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE DTRSEN_F95
