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

PURE SUBROUTINE SGEBRD_F95(A,D,E,TAUQ,TAUP,INFO)
    ! Fortran77 call:
    ! SGEBRD(M,N,A,LDA,D,E,TAUQ,TAUP,WORK,LWORK,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_GEBRD, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY SGEBRD_MKL95(A,D,E,TAUQ,TAUP,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: A(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: D(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: E(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: TAUQ(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: TAUP(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GEBRD'
    ! <<< Local scalars >>>
    INTEGER :: O_INFO
    INTEGER :: M
    INTEGER :: N
    INTEGER :: LDA
    INTEGER :: LWORK
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_D(:)
    REAL(WP), POINTER :: O_E(:)
    REAL(WP), POINTER :: O_TAUQ(:)
    REAL(WP), POINTER :: O_TAUP(:)
    REAL(WP), POINTER :: WORK(:)
    ! <<< Arrays to request optimal sizes >>>
    REAL(WP) :: S_WORK(1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    LDA = MAX(1,SIZE(A,1))
    M = SIZE(A,1)
    N = SIZE(A,2)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(D)) THEN
        O_D => D
    ELSE
        ALLOCATE(O_D(MIN(M,N)), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(E)) THEN
            O_E => E
        ELSE
            ALLOCATE(O_E(MIN(M,N)-1), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(TAUP)) THEN
            O_TAUP => TAUP
        ELSE
            ALLOCATE(O_TAUP(MIN(M,N)), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(TAUQ)) THEN
            O_TAUQ => TAUQ
        ELSE
            ALLOCATE(O_TAUQ(MIN(M,N)), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    ! <<< Request work array(s) size >>>
    LWORK = -1
    CALL F77_GEBRD(M,N,A,LDA,O_D,O_E,O_TAUQ,O_TAUP,S_WORK,LWORK,O_INFO)
    ! <<< Exit if error: bad parameters >>>
    IF(O_INFO /= 0) THEN
        GOTO 200
    ENDIF
    LWORK = S_WORK(1)
    ! <<< Allocate work arrays with requested sizes >>>
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(LWORK), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_GEBRD(M,N,A,LDA,O_D,O_E,O_TAUQ,O_TAUP,WORK,LWORK,      &
     &                                                           O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Deallocate work arrays with requested sizes >>>
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
200    CONTINUE
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(D)) THEN
        DEALLOCATE(O_D, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(E)) THEN
        DEALLOCATE(O_E, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(TAUP)) THEN
        DEALLOCATE(O_TAUP, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(TAUQ)) THEN
        DEALLOCATE(O_TAUQ, STAT=L_STAT_DEALLOC)
    ENDIF
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE SGEBRD_F95
