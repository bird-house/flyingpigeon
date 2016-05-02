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

PURE SUBROUTINE DGELSD1_F95(A,B,RANK,S,RCOND,INFO)
    ! Fortran77 call:
    ! DGELSD(M,N,NRHS,A,LDA,B,LDB,S,RCOND,RANK,WORK,LWORK,IWORK,INFO)
    ! Default RCOND=100*EPSILON(1.0_WP)
    ! <<< Use statements >>>
    USE F77_LAPACK1, ONLY: F77_GELSD
    USE F77_LAPACK, ONLY: F77_XERBLA, F77_ILAENV
    ! <<< ENTRY point >>>
    ENTRY DGELSD1_MKL95(A,B,RANK,S,RCOND,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(OUT), OPTIONAL :: RANK
    REAL(WP), INTENT(IN), OPTIONAL :: RCOND
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: A(:,:)
    REAL(WP), INTENT(INOUT) :: B(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: S(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GELSD'
    ! <<< Local scalars >>>
    INTEGER :: O_RANK
    REAL(WP) :: O_RCOND
    INTEGER :: O_INFO
    INTEGER :: M
    INTEGER :: N
    INTEGER :: NRHS
    INTEGER :: LDA
    INTEGER :: LDB
    INTEGER :: LWORK
    INTEGER :: LIWORK
    INTEGER :: MINMN
    INTEGER :: NLVL
    INTEGER :: SMLSIZ
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_S(:)
    REAL(WP), POINTER :: WORK(:)
    INTEGER, POINTER :: IWORK(:)
    ! <<< Arrays to request optimal sizes >>>
    REAL(WP) :: S_WORK(1)
    ! <<< Intrinsic functions >>>
    INTRINSIC DBLE, EPSILON, INT, LOG, MAX, MIN, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(RCOND)) THEN
        O_RCOND = RCOND
    ELSE
        O_RCOND = 100*EPSILON(1.0_WP)
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    LDB = MAX(1,SIZE(B,1))
    M = SIZE(A,1)
    N = SIZE(A,2)
    NRHS = 1
    SMLSIZ = F77_ILAENV(9,'DGELSD',' ', 0,0,0,0)
    MINMN = MAX(1,MIN(M,N))
    NLVL = MAX(INT(LOG(DBLE(MINMN)/DBLE(SMLSIZ+1))/LOG(2.0_WP))+1,0)
    LIWORK = 3*MINMN*(NLVL+1)+11*MINMN
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(S)) THEN
        O_S => S
    ELSE
        ALLOCATE(O_S(MIN(M,N)), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(IWORK(LIWORK), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Request work array(s) size >>>
    LWORK = -1
    CALL F77_GELSD(M,N,NRHS,A,LDA,B,LDB,O_S,O_RCOND,O_RANK,S_WORK,LWORK,&
     &                                                     IWORK,O_INFO)
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
        CALL F77_GELSD(M,N,NRHS,A,LDA,B,LDB,O_S,O_RCOND,O_RANK,WORK,    &
     &                                               LWORK,IWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(RANK)) THEN
        RANK = O_RANK
    ENDIF
    ! <<< Deallocate work arrays with requested sizes >>>
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
200    CONTINUE
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(S)) THEN
        DEALLOCATE(O_S, STAT=L_STAT_DEALLOC)
    ENDIF
    DEALLOCATE(IWORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE DGELSD1_F95
