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

PURE SUBROUTINE SORBDB_F95(X11,X12,X21,X22,THETA,PHI,TAUP1,TAUP2,TAUQ1, &
     &                                           TAUQ2,TRANS,SIGNS,INFO)
    ! Fortran77 call:
    ! SORBDB(TRANS,SIGNS,M,P,Q,X11,LDX11,X12,LDX12,X21,LDX21,X22,LDX22,
    !   THETA,PHI,TAUP1,TAUP2,TAUQ1,TAUQ2,WORK,LWORK,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_ORBDB, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY SORBDB_MKL95(X11,X12,X21,X22,THETA,PHI,TAUP1,TAUP2,TAUQ1,     &
     &                                           TAUQ2,TRANS,SIGNS,INFO)
   
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIGNS
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: X11(:,:)
    REAL(WP), INTENT(INOUT) :: X12(:,:)
    REAL(WP), INTENT(INOUT) :: X21(:,:)
    REAL(WP), INTENT(INOUT) :: X22(:,:)
    REAL(WP), INTENT(OUT) :: THETA(:)
    REAL(WP), INTENT(OUT) :: PHI(:)
    REAL(WP), INTENT(OUT) :: TAUP1(:)
    REAL(WP), INTENT(OUT) :: TAUP2(:)
    REAL(WP), INTENT(OUT) :: TAUQ1(:)
    REAL(WP), INTENT(OUT) :: TAUQ2(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'ORBDB'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_TRANS
    CHARACTER(LEN=1) :: O_SIGNS
    INTEGER :: O_INFO
    INTEGER :: LDX11
    INTEGER :: LDX12
    INTEGER :: LDX21
    INTEGER :: LDX22
    INTEGER :: M
    INTEGER :: P
    INTEGER :: Q
    INTEGER :: LWORK
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: WORK(:)
    ! <<< Arrays to request optimal sizes >>>
    REAL(WP) :: S_WORK(1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    !    Not inited: 1 scalars (special=1)
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(SIGNS)) THEN
        O_SIGNS = SIGNS
    ELSE
        O_SIGNS = 'O'
    ENDIF
    IF(PRESENT(TRANS)) THEN
        O_TRANS = TRANS
    ELSE
        O_TRANS = 'N'
    ENDIF
    LDX11 = MAX(1,SIZE(X11,1))
    LDX12 = MAX(1,SIZE(X12,1))
    LDX21 = MAX(1,SIZE(X21,1))
    LDX22 = MAX(1,SIZE(X22,1))
    M = SIZE(TAUP1) + SIZE(TAUP2)
    P = SIZE(TAUP1)
    Q = SIZE(TAUQ1)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    ! <<< Request work array(s) size >>>
    LWORK = -1
    CALL F77_ORBDB(O_TRANS,O_SIGNS,M,P,Q,X11,LDX11,X12,LDX12,X21,LDX21, &
     &  X22,LDX22,THETA,PHI,TAUP1,TAUP2,TAUQ1,TAUQ2,S_WORK,LWORK,O_INFO)
    ! <<< Exit if error: bad parameters >>>
    IF(O_INFO /= 0) THEN
        GOTO 200
    ENDIF
    LWORK = S_WORK(1)
    ! <<< Allocate work arrays with requested sizes >>>
    ALLOCATE(WORK(LWORK), STAT=L_STAT_ALLOC)
    ! Error while build wrapper: SORBDB
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_ORBDB(O_TRANS,O_SIGNS,M,P,Q,X11,LDX11,X12,LDX12,X21,   &
     &    LDX21,X22,LDX22,THETA,PHI,TAUP1,TAUP2,TAUQ1,TAUQ2,WORK,LWORK, &
     &                                                           O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Deallocate work arrays with requested sizes >>>
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
200    CONTINUE
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE SORBDB_F95
