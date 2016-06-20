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

PURE SUBROUTINE ZBBCSD_F95(THETA,PHI,U1,U2,V1T,V2T,B11D,B11E,B12D,B12E, &
     &         B21D,B21E,B22D,B22E,JOBU1,JOBU2,JOBV1T,JOBV2T,TRANS,INFO)
    ! Fortran77 call:
    ! ZBBCSD(JOBU1,JOBU2,JOBV1T,JOBV2T,TRANS,M,P,Q,THETA,PHI,U1,LDU1,U2,
    !   LDU2,V1T,LDV1T,V2T,LDV2T,B11D,B11E,B12D,B12E,B21D,B21E,B22D,
    !   B22E,RWORK,LRWORK,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_BBCSD, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY ZBBCSD_MKL95(THETA,PHI,U1,U2,V1T,V2T,B11D,B11E,B12D,B12E,B21D,&
     &              B21E,B22D,B22E,JOBU1,JOBU2,JOBV1T,JOBV2T,TRANS,INFO)
    
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBU1
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBU2
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBV1T
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBV2T
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: THETA(:)
    REAL(WP), INTENT(INOUT) :: PHI(:)
    COMPLEX(WP), INTENT(INOUT) :: U1(:,:)
    COMPLEX(WP), INTENT(INOUT) :: U2(:,:)
    COMPLEX(WP), INTENT(INOUT) :: V1T(:,:)
    COMPLEX(WP), INTENT(INOUT) :: V2T(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: B11D(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: B11E(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: B12D(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: B12E(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: B21D(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: B21E(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: B22D(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: B22E(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'BBCSD'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_JOBU1
    CHARACTER(LEN=1) :: O_JOBU2
    CHARACTER(LEN=1) :: O_JOBV1T
    CHARACTER(LEN=1) :: O_JOBV2T
    CHARACTER(LEN=1) :: O_TRANS
    INTEGER :: O_INFO
    INTEGER :: M
    INTEGER :: P
    INTEGER :: Q
    INTEGER :: LDU1
    INTEGER :: LDU2
    INTEGER :: LDV1T
    INTEGER :: LDV2T
    INTEGER :: LRWORK
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_B11D(:)
    REAL(WP), POINTER :: O_B11E(:)
    REAL(WP), POINTER :: O_B12D(:)
    REAL(WP), POINTER :: O_B12E(:)
    REAL(WP), POINTER :: O_B21D(:)
    REAL(WP), POINTER :: O_B21E(:)
    REAL(WP), POINTER :: O_B22D(:)
    REAL(WP), POINTER :: O_B22E(:)
    REAL(WP), POINTER :: RWORK(:)
    ! <<< Arrays to request optimal sizes >>>
    REAL(WP) :: S_RWORK(1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    !    Not inited: 1 scalars (special=1)
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(JOBU1)) THEN
        O_JOBU1 = JOBU1
    ELSE
        O_JOBU1 = 'Y'
    ENDIF
    IF(PRESENT(JOBU2)) THEN
        O_JOBU2 = JOBU2
    ELSE
        O_JOBU2 = 'Y'
    ENDIF
    IF(PRESENT(JOBV1T)) THEN
        O_JOBV1T = JOBV1T
    ELSE
        O_JOBV1T = 'Y'
    ENDIF
    IF(PRESENT(JOBV2T)) THEN
        O_JOBV2T = JOBV2T
    ELSE
        O_JOBV2T = 'Y'
    ENDIF
    IF(PRESENT(TRANS)) THEN
        O_TRANS = TRANS
    ELSE
        O_TRANS = 'N'
    ENDIF
    LDU1 = MAX(1,SIZE(U1,1))
    LDU2 = MAX(1,SIZE(U2,1))
    LDV1T = MAX(1,SIZE(V1T,1))
    LDV2T = MAX(1,SIZE(V2T,1))
    M = SIZE(U1,2) + SIZE(U2,2)
    P = SIZE(U1,2)
    Q = SIZE(V1T,2)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(B11D)) THEN
        O_B11D => B11D
    ELSE
        ALLOCATE(O_B11D(Q), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(B11E)) THEN
            O_B11E => B11E
        ELSE
            ALLOCATE(O_B11E(Q-1), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(B12D)) THEN
            O_B12D => B12D
        ELSE
            ALLOCATE(O_B12D(Q), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(B12E)) THEN
            O_B12E => B12E
        ELSE
            ALLOCATE(O_B12E(Q-1), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(B21D)) THEN
            O_B21D => B21D
        ELSE
            ALLOCATE(O_B21D(Q), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(B21E)) THEN
            O_B21E => B21E
        ELSE
            ALLOCATE(O_B21E(Q-1), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(B22D)) THEN
            O_B22D => B22D
        ELSE
            ALLOCATE(O_B22D(Q), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(B22E)) THEN
            O_B22E => B22E
        ELSE
            ALLOCATE(O_B22E(Q-1), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    ! <<< Request work array(s) size >>>
    LRWORK = -1
    CALL F77_BBCSD(O_JOBU1,O_JOBU2,O_JOBV1T,O_JOBV2T,O_TRANS,M,P,Q,     &
     &     THETA,PHI,U1,LDU1,U2,LDU2,V1T,LDV1T,V2T,LDV2T,O_B11D,O_B11E, &
     &  O_B12D,O_B12E,O_B21D,O_B21E,O_B22D,O_B22E,S_RWORK,LRWORK,O_INFO)
    ! <<< Exit if error: bad parameters >>>
    IF(O_INFO /= 0) THEN
        GOTO 200
    ENDIF
    LRWORK = S_RWORK(1)
    ! <<< Allocate work arrays with requested sizes >>>
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(RWORK(LRWORK), STAT=L_STAT_ALLOC)
    ENDIF
    ! Error while build wrapper: ZBBCSD
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_BBCSD(O_JOBU1,O_JOBU2,O_JOBV1T,O_JOBV2T,O_TRANS,M,P,Q, &
     &     THETA,PHI,U1,LDU1,U2,LDU2,V1T,LDV1T,V2T,LDV2T,O_B11D,O_B11E, &
     &    O_B12D,O_B12E,O_B21D,O_B21E,O_B22D,O_B22E,RWORK,LRWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Deallocate work arrays with requested sizes >>>
    DEALLOCATE(RWORK, STAT=L_STAT_DEALLOC)
200    CONTINUE
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(B11D)) THEN
        DEALLOCATE(O_B11D, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(B11E)) THEN
        DEALLOCATE(O_B11E, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(B12D)) THEN
        DEALLOCATE(O_B12D, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(B12E)) THEN
        DEALLOCATE(O_B12E, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(B21D)) THEN
        DEALLOCATE(O_B21D, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(B21E)) THEN
        DEALLOCATE(O_B21E, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(B22D)) THEN
        DEALLOCATE(O_B22D, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(B22E)) THEN
        DEALLOCATE(O_B22E, STAT=L_STAT_DEALLOC)
    ENDIF
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE ZBBCSD_F95
