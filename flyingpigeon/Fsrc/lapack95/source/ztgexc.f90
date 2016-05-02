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

PURE SUBROUTINE ZTGEXC_F95(A,B,IFST,ILST,Z,Q,INFO)
    ! Fortran77 call:
    ! ZTGEXC(WANTQ,WANTZ,N,A,LDA,B,LDB,Q,LDQ,Z,LDZ,IFST,ILST,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_TGEXC, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY ZTGEXC_MKL95(A,B,IFST,ILST,Z,Q,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(INOUT), OPTIONAL :: IFST
    INTEGER, INTENT(INOUT), OPTIONAL :: ILST
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(INOUT) :: A(:,:)
    COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    COMPLEX(WP), INTENT(INOUT), OPTIONAL, TARGET :: Z(:,:)
    COMPLEX(WP), INTENT(INOUT), OPTIONAL, TARGET :: Q(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'TGEXC'
    ! <<< Local scalars >>>
    INTEGER :: O_IFST
    INTEGER :: O_ILST
    INTEGER :: O_INFO
    LOGICAL :: WANTQ
    LOGICAL :: WANTZ
    INTEGER :: N
    INTEGER :: LDA
    INTEGER :: LDB
    INTEGER :: LDQ
    INTEGER :: LDZ
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    COMPLEX(WP), POINTER :: O_Z(:,:)
    COMPLEX(WP), POINTER :: O_Q(:,:)
    ! <<< Stubs to "allocate" optional arrays >>>
    COMPLEX(WP), TARGET :: L_A2_COMP(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(IFST)) THEN
        O_IFST = IFST
    ELSE
        O_IFST = 1
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    LDB = MAX(1,SIZE(B,1))
    IF(PRESENT(Q)) THEN
        LDQ = MAX(1,SIZE(Q,1))
    ELSE
        LDQ = 1
    ENDIF
    IF(PRESENT(Z)) THEN
        LDZ = MAX(1,SIZE(Z,1))
    ELSE
        LDZ = 1
    ENDIF
    N = SIZE(A,2)
    IF(PRESENT(Q)) THEN
        WANTQ = .TRUE.
    ELSE
        WANTQ = .FALSE.
    ENDIF
    IF(PRESENT(Z)) THEN
        WANTZ = .TRUE.
    ELSE
        WANTZ = .FALSE.
    ENDIF
    IF(PRESENT(ILST)) THEN
        O_ILST = ILST
    ELSE
        O_ILST = N
    ENDIF
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(Q)) THEN
        O_Q => Q
    ELSE
        O_Q => L_A2_COMP
    ENDIF
    IF(PRESENT(Z)) THEN
        O_Z => Z
    ELSE
        O_Z => L_A2_COMP
    ENDIF
    ! <<< Call lapack77 routine >>>
    CALL F77_TGEXC(WANTQ,WANTZ,N,A,LDA,B,LDB,O_Q,LDQ,O_Z,LDZ,O_IFST,    &
     &                                                    O_ILST,O_INFO)
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(IFST)) THEN
        IFST = O_IFST
    ENDIF
    IF(PRESENT(ILST)) THEN
        ILST = O_ILST
    ENDIF
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE ZTGEXC_F95
