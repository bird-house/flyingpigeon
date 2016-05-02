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

PURE SUBROUTINE CBDSQR_F95(D,E,VT,U,C,UPLO,INFO)
    ! Fortran77 call:
    ! CBDSQR(UPLO,N,NCVT,NRU,NCC,D,E,VT,LDVT,U,LDU,C,LDC,WORK,INFO)
    ! UPLO='U','L'; default: 'U'
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_BDSQR, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY CBDSQR_MKL95(D,E,VT,U,C,UPLO,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: D(:)
    REAL(WP), INTENT(INOUT) :: E(:)
    COMPLEX(WP), INTENT(INOUT), OPTIONAL, TARGET :: VT(:,:)
    COMPLEX(WP), INTENT(INOUT), OPTIONAL, TARGET :: U(:,:)
    COMPLEX(WP), INTENT(INOUT), OPTIONAL, TARGET :: C(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'BDSQR'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_UPLO
    INTEGER :: O_INFO
    INTEGER :: N
    INTEGER :: NCVT
    INTEGER :: NRU
    INTEGER :: NCC
    INTEGER :: LDVT
    INTEGER :: LDU
    INTEGER :: LDC
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    COMPLEX(WP), POINTER :: O_VT(:,:)
    COMPLEX(WP), POINTER :: O_U(:,:)
    COMPLEX(WP), POINTER :: O_C(:,:)
    REAL(WP), POINTER :: WORK(:)
    ! <<< Stubs to "allocate" optional arrays >>>
    COMPLEX(WP), TARGET :: L_A2_COMP(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(UPLO)) THEN
        O_UPLO = UPLO
    ELSE
        O_UPLO = 'U'
    ENDIF
    N = SIZE(D)
    IF(PRESENT(C)) THEN
        NCC = SIZE(C,2)
    ELSE
        NCC = 0
    ENDIF
    IF(PRESENT(VT)) THEN
        NCVT = SIZE(VT,2)
    ELSE
        NCVT = 0
    ENDIF
    IF(PRESENT(U)) THEN
        NRU = SIZE(U,1)
    ELSE
        NRU = 0
    ENDIF
    LDC = MAX(1,N)
    LDU = MAX(1,NRU)
    LDVT = MAX(1,N)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(C)) THEN
        O_C => C
    ELSE
        O_C => L_A2_COMP
    ENDIF
    IF(PRESENT(U)) THEN
        O_U => U
    ELSE
        O_U => L_A2_COMP
    ENDIF
    IF(PRESENT(VT)) THEN
        O_VT => VT
    ELSE
        O_VT => L_A2_COMP
    ENDIF
    ALLOCATE(WORK(4*N), STAT=L_STAT_ALLOC)
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_BDSQR(O_UPLO,N,NCVT,NRU,NCC,D,E,O_VT,LDVT,O_U,LDU,O_C, &
     &                                                  LDC,WORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE CBDSQR_F95
