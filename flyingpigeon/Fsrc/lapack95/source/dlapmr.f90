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

PURE SUBROUTINE DLAPMR_F95(X,K,FORWRD)
    ! Fortran77 call:
    ! DLAPMR(FORWRD,M,N,X,LDX,K)
    ! UPLO='U','L'; default: 'U'
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_LAPMR, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY DLAPMR_MKL95(X,K,FORWRD)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    LOGICAL, INTENT(IN), OPTIONAL :: FORWRD
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: X(:,:)
    INTEGER, INTENT(INOUT) :: K(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'LAPMR'
    ! <<< Local scalars >>>
    LOGICAL :: O_FORWRD
    INTEGER :: LDX
    INTEGER :: N
    INTEGER :: M
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(FORWRD)) THEN
        O_FORWRD = FORWRD
    ELSE
        O_FORWRD = .TRUE.
    ENDIF
    LDX = MAX(1,SIZE(X,1))
    M = SIZE(X,1)
    N = SIZE(X,2)
    ! <<< Call lapack77 routine >>>
    CALL F77_LAPMR(O_FORWRD,M,N,X,LDX,K)
END SUBROUTINE DLAPMR_F95
