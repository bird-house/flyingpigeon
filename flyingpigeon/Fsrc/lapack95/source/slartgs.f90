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

PURE SUBROUTINE SLARTGS_F95(X,Y,SIGMA,SN,CS)
    ! Fortran77 call:
    ! SLARTGS( X,Y,SIGMA,CS,SN )
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_LARTGS, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY SLARTGS_MKL95(X,Y,SIGMA,SN,CS)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(IN) :: X
    REAL(WP), INTENT(IN) :: Y
    REAL(WP), INTENT(IN) :: SIGMA
    REAL(WP), INTENT(OUT) :: SN
    REAL(WP), INTENT(OUT) :: CS
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=6), PARAMETER :: SRNAME = 'LARTGS'
    ! <<< Local scalars >>>
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    ! <<< Call lapack77 routine >>>
    CALL F77_LARTGS( X,Y,SIGMA,CS,SN )
END SUBROUTINE SLARTGS_F95
