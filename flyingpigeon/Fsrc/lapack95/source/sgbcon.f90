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

PURE SUBROUTINE SGBCON_F95(AB,IPIV,ANORM,RCOND,KL,NORM,INFO)
    ! Fortran77 call:
    ! SGBCON(NORM,N,KL,KU,AB,LDAB,IPIV,ANORM,RCOND,WORK,IWORK,INFO)
    ! NORM='1','O','I'; default: '1'
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_GBCON, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY SGBCON_MKL95(AB,IPIV,ANORM,RCOND,KL,NORM,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(IN) :: ANORM
    REAL(WP), INTENT(OUT) :: RCOND
    INTEGER, INTENT(IN), OPTIONAL :: KL
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(IN) :: AB(:,:)
    INTEGER, INTENT(IN) :: IPIV(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GBCON'
    ! <<< Local scalars >>>
    INTEGER :: O_KL
    CHARACTER(LEN=1) :: O_NORM
    INTEGER :: O_INFO
    INTEGER :: N
    INTEGER :: KU
    INTEGER :: LDAB
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: WORK(:)
    INTEGER, POINTER :: IWORK(:)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(NORM)) THEN
        O_NORM = NORM
    ELSE
        O_NORM = '1'
    ENDIF
    LDAB = MAX(1,SIZE(AB,1))
    N = SIZE(AB,2)
    IF(PRESENT(KL)) THEN
        O_KL = KL
    ELSE
        O_KL = (LDAB-1)/3
    ENDIF
    KU = LDAB-2*O_KL-1
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    ALLOCATE(IWORK(N), STAT=L_STAT_ALLOC)
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(3*N), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_GBCON(O_NORM,N,O_KL,KU,AB,LDAB,IPIV,ANORM,RCOND,WORK,  &
     &                                                     IWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    DEALLOCATE(IWORK, STAT=L_STAT_DEALLOC)
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE SGBCON_F95
