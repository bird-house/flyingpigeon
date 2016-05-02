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

PURE SUBROUTINE SDISNA_F95(D,SEP,JOB,MINMN,INFO)
    ! Fortran77 call:
    ! SDISNA(JOB,M,N,D,SEP,INFO)
    ! JOB='E','L','R'; default: 'E'
    ! MINMN='M','N'; default: 'M';  Superfluous if JOB='E'
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_DISNA, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY SDISNA_MKL95(D,SEP,JOB,MINMN,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOB
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: MINMN
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(IN) :: D(:)
    REAL(WP), INTENT(OUT) :: SEP(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'DISNA'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_JOB
    CHARACTER(LEN=1) :: O_MINMN
    INTEGER :: O_INFO
    INTEGER :: M
    INTEGER :: N
    ! <<< Intrinsic functions >>>
    INTRINSIC PRESENT
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(JOB)) THEN
        O_JOB = JOB
    ELSE
        O_JOB = 'E'
    ENDIF
    IF(PRESENT(MINMN)) THEN
        O_MINMN = MINMN
    ELSE
        O_MINMN = 'M'
    ENDIF
    IF((O_MINMN.EQ.'M'.OR.O_MINMN.EQ.'m')) THEN
        M = SIZE(D)
    ELSE
        M = SIZE(D)+1
    ENDIF
    IF((O_MINMN.EQ.'M'.OR.O_MINMN.EQ.'m')) THEN
        N = SIZE(D)+1
    ELSE
        N = SIZE(D)
    ENDIF
    ! <<< Call lapack77 routine >>>
    CALL F77_DISNA(O_JOB,M,N,D,SEP,O_INFO)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE SDISNA_F95
