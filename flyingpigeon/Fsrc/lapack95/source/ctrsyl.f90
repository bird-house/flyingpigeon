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

PURE SUBROUTINE CTRSYL_F95(A,B,C,SCALE,TRANA,TRANB,ISGN,INFO)
    ! Fortran77 call:
    ! CTRSYL(TRANA,TRANB,ISGN,M,N,A,LDA,B,LDB,C,LDC,SCALE,INFO)
    ! TRANA='N','C','T'; default: 'N'
    ! TRANB='N','C','T'; default: 'N'
    ! ISGN=+1,-1; default: +1
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_TRSYL, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY CTRSYL_MKL95(A,B,C,SCALE,TRANA,TRANB,ISGN,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(OUT) :: SCALE
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANA
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANB
    INTEGER, INTENT(IN), OPTIONAL :: ISGN
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(IN) :: A(:,:)
    COMPLEX(WP), INTENT(IN) :: B(:,:)
    COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'TRSYL'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_TRANA
    CHARACTER(LEN=1) :: O_TRANB
    INTEGER :: O_ISGN
    INTEGER :: O_INFO
    INTEGER :: M
    INTEGER :: N
    INTEGER :: LDA
    INTEGER :: LDB
    INTEGER :: LDC
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(ISGN)) THEN
        O_ISGN = ISGN
    ELSE
        O_ISGN = 1
    ENDIF
    IF(PRESENT(TRANA)) THEN
        O_TRANA = TRANA
    ELSE
        O_TRANA = 'N'
    ENDIF
    IF(PRESENT(TRANB)) THEN
        O_TRANB = TRANB
    ELSE
        O_TRANB = 'N'
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    LDB = MAX(1,SIZE(B,1))
    LDC = MAX(1,SIZE(C,1))
    M = SIZE(A,2)
    N = SIZE(B,2)
    ! <<< Call lapack77 routine >>>
    CALL F77_TRSYL(O_TRANA,O_TRANB,O_ISGN,M,N,A,LDA,B,LDB,C,LDC,SCALE,  &
     &                                                           O_INFO)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE CTRSYL_F95
