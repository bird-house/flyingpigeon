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

PURE SUBROUTINE DGBEQU_F95(AB,R,C,KL,ROWCND,COLCND,AMAX,INFO)
    ! Fortran77 call:
    ! DGBEQU(M,N,KL,KU,AB,LDAB,R,C,ROWCND,COLCND,AMAX,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_GBEQU, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY DGBEQU_MKL95(AB,R,C,KL,ROWCND,COLCND,AMAX,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(IN), OPTIONAL :: KL
    REAL(WP), INTENT(OUT), OPTIONAL :: ROWCND
    REAL(WP), INTENT(OUT), OPTIONAL :: COLCND
    REAL(WP), INTENT(OUT), OPTIONAL :: AMAX
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(IN) :: AB(:,:)
    REAL(WP), INTENT(OUT) :: R(:)
    REAL(WP), INTENT(OUT) :: C(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GBEQU'
    ! <<< Local scalars >>>
    INTEGER :: O_KL
    REAL(WP) :: O_ROWCND
    REAL(WP) :: O_COLCND
    REAL(WP) :: O_AMAX
    INTEGER :: O_INFO
    INTEGER :: M
    INTEGER :: N
    INTEGER :: KU
    INTEGER :: LDAB
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    LDAB = MAX(1,SIZE(AB,1))
    M = SIZE(R)
    N = SIZE(AB,2)
    IF(PRESENT(KL)) THEN
        O_KL = KL
    ELSE
        O_KL = (LDAB-1)/2
    ENDIF
    KU = LDAB-O_KL-1
    ! <<< Call lapack77 routine >>>
    CALL F77_GBEQU(M,N,O_KL,KU,AB,LDAB,R,C,O_ROWCND,O_COLCND,O_AMAX,    &
     &                                                           O_INFO)
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(AMAX)) THEN
        AMAX = O_AMAX
    ENDIF
    IF(PRESENT(COLCND)) THEN
        COLCND = O_COLCND
    ENDIF
    IF(PRESENT(ROWCND)) THEN
        ROWCND = O_ROWCND
    ENDIF
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE DGBEQU_F95
