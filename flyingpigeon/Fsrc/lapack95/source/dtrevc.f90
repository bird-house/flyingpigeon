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

PURE SUBROUTINE DTREVC_F95(T,HOWMNY,SELECT,VL,VR,M,INFO)
    ! Fortran77 call:
    ! DTREVC(SIDE,HOWMNY,SELECT,N,T,LDT,VL,LDVL,VR,LDVR,MM,M,WORK,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_TREVC, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY DTREVC_MKL95(T,HOWMNY,SELECT,VL,VR,M,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: HOWMNY
    INTEGER, INTENT(OUT), OPTIONAL :: M
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(IN) :: T(:,:)
    LOGICAL, INTENT(INOUT), OPTIONAL, TARGET :: SELECT(:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: VL(:,:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: VR(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'TREVC'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_HOWMNY
    INTEGER :: O_M
    INTEGER :: O_INFO
    CHARACTER(LEN=1) :: SIDE
    INTEGER :: N
    INTEGER :: LDT
    INTEGER :: LDVL
    INTEGER :: LDVR
    INTEGER :: MM
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    LOGICAL, POINTER :: O_SELECT(:)
    REAL(WP), POINTER :: O_VL(:,:)
    REAL(WP), POINTER :: O_VR(:,:)
    REAL(WP), POINTER :: WORK(:)
    ! <<< Stubs to "allocate" optional arrays >>>
    LOGICAL, TARGET :: L_A1_LOGI(1)
    REAL(WP), TARGET :: L_A2_REAL(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(SELECT).AND.PRESENT(HOWMNY)) THEN
        O_INFO=-1001; GOTO 1001
    ELSEIF(PRESENT(SELECT)) THEN
        O_HOWMNY = 'S'
    ELSEIF(PRESENT(HOWMNY)) THEN
        IF((HOWMNY.EQ.'A'.OR.HOWMNY.EQ.'a').OR.                         &
     &    (HOWMNY.EQ.'B'.OR.HOWMNY.EQ.'b')) THEN
            O_HOWMNY = HOWMNY
        ELSE
            O_INFO=-1001; GOTO 1001
        ENDIF
    ELSE
        O_HOWMNY = 'A'
    ENDIF
    LDT = MAX(1,SIZE(T,1))
    IF(PRESENT(VL)) THEN
        LDVL = MAX(1,SIZE(VL,1))
    ELSE
        LDVL = 1
    ENDIF
    IF(PRESENT(VR)) THEN
        LDVR = MAX(1,SIZE(VR,1))
    ELSE
        LDVR = 1
    ENDIF
    IF(PRESENT(VL)) THEN
        MM = SIZE(VL,2)
    ELSE
        MM = SIZE(VR,2)
    ENDIF
    N = SIZE(T,2)
    IF(PRESENT(VL).AND.PRESENT(VR)) THEN
        SIDE = 'B'
    ELSEIF(PRESENT(VL)) THEN
        SIDE = 'L'
    ELSEIF(PRESENT(VR)) THEN
        SIDE = 'R'
    ELSE
        O_INFO=-1001; GOTO 1001
    ENDIF
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(SELECT)) THEN
        O_SELECT => SELECT
    ELSE
        O_SELECT => L_A1_LOGI
    ENDIF
    IF(PRESENT(VL)) THEN
        O_VL => VL
    ELSE
        O_VL => L_A2_REAL
    ENDIF
    IF(PRESENT(VR)) THEN
        O_VR => VR
    ELSE
        O_VR => L_A2_REAL
    ENDIF
    ALLOCATE(WORK(3*N), STAT=L_STAT_ALLOC)
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_TREVC(SIDE,O_HOWMNY,O_SELECT,N,T,LDT,O_VL,LDVL,O_VR,   &
     &                                          LDVR,MM,O_M,WORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(M)) THEN
        M = O_M
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
1001    CONTINUE
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE DTREVC_F95
