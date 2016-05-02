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

PURE SUBROUTINE ZHSEIN_F95(H,W,SELECT,VL,VR,IFAILL,IFAILR,INITV,EIGSRC, &
     &                                                           M,INFO)
    ! Fortran77 call:
    ! ZHSEIN(JOB,EIGSRC,INITV,SELECT,N,H,LDH,W,VL,LDVL,VR,LDVR,MM,M,
    !   WORK,RWORK,IFAILL,IFAILR,INFO)
    ! INITV='N','U'; default: 'N'
    ! EIGSRC='N','Q'; default: 'N'
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_HSEIN, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY ZHSEIN_MKL95(H,W,SELECT,VL,VR,IFAILL,IFAILR,INITV,EIGSRC,M,   &
     &                                                             INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: INITV
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: EIGSRC
    INTEGER, INTENT(OUT), OPTIONAL :: M
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(IN) :: H(:,:)
    COMPLEX(WP), INTENT(INOUT) :: W(:)
    LOGICAL, INTENT(IN) :: SELECT(:)
    COMPLEX(WP), INTENT(INOUT), OPTIONAL, TARGET :: VL(:,:)
    COMPLEX(WP), INTENT(INOUT), OPTIONAL, TARGET :: VR(:,:)
    INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAILL(:)
    INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IFAILR(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'HSEIN'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_INITV
    CHARACTER(LEN=1) :: O_EIGSRC
    INTEGER :: O_M
    INTEGER :: O_INFO
    CHARACTER(LEN=1) :: JOB
    INTEGER :: N
    INTEGER :: LDH
    INTEGER :: LDVL
    INTEGER :: LDVR
    INTEGER :: MM
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    COMPLEX(WP), POINTER :: O_VL(:,:)
    COMPLEX(WP), POINTER :: O_VR(:,:)
    INTEGER, POINTER :: O_IFAILL(:)
    INTEGER, POINTER :: O_IFAILR(:)
    COMPLEX(WP), POINTER :: WORK(:)
    REAL(WP), POINTER :: RWORK(:)
    ! <<< Stubs to "allocate" optional arrays >>>
    INTEGER, TARGET :: L_A1_INTE(1)
    COMPLEX(WP), TARGET :: L_A2_COMP(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(EIGSRC)) THEN
        O_EIGSRC = EIGSRC
    ELSE
        O_EIGSRC = 'N'
    ENDIF
    IF(PRESENT(INITV)) THEN
        O_INITV = INITV
    ELSE
        O_INITV = 'N'
    ENDIF
    IF(PRESENT(VL).AND.PRESENT(VR)) THEN
        JOB = 'B'
    ELSEIF(PRESENT(VL)) THEN
        JOB = 'L'
    ELSEIF(PRESENT(VR)) THEN
        JOB = 'R'
    ELSE
        O_INFO=-1001; GOTO 1001
    ENDIF
    LDH = MAX(1,SIZE(H,1))
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
    N = SIZE(H,2)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Check compatibility for correct memory allocation >>>
    IF(.NOT.PRESENT(VR).AND.PRESENT(IFAILR)) THEN
        O_INFO=-1001; GOTO 1001
    ENDIF
    ! <<< Allocate local and work arrays >>>
    IF(.NOT.PRESENT(VL)) THEN
        IF(PRESENT(IFAILL)) THEN
            O_INFO=-1001; GOTO 1001
        ELSE
            O_IFAILL => L_A1_INTE
        ENDIF
    ELSE
        IF(PRESENT(IFAILL)) THEN
            O_IFAILL => IFAILL
        ELSE
            ALLOCATE(O_IFAILL(MM), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(.NOT.PRESENT(VR)) THEN
        O_IFAILR => L_A1_INTE
    ELSE
        IF(L_STAT_ALLOC==0) THEN
            IF(PRESENT(IFAILR)) THEN
                O_IFAILR => IFAILR
            ELSE
                ALLOCATE(O_IFAILR(MM), STAT=L_STAT_ALLOC)
            ENDIF
        ENDIF
    ENDIF
    IF(PRESENT(VL)) THEN
        O_VL => VL
    ELSE
        O_VL => L_A2_COMP
    ENDIF
    IF(PRESENT(VR)) THEN
        O_VR => VR
    ELSE
        O_VR => L_A2_COMP
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(RWORK(N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(N*N), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_HSEIN(JOB,O_EIGSRC,O_INITV,SELECT,N,H,LDH,W,O_VL,LDVL, &
     &             O_VR,LDVR,MM,O_M,WORK,RWORK,O_IFAILL,O_IFAILR,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(M)) THEN
        M = O_M
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    IF(PRESENT(VL) .AND..NOT. PRESENT(IFAILL)) THEN
        DEALLOCATE(O_IFAILL, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(PRESENT(VR) .AND..NOT. PRESENT(IFAILR)) THEN
        DEALLOCATE(O_IFAILR, STAT=L_STAT_DEALLOC)
    ENDIF
    DEALLOCATE(RWORK, STAT=L_STAT_DEALLOC)
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
1001    CONTINUE
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE ZHSEIN_F95
