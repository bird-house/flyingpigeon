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

PURE SUBROUTINE CGGBAK_F95(V,ILO,IHI,LSCALE,RSCALE,JOB,INFO)
    ! Fortran77 call:
    ! CGGBAK(JOB,SIDE,N,ILO,IHI,LSCALE,RSCALE,M,V,LDV,INFO)
    ! Default ILO=1
    ! Default IHI=N
    ! JOB='B','S','P','N'; default: 'B'
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_GGBAK, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY CGGBAK_MKL95(V,ILO,IHI,LSCALE,RSCALE,JOB,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(IN), OPTIONAL :: ILO
    INTEGER, INTENT(IN), OPTIONAL :: IHI
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOB
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(INOUT) :: V(:,:)
    ! LSCALE: INOUT intent instead of IN because PURE.
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: LSCALE(:)
    ! RSCALE: INOUT intent instead of IN because PURE.
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: RSCALE(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GGBAK'
    ! <<< Local scalars >>>
    INTEGER :: O_ILO
    INTEGER :: O_IHI
    CHARACTER(LEN=1) :: O_JOB
    INTEGER :: O_INFO
    CHARACTER(LEN=1) :: SIDE
    INTEGER :: N
    INTEGER :: M
    INTEGER :: LDV
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_LSCALE(:)
    REAL(WP), POINTER :: O_RSCALE(:)
    ! <<< Stubs to "allocate" optional arrays >>>
    REAL(WP), TARGET :: L_A1_REAL(1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(ILO)) THEN
        O_ILO = ILO
    ELSE
        O_ILO = 1
    ENDIF
    IF(PRESENT(JOB)) THEN
        O_JOB = JOB
    ELSE
        O_JOB = 'B'
    ENDIF
    LDV = MAX(1,SIZE(V,1))
    M = SIZE(V,2)
    N = SIZE(V,1)
    IF(PRESENT(LSCALE).AND.PRESENT(RSCALE)) THEN
        O_INFO=-1001; GOTO 1001
    ELSEIF(PRESENT(LSCALE)) THEN
        SIDE = 'L'
    ELSEIF(PRESENT(RSCALE)) THEN
        SIDE = 'R'
    ELSE
        O_INFO=-1001; GOTO 1001
    ENDIF
    IF(PRESENT(IHI)) THEN
        O_IHI = IHI
    ELSE
        O_IHI = N
    ENDIF
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(LSCALE)) THEN
        O_LSCALE => LSCALE
    ELSE
        O_LSCALE => L_A1_REAL
    ENDIF
    IF(PRESENT(RSCALE)) THEN
        O_RSCALE => RSCALE
    ELSE
        O_RSCALE => L_A1_REAL
    ENDIF
    ! <<< Call lapack77 routine >>>
    CALL F77_GGBAK(O_JOB,SIDE,N,O_ILO,O_IHI,O_LSCALE,O_RSCALE,M,V,LDV,  &
     &                                                           O_INFO)
1001    CONTINUE
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE CGGBAK_F95
