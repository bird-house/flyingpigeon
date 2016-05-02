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

PURE SUBROUTINE CGESVD_F95(A,S,U,VT,WW,JOB,INFO)
    ! Fortran77 call:
    ! CGESVD(JOBU,JOBVT,M,N,A,LDA,S,U,LDU,VT,LDVT,WORK,LWORK,RWORK,INFO)
    ! JOB='N','U','V'; default: 'N'
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_GESVD, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY CGESVD_MKL95(A,S,U,VT,WW,JOB,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOB
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(INOUT) :: A(:,:)
    REAL(WP), INTENT(OUT) :: S(:)
    COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: U(:,:)
    COMPLEX(WP), INTENT(OUT), OPTIONAL, TARGET :: VT(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL :: WW(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GESVD'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_JOB
    INTEGER :: O_INFO
    CHARACTER(LEN=1) :: JOBU
    CHARACTER(LEN=1) :: JOBVT
    INTEGER :: M
    INTEGER :: N
    INTEGER :: LDA
    INTEGER :: LDU
    INTEGER :: LDVT
    INTEGER :: LWORK
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    COMPLEX(WP), POINTER :: O_U(:,:)
    COMPLEX(WP), POINTER :: O_VT(:,:)
    COMPLEX(WP), POINTER :: WORK(:)
    REAL(WP), POINTER :: RWORK(:)
    ! <<< Arrays to request optimal sizes >>>
    COMPLEX(WP) :: S_WORK(1)
    ! <<< Stubs to "allocate" optional arrays >>>
    COMPLEX(WP), TARGET :: L_A2_COMP(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(JOB)) THEN
        O_JOB = JOB
    ELSE
        O_JOB = 'N'
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    IF(PRESENT(U)) THEN
        LDU = MAX(1,SIZE(U,1))
    ELSE
        LDU = 1
    ENDIF
    IF(PRESENT(VT)) THEN
        LDVT = MAX(1,SIZE(VT,1))
    ELSE
        LDVT = 1
    ENDIF
    M = SIZE(A,1)
    N = SIZE(A,2)
    IF(PRESENT(U)) THEN
        IF(SIZE(U,2)==M) THEN
            JOBU = 'A'
        ELSE
            JOBU = 'S'
        ENDIF
    ELSE
        IF((O_JOB.EQ.'U'.OR.O_JOB.EQ.'u')) THEN
            JOBU = 'O'
        ELSE
            JOBU = 'N'
        ENDIF
    ENDIF
    IF(PRESENT(VT)) THEN
        IF(SIZE(VT,1)==N) THEN
            JOBVT = 'A'
        ELSE
            JOBVT = 'S'
        ENDIF
    ELSE
        IF((O_JOB.EQ.'V'.OR.O_JOB.EQ.'v')) THEN
            JOBVT = 'O'
        ELSE
            JOBVT = 'N'
        ENDIF
    ENDIF
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
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
    ALLOCATE(RWORK(5*MIN(M,N)), STAT=L_STAT_ALLOC)
    ! <<< Request work array(s) size >>>
    LWORK = -1
    CALL F77_GESVD(JOBU,JOBVT,M,N,A,LDA,S,O_U,LDU,O_VT,LDVT,S_WORK,     &
     &                                               LWORK,RWORK,O_INFO)
    ! <<< Exit if error: bad parameters >>>
    IF(O_INFO /= 0) THEN
        GOTO 200
    ENDIF
    LWORK = S_WORK(1)
    ! <<< Allocate work arrays with requested sizes >>>
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(LWORK), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_GESVD(JOBU,JOBVT,M,N,A,LDA,S,O_U,LDU,O_VT,LDVT,WORK,   &
     &                                               LWORK,RWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional array parameters >>>
    IF(PRESENT(WW)) THEN
        WW = RWORK(1:MIN(M,N)-1)
    ENDIF
    ! <<< Deallocate work arrays with requested sizes >>>
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
200    CONTINUE
    ! <<< Deallocate local and work arrays >>>
    DEALLOCATE(RWORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE CGESVD_F95
