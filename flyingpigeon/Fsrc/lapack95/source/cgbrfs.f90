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

PURE SUBROUTINE CGBRFS_F95(AB,AFB,IPIV,B,X,KL,TRANS,FERR,BERR,INFO)
    ! Fortran77 call:
    ! CGBRFS(TRANS,N,KL,KU,NRHS,AB,LDAB,AFB,LDAFB,IPIV,B,LDB,X,LDX,FERR,
    !   BERR,WORK,RWORK,INFO)
    ! TRANS='N','C','T'; default: 'N'
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_GBRFS, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY CGBRFS_MKL95(AB,AFB,IPIV,B,X,KL,TRANS,FERR,BERR,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(IN), OPTIONAL :: KL
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(IN) :: AB(:,:)
    COMPLEX(WP), INTENT(IN) :: AFB(:,:)
    INTEGER, INTENT(IN) :: IPIV(:)
    COMPLEX(WP), INTENT(IN) :: B(:,:)
    COMPLEX(WP), INTENT(INOUT) :: X(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: FERR(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: BERR(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GBRFS'
    ! <<< Local scalars >>>
    INTEGER :: O_KL
    CHARACTER(LEN=1) :: O_TRANS
    INTEGER :: O_INFO
    INTEGER :: N
    INTEGER :: KU
    INTEGER :: NRHS
    INTEGER :: LDAB
    INTEGER :: LDAFB
    INTEGER :: LDB
    INTEGER :: LDX
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_FERR(:)
    REAL(WP), POINTER :: O_BERR(:)
    COMPLEX(WP), POINTER :: WORK(:)
    REAL(WP), POINTER :: RWORK(:)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(TRANS)) THEN
        O_TRANS = TRANS
    ELSE
        O_TRANS = 'N'
    ENDIF
    LDAB = MAX(1,SIZE(AB,1))
    LDAFB = MAX(1,SIZE(AFB,1))
    LDB = MAX(1,SIZE(B,1))
    LDX = MAX(1,SIZE(X,1))
    N = SIZE(AB,2)
    NRHS = SIZE(B,2)
    IF(PRESENT(KL)) THEN
        O_KL = KL
    ELSE
        O_KL = (LDAB-1)/2
    ENDIF
    KU = LDAB-O_KL-1
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(BERR)) THEN
        O_BERR => BERR
    ELSE
        ALLOCATE(O_BERR(NRHS), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(FERR)) THEN
            O_FERR => FERR
        ELSE
            ALLOCATE(O_FERR(NRHS), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(RWORK(N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(2*N), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_GBRFS(O_TRANS,N,O_KL,KU,NRHS,AB,LDAB,AFB,LDAFB,IPIV,B, &
     &                        LDB,X,LDX,O_FERR,O_BERR,WORK,RWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(BERR)) THEN
        DEALLOCATE(O_BERR, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(FERR)) THEN
        DEALLOCATE(O_FERR, STAT=L_STAT_DEALLOC)
    ENDIF
    DEALLOCATE(RWORK, STAT=L_STAT_DEALLOC)
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE CGBRFS_F95
