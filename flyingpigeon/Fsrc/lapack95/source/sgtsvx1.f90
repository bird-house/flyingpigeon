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

PURE SUBROUTINE SGTSVX1_F95(DL,D,DU,B,X,DLF,DF,DUF,DU2,IPIV,FACT,TRANS, &
     &                                             FERR,BERR,RCOND,INFO)
    ! Fortran77 call:
    ! SGTSVX(FACT,TRANS,N,NRHS,DL,D,DU,DLF,DF,DUF,DU2,IPIV,B,LDB,X,LDX,
    !   RCOND,FERR,BERR,WORK,IWORK,INFO)
    ! FACT='N','F'; default: 'N'
    ! TRANS='N','C','T'; default: 'N'
    ! <<< Use statements >>>
    USE F77_LAPACK1, ONLY: F77_GTSVX
    USE F77_LAPACK, ONLY: F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY SGTSVX1_MKL95(DL,D,DU,B,X,DLF,DF,DUF,DU2,IPIV,FACT,TRANS,FERR,&
     &                                                  BERR,RCOND,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: FACT
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
    REAL(WP), INTENT(OUT), OPTIONAL :: FERR
    REAL(WP), INTENT(OUT), OPTIONAL :: BERR
    REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(IN) :: DL(:)
    REAL(WP), INTENT(IN) :: D(:)
    REAL(WP), INTENT(IN) :: DU(:)
    REAL(WP), INTENT(IN) :: B(:)
    REAL(WP), INTENT(OUT) :: X(:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: DLF(:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: DF(:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: DUF(:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: DU2(:)
    INTEGER, INTENT(INOUT), OPTIONAL, TARGET :: IPIV(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GTSVX'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_FACT
    CHARACTER(LEN=1) :: O_TRANS
    REAL(WP) :: O_RCOND
    INTEGER :: O_INFO
    INTEGER :: N
    INTEGER :: NRHS
    INTEGER :: LDB
    INTEGER :: LDX
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_DLF(:)
    REAL(WP), POINTER :: O_DF(:)
    REAL(WP), POINTER :: O_DUF(:)
    REAL(WP), POINTER :: O_DU2(:)
    INTEGER, POINTER :: O_IPIV(:)
    REAL(WP), POINTER :: WORK(:)
    INTEGER, POINTER :: IWORK(:)
    REAL(WP) :: R_BERR(1)
    REAL(WP) :: R_FERR(1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(FACT)) THEN
        O_FACT = FACT
    ELSE
        O_FACT = 'N'
    ENDIF
    IF(PRESENT(TRANS)) THEN
        O_TRANS = TRANS
    ELSE
        O_TRANS = 'N'
    ENDIF
    LDB = MAX(1,SIZE(B,1))
    LDX = MAX(1,SIZE(X,1))
    N = SIZE(D)
    NRHS = 1
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(DF)) THEN
        O_DF => DF
    ELSE
        ALLOCATE(O_DF(N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(DLF)) THEN
            O_DLF => DLF
        ELSE
            ALLOCATE(O_DLF(N-1), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(DU2)) THEN
            O_DU2 => DU2
        ELSE
            ALLOCATE(O_DU2(N-2), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(DUF)) THEN
            O_DUF => DUF
        ELSE
            ALLOCATE(O_DUF(N-1), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(IPIV)) THEN
            O_IPIV => IPIV
        ELSE
            ALLOCATE(O_IPIV(N), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(IWORK(N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(3*N), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_GTSVX(O_FACT,O_TRANS,N,NRHS,DL,D,DU,O_DLF,O_DF,O_DUF,  &
     & O_DU2,O_IPIV,B,LDB,X,LDX,O_RCOND,R_FERR,R_BERR,WORK,IWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalars for reduced arrays >>>
    IF(PRESENT(BERR)) THEN
        BERR = R_BERR(1)
    ENDIF
    IF(PRESENT(FERR)) THEN
        FERR = R_FERR(1)
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(RCOND)) THEN
        RCOND = O_RCOND
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(DF)) THEN
        DEALLOCATE(O_DF, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(DLF)) THEN
        DEALLOCATE(O_DLF, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(DU2)) THEN
        DEALLOCATE(O_DU2, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(DUF)) THEN
        DEALLOCATE(O_DUF, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(IPIV)) THEN
        DEALLOCATE(O_IPIV, STAT=L_STAT_DEALLOC)
    ENDIF
    DEALLOCATE(IWORK, STAT=L_STAT_DEALLOC)
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE SGTSVX1_F95
