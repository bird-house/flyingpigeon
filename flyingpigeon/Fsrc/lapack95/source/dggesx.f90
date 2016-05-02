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

PURE SUBROUTINE DGGESX_F95(A,B,ALPHAR,ALPHAI,BETA,VSL,VSR,SELECT,SDIM,  &
     &                                               RCONDE,RCONDV,INFO)
    ! Fortran77 call:
    ! DGGESX(JOBVSL,JOBVSR,SORT,SELECT,SENSE,N,A,LDA,B,LDB,SDIM,ALPHAR,
    !   ALPHAI,BETA,VSL,LDVSL,VSR,LDVSR,RCONDE,RCONDV,WORK,LWORK,IWORK,
    !   LIWORK,BWORK,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_GGESX, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY DGGESX_MKL95(A,B,ALPHAR,ALPHAI,BETA,VSL,VSR,SELECT,SDIM,      &
     &                                               RCONDE,RCONDV,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(OUT), OPTIONAL :: SDIM
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: A(:,:)
    REAL(WP), INTENT(INOUT) :: B(:,:)
    REAL(WP), INTENT(OUT) :: ALPHAR(:)
    REAL(WP), INTENT(OUT) :: ALPHAI(:)
    REAL(WP), INTENT(OUT) :: BETA(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VSL(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VSR(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: RCONDE(2)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: RCONDV(2)
    ! <<< Function arguments >>>
    INTERFACE
        PURE LOGICAL FUNCTION SELECT(ALPHAR,ALPHAI,BETA)
            INTEGER, PARAMETER :: WP = KIND(1.0D0)
            REAL(WP), INTENT(IN) :: ALPHAR,ALPHAI,BETA
        END FUNCTION SELECT
    END INTERFACE
    OPTIONAL :: SELECT
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GGESX'
    ! <<< Local scalars >>>
    INTEGER :: O_SDIM
    INTEGER :: O_INFO
    CHARACTER(LEN=1) :: JOBVSL
    CHARACTER(LEN=1) :: JOBVSR
    CHARACTER(LEN=1) :: SORT
    CHARACTER(LEN=1) :: SENSE
    INTEGER :: N
    INTEGER :: LDA
    INTEGER :: LDB
    INTEGER :: LDVSL
    INTEGER :: LDVSR
    INTEGER :: LWORK
    INTEGER :: LIWORK
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_VSL(:,:)
    REAL(WP), POINTER :: O_VSR(:,:)
    REAL(WP), TARGET :: O_O_RCONDE(2)
    REAL(WP), POINTER :: O_RCONDE(:)
    REAL(WP), TARGET :: O_O_RCONDV(2)
    REAL(WP), POINTER :: O_RCONDV(:)
    REAL(WP), POINTER :: WORK(:)
    INTEGER, POINTER :: IWORK(:)
    LOGICAL, POINTER :: BWORK(:)
    ! <<< Stubs to "allocate" optional arrays >>>
    LOGICAL, TARGET :: L_A1_LOGI(1)
    REAL(WP), TARGET :: L_A1_REAL(1)
    REAL(WP), TARGET :: L_A2_REAL(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(VSL)) THEN
        JOBVSL = 'V'
    ELSE
        JOBVSL = 'N'
    ENDIF
    IF(PRESENT(VSR)) THEN
        JOBVSR = 'V'
    ELSE
        JOBVSR = 'N'
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    LDB = MAX(1,SIZE(B,1))
    IF(PRESENT(VSL)) THEN
        LDVSL = MAX(1,SIZE(VSL,1))
    ELSE
        LDVSL = 1
    ENDIF
    IF(PRESENT(VSR)) THEN
        LDVSR = MAX(1,SIZE(VSR,1))
    ELSE
        LDVSR = 1
    ENDIF
    N = SIZE(A,2)
    IF(PRESENT(RCONDE).AND.PRESENT(RCONDV)) THEN
        SENSE = 'B'
    ELSEIF(PRESENT(RCONDE)) THEN
        SENSE = 'E'
    ELSEIF(PRESENT(RCONDV)) THEN
        SENSE = 'V'
    ELSE
        SENSE = 'N'
    ENDIF
    IF(PRESENT(SELECT)) THEN
        SORT = 'S'
    ELSE
        SORT = 'N'
    ENDIF
    LIWORK = MAX(1,N+6)
    LWORK = MAX(8*(N+1)+16,(N*N+1)/2)
    IF((PRESENT(RCONDE).OR.PRESENT(RCONDV)).AND.                        &
     &                                        .NOT.PRESENT(SELECT)) THEN
        O_INFO=-1001; GOTO 1001
    ENDIF
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(RCONDE)) THEN
        O_RCONDE => RCONDE
    ELSE
        O_RCONDE => L_A1_REAL
    ENDIF
    IF(PRESENT(RCONDV)) THEN
        O_RCONDV => RCONDV
    ELSE
        O_RCONDV => L_A1_REAL
    ENDIF
    IF(PRESENT(VSL)) THEN
        O_VSL => VSL
    ELSE
        O_VSL => L_A2_REAL
    ENDIF
    IF(PRESENT(VSR)) THEN
        O_VSR => VSR
    ELSE
        O_VSR => L_A2_REAL
    ENDIF
    IF(.NOT.PRESENT(SELECT)) THEN
        BWORK => L_A1_LOGI
    ELSE
        ALLOCATE(BWORK(N), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(IWORK(LIWORK), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(LWORK), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_GGESX(JOBVSL,JOBVSR,SORT,SELECT,SENSE,N,A,LDA,B,LDB,   &
     &      O_SDIM,ALPHAR,ALPHAI,BETA,O_VSL,LDVSL,O_VSR,LDVSR,O_RCONDE, &
     &                    O_RCONDV,WORK,LWORK,IWORK,LIWORK,BWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(SDIM)) THEN
        SDIM = O_SDIM
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    IF(PRESENT(SELECT)) THEN
        DEALLOCATE(BWORK, STAT=L_STAT_DEALLOC)
    ENDIF
    DEALLOCATE(IWORK, STAT=L_STAT_DEALLOC)
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
1001    CONTINUE
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE DGGESX_F95
