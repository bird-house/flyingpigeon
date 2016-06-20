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

PURE SUBROUTINE DGEESX_F95(A,WR,WI,VS,SELECT,SDIM,RCONDE,RCONDV,INFO)
    ! Fortran77 call:
    ! DGEESX(JOBVS,SORT,SELECT,SENSE,N,A,LDA,SDIM,WR,WI,VS,LDVS,RCONDE,
    !   RCONDV,WORK,LWORK,IWORK,LIWORK,BWORK,INFO)
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_GEESX, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY DGEESX_MKL95(A,WR,WI,VS,SELECT,SDIM,RCONDE,RCONDV,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(OUT), OPTIONAL :: SDIM
    REAL(WP), INTENT(OUT), OPTIONAL :: RCONDE
    REAL(WP), INTENT(OUT), OPTIONAL :: RCONDV
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: A(:,:)
    REAL(WP), INTENT(OUT) :: WR(:)
    REAL(WP), INTENT(OUT) :: WI(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VS(:,:)
    ! <<< Function arguments >>>
    INTERFACE
        PURE LOGICAL FUNCTION SELECT(WR,WI)
            INTEGER, PARAMETER :: WP = KIND(1.0D0)
            REAL(WP), INTENT(IN) :: WR,WI
        END FUNCTION SELECT
    END INTERFACE
    OPTIONAL :: SELECT
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GEESX'
    ! <<< Local scalars >>>
    INTEGER :: O_SDIM
    REAL(WP) :: O_RCONDE
    REAL(WP) :: O_RCONDV
    INTEGER :: O_INFO
    CHARACTER(LEN=1) :: JOBVS
    CHARACTER(LEN=1) :: SORT
    CHARACTER(LEN=1) :: SENSE
    INTEGER :: N
    INTEGER :: LDA
    INTEGER :: LDVS
    INTEGER :: LWORK
    INTEGER :: LIWORK
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_VS(:,:)
    REAL(WP), POINTER :: WORK(:)
    INTEGER, POINTER :: IWORK(:)
    LOGICAL, POINTER :: BWORK(:)
    ! <<< Stubs to "allocate" optional arrays >>>
    LOGICAL, TARGET :: L_A1_LOGI(1)
    REAL(WP), TARGET :: L_A2_REAL(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(VS)) THEN
        JOBVS = 'V'
    ELSE
        JOBVS = 'N'
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    IF(PRESENT(VS)) THEN
        LDVS = MAX(1,SIZE(VS,1))
    ELSE
        LDVS = 1
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
    LIWORK = MAX(1,N*N/4)
    LWORK = MAX(1,3*N,(N*N+1)/2+N)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(VS)) THEN
        O_VS => VS
    ELSE
        O_VS => L_A2_REAL
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
        CALL F77_GEESX(JOBVS,SORT,SELECT,SENSE,N,A,LDA,O_SDIM,WR,WI,    &
     & O_VS,LDVS,O_RCONDE,O_RCONDV,WORK,LWORK,IWORK,LIWORK,BWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(RCONDE)) THEN
        RCONDE = O_RCONDE
    ENDIF
    IF(PRESENT(RCONDV)) THEN
        RCONDV = O_RCONDV
    ENDIF
    IF(PRESENT(SDIM)) THEN
        SDIM = O_SDIM
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    IF(PRESENT(SELECT)) THEN
        DEALLOCATE(BWORK, STAT=L_STAT_DEALLOC)
    ENDIF
    DEALLOCATE(IWORK, STAT=L_STAT_DEALLOC)
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE DGEESX_F95
