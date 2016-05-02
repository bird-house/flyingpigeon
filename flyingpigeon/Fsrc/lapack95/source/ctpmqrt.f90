!*******************************************************************************
!   Copyright(C) 2005-2013 Intel Corporation. All Rights Reserved.
!   The source code contained  or  described herein and all documents related to
!   the source code ("Material") are owned by Intel Corporation or its suppliers
!   or licensors.  Title to the  Material remains with  Intel Corporation or its
!   suppliers and licensors. The Material contains trade secrets and proprietary
!   and  confidential  information of  Intel or its suppliers and licensors. The
!   Material  is  protected  by  worldwide  copyright  and trade secret laws and
!   treaty  provisions. No part of the Material may be used, copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way without Intel's prior express written permission.
!   No license  under any  patent, copyright, trade secret or other intellectual
!   property right is granted to or conferred upon you by disclosure or delivery
!   of the Materials,  either expressly, by implication, inducement, estoppel or
!   otherwise.  Any  license  under  such  intellectual property  rights must be
!   express and approved by Intel in writing.
!
!*******************************************************************************
!  Content:
!      F95 interface for LAPACK routines
!*******************************************************************************
! This file was generated automatically!
!*******************************************************************************

PURE SUBROUTINE CTPMQRT_F95(V,T,A,B,K,NB,TRANS,SIDE,INFO)
    ! Fortran77 call:
    ! CTPMQRT( SIDE,TRANS,M,N,K,L,NB,V,LDV,T,LDT,A,LDA,B,LDB,WORK,INFO )
    ! SIDE='l','r' ; default: 'l'
    ! TRANS='n','c' ; default: 'n'
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_TPMQRT, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY CTPMQRT_MKL95(V,T,A,B,K,NB,TRANS,SIDE,INFO)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(IN) :: K
    INTEGER, INTENT(IN) :: NB
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(IN) :: V(:,:)
    COMPLEX(WP), INTENT(IN) :: T(:,:)
    COMPLEX(WP), INTENT(INOUT) :: A(:,:)
    COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=6), PARAMETER :: SRNAME = 'TPMQRT'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_TRANS
    CHARACTER(LEN=1) :: O_SIDE
    INTEGER :: O_INFO
    INTEGER :: LDA
    INTEGER :: LDV
    INTEGER :: LDB
    INTEGER :: LDT
    INTEGER :: N
    INTEGER :: L
    INTEGER :: M
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    COMPLEX(WP), POINTER :: WORK(:)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(SIDE)) THEN
        O_SIDE = SIDE
    ELSE
        O_SIDE = 'L'
    ENDIF
    IF(PRESENT(TRANS)) THEN
        O_TRANS = TRANS
    ELSE
        O_TRANS = 'N'
    ENDIF
    L = SIZE(V,2)
    LDA = MAX(1,SIZE(A,1))
    LDB = MAX(1,SIZE(B,1))
    LDT = MAX(1,SIZE(T,1))
    LDV = MAX(1,SIZE(V,1))
    M = SIZE(B,1)
    N = SIZE(B,2)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    ALLOCATE(WORK(MAX(M,N)*NB), STAT=L_STAT_ALLOC)
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_TPMQRT( SIDE,O_TRANS,M,N,K,L,NB,V,LDV,T,LDT,A,LDA,B,LDB,WORK,INFO )
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL F77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE CTPMQRT_F95
