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

PURE SUBROUTINE CTPRFB_F95(T,V,A,B,DIRECT,STOREV,TRANS,SIDE)
    ! Fortran77 call:
    ! CTPRFB( SIDE,TRANS,DIRECT,STOREV,M,N,K,L,V,LDV,T,LDT,A,LDA,B,LDB,WORK,LDWORK )
    ! SIDE='l','r' ; default: 'l'
    ! TRANS='n','t' ; default: 'n'
    ! DIRECT='f','b' ; default: 'f'
    ! STOREV='c','r' ; default: 'c'
    ! <<< Use statements >>>
    USE F77_LAPACK, ONLY: F77_TPRFB, F77_XERBLA
    ! <<< ENTRY point >>>
    ENTRY CTPRFB_MKL95(T,V,A,B,DIRECT,STOREV,TRANS,SIDE)
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: DIRECT
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: STOREV
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: SIDE
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(IN) :: T(:,:)
    COMPLEX(WP), INTENT(IN) :: V(:,:)
    COMPLEX(WP), INTENT(INOUT) :: A(:,:)
    COMPLEX(WP), INTENT(INOUT) :: B(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'TPRFB'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_DIRECT
    CHARACTER(LEN=1) :: O_STOREV
    CHARACTER(LEN=1) :: O_TRANS
    CHARACTER(LEN=1) :: O_SIDE
    INTEGER :: O_INFO
    INTEGER :: N
    INTEGER :: LDT
    INTEGER :: L
    INTEGER :: LDA
    INTEGER :: LDV
    INTEGER :: LDB
    INTEGER :: K
    INTEGER :: M
    INTEGER :: LDWORK
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    INTEGER :: L_NN
    ! <<< Local arrays >>>
    COMPLEX(WP), POINTER :: WORK(:,:)
    ! <<< Intrinsic functions >>>
    INTRINSIC INT, MAX, PRESENT, REAL, SIZE, SQRT
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(DIRECT)) THEN
        O_DIRECT = DIRECT
    ELSE
        O_DIRECT = 'F'
    ENDIF
    IF(PRESENT(SIDE)) THEN
        O_SIDE = SIDE
    ELSE
        O_SIDE = 'L'
    ENDIF
    IF(PRESENT(STOREV)) THEN
        O_STOREV = STOREV
    ELSE
        O_STOREV = 'C'
    ENDIF
    IF(PRESENT(TRANS)) THEN
        O_TRANS = TRANS
    ELSE
        O_TRANS = 'N'
    ENDIF
    L_NN = SIZE(T)
    IF(L_NN <= 0) THEN
        K = L_NN
    ELSE
        ! Packed matrix "T(K*(K+1)/2)", so: K=(-1+8*(SIZE(T)))/2
        K = INT((-1+SQRT(1+8*REAL(L_NN,WP)))*0.5)
    ENDIF
    L = SIZE(V,2)
    LDA = MAX(1,SIZE(A,1))
    LDB = MAX(1,SIZE(B,1))
    LDT = MAX(1,SIZE(T,1))
    LDV = MAX(1,SIZE(V,1))
    M = SIZE(B,1)
    N = SIZE(B,2)
    LDWORK = MAX(K,M)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    ALLOCATE(WORK(LDWORK,N), STAT=L_STAT_ALLOC)
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_TPRFB( O_SIDE,O_TRANS,O_DIRECT,O_STOREV,M,N,K,L,V,LDV,T,LDT,A,LDA,B,LDB,WORK,LDWORK )
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
END SUBROUTINE CTPRFB_F95
