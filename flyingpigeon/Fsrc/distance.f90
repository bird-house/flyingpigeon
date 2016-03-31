!> © LSCE – Laboratory related to CEA/DSF – CNRS – UVSQ, 
!! Sabine Radanovics (sabine.radanovics@lsce.ipsl.fr) andPascal Yiou (pascal.yiou@lsce.ipsl.fr)
!! This source code is part of the CASTf90 software IDDN.FR.001.030008.000.S.P.2016.000.20700
!!
!! This software is governed by the CeCILL license under French law and abiding by the rules of distribution 
!! of free software. You can use, modify and / or redistribute the software under the terms of the 
!! CeCILL license as circulated by CEA, CNRS and INRIA at the following URL "http://www.cecill.info".
!!
!! As a counterpart to the access to the source code and rights to copy, modify and redistribute granted by 
!! the license, users are provided only with a limited warranty and the software's author, 
!! the holder of the economic rights, and the successive licensors have only limited liability.
!!
!! In this respect, the user's attention is drawn to the risks associated with loading, using, 
!! modifying and/or developing or reproducing the software by the user in light of its specific status 
!! of free software, that may mean that it is complicated to manipulate, and that also therefore means 
!! that it is reserved for developers and experienced professionals having in-depth computer knowledge. 
!! Users are therefore encouraged to load and test the software's suitability as regards their requirements 
!! in conditions enabling the security of their systems and/or data to be ensured and, more generally, 
!! to use and operate it in the same conditions as regards security.
!!
!! The fact that you are presently reading this means that you have had knowledge of the CeCILL license 
!! and that you accept its terms.
!!
MODULE distance
!USE lapack95
USE f95_lapack
CONTAINS

!> calculates euclidean (squared) distance between two arrays of type real
FUNCTION rms(arr_test, arr_ref, nx, ny)
IMPLICIT NONE
INTEGER, INTENT(IN) :: nx
INTEGER, INTENT(IN) :: ny
REAL(8), INTENT(IN) :: arr_test(nx,ny)
REAL(8), INTENT(IN) :: arr_ref(nx,ny)
REAL(8) :: rms

rms = SQRT(SUM((arr_test-arr_ref)**2)/(nx*ny))

END FUNCTION rms

!****************************

!> displacement and amplitude  distance based on optical flow field deformation.
FUNCTION of(arr_test, arr_ref, nx, ny, info)
IMPLICIT NONE
INTEGER, INTENT(IN) :: nx
INTEGER, INTENT(IN) :: ny
REAL(8), INTENT(IN) :: arr_test(nx,ny)
REAL(8), INTENT(IN) :: arr_ref(nx,ny)
INTEGER, OPTIONAL :: info
REAL(8) :: of

INTEGER :: W
REAL(8) :: gx(nx,ny), gy(nx,ny), gxx(nx,ny), gyy(nx,ny), gxy(nx,ny)
INTEGER :: iw, jw
REAL(8) :: of_field(nx-3,ny-3)
INTEGER :: cut_inds(6) ![startind x, endind x, nx, startind y, endind y, ny]

W=5
! calculate local "derivatives"
CALL grads(arr_test, nx, ny, gx, gy, gxx, gyy, gxy)
!PRINT*, gy(5,:)
!STOP
! split in windows
DO jw=1,(ny-3)
!PRINT*, 'jw = ', jw
 DO iw=1,(nx-3)
! calculate the displacement and amplitude score for each subdomain
!  PRINT*, 'iw = ', iw
  cut_inds = inds(iw,jw, nx, ny, W)
  IF (PRESENT(info) .AND. info == 9075) THEN
  of_field(iw, jw) = oflow(arr_ref(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & arr_test(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & gx(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & gy(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & gxx(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & gyy(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & gxy(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), cut_inds(3), cut_inds(6), iw) 
    PRINT*, jw, iw,  of_field(iw,jw)
   ELSE
    of_field(iw, jw) = oflow(arr_ref(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & arr_test(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & gx(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & gy(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & gxx(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & gyy(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), &
   & gxy(cut_inds(1):cut_inds(2),cut_inds(4):cut_inds(5)), cut_inds(3), cut_inds(6)) 
   END IF
 END DO
END DO
!OPEN(25, FILE="of_field.txt")
!OPEN(25, FILE="add_field.txt")
!OPEN(25, FILE="dx_field.txt")
!OPEN(25, FILE="dy_field.txt")
!DO jw=1,(ny-3)
! WRITE(25,'(47F9.3)') of_field(:,jw)
!END DO
!CLOSE(25)
! average over all subdomains
of = SUM(of_field)/SIZE(of_field)
END FUNCTION of

! ****************************
FUNCTION inds(iw,jw, nx, ny, W)
IMPLICIT NONE
INTEGER :: iw, jw, nx,ny, W
INTEGER :: inds(6) ![startind x, endind x, nx, startind y, endind y, ny]

inds(1) = MAX((iw+1) - (W-1)/2,1)
inds(2) = MIN((iw+1) + (W-1)/2,nx)
inds(3) = inds(2) - inds(1) +1
inds(4) = MAX((jw+1) - (W-1)/2,1)
inds(5) = MIN((jw+1) + (W-1)/2,ny)
inds(6) = inds(5) - inds(4) +1

END FUNCTION inds

!*****************************

!> calculate gradients (local derivatives)
SUBROUTINE grads(field, nx, ny, gx, gy, gxx, gyy, gxy)
IMPLICIT NONE
INTEGER, INTENT(IN) :: nx
INTEGER, INTENT(IN) :: ny
REAL(8), INTENT(IN) :: field(nx,ny)

REAL(8), INTENT(OUT) :: gx(nx,ny)
REAL(8), INTENT(OUT) :: gy(nx,ny)
REAL(8), INTENT(OUT) :: gxx(nx,ny)
REAL(8), INTENT(OUT) :: gyy(nx,ny)
REAL(8), INTENT(OUT) :: gxy(nx,ny)
INTEGER :: i, j

gx = 0.
gy = 0.
gxx = 0.
gyy = 0.
gxy = 0.

DO j=1,ny-2
 DO i=1,nx-2
  gx(i,j) = field(i+1,j)-field(i,j)
  gy(i,j) = field(i,j+1)-field(i,j)
  gxx(i,j) = field(i+2,j)-2*field(i+1,j) + field(i,j)
  gyy(i,j) = field(i,j+2)-2*field(i,j+1) + field(i,j)
  gxy(i,j) = field(i+1,j+1) - field(i,j+1) - field(i+1,j) + field(i,j)
! gx(i+1,j+1) = field(i+1,j)-field(i,j)
! gy(i+1,j+1) = field(i,j+1)-field(i,j)
! gxx(i+1,j+1) = field(i+2,j)-2*field(i+1,j) + field(i,j)
! gyy(i+1,j+1) = field(i,j+2)-2*field(i,j+1) + field(i,j)
! gxy(i+1,j+1) = field(i+1,j+1) - field(i,j+1) - field(i+1,j) + field(i,j)
 END DO
END DO
END SUBROUTINE grads

! ***************************************

!> Function for local displacement and amplitude score (on a subdomain)
REAL(8) FUNCTION oflow(arr_ref, arr_test, gx, gy, gxx, gyy, gxy, nx, ny, infoin)
IMPLICIT NONE
INTEGER, INTENT(IN) :: nx
INTEGER, INTENT(IN) :: ny
REAL(8), INTENT(IN) :: arr_test(nx,ny)
REAL(8), INTENT(IN) :: arr_ref(nx,ny)
REAL(8) :: gx(nx,ny), gy(nx,ny), gxx(nx,ny), gyy(nx,ny), gxy(nx,ny)
INTEGER, OPTIONAL :: infoin
REAL(8) :: betaoptim(3)
REAL(8) :: beta(nx*ny)
REAL(8) :: gradmat(nx*ny,3)
INTEGER :: mcount
INTEGER :: ix, iy
LOGICAL :: fail
INTEGER :: info
REAL(8) :: unity=1.


!PRINT*, arr_ref
!PRINT*, arr_test
mcount=0
DO iy=1,ny
 DO ix=1,nx
  mcount=mcount+1
  beta(mcount) = arr_ref(ix,iy) - arr_test(ix,iy)
  gradmat(mcount,:) = (/unity, gx(ix,iy), gy(ix,iy)/)
 END DO
END DO 
!PRINT*, gradmat(1,:)
!PRINT*, nx, ny, mcount
! linear model (lapack library) to derive beta 0-2, that is intensity error, delta x and delta y
CALL la_gels(gradmat,beta,'N',info)
IF (PRESENT(infoin) .AND. infoin == 3) THEN
 PRINT*, 'lm beta ', beta(1:3)
! optimise betas using function with non-linear terms and BFGS optimisation
CALL optim(3,beta(1:3), betaoptim, nx*ny, RESHAPE(arr_ref,(/nx*ny/)), &
 RESHAPE(arr_test,(/nx*ny/)), RESHAPE(gx,(/nx*ny/)), &
 RESHAPE(gy,(/nx*ny/)), RESHAPE(gxx,(/nx*ny/)), RESHAPE(gyy,(/nx*ny/)), &
 RESHAPE(gxy,(/nx*ny/)), fail, infoin)
 PRINT*, 'beta optim ', betaoptim
ELSE 
! optimise betas using function with non-linear terms and BFGS optimisation
CALL optim(3,beta(1:3), betaoptim, nx*ny, RESHAPE(arr_ref,(/nx*ny/)), &
 RESHAPE(arr_test,(/nx*ny/)), RESHAPE(gx,(/nx*ny/)), &
 RESHAPE(gy,(/nx*ny/)), RESHAPE(gxx,(/nx*ny/)), RESHAPE(gyy,(/nx*ny/)), &
 RESHAPE(gxy,(/nx*ny/)), fail)
END IF
!IF (fail) THEN
! PRINT*, 'error: optimisation failed'
! STOP
!END IF
! normalise the components and calculate the length of the distance vector.
betaoptim(2:3) = betaoptim(2:3)/(/nx, ny/) ! delta x and delta y are normalised by the subdomain dimension.
betaoptim(1) = betaoptim(1)/(MAXVAL(arr_ref)-MINVAL(arr_ref)) ! the amplitude is normalised by the range  of values within the subdomain.
oflow = SQRT(SUM(betaoptim**2))

END FUNCTION oflow

! **********************************

!> flow function, returns mean squared residuals between ref and morphed test
REAL(8) FUNCTION my_flow(lenarr, arr_ref, arr_test, beta, gx, gy, gxx, gyy, gxy)
IMPLICIT NONE
INTEGER :: lenarr
REAL(8) :: arr_ref(lenarr)
REAL(8) :: arr_test(lenarr)
REAL(8) :: beta(3)
REAL(8) :: gx(lenarr), gy(lenarr), gxx(lenarr), gyy(lenarr), gxy(lenarr)

my_flow = SUM((arr_ref - arr_test - beta(1) - beta(2)*gx - beta(3)*gy - &
 & 0.5*(beta(2)**2 * gxx + beta(3)**2 * gyy + beta(2)*beta(3)*gxy))**2)/lenarr
END FUNCTION my_flow

!***********************************************

!> gradient function
FUNCTION my_grad(lenarr, arr_ref, arr_test, beta, gx, gy, gxx, gyy, gxy)
IMPLICIT NONE
INTEGER :: lenarr
REAL(8) :: arr_ref(lenarr)
REAL(8) :: arr_test(lenarr)
REAL(8) :: beta(3)
REAL(8) :: gx(lenarr), gy(lenarr), gxx(lenarr), gyy(lenarr), gxy(lenarr)
REAL(8) :: my_grad(3)

REAL(8) :: expression_store(lenarr)

expression_store = arr_ref - arr_test - beta(1) - beta(2)*gx - beta(3)*gy - &
 & 0.5*(beta(2)**2 * gxx + beta(3)**2 * gyy + beta(2)*beta(3)*gxy)
 
my_grad(1) = -2*SUM(expression_store)/lenarr
my_grad(2) = -2*SUM(expression_store*(gx + beta(2)*gxx + 0.5*beta(3)*gxy))/lenarr
my_grad(3) = -2*SUM(expression_store*(gy + beta(3)*gyy + 0.5*beta(2)*gxy))/lenarr

END FUNCTION my_grad

!*******************************

!> optimisation subroutine BFGS method translated from C code.
SUBROUTINE optim(n0,beta, betaoptim,lenarr, arr_ref, arr_test, gx, gy, gxx, gyy, gxy, fail, infoin)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n0
REAL(8), INTENT(IN) :: beta(n0)
INTEGER, INTENT(IN) :: lenarr
REAL(8), INTENT(IN) :: arr_ref(lenarr)
REAL(8), INTENT(IN) :: arr_test(lenarr)
REAL(8), INTENT(IN) :: gx(lenarr), gy(lenarr)
REAL(8), INTENT(IN) :: gxx(lenarr), gyy(lenarr), gxy(lenarr)
REAL(8), INTENT(OUT) :: betaoptim(n0)
LOGICAL, INTENT(OUT) :: fail
INTEGER, OPTIONAL :: infoin

REAL(8) :: stepredn=0.2
REAL(8) :: acctol=0.0001
REAL(8) :: reltest= 10.
REAL(8) :: big=1.0e+35 
REAL(8) :: reltol = 1.490116e-08 !1.e-04 !
INTEGER :: maxit = 200
REAL(8) :: beta_tmp(n0)
LOGICAL ::  accpoint, enough
REAL(8) :: g(n0), t(n0), X(n0), c(n0), B(n0,n0)
INTEGER :: icount, funcount, gradcount
REAL(8) :: f, gradproj
INTEGER :: i, j, ilast, iter
REAL(8) :: steplength
REAL(8) :: D1, D2 
REAL(8) :: Fmin

 iter = 0
 f = my_flow(lenarr, arr_ref, arr_test, beta, gx, gy, gxx, gyy, gxy)
 IF (.NOT. isfinite(f)) THEN
     PRINT*, "Error: initial value in 'vmmin' is not finite"
     PRINT*, "program stops"
     STOP
 END IF
!PRINT*,"initial  value ", f
! initialize
 Fmin = f
 funcount = 1
 gradcount = 1
 g = my_grad(lenarr, arr_ref, arr_test, beta, gx, gy, gxx, gyy, gxy)
 ilast = gradcount
 beta_tmp = beta
! first iteration
 iter = iter + 1
 icount = 0 
 DO WHILE (icount /= n0 .OR. ilast /= gradcount) ! achtung auf schleifenbedingung
  IF (PRESENT(infoin)) THEN
   PRINT*, iter<=1, icount /= n0, ilast /= gradcount, iter, icount, gradcount, ilast
   READ(*,*)
  END IF
  IF (ilast == gradcount) THEN
	  B = 0.0
	  DO i = 1,n0
		 B(i,i) = 1.0
	  END DO
  END IF
  X = beta_tmp
  c = g
  DO i = 1,n0
	    t(i) = - SUM(B(i,:)*g)
  END DO
  gradproj = SUM(t * g)
  IF (PRESENT(infoin)) PRINT*, 'gradproj = ',gradproj, Fmin, f
  IF (gradproj < 0.0) THEN	! search direction is downhill 
	    steplength = 1.0
	    accpoint = .FALSE.
	    DO WHILE (.NOT.(icount == n0 .OR. accpoint))
	     IF (PRESENT(infoin)) PRINT*, icount, accpoint
		 icount = 0
		 beta_tmp = X + steplength * t
		 icount = COUNT(reltest+X == reltest+beta_tmp) ! no change 
		 IF (PRESENT(infoin)) PRINT*, icount
		 IF (icount < n0) THEN
		    f = my_flow(lenarr, arr_ref, arr_test, beta_tmp, gx, gy, gxx, gyy, gxy)
		!    IF (PRESENT(infoin)) PRINT*, f
		    funcount = funcount+1
		    accpoint = isfinite(f) .AND. f <= (Fmin + gradproj * steplength * acctol)
		    IF (PRESENT(infoin)) PRINT*, 'accpoint ', accpoint
		    IF (.NOT. accpoint) THEN
			 steplength = steplength*stepredn
		    END IF
		  END IF
	     END DO 
	     enough = ABS(f - Fmin) <= reltol * (ABS(Fmin) + reltol)
	     !  stop if value if small or if relative change is low */
	     IF (PRESENT(infoin)) PRINT*, 'enough ', enough
	     IF (enough) THEN
		  icount = n0
		  Fmin = f
	     END IF
	     IF (icount < n0) THEN ! making progress */
		  Fmin = f
		  g = my_grad(lenarr, arr_ref, arr_test, beta_tmp, gx, gy, gxx, gyy, gxy)
		  gradcount = gradcount+1
		  iter = iter + 1
		  t = steplength * t
		  c = g - c
		  D1 = SUM(t * c)
		  IF (D1 > 0.) THEN
		    DO i=1,n0
			 X(i) = SUM(B(i,:)*c)
		    END DO
		    D2 = SUM(X*c)
		    D2 = 1.0 + D2 / D1
		    DO i = 1, n0 
			 DO j = 1,i 
			    B(i,j) = B(i,j) + (D2 * t(i) * t(j) - X(i) * t(j) - t(i) * X(j)) / D1
			    IF (i /= j) THEN
				 B(j,i) = B(i,j)
				END IF
			 END DO
		    END DO
		   ELSE ! D1 < 0 */
		    ilast = gradcount
		   END IF
	    ELSE !	/* no progress */
		 IF (ilast < gradcount) THEN
		  icount = 0
          ilast = gradcount
		 END IF
	    END IF
	  ELSE		! uphill search 
	      icount = 0
	      IF (ilast == gradcount) THEN
	       icount = n0
	      ELSE 
	       ilast = gradcount
	      END IF
	    !  Resets unless has just been reset */
	  END IF
	  IF (iter >= maxit) THEN
	 !  PRINT*, "stopped after ", iter,  " iterations"
       fail = .TRUE.
	   EXIT
	  END IF
	  IF (gradcount - ilast > 2 * n0) THEN
	   ilast = gradcount	! periodic restart */
	  END IF
 END DO

!PRINT*, "final  value ", Fmin
 IF (iter < maxit) THEN 
!  PRINT*, "converged"
  fail = .FALSE.
 END IF
 betaoptim = beta_tmp
END SUBROUTINE optim

!*******************************

LOGICAL FUNCTION isfinite(x)
IMPLICIT NONE
REAL(8) :: x
REAL(8) :: big=1.0e+35   !a very large number

IF (x < big) THEN
 isfinite = .TRUE.
ELSE
 isfinite = .FALSE.
END IF
END FUNCTION isfinite

!**********************************

END MODULE distance
