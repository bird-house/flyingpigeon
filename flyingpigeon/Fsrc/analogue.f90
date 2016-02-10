!> © LSCE – Laboratory related to CEA/DSM – CNRS – UVSQ, 
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
PROGRAM analogue
!! Program uses IBM extension SYSTEM()
USE config
USE routines
USE eofs

IMPLICIT NONE
TYPE (config_type) :: configs
REAL(8), ALLOCATABLE :: var_archi(:,:,:)
REAL(8), ALLOCATABLE :: var_sim(:,:,:)
TYPE (dims_type) :: dim_archi
TYPE (dims_type) :: dim_sim
INTEGER, ALLOCATABLE :: dates_archi(:)
INTEGER, ALLOCATABLE :: dates_sim(:)
CHARACTER(8) :: clockdate
CHARACTER(10) :: clocktime
INTEGER, ALLOCATABLE :: analogue_dates(:,:)
REAL(8), ALLOCATABLE :: distances(:,:)
REAL, ALLOCATABLE :: spatial_corr(:,:)
INTEGER, ALLOCATABLE :: ranks_archi(:,:,:)
INTEGER, ALLOCATABLE :: ranks_sim(:,:,:)
CHARACTER(4000) :: headerline
CHARACTER(50) :: formatstring
!CHARACTER(50) :: headerformat
CHARACTER(11), ALLOCATABLE :: headerpieces(:)
INTEGER :: ia, it
!EXTERNAL :: rms
INTEGER :: arg_count
CHARACTER(50) :: configfilename
REAL(8), ALLOCATABLE :: eigenvalues(:), eigenvectors(:,:)
REAL(8), ALLOCATABLE :: variance(:), cumvariance(:), sqrootweights(:)
REAL(8), ALLOCATABLE :: pcs_archi(:,:), pcs_sim(:,:)
TYPE (dims_type) :: dim_pcsim
TYPE (dims_type) :: dim_pcarchi

CALL DATE_AND_TIME(clockdate, clocktime)
PRINT*, clockdate, ' ', clocktime
! how many arguments were given to the file?
arg_count = COMMAND_ARGUMENT_COUNT()
SELECT CASE (arg_count)
 CASE (0)
  configfilename = "config.txt"
 CASE DEFAULT
  call GET_COMMAND_ARGUMENT(1, configfilename)
END SELECT
PRINT*, configfilename
! read configuration file
configs = get_configuration(TRIM(configfilename))
IF (.NOT. configs%param%silent) THEN
 PRINT*, "read config"
 PRINT*, TRIM(configs%files%outputfile)
END IF
! get dimensions of the input data files
dim_archi = get_dims(TRIM(configs%files%archivefile))
dim_sim = get_dims(TRIM(configs%files%simulationfile))
IF (.NOT. configs%param%silent) PRINT*, "got dimensions"
! verify that lat and lon dimensions are equally long in archive and simulation file
IF (dim_archi%lon_dim == dim_sim%lon_dim .AND. dim_archi%lat_dim == dim_sim%lat_dim) THEN 
! allocate data arrays
 ALLOCATE(var_archi(dim_archi%lon_dim, dim_archi%lat_dim, dim_archi%time_dim), &
  & var_sim(dim_sim%lon_dim, dim_sim%lat_dim, dim_sim%time_dim), &
  & dates_archi(dim_archi%time_dim), dates_sim(dim_sim%time_dim), &
  & analogue_dates(configs%param%nanalog, dim_sim%time_dim), &
  & distances(configs%param%nanalog, dim_sim%time_dim), &
  & spatial_corr(configs%param%nanalog, dim_sim%time_dim), &
  & ranks_archi(dim_archi%lon_dim, dim_archi%lat_dim, dim_archi%time_dim), &
  & ranks_sim(dim_sim%lon_dim, dim_sim%lat_dim, dim_sim%time_dim) )
  IF (configs%param%calccor) THEN
   ALLOCATE(headerpieces(3*configs%param%nanalog))
  ELSE
   ALLOCATE(headerpieces(2*configs%param%nanalog))
  END IF
!PRINT*, "allocated variables"
ELSE
 PRINT*, 'analogue error: lat or lon dimesions differ in archive and simulation file.' 
 PRINT*, 'Program stops'
 STOP
END IF

! read input files
 CALL DATE_AND_TIME(clockdate, clocktime)
IF (.NOT. configs%param%silent) PRINT*,'read input ', clockdate, ' ', clocktime
var_archi = get_data(TRIM(configs%files%archivefile), TRIM(configs%param%varname), dim_archi)
var_sim = get_data(TRIM(configs%files%simulationfile), TRIM(configs%param%varname), dim_sim)
IF (.NOT. configs%param%silent) PRINT*, "got data"
dates_archi = get_dates(TRIM(configs%files%archivefile), dim_archi%time_dim, "base_dates.txt")
dates_sim = get_dates(TRIM(configs%files%simulationfile), dim_sim%time_dim, "sim_dates.txt")
IF (.NOT. configs%param%silent) PRINT*, "got dates" 

! remove seasonal cycle, that is calculate anomalies
IF (configs%param%seacyc) THEN
 CALL DATE_AND_TIME(clockdate, clocktime)
IF (.NOT. configs%param%silent) PRINT*,'rm seasonal cycle', clockdate, ' ', clocktime
 var_archi = rm_cyc("seasoncyc_base.nc", var_archi, dates_archi, dim_archi%lon_dim, &
  & dim_archi%lat_dim, dim_archi%time_dim, TRIM(configs%param%varname), configs%param%cycsmooth)
 var_sim = rm_cyc("seasoncyc_sim.nc", var_sim, dates_sim, dim_sim%lon_dim, &
  & dim_sim%lat_dim, dim_sim%time_dim, TRIM(configs%param%varname), configs%param%cycsmooth)
END IF
! precalculate ranks for later rank correlation calculation
IF (configs%param%calccor) THEN
 CALL DATE_AND_TIME(clockdate, clocktime)
IF (.NOT. configs%param%silent) PRINT*,'get ranks ', clockdate, ' ', clocktime
 ranks_archi = get_ranks(dim_archi,var_archi)
 ranks_sim = get_ranks(dim_sim, var_sim)
 CALL DATE_AND_TIME(clockdate, clocktime)
IF (.NOT. configs%param%silent) PRINT*, 'get ranks done ', clockdate, ' ', clocktime
END IF

IF (.NOT. configs%param%silent) PRINT*, configs%param%distfun
! select distance to calculate
SELECT CASE (TRIM(configs%param%distfun))
 CASE("rms", "rmse","euclidean")
   ! compute analogues 
   IF (configs%param%calccor) THEN
    CALL compute_analogues(dates_sim, dates_archi, var_sim, var_archi, dim_archi, &
     & dim_sim, rms, configs%param%nanalog, configs%param%seasonwin, &
     & configs%param%timewin, configs%param%silent, analogue_dates, distances, &
     & ranks_archi, ranks_sim, spatial_corr)
   ELSE 
    CALL compute_analogues(dates_sim, dates_archi, var_sim, var_archi, dim_archi, &
     & dim_sim, rms, configs%param%nanalog, configs%param%seasonwin, &
     & configs%param%timewin, configs%param%silent, analogue_dates, distances)
    END IF
 CASE("mahalanobis")
! allocate additional variables needed in case of mahalanobis distance
  ALLOCATE(eigenvalues(dim_archi%lon_dim*dim_archi%lat_dim), &
   & eigenvectors(dim_archi%lon_dim*dim_archi%lat_dim, dim_archi%lon_dim*dim_archi%lat_dim), &
   & variance(dim_archi%lon_dim*dim_archi%lat_dim), cumvariance(dim_archi%lon_dim*dim_archi%lat_dim), &
   & sqrootweights(dim_archi%lon_dim*dim_archi%lat_dim), &
   & pcs_archi(dim_archi%lon_dim*dim_archi%lat_dim, dim_archi%time_dim), &
   & pcs_sim(dim_sim%lon_dim*dim_sim%lat_dim, dim_sim%time_dim))
   sqrootweights = 1
! calculate EOFs of archive
!CALL DATE_AND_TIME(clockdate, clocktime)
!PRINT*, clockdate, ' ', clocktime
IF (.NOT. configs%param%silent) PRINT*, 'calculate EOFs'
  CALL deof( RESHAPE(var_archi,(/dim_archi%lon_dim*dim_archi%lat_dim,dim_archi%time_dim/)), &
   & dim_archi%lon_dim*dim_archi%lat_dim, dim_archi%time_dim, &
   & dim_archi%lon_dim*dim_archi%lat_dim,0 , &
     & eigenvalues, eigenvectors, variance, cumvariance, &
     & sqrootweights )
! PRINT*, 'first eigenvector', eigenvectors(:,1)
IF (.NOT. configs%param%silent) THEN 
 CALL DATE_AND_TIME(clockdate, clocktime)
 PRINT*, clockdate, ' ', clocktime
 PRINT*, 'project'
END IF
! project archive and simulations of EOFs
  call deofpcs( RESHAPE(var_archi,(/dim_archi%lon_dim*dim_archi%lat_dim,dim_archi%time_dim/)), &
   & dim_archi%lon_dim*dim_archi%lat_dim, dim_archi%time_dim, dim_archi%lon_dim*dim_archi%lat_dim, &
   & eigenvectors, pcs_archi)
  call deofpcs( RESHAPE(var_sim,(/dim_sim%lon_dim*dim_sim%lat_dim,dim_sim%time_dim/)), &
   & dim_sim%lon_dim*dim_sim%lat_dim, dim_sim%time_dim, dim_sim%lon_dim*dim_sim%lat_dim, &
   & eigenvectors, pcs_sim)
! weight with eigenvalues
   DO it = 1,dim_sim%time_dim
    pcs_sim(:,it) = pcs_sim(:,it)*eigenvalues
   END DO
   DO it = 1,dim_archi%time_dim
    pcs_archi(:,it) = pcs_archi(:,it)*eigenvalues
   END DO
!CALL DATE_AND_TIME(clockdate, clocktime)
!PRINT*, clockdate, ' ', clocktime
!PRINT*, 'pcs_sim', pcs_sim(:,1)
! compute analogues
   dim_pcsim%lon_dim = dim_sim%lon_dim*dim_sim%lat_dim
   dim_pcsim%lat_dim = 1
   dim_pcsim%time_dim = dim_sim%time_dim
   dim_pcarchi%lon_dim = dim_archi%lon_dim*dim_archi%lat_dim
   dim_pcarchi%lat_dim = 1
   dim_pcarchi%time_dim = dim_archi%time_dim
  IF (configs%param%calccor) THEN
   CALL compute_analogues(dates_sim, dates_archi, pcs_sim, pcs_archi, dim_pcarchi, &
    & dim_pcsim, rms, configs%param%nanalog, configs%param%seasonwin, &
    & configs%param%timewin, configs%param%silent, analogue_dates, distances, &
    & ranks_archi, ranks_sim, spatial_corr)
  ELSE 
   CALL compute_analogues(dates_sim, dates_archi, pcs_sim, pcs_archi, dim_pcarchi,  &
    & dim_pcsim, rms, configs%param%nanalog, configs%param%seasonwin, &
    & configs%param%timewin, configs%param%silent, analogue_dates, distances)
  END IF
 CASE("of","opticalflow")
  IF (configs%param%calccor) THEN
    CALL compute_analogues(dates_sim, dates_archi, var_sim, var_archi, dim_archi, &
     & dim_sim, of, configs%param%nanalog, configs%param%seasonwin, &
     & configs%param%timewin, configs%param%silent, analogue_dates, distances, &
     & ranks_archi, ranks_sim, spatial_corr)
   ELSE 
    CALL compute_analogues(dates_sim, dates_archi, var_sim, var_archi, dim_archi, &
     & dim_sim, of, configs%param%nanalog, configs%param%seasonwin, &
     & configs%param%timewin, configs%param%silent, analogue_dates, distances)
   END IF
 CASE DEFAULT
   PRINT*, 'No valid distance function found. Please choose one of "rms", "rmse", "euclidean", "mahalanobis", "of", "optical flow".'
   PRINT*, 'Program stops.'
   STOP 
END SELECT
! PRINT*, analogue_dates(:,1), distances(:,1), spatial_corr(:,1)
! write output with correlations
IF (configs%param%calccor) THEN
! construct header
 DO ia=1,configs%param%nanalog
  IF (ia < 10 ) THEN
   WRITE(headerpieces(ia), '(A, I1)') 'date.an', ia
   WRITE(headerpieces(ia+configs%param%nanalog), '(A, I1)') 'dis', ia
   WRITE(headerpieces(ia+2*configs%param%nanalog), '(A, I1)') 'cor', ia
  ELSE IF (ia < 100) THEN
   WRITE(headerpieces(ia), '(A, I2)') 'date.an', ia
   WRITE(headerpieces(ia+configs%param%nanalog), '(A, I2)') 'dis', ia
   WRITE(headerpieces(ia+2*configs%param%nanalog), '(A, I2)') 'cor', ia
  ELSE 
   WRITE(headerpieces(ia), '(A, I3)') 'date.an', ia
   WRITE(headerpieces(ia+configs%param%nanalog), '(A, I3)') 'dis', ia
   WRITE(headerpieces(ia+2*configs%param%nanalog), '(A, I3)') 'cor', ia
  END IF
 END DO 
! write header
! WRITE(headerformat,'(A,I3.3,A)') '(',configs%param%nanalog*3 + 1,'A)'
! PRINT*, headerformat, size(headerpieces)
 WRITE(headerline,*) 'date ', headerpieces
! PRINT*, 'headerline written' 
 WRITE(formatstring,'(A,3(I3.3,A))') '(I9,',configs%param%nanalog, 'I9,', configs%param%nanalog,'F17.3,',configs%param%nanalog,'F13.7 )'
! PRINT*, formatstring
 OPEN(22,FILE=TRIM(configs%files%outputfile))
  WRITE(22,'(A)') TRIM(headerline)
IF (.NOT. configs%param%silent)  PRINT*, 'header written'
! write data
  DO it=1,dim_sim%time_dim
   WRITE(22,TRIM(formatstring)) &
    & dates_sim(it), analogue_dates(:, it), distances(:, it), spatial_corr(:, it)
  END DO
 CLOSE(22)
! write output without correlations
ELSE
! construct header
 DO ia=1,configs%param%nanalog
  IF (ia < 10) THEN
   WRITE(headerpieces(ia), '(A, I1)') 'date.an', ia
   WRITE(headerpieces(ia+configs%param%nanalog), '(A, I1)') 'dis', ia
  ELSE IF (ia < 100 ) THEN
   WRITE(headerpieces(ia), '(A, I2)') 'date.an', ia
   WRITE(headerpieces(ia+configs%param%nanalog), '(A, I2)') 'dis', ia
  ELSE
   WRITE(headerpieces(ia), '(A, I3)') 'date.an', ia
   WRITE(headerpieces(ia+configs%param%nanalog), '(A, I3)') 'dis', ia
  END IF
 END DO 
! write header
 WRITE(headerline,*) 'date ', headerpieces
 WRITE(formatstring,'(A,3(I3.3,A))') '(I9,',configs%param%nanalog, 'I9,', configs%param%nanalog,'F17.3 )'
 OPEN(22,FILE=TRIM(configs%files%outputfile))
  WRITE(22,'(A)') headerline
! write data
  DO it=1,dim_sim%time_dim
   WRITE(22,TRIM(formatstring)) &
    & dates_sim(it), analogue_dates(:, it), distances(:, it)
  END DO
 CLOSE(22)
END IF
CALL DATE_AND_TIME(clockdate, clocktime)
PRINT*, clockdate, ' ', clocktime

END PROGRAM analogue
