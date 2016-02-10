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
MODULE routines

USE read_files
USE distance
USE omp_lib

TYPE civildate_type
 INTEGER :: yyyy
 INTEGER :: mm
 INTEGER :: dd
 INTEGER :: hh
END TYPE civildate_type


CONTAINS

!> removing smoothed seasonal cycle -> calculate anomalies
!! smoothing is done with weighted moving average.
FUNCTION rm_cyc(filename,raw_data, raw_dates, lon_dim, lat_dim, time_dim, &
 & varname, cycsmooth)
IMPLICIT NONE
CHARACTER(*), INTENT(IN) :: filename
INTEGER, INTENT(IN) :: lon_dim
INTEGER, INTENT(IN) :: lat_dim
INTEGER, INTENT(IN) :: time_dim
REAL(8), INTENT(IN) :: raw_data(lon_dim, lat_dim, time_dim) 
INTEGER, INTENT(IN) :: raw_dates(time_dim)
REAL(8) :: rm_cyc(lon_dim, lat_dim, time_dim) 
CHARACTER(*) :: varname
INTEGER, INTENT(IN) :: cycsmooth

TYPE (dims_type) :: dim_cyc
REAL(8), ALLOCATABLE :: var_cyc(:,:,:)
INTEGER, ALLOCATABLE :: dates_cyc(:)
INTEGER :: tt, t , i,j
INTEGER, ALLOCATABLE :: moddate_cyc(:)
INTEGER :: moddate(time_dim)
CHARACTER(8) :: clockdate
CHARACTER(10) :: clocktime

REAL(8), ALLOCATABLE :: test_cyc(:,:,:)
INTEGER, ALLOCATABLE :: test_count(:)


! get dimension for cycle variable
dim_cyc = get_dims(TRIM(filename))
! verify that lat and lon dimensions are equally long in data and cycle file
IF (lon_dim == dim_cyc%lon_dim .AND. lat_dim == dim_cyc%lat_dim) THEN 
 ALLOCATE(var_cyc(dim_cyc%lon_dim, dim_cyc%lat_dim, dim_cyc%time_dim), &
  & dates_cyc(dim_cyc%time_dim), moddate_cyc(dim_cyc%time_dim), &
  & test_cyc(dim_cyc%lon_dim, dim_cyc%lat_dim, dim_cyc%time_dim), &
  & test_count(dim_cyc%time_dim))
ELSE
 PRINT*, 'analogue error: lat or lon dimesions differ in data and seasonal cycle file.' 
 PRINT*, 'Program stops'
 STOP
END IF
! get cycle data
var_cyc = get_data(TRIM(filename), varname, dim_cyc)
!CALL DATE_AND_TIME(clockdate, clocktime)
!PRINT*, 'smooth'
!PRINT*, clockdate, ' ', clocktime
! get cycle dates
dates_cyc = get_dates(TRIM(filename), dim_cyc%time_dim, "cyc_dates.txt")
 moddate = MOD(raw_dates, 10000)
 moddate_cyc = MOD(dates_cyc, 10000)
 
! smooth the seasonal cycle with weighted moving average.
var_cyc = wmasmooth(var_cyc, dim_cyc, cycsmooth)
OPEN(9,FILE="cyc_smooth.txt")
DO t=1,dim_cyc%time_dim
WRITE(9,*) var_cyc(1,1,t)
END DO
CLOSE(9)

! actually remove cycle
t=1
tt=1
DO WHILE (t <= time_dim)
 IF (moddate(t) == moddate_cyc(tt)) THEN
  rm_cyc(:,:,t) = raw_data(:,:,t)-var_cyc(:,:,tt)
  t=t+1
 ELSE IF (tt < dim_cyc%time_dim) THEN
  tt=tt+1
 ELSE
  tt=1
 END IF 
END DO

!PRINT*, 'raw = ', raw_data(1,1,391)
!PRINT*, 'syc = ', var_cyc(1,1,25)
!PRINT*, 'out = ', rm_cyc(1,1,391)

END FUNCTION rm_cyc

! *****************************

!> smoothing function using weighted moving average. 
!! weights are linearly decreasing to both sides of the center value.
FUNCTION wmasmooth(var_cyc, dim_cyc, cycsmooth)
IMPLICIT NONE
TYPE (dims_type), INTENT(IN) :: dim_cyc
REAL(8), INTENT(IN) :: var_cyc(dim_cyc%lon_dim, dim_cyc%lat_dim, dim_cyc%time_dim)
REAL(8) :: wmasmooth(dim_cyc%lon_dim, dim_cyc%lat_dim, dim_cyc%time_dim)
INTEGER :: cycsmooth
REAL(8), ALLOCATABLE :: var_tmp(:,:,:)
INTEGER :: halfsmooth
REAL(8) :: weights(cycsmooth)
INTEGER :: iw, ii, it
REAL(8) :: var_weight(dim_cyc%lon_dim, dim_cyc%lat_dim, cycsmooth)

! make sure that cycsmooth is an odd number
IF (MOD(cycsmooth,2)==0) THEN
 cycsmooth = cycsmooth+1
END IF
halfsmooth=FLOOR(cycsmooth/2.)
IF (halfsmooth > 0) THEN
! allocate temporary array such that half the smoothing interval can be added at each end of the time dimension.
 ALLOCATE( var_tmp(dim_cyc%lon_dim, dim_cyc%lat_dim, dim_cyc%time_dim+cycsmooth-1))
! fill temporary array
 var_tmp(:,:,1:halfsmooth) = var_cyc(:,:,dim_cyc%time_dim-halfsmooth+1:dim_cyc%time_dim)
 var_tmp(:,:,halfsmooth+1:dim_cyc%time_dim+halfsmooth) = var_cyc
 var_tmp(:,:,dim_cyc%time_dim+halfsmooth+1:dim_cyc%time_dim+cycsmooth-1) = var_cyc(:,:,1:halfsmooth)
! create weights with maximum in the center and linear decrease to both sides
 iw =1
 ii=1
 DO WHILE (iw <= cycsmooth)
  weights(ii) = iw
  iw=iw+2
  ii=ii+1
 END DO
  iw=iw-2
 DO WHILE (iw > 0 .AND. ii<= cycsmooth)
  iw = iw-2
  weights(ii) = iw
  ii=ii+1
 END DO
! PRINT*, weights
 weights=weights/SUM(weights)
! PRINT*, SUM(weights)
!PRINT*, weights
! smooth it
 DO it=1,dim_cyc%time_dim
! apply weights
  DO ii = 1, cycsmooth
   var_weight(:,:,ii) = var_tmp(:,:,it+ii-1)*weights(ii)
  END DO 
  wmasmooth(:,:,it) = SUM(var_weight,DIM=3)
 END DO
ELSE
 wmasmooth = var_cyc
END IF
END FUNCTION wmasmooth

! *************************************

!> Actual analogue computation subroutine.
SUBROUTINE compute_analogues(dates_sim, dates_archi, var_sim, var_archi, dim_archi, &
 & dim_sim, distfun, nanalog, seasonwin, timewin, silent, analogue_dates, distances, &
 & ranks_archi, ranks_sim,  spatial_corr)
IMPLICIT NONE
TYPE (dims_type), INTENT(IN) :: dim_sim
TYPE (dims_type), INTENT(IN) :: dim_archi
INTEGER, INTENT(IN) :: nanalog
INTEGER, INTENT(IN) :: seasonwin
INTEGER, INTENT(IN) :: timewin
INTEGER, INTENT(IN) :: dates_sim(dim_sim%time_dim)
INTEGER, INTENT(IN) :: dates_archi(dim_archi%time_dim)
REAL(8), INTENT(IN) :: var_sim(dim_sim%lon_dim, dim_sim%lat_dim, dim_sim%time_dim)
REAL(8), INTENT(IN) :: var_archi(dim_archi%lon_dim, dim_archi%lat_dim, dim_archi%time_dim)
LOGICAL, INTENT(IN) :: silent
INTEGER, OPTIONAL, INTENT(IN) :: ranks_archi(dim_archi%lon_dim, dim_archi%lat_dim, dim_archi%time_dim)
INTEGER, OPTIONAL, INTENT(IN) :: ranks_sim(dim_sim%lon_dim, dim_sim%lat_dim, dim_sim%time_dim)
REAL(8), EXTERNAL ::  distfun
INTEGER, INTENT(OUT) :: analogue_dates(nanalog,dim_sim%time_dim)
REAL(8), INTENT(OUT) :: distances(nanalog,dim_sim%time_dim)
REAL,OPTIONAL, INTENT(OUT) :: spatial_corr(nanalog,dim_sim%time_dim)

INTEGER :: st
INTEGER :: candilen
INTEGER :: candidate_indices(dim_archi%time_dim, timewin)
INTEGER :: itwin
REAL(8) :: alldist(dim_archi%time_dim)
CHARACTER(8) :: clockdate
CHARACTER(10) :: clocktime
INTEGER :: sorted_ind(nanalog)
!REAL :: sorted_dist(dim_archi%time_dim)

CALL DATE_AND_TIME(clockdate, clocktime)
IF (.NOT. silent) PRINT*,'begin sim loop', clockdate, ' ', clocktime
IF (PRESENT(spatial_corr) .AND. PRESENT(ranks_archi) .AND. PRESENT(ranks_sim)) THEN
! loop over simulation days with spatial correlation output
!$omp parallel default(shared) private(candidate_indices, candilen, alldist, sorted_ind) 
!$omp do schedule(static)
 DO st=1,dim_sim%time_dim-timewin+1
! get candidate dates. 
! Candidate dates are all days of the archive period within a seasonal window around the simulated day
! and not from the same year as the simulated day.
!PRINT*, st
  candidate_indices=0
  candidate_indices(1:(dim_archi%time_dim-timewin+1),1) = get_candidates(dim_archi%time_dim-timewin+1, &
   & dates_archi(1:(dim_archi%time_dim-timewin+1)), dates_sim(st), &
   & seasonwin)
!  READ(*,*)
  candilen = COUNT(candidate_indices(:,1) > 0)
!  PRINT*, candidate_indices(candilen,1)
!  PRINT*, 'candilen', candilen
! Define the candidate dates in the case of multi-day average distance as the days following the initial ones.
  IF (timewin > 1) THEN
   DO itwin=2,timewin
    WHERE (candidate_indices(:,1) > 0)
     candidate_indices(:,itwin) = candidate_indices(:,1)+itwin-1
    END WHERE
   END DO
  END IF
! get distances for candidate_indices
  alldist(1:candilen) = get_dist(candilen, timewin, candidate_indices(1:candilen,:), dim_sim, &
   & dim_archi, var_sim(:,:,st:st+timewin-1), var_archi, distfun)
!PRINT*, 'st = ', st !alldist(1)
! select smallest distances 
  CALL sort_index(candidate_indices(1:candilen,1), alldist(1:candilen), &
   & candilen, sorted_ind, distances(:,st), nanalog)
! and their corresponding dates
  analogue_dates(:,st) = dates_archi(sorted_ind)
! get spatial correlation for the selected days
  spatial_corr(:,st) = get_spearman_cor(dim_archi, nanalog, ranks_sim(:,:,st), &
   & ranks_archi(:,:,sorted_ind))
 END DO
!$omp end do
!$omp end parallel
ELSE
 ! loop over simulation days without spatial correlation output
!$omp parallel default(shared) private(candidate_indices, candilen, alldist, sorted_ind) 
!$omp do schedule(static)
 DO st=1,dim_sim%time_dim-timewin+1
! get candidate dates. 
! Candidate dates are all days of the archive period within a seasonal window around the simulated day
! and not from the same year as the simulated day.
  candidate_indices=0
!PRINT*, st
  candidate_indices(1:(dim_archi%time_dim-timewin+1),1) = get_candidates(dim_archi%time_dim-timewin+1, &
   & dates_archi(1:(dim_archi%time_dim-timewin+1)), dates_sim(st), &
   & seasonwin)
  candilen = COUNT(candidate_indices(:,1) > 0)
!  PRINT*, 'candilen', candilen, candidate_indices(candilen,1)
! Define the candidate dates in the case of multi-day average distance as the days following the initial ones.
  IF (timewin > 1) THEN
   DO itwin=2,timewin
    WHERE (candidate_indices(:,1) > 0)
     candidate_indices(:,itwin) = candidate_indices(:,1)+itwin-1
    END WHERE
   END DO
  END IF
! get distances for candidate_indices
  alldist(1:candilen) = get_dist(candilen, timewin, candidate_indices(1:candilen,:), dim_sim, &
   & dim_archi, var_sim(:,:,st:st+timewin-1), var_archi, distfun)
!PRINT*, alldist(1)
! select smallest distances 
  CALL sort_index(candidate_indices(1:candilen,1), alldist(1:candilen), &
   & candilen, sorted_ind, distances(:,st), nanalog)
! and their corresponding dates
  analogue_dates(:,st) = dates_archi(sorted_ind)
 END DO
!$omp end do
!$omp end parallel
END IF
IF (.NOT. silent) THEN
 CALL DATE_AND_TIME(clockdate, clocktime)
 PRINT*, 'end sim loop', clockdate, ' ', clocktime
END IF

END SUBROUTINE compute_analogues

!*******************************

!> Function for rank correlation calculation. 
!! Non-unique ranks are neglected for the sake of computation speed.
FUNCTION get_spearman_cor(dim_archi, nanalog, ranks_sim, ranks_archi)
IMPLICIT NONE
TYPE (dims_type), INTENT(IN) :: dim_archi
INTEGER, INTENT(IN) :: nanalog
REAL :: get_spearman_cor(nanalog)
INTEGER, INTENT(IN) :: ranks_sim(dim_archi%lon_dim, dim_archi%lat_dim)
INTEGER, INTENT(IN) :: ranks_archi(dim_archi%lon_dim, dim_archi%lat_dim, nanalog)

INTEGER :: ia !, ila, ilo
!CHARACTER(8) :: clockdate
!CHARACTER(10) :: clocktime
!REAL :: meansim
!REAL :: meanarch

!meansim = SUM(ranks_sim)/REAL(dim_archi%lon_dim*dim_archi%lat_dim)

!CALL DATE_AND_TIME(clockdate, clocktime)
!PRINT*,'begin first rank', clockdate, ' ', clocktime
DO ia=1,nanalog
! calculate correlation
 get_spearman_cor(ia) = 1-((6.0*SUM((ranks_archi(:,:,ia)-ranks_sim)**2))/ &
  &((dim_archi%lon_dim*dim_archi%lat_dim)**3-(dim_archi%lon_dim*dim_archi%lat_dim)))
! meanarch = SUM(ranks_archi(:,:,ia))/REAL(dim_archi%lon_dim*dim_archi%lat_dim)
! get_spearman_cor(ia) = ()/()
END DO

END FUNCTION get_spearman_cor

!*****************************

!> Determine the rank of each data point along the time dimension of a (/lon,lat,time/) array
!! for every spatial gridpoint.
!! Non-unique ranks are neglected for the sake of computation speed.
FUNCTION get_ranks(dims, dataarr)
IMPLICIT NONE
TYPE (dims_type), INTENT(IN) :: dims
REAL(8), INTENT(IN) :: dataarr(dims%lon_dim, dims%lat_dim, dims%time_dim)
INTEGER :: get_ranks(dims%lon_dim, dims%lat_dim, dims%time_dim)

INTEGER :: it, ila, ilo

!$omp parallel default(shared)
!$omp do schedule(static)
DO it=1, dims%time_dim
 DO ila=1,dims%lat_dim
  DO ilo=1, dims%lon_dim
   get_ranks(ilo,ila,it) = COUNT(dataarr(:,:,it) <= dataarr(ilo,ila,it))
  END DO
 END DO
END DO
!$omp end do
!$omp end parallel
END FUNCTION get_ranks

!********************************

!> calculate ranks of a vector
FUNCTION ranks(vec, length)
IMPLICIT NONE
INTEGER, INTENT(IN) :: length
REAL, INTENT(IN) :: vec(length)
INTEGER :: ranks(length)
INTEGER :: ii, jj

ranks = 1
DO ii =2,length
 DO jj=1,ii-1
  IF (vec(jj) < vec(ii)) THEN
   ranks(ii) = ranks(ii)+1
  ELSE
   ranks(jj) = ranks(jj)+1
  END IF  
 END DO
END DO


END FUNCTION ranks

!*********************************

!> Function for distance calculation.
!! Calculated the distance that should be minimised in order to define the closest analogues
FUNCTION get_dist(candilen, timewin, candidate_indices, dim_sim, &
 & dim_archi, var_sim, var_archi, distfun)
IMPLICIT NONE
INTEGER, INTENT(IN) :: candilen
INTEGER, INTENT(IN) :: timewin
INTEGER, INTENT(IN) :: candidate_indices(candilen,timewin)
TYPE (dims_type), INTENT(IN) :: dim_sim
TYPE (dims_type), INTENT(IN) :: dim_archi
REAL(8), INTENT(IN) :: var_sim(dim_sim%lon_dim, dim_sim%lat_dim, timewin)
REAL(8), INTENT(IN) :: var_archi(dim_archi%lon_dim, dim_archi%lat_dim, dim_archi%time_dim)
REAL(8), EXTERNAL ::  distfun
REAL(8) :: get_dist(candilen)

INTEGER :: ct
REAL(8) :: tempdist(candilen, timewin)
INTEGER :: tw
CHARACTER(8) :: clockdate
CHARACTER(10) :: clocktime

! The timewin loop is for the number of consecutive days that should be averaged over.
DO tw=1,timewin
! Loop over all candidate dates in the archive.
 DO ct=1,candilen
 ! CALL DATE_AND_TIME(clockdate, clocktime)
 ! PRINT*, clockdate, clocktime, '  ', candidate_indices(ct,:), dim_archi%time_dim
  tempdist(ct,tw)=distfun(var_archi(:,:,candidate_indices(ct,tw)), var_sim(:,:,tw), &
   & dim_archi%lon_dim, dim_archi%lat_dim)
 END DO
END DO
! sum distances over timewin days, if multiday distances were selected.
get_dist = SUM(tempdist, DIM=2)
!PRINT*, candilen, SHAPE(get_dist)
END FUNCTION get_dist

!******************************************

!> Select the candidate dates from the archive period. 
!! This includes all days within a given season window around the simulated day 
!! and from different years than the simulated day.
FUNCTION get_candidates(tdim, dates_archi, date_target, seasonwin)
IMPLICIT NONE
INTEGER, INTENT(IN) :: tdim 
INTEGER, INTENT(IN) :: dates_archi(tdim)
INTEGER, INTENT(IN) :: date_target
INTEGER, INTENT(IN) :: seasonwin
INTEGER :: get_candidates(tdim)

INTEGER :: at
INTEGER :: k
INTEGER :: yyyy_archi(tdim)
INTEGER :: yyyy_target
INTEGER :: mmdd_archi(tdim)
INTEGER :: mmdd_target
INTEGER :: range_exclude(2)
INTEGER :: mmdd_permitted(2*seasonwin+1)
INTEGER :: nonleapadd

!PRINT*, tdim
get_candidates = 0
! seperate archive and target year from month and day
yyyy_archi = dates_archi/10000
yyyy_target = date_target/10000
mmdd_archi = MOD(dates_archi,10000)
mmdd_target = MOD(date_target,10000)
! calculate the dates to be excluded because they are within one year from the simulated date
range_exclude(1) = civildate2int_day(add_days(int2civildate(date_target),-184))
range_exclude(2) = civildate2int_day(add_days(int2civildate(date_target),184))
! get all month+day combinations that are within the season window in leapyears and 
! an additional day for non-leapyears when the 29th of february is contained in the interval.
CALL get_mmdd_permitted(mmdd_target, seasonwin, mmdd_permitted, nonleapadd)
!PRINT*, mmdd_permitted, nonleapadd
k=0
! loop over all archive days
DO at=1,tdim
! IF (seasondiff(mmdd_archi(at), mmdd_target) <= seasonwin ) THEN
! If the day has a valid month+day combination ...
 IF (ANY(mmdd_permitted == mmdd_archi(at)) .OR. (.NOT. is_leapyear(yyyy_archi(at)) .AND. mmdd_archi(at)==nonleapadd)) THEN
! and is not in the excluded range (because it is to close to the simulation date)
  IF (dates_archi(at) < range_exclude(1) .OR. dates_archi(at) > range_exclude(2)) THEN
! increase the candidate count and store it as a candidate index
      k=k+1
      get_candidates(k) = at
   ELSE 
    CYCLE
   END IF
 ELSE
  CYCLE
 END IF
END DO
!PRINT*, get_candidates(1:370)
!STOP
END FUNCTION get_candidates

! **************************

!> Define month+day combinations within a given seasonwindow in leapyears and
!! if necessary a day to add for non-leap years.
SUBROUTINE get_mmdd_permitted(mmdd, seasonwin, permitted, nonleapadd)
IMPLICIT NONE
INTEGER, INTENT(IN) :: mmdd
INTEGER, INTENT(IN) :: seasonwin
INTEGER, INTENT(OUT) :: permitted(2*seasonwin+1)
INTEGER, INTENT(OUT) :: nonleapadd

INTEGER :: mm, dd
INTEGER :: ic
INTEGER :: id

nonleapadd=0
permitted=0

mm=mmdd/100
dd=MOD(mmdd,100)
id =1
! start from simulation month+day
permitted(1) = mmdd
! add the seasonwin following days to the permitted ones
DO ic=1,seasonwin
 id=id+1
 dd=dd+1
! IF (dd > monthlength(3, mm)) THEN
 IF (dd > monthlength(4, mm)) THEN
  mm = mm+1
  IF (mm > 12) mm = 1
  dd = 1
 END IF
 permitted(id) = mm*100+dd
END DO
! add the following day to the non-leapyear additional variable if 29th of 
! February is included in the days permitted so far
IF (ANY(permitted(2:seasonwin+1) == 229)) THEN
 dd=dd+1
 IF (dd > monthlength(4, mm)) THEN
  mm = mm+1
  IF (mm > 12) mm = 1
  dd = 1
 END IF
 nonleapadd = mm*100+dd
END IF
! start again from the simulation day
mm=mmdd/100
dd=MOD(mmdd,100)
! this time go backwards and add the seasonwin preceeding days to the permitted ones
DO ic=1,seasonwin
 id=id+1
 dd=dd-1
 IF (dd < 1) THEN
  mm = mm-1
  IF (mm < 1) mm = 12
  dd = monthlength(4, mm)
 ! dd = monthlength(3, mm)
 END IF
 permitted(id) = mm*100+dd
END DO
! again, add another day to the non-leapyear additional variable if 29th of 
! February is included in the days permitted within the preceeding days
IF (ANY(permitted(seasonwin+2:2*seasonwin+1) == 229)) THEN
 dd = dd-1
 IF (dd < 1) THEN
  mm = mm-1
  IF (mm < 1) mm = 12
  dd = monthlength(4, mm)
 END IF
 nonleapadd = mm*100+dd
END IF


END SUBROUTINE get_mmdd_permitted

!*******************************

!> calculates the calender day difference between two dates
!! where the first two digits represent the month and the last two digits the day
INTEGER FUNCTION seasondiff(mmdd1,mmdd2)
IMPLICIT NONE
INTEGER :: mmdd1
INTEGER :: mmdd2
INTEGER :: mm1, mm2, dd1, dd2
INTEGER :: diff1, diff2

mm1 = mmdd1/100
mm2 = mmdd2/100
dd1 = MOD(mmdd1,100)
dd2 = MOD(mmdd2,100)
diff1=0
DO WHILE (mm1 /= mm2 .OR. dd1 /= dd2)
 dd1 = dd1+1
 diff1 = diff1+1
 IF (dd1 > monthlength(4, mm1)) THEN
  mm1 = mm1+1
  IF (mm1 > 12) mm1 = 1
  dd1 = 1
 END IF
END DO
mm1 = mmdd1/100
mm2 = mmdd2/100
dd1 = MOD(mmdd1,100)
dd2 = MOD(mmdd2,100)
diff2=0
DO WHILE (mm1 /= mm2 .OR. dd1 /= dd2)
 dd2 = dd2+1
 diff2 = diff2+1
 IF (dd2 > monthlength(4, mm2)) THEN
  mm2 = mm2+1
  IF (mm2 > 12) mm2 = 1
  dd2 = 1
 END IF
END DO
seasondiff = MIN(diff2,diff1)
!PRINT*, seasondiff
END FUNCTION seasondiff

!*****************************

!> calculates the length of a given month
!!
!! uses: function is_leapyear
INTEGER FUNCTION monthlength(yyyy, mm)
IMPLICIT NONE
INTEGER :: mm
INTEGER :: yyyy

SELECT CASE (mm)
  CASE (1,3,5,7,8,10,12)
    monthlength = 31
  CASE (4,6,9,11)
    monthlength = 30
  CASE (2)
    IF (is_leapyear(yyyy)) THEN
      monthlength = 29
    ELSE
      monthlength = 28
    END IF
  CASE DEFAULT
    PRINT*, 'invalid month value in function monthlength: ', mm
  STOP
END SELECT
END FUNCTION monthlength

! **************************************

!> calculates the length of a given year
!!
!! uses function is_leapyear
INTEGER FUNCTION yearlength(yyyy,mm)
IMPLICIT NONE
INTEGER :: mm
INTEGER :: yyyy

IF (mm > 2) THEN
  IF (is_leapyear(yyyy+1)) THEN
    yearlength = 366
  ELSE
    yearlength = 365
  END IF
ELSE
  IF (is_leapyear(yyyy)) THEN
    yearlength = 366
  ELSE
    yearlength = 365
  END IF
END IF

END FUNCTION yearlength

!*********************************

LOGICAL FUNCTION is_leapyear(yyyy)
IMPLICIT NONE
INTEGER :: yyyy

IF (MOD(yyyy, 4) /= 0) THEN
  is_leapyear = .FALSE.
ELSE IF (MOD(yyyy,100) == 0 .AND. MOD(yyyy,400) /= 0 ) THEN
  is_leapyear = .FALSE.
ELSE
  is_leapyear = .TRUE.
END IF

END FUNCTION is_leapyear

! ***************************************

!> takes a date in integer format YYYYMMDD and returns a date in civildate_type.
TYPE (civildate_type) FUNCTION int2civildate(intdate)
IMPLICIT NONE
INTEGER :: intdate
INTEGER :: tmpdate

tmpdate = intdate
int2civildate%yyyy = tmpdate/10000
tmpdate = MOD(tmpdate,10000)
int2civildate%mm = tmpdate/100
tmpdate = MOD(tmpdate,100)
int2civildate%dd = tmpdate
int2civildate%hh = 00

END FUNCTION int2civildate

!*******************************

!> adds a number of days (plusdays) to a date (ref_date)
TYPE (civildate_type) FUNCTION add_days(ref_date, plusdays)
IMPLICIT NONE
TYPE (civildate_type) :: ref_date
INTEGER :: plusdays
TYPE (civildate_type) :: tmp_date

tmp_date=ref_date
tmp_date%dd=tmp_date%dd+plusdays
DO WHILE (tmp_date%dd <= 0)
!PRINT*, 'tmp_date', tmp_date
 tmp_date%mm = tmp_date%mm-1
 IF (tmp_date%mm <= 0) THEN
  tmp_date%yyyy=tmp_date%yyyy-1
  tmp_date%mm=tmp_date%mm+12
 END IF
! PRINT*, 'month', tmp_date%mm
 tmp_date%dd=tmp_date%dd+monthlength(tmp_date%yyyy,tmp_date%mm)
! PRINT*, 'tmp_date', tmp_date
END DO

DO WHILE (tmp_date%dd > monthlength(tmp_date%yyyy,tmp_date%mm))
! IF (tmp_date%dd == 31) PRINT*, 'tmp_date', tmp_date
 tmp_date%dd = tmp_date%dd - monthlength(tmp_date%yyyy,tmp_date%mm)
 tmp_date%mm = tmp_date%mm + 1
 IF (tmp_date%mm > 12) THEN
  tmp_date%yyyy=tmp_date%yyyy+1
  tmp_date%mm=tmp_date%mm-12
 END IF
END DO 
add_days = tmp_date
END FUNCTION add_days

! *******************************************

!> function to convert civildate to integer date format yyyymmdd
INTEGER FUNCTION civildate2int_day(c_date)
IMPLICIT NONE
TYPE (civildate_type) :: c_date

civildate2int_day= c_date%yyyy*10000+c_date%mm*100+c_date%dd

END FUNCTION civildate2int_day

!********************************************

!> returns an index vector sorted according to the values in datavector, and the sorted data vector itself.
!! The index corresponding to the smallest value in datavector comes first.
SUBROUTINE sort_index(indexvector, datavector, length, sorted_index, vector, nanalog)
IMPLICIT NONE
INTEGER, INTENT(OUT) :: sorted_index(nanalog)
INTEGER, INTENT(IN) :: length
INTEGER, INTENT(IN) :: indexvector(length)
REAL(8), INTENT(IN) :: datavector(length)
REAL(8), INTENT(OUT) :: vector(nanalog)
INTEGER, INTENT(IN) :: nanalog
INTEGER :: ii, jj
REAL(8) :: tmp
INTEGER :: itmp

vector = datavector(1:nanalog)
sorted_index = indexvector(1:nanalog)   
DO  ii=2,nanalog
  tmp=vector(ii)
  itmp = sorted_index(ii)
  jj=ii-1
  DO WHILE (vector(jj) > tmp )
    vector(jj+1)=vector(jj)
    sorted_index(jj+1)= sorted_index(jj)
    jj = jj-1
    IF (jj < 1) EXIT
  END DO
  vector(jj+1) = tmp
  sorted_index(jj+1) = itmp
END DO
DO  ii=nanalog+1,length
 IF (datavector(ii) < vector(nanalog)) THEN
  tmp=datavector(ii)
  itmp = indexvector(ii)
  jj=nanalog-1
  DO WHILE (vector(jj) > tmp )
    vector(jj+1)=vector(jj)
    sorted_index(jj+1)= sorted_index(jj)
    jj = jj-1
    IF (jj < 1) EXIT
  END DO
  vector(jj+1) = tmp
  sorted_index(jj+1) = itmp
 END IF
END DO

  
END SUBROUTINE sort_index

!************************************


END MODULE routines
