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
MODULE read_files
USE netcdf

TYPE dims_type
 INTEGER :: lat_dim
 INTEGER :: lon_dim
 INTEGER :: time_dim
END TYPE dims_type

CONTAINS

!> function to get dimensions from a netcdf file
TYPE (dims_type) FUNCTION get_dims(filename)
IMPLICIT NONE
CHARACTER(*), INTENT(IN) :: filename

INTEGER :: state
INTEGER :: fileid
INTEGER :: nDimensions
CHARACTER*80, ALLOCATABLE :: dimname(:)
INTEGER, ALLOCATABLE :: dimlen(:)
INTEGER :: dimid

get_dims%lon_dim = -999
get_dims%lat_dim = -999
get_dims%time_dim = -999

!PRINT*, "dims init done"
state = nf90_open(trim(filename),nf90_nowrite,fileid)
!PRINT*, "ncdf opened"
CALL check(NF90_INQUIRE(fileid, nDimensions), fileid)
ALLOCATE(dimname(nDimensions), dimlen(nDimensions))
DO dimid = 1, nDimensions
  CALL check(NF90_INQUIRE_DIMENSION(fileid, dimid, dimname(dimid), dimlen(dimid)), fileid) ! get dimension names, lengths
  IF (dimname(dimid) == 'lon') THEN
    get_dims%lon_dim = dimlen(dimid)
  ELSE IF (dimname(dimid) == 'lat') THEN
    get_dims%lat_dim = dimlen(dimid)
  ELSE IF (dimname(dimid) == 'time') THEN
    get_dims%time_dim = dimlen(dimid)
  END IF
END DO

IF (get_dims%lon_dim <= 0 .OR. get_dims%lat_dim <= 0 .OR. get_dims%time_dim <= 0) THEN
  PRINT*, 'no time, lon or lat dimension found in netcdf file ', TRIM(filename),': program stops'
  state = NF90_CLOSE(fileid)               ! close netCDF dataset
  STOP
END IF
state = NF90_CLOSE(fileid) 

END FUNCTION get_dims

!******************

!> Read the one and only variable from a netcdf file where the dimensions are already known.
FUNCTION get_data(filename, varname, dims)
IMPLICIT NONE
CHARACTER(*), INTENT(IN) :: filename
CHARACTER(*), INTENT(IN) :: varname
TYPE (dims_type) :: dims
REAL :: get_data(dims%lon_dim, dims%lat_dim, dims%time_dim)

INTEGER :: state
INTEGER :: fileid
INTEGER :: varid

state = nf90_open(trim(filename),nf90_nowrite,fileid)
CALL check(NF90_INQ_VARID(fileid, varname, varid ), fileid )
CALL check(NF90_GET_VAR(fileid, varid, get_data), fileid)
state = NF90_CLOSE(fileid) 
END FUNCTION get_data

!********************************

!> write the dates in a netcdf file to a textfile using the cdo command showdate,
!! that is called using the fortran extension command SYSTEM()
FUNCTION get_dates(filename, length, outfilename)
IMPLICIT NONE
CHARACTER(*), INTENT(IN) :: filename
INTEGER, INTENT(IN) :: length
INTEGER :: get_dates(length)
CHARACTER(*), INTENT(IN) :: outfilename

CHARACTER(300) :: command

! write files with dates
WRITE(command, '(6A)') "cdo -s showdate ", filename, &
 & " | awk '{gsub(/-/,""""); print}' | awk 'gsub(/  /,""\n"") {print}'> ", outfilename, "; echo "" "" >> ", outfilename
!PRINT*, command
CALL SYSTEM(command) ! there is a risk that this does not work with every compiler !!!
! read the dates back to the variable.
OPEN(15,FILE=outfilename)
READ(15,*) get_dates
CLOSE(15)

END FUNCTION get_dates

!*******************************

!> check status of netcdf operations
subroutine check(status, fileid)
  
  IMPLICIT NONE
    integer :: status
    integer :: fileid

    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status)), fileid
      stop 'error reading netcdf file: program Stopped'
      status = nf90_close(fileid)
    end if

end subroutine check
  
!*********************

END MODULE read_files
