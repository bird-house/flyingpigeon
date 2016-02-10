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
MODULE config

TYPE files_type
 CHARACTER(200) :: archivefile
 CHARACTER(200) :: simulationfile
 CHARACTER(200) :: outputfile
END TYPE files_type

TYPE param_type
 INTEGER :: timewin
 CHARACTER(20) :: varname
 LOGICAL :: seacyc
 INTEGER :: cycsmooth
 INTEGER :: nanalog
 INTEGER :: seasonwin
 CHARACTER(20) :: distfun
 LOGICAL :: calccor
 LOGICAL :: silent
END TYPE param_type

TYPE config_type
 TYPE (files_type) :: files
 TYPE (param_type) :: param
END TYPE config_type

CONTAINS

!> function to read the configuration (namelist) file. 
TYPE (config_type) FUNCTION get_configuration(filename)
IMPLICIT NONE
CHARACTER(*) :: filename
TYPE (files_type) :: my_files
TYPE (param_type) :: my_params
NAMELIST /FILES/ my_files
NAMELIST /PARAM/ my_params

OPEN(10, FILE=TRIM(filename))
  READ(10, NML=FILES)
  READ(10, NML=PARAM)
CLOSE(10)

get_configuration%files = my_files
get_configuration%param = my_params

END FUNCTION get_configuration


END MODULE config
