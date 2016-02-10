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
MODULE eofs
!> This module contains subroutines by David W. Pierce for dealing with EOFs and projecting data on EOFs.
!! They are slightly simplified, removing code dealing with cases that are not relevant in the context of CASTf90.
!! Lapack routines are used inside.

CONTAINS
	subroutine deof( data, nx, nt, nmodes, icovcor, &
     &		eigenvalues, eigenvectors, variance, cumvariance, &
     &   sqrootweights )

!	------------------------------------------------------------------
!	This routine generates the Empirical Orthogonal Functions (EOFs)
!	for a given time-space data set.  (Note that although the routine
!	is set up as if the data is exactly two dimensional (X,T), that
!	you can compute the EOFs of 3-D (X,Y,T) fields simply by concat-
!	enating the data along the X,Y axes into one big 2-D array and
!	then computing the EOFs of that 2-D array).  The "typical" 
!	normalization is applied, i.e., the magnitude of the eigenvectors
!	is scaled to be equal to 1.
!
!	David W. Pierce
!	dpierce@ucsd.edu
!	Scripps Institution of Oceanography
!	Climate Research Division, 0224
!	Jan 29, 1996	
!	
!	Inputs:
!
!		data(nx,nt): data to compute the EOFs of.  THIS MUST
!			BE ANOMALIES.
!
!		nx, nt: number of space (nx) and time (nt) points in the
!			input data array.
!
!		nmodes: number of modes (EOFs) to compute.
		
!		icovcor: if =0, then compute using covariance matrix;
!			 if =1, then compute using correlation matrix.
!			See, for example, discussion in Daniel S. Wilks
!			1995, "Statistical Methods in the Atmospheric
!			Sciences", p. 383 for a discussion.
!
!
!		nx: the SMALLER of 'nx' and 'nt'.
!
!		nt: the LARGER of 'nx' and 'nt'.
!
!		spacked(nx*(nx+1)/2): workspace.  This is used to 
!			store the packed covariance or correlation matrix.
!
!		evals(nx): workspace.  This is used to store the
!			complete series of eigenvalues.  I suppose you
!			could access this if you wanted to, remembering
!			that a) it's more modes than you asked for; b)
!			they are in ASCENDING order rather than descending.
!
!		evecs(nx,nmodes): workspace.  This is used to store
!			the (possibly switched) eigenvectors, which are
!			in ascending order.
!
!		rlawork(8*nx): Real LAPACK workspace.
!
!		ilawork(5*nx): Integer LAPACK workspace.
!
!		ifail(nx): Integer LAPACK workspace.  This is used
!			to indicate which eigenvectors didn't converge,
!			if that happened.
!
!		tentpcs(nt,nmodes): Real workspace.  This is 
!			used to store the 'tentative' principal components.
!
!		sqrootweights(nx) : the SQUARE ROOT of the areal weighting to
!			use.  Just set this to all 1.0's if you don't care
!			about areal weighting.  I think you can set places
!			which should be ignored to zero to have them not
!			participate in the calculation, thus implementing a
!			crude form of data masking.
!
!	Outputs:
!
!		eigenvalues(nmodes): the computed eigenvalues of
!			the data, the largest returned first.
!
!		eigenvectors(nx,nmodes): the computed eigenvectors
!			of the data, the most important returned first.
!
!		princomp(nt,nmodes): the principal components of 
!			the data.
!
!		variance(nmodes): the percent of variance explained
!			by each mode.
!
!		cumvariance(nmodes): the cumulative percent of 
!			variance explained by each mode.  This is
!			a bit redundant -- it's just the sum
!			of "variance".
!			
!	Method:
!
!	EOFs are simply the eigenvectors of the covariance (or correlation)
!	matrix.  So form the proper matrix from the data and pass it to
!	a LAPACK routine which calculates eigenvectors/eigenvalues.  Then
!	calculate the principal components explicitly using the fact that
!	the principal component for mode M at time T is just the projection 
!	of the data at time T onto eigenvector M.  
!
!	There is a slight complication for efficiency's sake.  That is, 
!	we often have nx >> nt, i.e., there are many more spatial than
!	temporal points.  The traditional correlation/covariance matrix
!	is size (nx,nx), which can be huge.  Think, for example, of a 3-D
!	field of size (100,100,120) which has been concatenated into a
!	2-D field of size (10000,120).  The traditional covariance/correlation
!	matrix in this case is size (10000,10000)!  That's way too big to 
!	easily work with.  So, following the discussion in Preisendorfer
!	(1988, "Principal Component Analysis in Meteorology and Oceanography",
!	p. 64) we work, in such cases, in the "dual" of the space.  All
!	that means is that we logically switch X and T; the 
!	covariance/correlation matrix for the example given above is then
!	size (120,120), which is much easier and more efficient to work
!	with.  If you do this kind of switch, the basic idea is that
!	the eigenvectors of the switched matrix are the principal components
!	of the original matrix, and the principal components of the switched
!	matrix are the eigenvectors of the original matrix.  Which is to
!	say, if you switch T and X to begin with, the final result is that
!	the T dependence is in the X part and the X dependence is in the
!	T part.  There is also a normalization which has to be applied--
!	see Preisendorfer for details.
!	------------------------------------------------------------------

	implicit none

	integer	covariance, correlation
	parameter( covariance=0, correlation=1 )  ! possible values of 'icovcor'
	
!	-----------------
!	Passed parameters
!	-----------------
	integer	nx, nt, nmodes, icovcor
	double precision data(nx,nt), eigenvalues(nmodes), &
     &		eigenvectors(nx,nmodes), &
     &		variance(nmodes), cumvariance(nmodes),  &
     &		sqrootweights(nx)
	

!	---------------
!	Local variables
!	---------------
!	logical		doswitched
	integer		orderofs, i, j, jascending, jdescending
	double precision sum, fact, totvar
	character*1	jobz, range, uplo	! for LAPACK routine
	integer		m, n, il, iu, ldz, info	! for LAPACK routine
	double precision vl, vu, abstol		! for LAPACK routine
	integer 	lwork, mode
	REAL(8) :: spacked(nx*(nx+1)/2) 
	REAL(8) :: evals(nx), evecs(nx,nmodes)
	REAL(8) :: rlawork(8*nx)
	integer :: ilawork(5*nx), ifail(nx)

!#ifdef VERBOSE
	print *, 'entering deof with nx, nt, nmodes=', nx, nt, nmodes
!#endif

!	----------------------------------------------
!	Weight the data by the square root of the area
!	----------------------------------------------
    IF (.NOT. ALL(sqrootweights == 1)) THEN
    print *, 'sqrootweights:',sqrootweights(1:3)
	 do i=1, nx
		data(i,:) = data(i,:) * sqrootweights(i)
	 enddo
	END IF
	
!	---------------------------------------------------------
!	Figure out whether we should do the 'regular' EOF or the
!	one with switched X and T axes.  The correlation matrix
!	for the regular method is size (nx,nx) and for the 
!	switched method it is size (nt,nt); choose based on which
!	of these is smaller.
!	---------------------------------------------------------
!#ifdef VERBOSE
!	print *, 'figuring switched or not'
!#endif
!	doswitched = (nx .gt. nt)
!	if( doswitched ) then
!		orderofs = nt
!		print *, 'deof: Working in switched mode'
!	else
		orderofs = nx
!		print *, 'deof: Working in unswitched mode'
!	endif
	if( orderofs .gt. nx ) then
		write(0,*) 'Error!  EOF routine must be supplied '
		write(0,*) 'with enough workspace; passed parameter'
		write(0,*) 'nx must be at least ', orderofs
		write(0,*) 'Passed value was nx=',nx
		stop 'eof.F near line 145'
	endif
	if( nmodes .gt. orderofs ) then
		write(0,*) 'Error! EOF routine called requesting more'
		write(0,*) 'modes than exist!  Request=',nmodes,' exist=', &
     &			orderofs
		stop 'eof.F near line 170'
	endif

!	-------------------------------------------------
!	Form the covariance or correlation matrix, put it 
!	into 's'.  Note that 's' is always symmetric --
!	the correlation between X and Y is the same as 
!	between Y and X -- so use packed storage for 's'.
!	The packed storage scheme we use is the same as
!	LAPACK uses so we can pass 's' directly to the
!	solver routine: the matrix is lower triangular,
!	and s(i,j) = spacked(i+(j-1)*(2*n-j)/2).
!	-------------------------------------------------
	call deofcovcor( data, nx, nt, icovcor, covariance, &
     &	 spacked )

!	------------------------------------------------------
!	Now call the LAPACK solver to get the eigenvalues
!	and eigenvectors.  The eigenvalues express the
!	amount of variance explained by the various modes,
!	so choose to return those 'nmodes' modes which 
!	explain the most variance.
!	Dims of arrays:
!		evals  (n)	  ! The calculated eigenvalues
!		evecs  (n,nmodes) ! The calculated eigenvectors
!		rlawork(8*n)
!		ilawork(5*n)
!		ifail  (n)
!	Remember that the calculated eigenvectors may not be
!	the ones we really want if we are doing switched
!	X and T axes.  However the eigen*values* are the same
!	either way, according to Preisendorfer.
!	*NOTE* that the LAPACK routine returns the eigenvalues
!	(and corresponding eigenvectors) in ASCENDING order,
!	but this routine returns them in DESCENDING order; 
!	this will be switched in the final assembly phase,
!	below.
!	------------------------------------------------------
	jobz  = 'V' 	! Both eigenvalues and eigenvectors
	range = 'I'	! Specify range of eigenvalues to get.
	uplo  = 'L'	! 'spacked' has lower triangular part of s
	n     = orderofs
	il    = n - nmodes + 1	! Smallest eigenvalue to get
	iu    = n		! Largest eigenvalue to get
	abstol= 0.0		! See LAPACK documentation
	ldz   = n

	lwork = 8*nx

    evals = 0.0
    evecs = 0.0

	print *, 'about to call dspevx, spacked=', spacked(1:3)

	call dspevx( jobz, range, uplo, n, spacked, &
     &		vl, vu, il, iu, abstol, m, evals, evecs, ldz, &
     &		rlawork, ilawork, ifail, info )

!	------------------------------
!	Check for LAPACK routine error
!	------------------------------
	if( info .ne. 0 ) then
		if( info .lt. 0 ) then
			write(0,*) 'LAPACK error: argument ', &
     &				-info, ' had illegal value'
			stop 'eof.F near line 167'
		else
			write(0,*) 'LAPACK error: ', info, &
     &				'eigenvectors failed to converge!' 
			write(0,*) 'Consult the LAPACK docs!'
			stop 'eof.F near line 172'
		endif
	endif

!	------------------------------------------------
!	Make sure that no eigenvalues <= zero.  Besides
!	being mathematically forbidden, this would cause
!	a divide by zero or negative sqrt error later on.
!	------------------------------------------------
	do i=1, nmodes
		if( evals(i) .le. 0.0 ) then
			write(0,*) 'Error! LAPACK routine returned'
			write(0,*) 'eigenvalue <= 0!! ', i, evals(i)
			do j=1, nmodes
				print *, j, evals(j)
			enddo
			write(0,*) 'Note: This often means you are asking'
			write(0,*) 'for more modes than the data supports.'
			write(0,*) 'Try reducing the number of requested'
			write(0,*) 'modes.'
			stop 'eof.F near line 197'
		endif
	enddo


!	--------------------------------------------------

!		-------------------------------------------------
!		This is the unswitched case, and so it is easier.
!		All we have to do is return things in DESCENDING
!		order despite the fact that LAPACK returns them
!		in ASCENDING order.
!		Do the eigenvectors first...
!		-------------------------------------------------
		do jascending=1, nmodes
			jdescending = nmodes - jascending + 1
			do i=1, nx
				eigenvectors(i,jdescending) = &
     &					evecs(i,jascending)
			enddo
		enddo
			

!	--------------------------------------------
!	Do the second half of the areal weighting...
!	--------------------------------------------
    IF (.NOT. ALL(sqrootweights == 1)) THEN
	do mode=1, nmodes
	do i=1, nx
		if( sqrootweights(i) .eq. 0.0 ) then
			eigenvectors(i,mode) = 0.0
		else
			eigenvectors(i,mode) =  &
     &				eigenvectors(i,mode) / sqrootweights(i)
		endif
	enddo
	enddo
	END IF

!	------------------------------------------------
!	Scale the eigenvectors to have a magnitude of 1;
!	scale the corresponding principal components to
!	reproduce the original data.
!	------------------------------------------------
	do mode=1, nmodes
!		----------------------------
!		Get the normalization factor
!		----------------------------
		sum = 0.0
		do i=1, nx
			sum = sum + eigenvectors(i,mode)*eigenvectors(i,mode)
		enddo
		fact = sqrt(sum)
!		--------------------------
!		Normalize the eigenvectors
!		--------------------------
		do i=1, nx
			eigenvectors(i,mode) = eigenvectors(i,mode)/fact
		enddo
!		----------------------------------
!		
	enddo

!	-------------------------------------------------
!	Copy over just the requested number of
!	eigenvalues, and calculate the cumulative percent
!	variance explained.  Start by getting the total
!	variance in the field, so we can normalize by 
!	that.
!	-------------------------------------------------
	call deoftotvar( data, nx, nt, totvar, &
     &		icovcor, correlation )
	sum = 0.0
	do jascending=nmodes, 1, -1
		jdescending = nmodes - jascending + 1
		eigenvalues(jdescending) = evals(jascending)
		variance(jdescending)    = &
     &			eigenvalues(jdescending)/totvar*100.0
		sum = sum + variance(jdescending)
		cumvariance(jdescending) = sum
	enddo

	print *, 'deof.F done: returning'

	return
	end subroutine deof

!####################################################

	

!	--------------------------------------------------
!

	subroutine deofcovcor( data, nx, nt, icovcor, covariance, &
     &	spacked)

!	-------------------------------------------------
!	Form the covariance or correlation matrix, put it 
!	into 's'.  Note that 's' is always symmetric --
!	the correlation between X and Y is the same as 
!	between Y and X -- so use packed storage for 's'.
!	The packed storage scheme we use is the same as
!	LAPACK uses so we can pass 's' directly to the
!	solver routine: the matrix is lower triangular,
!	and s(i,j) = spacked(i+(j-1)*(1*n-j)/2).
!
!	Inputs:
!		data(nx,nt): The basic data array.  THIS MUST
!			BE ANOMALIES.
!
!		nx, nt: size of data array
!			[INTEGER]
!
!		icovcor: if .eq. covariance, then calculate
!			the covariance array;
!			 if .eq. correlation, then calculate
!			the correlation array.
!			[INTEGER]
!
!		covariance, correlation: integer values to
!			indicate each of these options.
!			[INTEGER]
!
!
!		nx: min(nt,nx).  Used to dimension
!			'spacked'.
!
!	Outputs:
!
!		spacked(nx*(nx+1)/2): the covariance 
!			or correlation array.  This is in packed 
!			form corresponding to LAPACK's lower
!			triangular form.  
!
!	David Pierce
!	Scripps Institution of Oceanography
!	Climate Research Division
!	dpierce@ucsd.edu
!	Jan 29, 1996
!	-------------------------------------------------

	implicit none

!	-----------------
!	Passed parameters
!	-----------------
	integer	nx, nt, icovcor, covariance
	double precision data(nx,nt), spacked(nx*(nx+1)/2)

!	---------------
!	Local variables
!	---------------
	integer	i, j, k
	double precision sum, sum2, sum3, fact

	if( nx .le. 1 ) then
		write(0,*) 'deofcovcor.F: error: nx too small!! nx=', nx
		call exit(-1)
	endif
	do j=1, nx
		do i=j, nx
			sum  = 0.0
			sum2 = 0.0
			sum3 = 0.0
			do k=1, nt
				sum  = sum  + data(j,k)*data(i,k)
				sum2 = sum2 + data(i,k)*data(i,k)
				sum3 = sum3 + data(j,k)*data(j,k)
			enddo
			if( icovcor .eq. covariance ) then
				fact = 1.0/float(nt-1)
			else
				fact = 1.0/(sqrt(sum2)*sqrt(sum3))
			endif
			spacked(i+(j-1)*(2*nx-j)/2) = sum*fact
		enddo
	enddo
			
	return
	end subroutine deofcovcor
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	subroutine deofpcs( data, nx, nt, nmodes,  &
     &		evecs, tentpcs )

!       ------------------------------------------------
!       Compute the tentative principal components; they
!       are 'tentative' because they might be the PCs
!       of the switched data.  Put them into 'tentpcs',
!       which is of size (nt,nmodes) if we are doing
!       regular EOFs and (nx,nmodes) if we are doing
!       switched EOFs.  These PCs come out in order
!       corresponding to the order of 'evecs', which is
!       in ASCENDING order.
!
!	Inputs:
!
!		data(nx,nt): The input data.  THESE MUST
!			BE ANOMALIES.
!
!		nmodes: # of modes to calculate.
!
!		nx: min(nx,nt)
!
!		evecs(nx,nmodes): the eigenvectors 
!			(which might be switched).
!
!
!	Outputs:
!
!		tentpcs(nt,nmodes): the tentative
!			(possibly switched) principal
!			components.
!
!	David W. Pierce
!	Scripps Institution of Oceanography
!	Climate Research Division
!	dpierce@ucsd.edu
!	Jan 29, 1996
!       ------------------------------------------------

	implicit none

!	-----------------
!	Passed parameters
!	-----------------
	integer	nx, nt, nmodes
	double precision data(nx,nt), evecs(nx,nmodes), &
     &		tentpcs(nmodes, nt)

!	---------------
!	Local variables
!	---------------
	integer	i, j

	do j=1, nt
		do i=1, nmodes
			tentpcs(i,j) = SUM(data(:,j)*evecs(:,i))
		enddo
	enddo

	return
	end subroutine deofpcs
	
!############################################3

	subroutine deoftotvar( data, nx, nt, totvar, &
     &		icovcor, correlation )

!	-------------------------------------------------
!	Returns the total variance in the field so we can
!	normalize by it.
!
!	Inputs:
!
!		data(nx,nt): data to calculate upon
!
!		nx, nt:	size of 'data'.
!
!		icovcor: if (icovcor .eq. covariance), then
!			we are using the covariance array;
!			if( icovcor .eq. correlation) then
!			we are using the correlation array.
!
!	Outputs:
!
!		totvar: estimate of total variance
!
!	-------------------------------------------------

	implicit none

	integer	nx, nt, icovcor, correlation
	double precision	data(nx,nt), totvar, sum, fact
	integer	i, j

	if( icovcor .eq. correlation ) then
		totvar = float( nx )
		return
	endif

	totvar = 0.0
	fact = 1.0/float(nt-1)
	do j=1, nt
		sum = 0.0
		do i=1, nx
			sum = sum + data(i,j)*data(i,j)
		enddo
		totvar = totvar + sum*fact
	enddo

	return
	end subroutine deoftotvar

	
END MODULE eofs
