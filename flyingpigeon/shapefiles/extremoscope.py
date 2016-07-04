def regionalize(infile, basedir, variable, aggregate, scenario ): 
	"""generates a file structure for polygons and fieldmeans

	:param infile: netCDF infile with DRS name convention
	:param basedir: path to base directory
	:param variable: varable name 
	:param aggregate: time aggregartion
	:param scenario: scenarion name (e.g 'rcp45')"""

	from flyingpigeon import subset
	from flyingpigeon.subset import _POLYGONS_EXTREMOSCOPE_

	for polygon in _POLYGONS_EXTREMOSCOPE_: 
		region = polygon.replace('.','-')
		prefix = os.basename(infile).replace('EUR',region)
		dir_output = os.path.join( basedir , 'polygons',  variable, aggregate, scenario, region)









/home/estimr2/nhempelmann/data/extremoscope_test/chunk_test.nc