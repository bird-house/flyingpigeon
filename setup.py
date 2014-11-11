import os

from setuptools import setup, find_packages

here = os.path.abspath(os.path.dirname(__file__))
README = open(os.path.join(here, 'README.rst')).read()
CHANGES = open(os.path.join(here, 'CHANGES.rst')).read()

requires = [
    'malleefowl',
    'cdo',
    'bokeh',
    'ocgis',
    'matplotlib',
    'nose',
    ]

classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Science/Research',
        'Operating System :: MacOS :: MacOS X',
        'Operating System :: Microsoft :: Windows',
        'Operating System :: POSIX',
        'Programming Language :: Python',
        'Topic :: Scientific/Engineering :: Atmospheric Science',
        ]

setup(name='flyingpigeon',
      version='0.1.1',
      description='Processes for climate data, indices and extrem events',
      long_description=README + '\n\n' + CHANGES,
      classifiers=classifiers,
      author='Nils Hempelmann',
      author_email='nils.hempelmann@ipsl.jussieu.fr',
      url='http://www.lsce.ipsl.fr/',
      license = "http://www.apache.org/licenses/LICENSE-2.0",
      keywords='wps pidgeon PyWPS Python Malleefowl CSC',
      packages=find_packages(),
      include_package_data=True,
      zip_safe=False,
      test_suite='nose.collector',
      install_requires=requires,
      entry_points = {
          'console_scripts': [
              ]}     
      ,
      )
