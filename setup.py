import os

from setuptools import setup, find_packages

version = __import__('flyingpigeon').__version__
here = os.path.abspath(os.path.dirname(__file__))
README = open(os.path.join(here, 'README.rst')).read()
CHANGES = open(os.path.join(here, 'CHANGES.rst')).read()

reqs = [line.strip() for line in open('requirements/deploy.txt')]

classifiers = [
    'Development Status :: 3 - Alpha',
    'Intended Audience :: Science/Research',
    'Operating System :: MacOS :: MacOS X',
    'Operating System :: Microsoft :: Windows',
    'Operating System :: POSIX',
    'Programming Language :: Python',
    'Topic :: Scientific/Engineering :: Atmospheric Science',
]

setup(name='flyingpigeon',
      version=version,
      description='Processes for climate data, indices and extreme events',
      long_description=README + '\n\n' + CHANGES,
      classifiers=classifiers,
      author='Nils Hempelmann',
      author_email='info@nilshempelmann.de',
      url='http://flyingpigeon.readthedocs.io/en/latest/',
      license="http://www.apache.org/licenses/LICENSE-2.0",
      keywords='wps flyingpigeon pywps ipsl birdhouse conda climate indices species',
      packages=find_packages(),
      include_package_data=True,
      zip_safe=False,
      test_suite='flyingpigeon',
      install_requires=reqs,
      entry_points={
          'console_scripts': [
             'flyingpigeon=flyingpigeon:main',
          ]},
      )
