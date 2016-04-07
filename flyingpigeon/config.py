import os

def shapefiles_dir():
    return os.path.join(os.path.abspath(os.path.dirname(__file__)), 'shapefiles')

def masks_dir():
    return os.path.join(os.path.abspath(os.path.dirname(__file__)), 'masks')

