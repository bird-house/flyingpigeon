import pytest
from rpy2.robjects.packages import importr

from os import remove
grDevices = importr("grDevices")
base = importr("base")


def test_graphics():
    # ds = importr("datasets")
    gr = importr("graphics")
    gr.plot(500, 400)
    grDevices.graphics_off()


def test_pdf_garbage():
    grDevices.pdf(file='Rplot.pdf')
    garbage = grDevices.dev_off()
    remove('Rplot.pdf')


# def test_png_garbage():
#     png = grDevices.png(filename='Rplot.png')
#     garbage = grDevices.dev_off()
