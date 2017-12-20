import pytest


@pytest.mark.skip(reason="test is broken")
def test_graphics():
    from rpy2.robjects.packages import importr
    # ds = importr("datasets")
    gr = importr("graphics")
    gr.plot(500, 400)
    grDevices.graphics_off()


@pytest.mark.skip(reason="test is broken")
def test_pdf_garbage():
    from rpy2.robjects.packages import importr
    grDevices = importr("grDevices")
    base = importr("base")
    grDevices.pdf(file='Rplot.pdf')
    garbage = grDevices.dev_off()
    remove('Rplot.pdf')


# def test_png_garbage():
#     png = grDevices.png(filename='Rplot.png')
#     garbage = grDevices.dev_off()
