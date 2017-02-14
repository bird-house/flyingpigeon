from PyPDF2 import PdfFileWriter, PdfFileReader
from reportlab.pdfgen import canvas


c = canvas.Canvas('watermark.pdf')
c.drawImage('testplot.png', 350, 550, width=150, height=150)  # , mask=None, preserveAspectRatio=False)
#  c.drawString(15, 720, "Hello World")
c.save()


c = canvas.Canvas('watermark2.pdf')
c.drawImage('testplot.png', 250, 350, width=250, height=150)  # , mask=None, preserveAspectRatio=False)
#  c.drawString(15, 720, "Hello World")
c.save()


output_file = PdfFileWriter()
watermark = PdfFileReader(open("watermark.pdf", 'rb'))
watermark2 = PdfFileReader(open("watermark2.pdf", 'rb'))
input_file = PdfFileReader(file('../flyingpigeon/static/pdf/climatefactsheettemplate.pdf', 'rb'))

page_count = input_file.getNumPages()

for page_number in range(page_count):
    print "Plotting png to {} of {}".format(page_number, page_count)
    input_page = input_file.getPage(page_number)
    input_page.mergePage(watermark.getPage(0))
    input_page.mergePage(watermark2.getPage(0))
    output_file.addPage(input_page)

with open('output.pdf', 'wb') as outputStream:
    output_file.write(outputStream)
