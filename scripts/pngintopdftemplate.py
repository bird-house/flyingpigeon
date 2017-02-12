from PyPDF2 import PdfFileWriter, PdfFileReader
from reportlab.pdfgen import canvas


c = canvas('watermark.pdf')
c.drawImage('testplot.png', 15, 720)

c.drawString(15, 720, "Hello World")
c.save()

output_file = PdfFileWriter()
watermark = PdfFileReader(open("watermark.pdf"))
input_pdf = PdfFileReader(file('../flyingpigeon/static//pdf/climatefactsheettemplate.pdf', 'rb'))

page_count = input_file.getNumPages()

for page_number in range(page_count):
    print "Plotting png to {} of {}".format(page_number, page_count)
    input_page = input_file.getPage(page_number)
    input_page.mergePage(watermark.getPage(0))
    output_file.addPage(input_page)

with open('climatefactsheet.pdf', 'wb') as outputStream:
    output_file.write(outputStream)
