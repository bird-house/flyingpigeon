import logging
logger = logging.getLogger(__name__)


def write_file(urls):
  pathes = open("filepathes.txt" , "w")
  logger.info('created the filepathes.txt file ')
  
  pathes.write('###############################################\n')
  pathes.write('###############################################\n')
  pathes.write('Following files are stored to your local discs: \n')
  pathes.write('\n')
  # try: 
  #   for url in urls: 
  #     pathes.write('%s \n' % url)
  # except Exception as e: 
  #   logger.error('failed to write path to file')
  
  textfile = pathes.name

  pathes.close() 

  return textfile

