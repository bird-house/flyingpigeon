def get_data_worker(variable, polygons=['FRA','DEU'], dir_output=None, start = 2014, end = 2014):
  from flyingpigeon import get_eobs_as_cordex
  
  def worker(variable, polygon, dir_output, start, end, que):
    EOBS_file = get_eobs_as_cordex.get_data(variable=variable, polygon=polygon, 
             dir_output=dir_output, start = start, end = end)
    que.put(EOBS_file)
    return
  
  from multiprocessing import Process, Queue
  
  if __name__ == '__main__':
    q = Queue()
    jobs = []
    files = []
    for i, polygon in enumerate(polygons): #for i in range(5):
      p = Process(target=worker, args=(variable, polygon, dir_output,
                                       start, end , q))
      jobs.append(p)
      p.start()
      p.join()
      files.append('%s' %(q.get()))
    
  return files
