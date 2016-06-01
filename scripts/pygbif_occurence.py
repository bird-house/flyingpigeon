from pygbif import species, occurrences
<<<<<<< HEAD
from numpy import nan, empty 
TName = "Fagus sylvatica"
key = species.name_backbone(name=TName, rank="species")["usageKey"]
n = occurrences.count(taxonKey=key, isGeoreferenced=True)

if n > 200000:
    max = 200000
else:
    max = n
results = occurrences.search(taxonKey=key, limit=max)

print '(', key, ')', '-', format(n, ','), " ocurrence(s)"

# lonslats = []
latlon = empty([max,2], dtype=float, order='C')

for i, x in enumerate(results["results"]):
    #try:
        #if x['continent'].find('_') != -1:
            #Continent = ' '.join(x['continent'].split('_')).title()
        #else:
            #Continent = x['continent'].capitalize()
    #except:
            #Continent = ""  
    #try:
        #Country = x['country']
    #except:
        #Country = ""
           
    #try:
        #State = x['stateProvince']
    #except:
        #State = ""
           
    #try:
        #County = x['county']
    #except:
        #County = ""
           
    #try:
        #Locality = x['locality']
    #except:
        #Locality = ""
    try:
      Latitude = (x['decimalLatitude'])
    except:
        Latitude = nan
    try:
      Longitude = (x['decimalLongitude'])
    except:
      Longitude = nan
    #lonslats.append([Longitude,Latitude])
    
    # print x['decimalLongitude'], x['decimalLatitude']
    # print Continent, Country, State, County, Locality, Latitude, Longitude
    latlon[i][0] = Latitude  
    latlon[i][1] = Longitude
