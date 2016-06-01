from pygbif import species, occurrences
TName = "Fagus sylvatica"
key = species.name_backbone(name=TName, rank="species")["usageKey"]
n = occurrences.count(taxonKey=key, isGeoreferenced=True)
if n > 300:
    max = 300
else:
    max = n
results = occurrences.search(taxonKey=key, limit=max)
print '(', key, ')', '-', format(n, ','), " ocurrence(s)"

for x in results["results"]:
    try:
        if x['continent'].find('_') != -1:
            Continent = ' '.join(x['continent'].split('_')).title()
        else:
            Continent = x['continent'].capitalize()
    except:
            Continent = ""
           
    try:
        Country = x['country']
    except:
        Country = ""
           
    try:
        State = x['stateProvince']
    except:
        State = ""
           
    try:
        County = x['county']
    except:
        County = ""
           
    try:
        Locality = x['locality']
    except:
        Locality = ""
           
    try:
        Latitude = x['decimalLatitude']
    except:
        Latitude = 0.0
       
    try:
        Longitude = x['decimalLongitude']
    except:
        Longitude = 0.0

    #print Continent, Country, State, County, Locality, Latitude, Longitude
    print Latitude, Longitude