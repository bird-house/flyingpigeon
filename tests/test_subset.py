from flyingpigeon.subset_base import get_feature

def test_get_feature():
    url = "https://pavics.ouranos.ca/geoserver/wfs"
    typename = "public:USGS_HydroBASINS_lake_na_lev12"
    feature_pat = "USGS_HydroBASINS_lake_na_lev12.{}"

    #features = [feature_pat.format(i) for i in range(67088, 67449)]
    features = [feature_pat.format(i) for i in range(67088, 67090)]
    get_feature(url, typename, features)

