Example
Data: NCEP 1948-2014
Reference Period: 1970-2010
seas: JJA
Functions:
/home/estimr2/calvarez/birdhouse/libraryregimes.R 


1. To compute Weather Regimes during a Reference Period: /home/estimr2/calvarez/birdhouse/regimes_ref_NCEP.R 
input: you have to load "libraryregimes.R" and "slp.1948-2014_NA.nc"
output: "slp_EOF_JJA_clim.dat "and "NCEP_regimes_1970-2010_JJA.Rdat" 
(you can get also "slp_PC_JJA_clim.dat","slp_vap_JJA_clim.dat" and "NCEP_regimes_1970-2010_JJA", right now are commented)

2. To project weather regimes computed during a reference period: /home/estimr2/calvarez/birdhouse/project_EOF_NCEP.R 

input: you have to load "libraryregimes.R","slp.1948-2014_NA.nc", "slp_EOF_JJA_clim.dat "and "NCEP_regimes_1970-2010_JJA.Rdat"
output: you will get "frec_percent_WR_NCEP_SLP_JJA_1948-2014.dat"(file with the % of each weather regime per JJA), 
"NCEP_SLP_JJA_1948-2014_classif.dat" (daily best weather regimes with distances and correlations) 
and "projNCEP_regimes_1948-2014_JJA.pdf" (maps of weather regimes)




