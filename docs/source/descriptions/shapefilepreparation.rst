Shapefile prepreparation
========================

Pour agrandir le « scope » du Extremoscope de la France en Europe, nous obtenons les régions du GADM database, spécifiquement la vérsion 2.5 pour le monde entier, avec six couches (une couche pour chaque niveau de subdivision). Le téléchargement est un répertoire (gadm26_levels.gdb) qui peut être lu avec qgis 2.8. La version 2.8 de qgis peut être installée pour Fedora 22. Si vous avez une vérsion de Fedora plus ancienne, il faut le mettre à jour.

Ouvre le répertoire gdb

Pour ouvrir gadm26_levels.gdb avec qgis, suivre les étapes ci-dessous:

Add Vector Layer ->

………Source type = Directory

………Source:

………………Type: OpenFileGDB

Après la sélection du répertoire, sélectionner « Open » et une fenêtre s’affiche avec les six couches. Sélectionner la couche qui vous voudriez. Nous utilisons « adm1 » qui correspond au niveau administratif du pays.

Filtre les pays

Pour choisir les pays européens, utiliser le « Advanced Filter (Expression) » dans le Attribute Table qui correspond au adm1. Utiliser l’expression suivante qui contient les pays européens désirés:

    « ISO » LIKE ‘%AUT%’ OR  « ISO »  LIKE ‘%BEL%’ OR « ISO »  LIKE ‘%BGR%’ OR « ISO »  LIKE ‘%CYP%’ OR « ISO »  LIKE ‘%CZE%’ OR « ISO »  LIKE ‘%DEU%’ OR « ISO »  LIKE ‘%DNK%’ OR « ISO »  LIKE ‘%ESP%’ OR « ISO »  LIKE ‘%EST%’ OR « ISO »  LIKE ‘%FIN%’ OR « ISO »  LIKE ‘%FRA%’ OR « ISO »  LIKE ‘%GBR%’ OR « ISO »  LIKE ‘%GRC%’ OR « ISO »  LIKE ‘%HUN%’ OR « ISO »  LIKE ‘%HRV%’ OR « ISO »  LIKE ‘%IRL%’ OR « ISO »  LIKE ‘%ITA%’ OR « ISO »  LIKE ‘%LVA%’ OR « ISO »  LIKE ‘%LTU%’ OR « ISO »  LIKE ‘%LUX%’ OR « ISO »  LIKE ‘%MLT%’ OR « ISO »  LIKE ‘%NLD%’ OR « ISO »  LIKE ‘%POL%’ OR « ISO »  LIKE ‘%PRT%’ OR « ISO »  LIKE ‘%ROU%’ OR « ISO »  LIKE ‘%SVK%’ OR « ISO »  LIKE ‘%SVN%’ OR « ISO »  LIKE ‘%SWE%’ OR « ISO »  LIKE ‘%NOR%’ OR « ISO »  LIKE ‘%CHE%’ OR « ISO »  LIKE ‘%ISL%’ OR « ISO »  LIKE ‘%MKD%’ OR « ISO »  LIKE ‘%MNE%’ OR « ISO »  LIKE ‘%SRB%’ OR « ISO »  LIKE ‘%MDA%’ OR « ISO »  LIKE ‘%UKR%’ OR « ISO »  LIKE ‘%BIH%’ OR « ISO »  LIKE ‘%ALB%’ OR « ISO »  LIKE ‘%BLR%’ OR  « ISO » LIKE ‘%XKO%’

Le « Attribute Table » va être met à jour, et vous pouvez choisir tous les rangées. Cette sélection est affichée sur la carte dans la fenêtre principale de qgis. Vous pouvez télécharger cette selection en format ESRI Shapefile:

Layer -> Save as -> Save only selected features

Et voilà!

Screenshot from 2015-08-06 15-33-20

Simplify avec mapshaper (command line)

Noter que les options défauts pour le command line sont: Visvalingam Weighted Area, no Snap Vertices.

$ mapshaper -i adm1-EU.shp -simplify 1% -o adm1-EU-mapshaped1.shp

=> Repaired 98 intersections; unable to repair 1 intersection.

Voici le côte de Norvege simplifiée (violet), superposée sur la carte originale (bleu) :

Screenshot from 2015-08-06 18-01-09

$ mapshaper -i adm1-EU.shp -simplify 10% -o adm1-EU-mapshaped10.shp

=> Repaired 69 intersections; unable to repair 46 intersection.

$ mapshaper -i adm1-EU.shp -simplify 5% -o adm1-EU-mapshaped5.shp

=> Repaired 83 intersections; unable to repair 30 intersection.

Simplify avec mapshaper (GUI)

J’ai utilisé le GUI de mapshaper pour tester les options différents, à savoir : Visvalingam Weighted Area vs. Effective Area, et Snap Vertices ON vs OFF. Le niveau de simplification c’était toujours 1%.

1. Visvalingam Weighted Area, Snap Vertices OFF (même que sur le command line):

    comme avant, il y avait une intersection pas réparée sur 99. Cette intersection se trouve dans la frontière de Montenegro, municipalité de Pljevlja (point rouge) :

Screenshot from 2015-08-07 15-04-52Mais il ne semble pas que cette intersection a impactée le résultat finale.

2. Visvalingam Weighted Area, Snap Vertices ON : tous les intersections étaient réparées.

3. Visvalingam Effective Area, Snap Vertices ON : tous les intersections étaient réparées.

4. Visvalingam Effective Area, Snap Vertices OFF : tous les intersections étaient réparées.

Comparison

Effective Area préserve avec mieux fidelité les frontières (pays, lacs, fjords) que Weighted Area.

Images dans l’ordre (côte ouest du norvège) : original (cyan), NoSnapVertices-WeightedArea (rose vif), NoSnapVertices-EffectiveArea (violet):