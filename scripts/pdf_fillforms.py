from fdfgen import forge_fdf
fields = [('countries', 'Cameroon')]
fdf = forge_fdf("", fields, [], [], [])
fdf_file = open("data.fdf", "w")
fdf_file.write(fdf)
fdf_file.close()
