# This script provides functionality to download all necessary files.

# Aurisano
## https://doi.org/10.1016/j.cogsc.2021.100513

aurisano_url <- 
  "https://ars.els-cdn.com/content/image/1-s2.0-S2452223621000699-mmc2.xlsx"

download.file(
  url = aurisano_url,
  destfile = "data/input/raw/Aurisano/1-s2.0-S2452223621000699-mmc2.xlsx",
  mode = ifelse(Sys.info()[["sysname"]] == "Windows", "wb", "w")
)

# CPP
## https://zenodo.org/record/1287773

cpp_url <- paste(
  "https://zenodo.org/record/1287773/files",
  "CPPdb_ListA_ListB_181009_ZenodoV1.xlsx?download=1",
  sep = "/"
)

download.file(
  url = cpp_url,
  destfile = "data/input/raw/CPPdb/CPPdb_ListA_ListB_181009_ZenodoV1.xlsx",
  mode = ifelse(Sys.info()[["sysname"]] == "Windows", "wb", "w")
)

# ECHA
## https://echa.europa.eu/plastic-material-food-contact
## the file needs to be manually downloaded :(
## the .xlsx file is _much_ better formatted than the .csv file

# FCC
## https://zenodo.org/record/4296944

fcc_url <- paste(
  "https://zenodo.org/record/4296944/files",
  "FCCdb_201130_v5_Zenodo.xlsx?download=1",
  sep = "/"
)

download.file(
  url = fcc_url,
  destfile = "data/input/raw/FCCdb/FCCdb_201130_v5_Zenodo.xlsx",
  mode = ifelse(Sys.info()[["sysname"]] == "Windows", "wb", "w")
)

# PlasticMAP
## https://pubs.acs.org/doi/full/10.1021/acs.est.1c00976

pmap_url <- paste(
  "https://pubs.acs.org/doi/suppl/10.1021/acs.est.1c00976",
  "suppl_file/es1c00976_si_001.zip",
  sep = "/"
)

download.file(
  url = pmap_url,
  destfile = "data/input/raw/PlasticMAP/es1c00976_si_001.zip",
  mode = ifelse(Sys.info()[["sysname"]] == "Windows", "wb", "w")
)

unzip(
  zipfile = "data/input/raw/PlasticMAP/es1c00976_si_001.zip",
  exdir = "data/input/raw/PlasticMAP"
)

# FCCmigex
## the files were confidentially shared with the project team

# LitChem
## the files were confidentially shared with the project team
