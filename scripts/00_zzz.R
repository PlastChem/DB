# setup the necessary folders

dir.create("data")
dir.create("data/input")
dir.create("data/input/raw")
dir.create("data/input/raw/Aurisano")
dir.create("data/input/raw/CPP")
dir.create("data/input/raw/ECHA")
dir.create("data/input/raw/FCC")
dir.create("data/input/raw/PlasticMAP")
# dir.create("data/input/raw/FCCmigex") ### confidential
# dir.create("data/input/raw/LitChem") ### confidential

dir.create("data/input/clean")
dir.create("data/input/clean/Aurisano")
dir.create("data/input/clean/CPP")
dir.create("data/input/clean/ECHA")
dir.create("data/input/clean/FCC")
dir.create("data/input/clean/PlasticMAP")
# dir.create("data/input/clean/FCCmigex") ### confidential
# dir.create("data/input/clean/LitChem") ### confidential

# check if necessary packages are installed

if(any(!(c("openxlsx", "pbapply", "skimr") %in% installed.packages()))) {
  install.packages(
    c("openxlsx", "pbapply", "skimr")[
      !(c("openxlsx", "pbapply", "skimr") %in% installed.packages())
    ]
  )
}

if(any(!(c("cleanventory", "pcapi") %in% installed.packages()))) {
  if(!("remotes" %in% installed.packages())) {
    install.packages("remotes")
  }
  remotes::install_github("RaoulWolf/cleanventory")
  remotes::install_github("RaoulWolf/pcapi")
}
