# Aurisano

aurisano <- readRDS("data/input/clean/Aurisano/aurisano_clean.rds")
aurisano_ec <- unique(na.omit(unlist(strsplit(aurisano[is.na(aurisano$cas_number), ]$ec, ";"))))
aurisano_name <- unique(na.omit(unlist(strsplit(aurisano[is.na(aurisano$cas_number) & is.na(aurisano$ec), ]$substance_name, ";"))))

ec_df <- data.frame(
  "ec" = aurisano_ec
)

name_df <- data.frame(
  "name" = aurisano_name
)


# CPPdb
cpp <- readRDS("data/input/clean/CPPdb/CPPdb.rds")
cpp_ec <- unique(na.omit(unlist(strsplit(cpp[is.na(cpp$cas), ]$ec_number, ";"))))
cpp_name <- unique(na.omit(unlist(strsplit(cpp[is.na(cpp$cas) & is.na(cpp$ec_number), ]$name, ";"))))

ec_df <- rbind(
  ec_df,
  data.frame(
    "ec" = cpp_ec[!(cpp_ec %in% ec_df$ec)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)

name_df <- rbind(
  name_df,
  data.frame(
    "name" = cpp_name[!(cpp_name %in% name_df$ec)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)

# ECHA db
echa_clean <- readRDS("data/input/clean/ECHA_db/echa_clean.rds")
echa_clean_ec <- unique(na.omit(unlist(strsplit(echa_clean[is.na(echa_clean$cas_no), ]$ec_no, ";"))))
echa_clean_name <- unique(na.omit(unlist(strsplit(echa_clean[is.na(echa_clean$cas_no) & is.na(echa_clean$ec_no), ]$substance_name, ";"))))

name_df <- rbind(
  name_df,
  data.frame(
    "name" = echa_clean_name[!(echa_clean_name %in% name_df$ec)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)


# FCCdb
fcc <- readRDS("data/input/clean/FCCdb/FCCdb_v5.rds")
fcc_name <- unique(na.omit(unlist(strsplit(fcc[is.na(fcc$cas_number), ]$name, ";"))))

name_df <- rbind(
  name_df,
  data.frame(
    "name" = fcc_name[!(fcc_name %in% name_df$ec)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)


# FCCmigex
fccmigex_clean <- readRDS("data/input/clean/FCCmigex_db/fccmigex_clean.rds")
fccmigex_name <- unique(na.omit(unlist(strsplit(fccmigex_clean[is.na(fccmigex_clean$cas_id_final), ]$fcc_main, ";"))))

name_df <- rbind(
  name_df,
  data.frame(
    "name" = fccmigex_name[!(fccmigex_name %in% name_df$ec)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)


# PlasticMAP
pmap <- readRDS("data/input/clean/PlasticMAP/pmap.rds")
pmap_name <- unique(na.omit(unlist(strsplit(pmap[is.na(pmap$casrn), ]$name, ";"))))

# PlasticMAP_2
pmap2 <- readRDS("data/input/clean/PlasticMAP_2/pmap2.rds")
pmap2_name <- unique(na.omit(unlist(strsplit(pmap2[is.na(pmap2$casrn), ]$name, ";"))))

name_df <- rbind(
  name_df,
  data.frame(
    "name" = pmap2_name[!(pmap2_name %in% name_df$ec)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)

##

ec_df <- cbind("identified_by" = "ec", ec_df)
ec_df <- transform(
  ec_df,
  "in_aurisano" = as.integer(ec %in% aurisano_ec),
  "in_cpp" = as.integer(ec %in% cpp_ec),
  "in_echa" = as.integer(ec %in% echa_clean_ec),
  "in_fcc" = 0L,
  "in_fccmigex" = 0L,
  "in_plasticmap" = 0L,
  "in_plasticmap2" = 0L
)

ec_cid <- pbapply::pblapply(
  ec_df$ec,
  FUN = function(x) {
    Sys.sleep(0.2)
    res <- pcapi::post_to_cid(x, format = "Name")
    res
  }
)

ec_df <- transform(
  ec_df, 
  "pubchem_cid" = sapply(ec_cid, \(x) x[[1]])
)

ec_df <- transform(
  ec_df,
  "pubchem_cid" = NA_integer_, 
  "molecular_formula" = NA_character_, 
  "molecular_weight" = NA_real_, 
  "canonical_smiles" = NA_character_, 
  "isomeric_smiles" = NA_character_, 
  "inchi" = NA_character_, 
  "inchikey" = NA_character_, 
  "iupac_name" = NA_character_, 
  "xlogp" = NA_real_, 
  "exact_mass" = NA_real_, 
  "monoisotopic_mass" = NA_real_, 
  "tpsa" = NA_real_, 
  "complexity" = NA_integer_, 
  "charge" = NA_integer_, 
  "title" = NA_character_
)

ec_df_clean <- cbind("identified_by" = ec_df[, 1], "cas" = NA_character_, "ec" = ec_df[, 2], "name" = NA_character_, "pubchem_cid" = ec_df[, 10], ec_df[c(3:9, 11:24)])

saveRDS(ec_df_clean, "data/process/ec_df.rds")

ec_df_clean <- readRDS("data/process/ec_df.rds")

##

name_df <- cbind("identified_by" = "name", name_df)
name_df <- transform(
  name_df,
  "in_aurisano" = as.integer(tolower(name) %in% tolower(aurisano_name)),
  "in_cpp" = as.integer(tolower(name) %in% tolower(cpp_name)),
  "in_echa" = as.integer(tolower(name) %in% tolower(echa_clean_name)),
  "in_fcc" = as.integer(tolower(name) %in% tolower(fcc_name)),
  "in_fccmigex" = as.integer(tolower(name) %in% tolower(fccmigex_name)),
  "in_plasticmap" = as.integer(tolower(name) %in% tolower(pmap_name)),
  "in_plasticmap2" = as.integer(tolower(name) %in% tolower(pmap2_name))
)

name_cid <- pbapply::pblapply(
  name_df$name,
  FUN = function(x) {
    Sys.sleep(0.2)
    res <- pcapi::post_to_cid(x, format = "Name")
    res
  }
)

name_df <- transform(
  name_df, 
  "pubchem_cid" = sapply(name_cid, \(x) x[[1]])
)

name_prop <- pcapi::post_to_property(
  unique(na.omit(name_df$pubchem_cid)), 
  property = c("MolecularFormula", "MolecularWeight", "CanonicalSMILES", 
               "IsomericSMILES", "InChI", "InChIKey", "IUPACName", "Title", 
               "XLogP", "ExactMass", "MonoisotopicMass", "TPSA", "Complexity", 
               "Charge")
)
colnames(name_prop) <- c("pubchem_cid", "molecular_formula", "molecular_weight", 
                        "canonical_smiles", "isomeric_smiles", "inchi", 
                        "inchikey", "iupac_name", "xlogp", "exact_mass", 
                        "monoisotopic_mass", "tpsa", "complexity", "charge", "title")

name_df <- 
  merge(
    name_df,
    name_prop,
    by.x = "pubchem_cid",
    by.y = "pubchem_cid",
    all.x = TRUE
  )

name_df_clean <- cbind("identified_by" = name_df[, 2], "cas" = NA_character_, "ec" = NA_character_, "name" = name_df[, 3], "pubchem_cid" = name_df[, 1], name_df[, 4:24])

saveRDS(name_df_clean, "data/process/name_df.rds")

name_df_clean <- readRDS("data/process/name_df.rds")

#

no_cas_df <- rbind(ec_df_clean, name_df_clean, make.row.names = FALSE)

##

## EC 

ec_df_new <- merge(
  ec_df_clean,
  subset(aurisano, select = -c(cas_number)),
  by.x = c("ec"),
  by.y = c("ec"),
  all.x = TRUE
)

ec_df_new <- merge(
  ec_df_new,
  subset(cpp, select = -c(cas)),
  by.x = c("ec"),
  by.y = c("ec_number"),
  all.x = TRUE
)

ec_df_new <- merge(
  ec_df_new,
  subset(echa_clean, select = -c(cas_no)),
  by.x = c("ec"),
  by.y = c("ec_no"),
  all.x = TRUE
)

ec_df_new <- merge(
  ec_df_new,
  transform(subset(fcc, select = -c(cas_number)), "ec" = NA_character_),
  by.x = "ec",
  by.y = "ec",
  all.x = TRUE
)

ec_df_new <- merge(
  ec_df_new,
  transform(subset(fccmigex_clean, select = -c(cas_id_final)), ec = NA_character_),
  by.x = "ec",
  by.y = "ec",
  all.x = TRUE
)

ec_df_new <- merge(
  ec_df_new,
  transform(subset(pmap, select = -c(casrn)), "ec" = NA_character_),
  by.x = "ec",
  by.y = "ec",
  all.x = TRUE
)

ec_df_new <- merge(
  ec_df_new,
  transform(subset(pmap2, select = -c(casrn)), "ec" = NA_character_),
  by.x = "ec",
  by.y = "ec",
  all.x = TRUE
)

colnames(cas_df_clean)[!(colnames(cas_df_clean) %in% colnames(ec_df_new))]

## Name

name_df_new <- merge(
  name_df_clean,
  transform(subset(aurisano_clean, select = -c(cas_number, aurisano_ec)), "name" = aurisano_substance_name),
  by.x = c("name"),
  by.y = c("name"),
  all.x = TRUE
)

name_df_new <- merge(
  name_df_new,
  transform(subset(cpp, select = -c(cas, ec_number)), "name" = cpp_name),
  by.x = c("name"),
  by.y = c("name"),
  all.x = TRUE
)

name_df_new <- merge(
  name_df_new,
  transform(subset(echa_clean, select = -c(cas_no, ec_no)), "name" = echa_substance_name),
  by.x = c("name"),
  by.y = c("name"),
  all.x = TRUE
)

name_df_new <- merge(
  name_df_new,
  transform(subset(fcc, select = -c(cas_number)), "name" = fcc_name),
  by.x = "name",
  by.y = "name",
  all.x = TRUE
)

name_df_new <- merge(
  name_df_new,
  transform(subset(fccmigex_clean, select = -c(cas_id_final)), "name" = fccmigex_fcc_main),
  by.x = "name",
  by.y = "name",
  all.x = TRUE
)

name_df_new <- merge(
  name_df_new,
  transform(subset(pmap, select = -c(casrn)), "name" = plasticmap_name),
  by.x = "name",
  by.y = "name",
  all.x = TRUE
)

name_df_new <- merge(
  name_df_new,
  transform(subset(pmap2, select = -c(casrn)), "name" = plasticmap2_name),
  by.x = "name",
  by.y = "name",
  all.x = TRUE
)

colnames(cas_df_new)[!(colnames(cas_df_new) %in% colnames(name_df_new))]

##

##

colnames(ec_df_new)[!(colnames(ec_df_new) %in% colnames(name_df_new))]
colnames(name_df_new)[!(colnames(name_df_new) %in% colnames(ec_df_new))]

subset(ec_df_new, select = -c(name)) |> colnames()
subset(name_df_new, select = -c(name)) |> colnames()

ec_df_new2 <- cbind(ec_df_new[, 2:3], "ec" = ec_df_new[, 1], ec_df_new[4:144])

no_cas_df <- rbind(
  subset(ec_df_new2, select = -c(name)),
  subset(name_df_new, select = -c(name)),
  make.row.names = FALSE
)

saveRDS(no_cas_df, "data/process/no_cas_df.rds")

# Saving

full_data <- rbind(subset(cas_df_new, select = -c(cas_id)), no_cas_df, make.row.names = FALSE)
full_data <- cbind("plastchem_id" = 1:nrow(full_data), full_data)
full_data <- subset(full_data, select = -c(plasticmap2_name.1))
colnames(full_data)[c(54, 100)] <- c("plasticmap_id", "plasticmap2_id")
colnames(full_data)[13:26] <- paste("pubchem", colnames(full_data)[13:26], sep = "_")

full_data <- cbind(full_data[, 1:4], full_data[, 6:12], "pubchem_cid" = full_data[, 5], full_data[, 13:143])

saveRDS(full_data, "data/process/full_data.rds")
