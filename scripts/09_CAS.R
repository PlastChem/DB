# Aurisano

aurisano_clean <- readRDS("data/input/clean/Aurisano/aurisano_clean.rds")
aurisano_cas <- unique(na.omit(unlist(strsplit(aurisano_clean$cas_number, ";"))))

cas_df <- data.frame(
  "cas_id" = seq_along(aurisano_cas), 
  "cas" = aurisano_cas,
  stringsAsFactors = FALSE
)


# CPPdb
cpp <- readRDS("data/input/clean/CPPdb/CPPdb.rds")
cpp_cas <- unique(na.omit(unlist(strsplit(cpp$cas, ";"))))

cas_df <- rbind(
  cas_df,
  data.frame(
    "cas_id" = max(cas_df$cas_id) + 
      seq_along(cpp_cas[!(cpp_cas %in% cas_df$cas)]), 
    "cas" = cpp_cas[!(cpp_cas %in% cas_df$cas)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)


# ECHA db
echa_clean <- readRDS("data/input/clean/ECHA_db/echa_clean.rds")

echa_cas <- unique(na.omit(unlist(strsplit(echa_clean$cas_no, ";"))))

cas_df <- rbind(
  cas_df,
  data.frame(
    "cas_id" = max(cas_df$cas_id) + 
      seq_along(echa_cas[!(echa_cas %in% cas_df$cas)]), 
    "cas" = echa_cas[!(echa_cas %in% cas_df$cas)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)


# FCCdb
fcc <- readRDS("data/input/clean/FCCdb/FCCdb_v5.rds")

fcc_cas <- unique(na.omit(unlist(strsplit(fcc$cas, ";"))))

cas_df <- rbind(
  cas_df,
  data.frame(
    "cas_id" = max(cas_df$cas_id) + 
      seq_along(fcc_cas[!(fcc_cas %in% cas_df$cas)]), 
    "cas" = fcc_cas[!(fcc_cas %in% cas_df$cas)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)

# FCCmigex
fccmigex_clean <- readRDS("data/input/clean/FCCmigex_db/fccmigex_clean.rds")

fccmigex_cas <- unique(na.omit(unlist(strsplit(fccmigex_clean$cas_id_final, ";"))))

cas_df <- rbind(
  cas_df,
  data.frame(
    "cas_id" = max(cas_df$cas_id) + 
      seq_along(fccmigex_cas[!(fccmigex_cas %in% cas_df$cas)]), 
    "cas" = fccmigex_cas[!(fccmigex_cas %in% cas_df$cas)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)


# PlasticMAP
pmap <- readRDS("data/input/clean/PlasticMAP/pmap.rds")
pmap_cas <- unique(na.omit(unlist(strsplit(pmap$casrn, ";"))))

cas_df <- rbind(
  cas_df,
  data.frame(
    "cas_id" = max(cas_df$cas_id) + 
      seq_along(pmap_cas[!(pmap_cas %in% cas_df$cas)]), 
    "cas" = pmap_cas[!(pmap_cas %in% cas_df$cas)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)

# PlasticMAP_2
pmap2 <- readRDS("data/input/clean/PlasticMAP_2/pmap2.rds")
pmap2_cas <- unique(na.omit(unlist(strsplit(pmap2$casrn, ";"))))

cas_df <- rbind(
  cas_df,
  data.frame(
    "cas_id" = max(cas_df$cas_id) + 
      seq_along(pmap2_cas[!(pmap2_cas %in% cas_df$cas)]), 
    "cas" = pmap2_cas[!(pmap2_cas %in% cas_df$cas)],
    stringsAsFactors = FALSE
  ),
  make.row.names = FALSE
)

# Saving

saveRDS(cas_df, "data/process/cas_list.rds")

##

cas_df <- transform(
  cas_df,
  "in_aurisano" = as.integer(cas %in% aurisano_cas),
  "in_cpp" = as.integer(cas %in% cpp_cas),
  "in_echa" = as.integer(cas %in% echa_cas),
  "in_fcc" = as.integer(cas %in% fcc_cas),
  "in_fccmigex" = as.integer(cas %in% fccmigex_cas),
  "in_plasticmap" = as.integer(cas %in% pmap_cas),
  "in_plasticmap2" = as.integer(cas %in% pmap2_cas)
)

##

cas_cid <- pbapply::pblapply(
  cas_df$cas,
  FUN = function(x) {
    res <- try(
      {
        Sys.sleep(0.2)
        pcapi::post_to_cid(x, format = "Name")
      },
      silent = TRUE
    )
    while(inherits(res, "try-error")) {
      Sys.sleep(60)
      res <- try(
        {
          Sys.sleep(0.2)
          pcapi::post_to_cid(x, format = "Name")
        },
        silent = TRUE
      )
    }
    res
  }
)

saveRDS(cas_cid, "data/process/cas_cid.rds")

cas_cid <- readRDS("data/process/cas_cid.rds")

cas_df <- transform(
  cas_df, 
  "pubchem_cid" = sapply(cas_cid, \(x) x[[1]])
)

cas_prop <- pcapi::post_to_property(
  unique(na.omit(cas_df$pubchem_cid)), 
  property = c("MolecularFormula", "MolecularWeight", "CanonicalSMILES", 
               "IsomericSMILES", "InChI", "InChIKey", "IUPACName", "Title", 
               "XLogP", "ExactMass", "MonoisotopicMass", "TPSA", "Complexity", 
               "Charge")
)
colnames(cas_prop) <- c("pubchem_cid", "molecular_formula", "molecular_weight", 
                        "canonical_smiles", "isomeric_smiles", "inchi", 
                        "inchikey", "iupac_name", "xlogp", "exact_mass", 
                        "monoisotopic_mass", "tpsa", "complexity", "charge", "title")

cas_df <- 
  merge(
    cas_df,
    cas_prop,
    by.x = "pubchem_cid",
    by.y = "pubchem_cid",
    all.x = TRUE
  )

colnames(aurisano_clean)[3:8] <- paste("aurisano", colnames(aurisano_clean)[3:8], sep = "_")

cas_df_new <- merge(
  cas_df,
  aurisano_clean,
  by.x = "cas",
  by.y = "cas_number",
  all.x = TRUE
)

cpp <- readRDS("data/input/clean/CPPdb/CPPdb.rds")
colnames(cpp)[4:9] <- paste("cpp", colnames(cpp)[4:9], sep = "_")

cas_df_new <- merge(
  cas_df_new,
  cpp,
  by.x = c("cas", "aurisano_ec"),
  by.y = c("cas", "ec_number"),
  all.x = TRUE
)
colnames(cas_df_new)[2] <- "ec"

colnames(echa_clean)[c(2, 5:6)] <- paste("echa", colnames(echa_clean)[c(2, 5:6)], sep = "_")

cas_df_new <- merge(
  cas_df_new,
  echa_clean,
  by.x = c("cas", "ec"),
  by.y = c("cas_no", "ec_no"),
  all.x = TRUE
)

colnames(fcc)[3] <- "fcc_name"

cas_df_new <- merge(
  cas_df_new,
  fcc,
  by.x = "cas",
  by.y = "cas_number",
  all.x = TRUE
)

colnames(fccmigex_clean)[c(2:6, 8)] <- paste("fccmigex", colnames(fccmigex_clean)[c(2:6, 8)], sep = "_")

cas_df_new <- merge(
  cas_df_new,
  fccmigex_clean,
  by.x = "cas",
  by.y = "cas_id_final",
  all.x = TRUE
)

pmap <- readRDS("data/input/clean/PlasticMAP/pmap.rds")
colnames(pmap)[c(1, 4:47)] <- paste("plasticmap", colnames(pmap)[c(1, 4:47)], sep = "_")

cas_df_new <- merge(
  cas_df_new,
  pmap,
  by.x = "cas",
  by.y = "casrn",
  all.x = TRUE
)

colnames(pmap2)[c(2, 4:47)] <- paste("plasticmap2", colnames(pmap2)[c(2, 4:47)], sep = "_")
colnames(pmap2)[3] <- "pmap2_id"

cas_df_new <- merge(
  cas_df_new,
  pmap2,
  by.x = "cas",
  by.y = "casrn",
  all.x = TRUE
)

cas_df_new <- cbind("identified_by" = "cas", cas_df_new)

saveRDS(cas_df_new, "data/process/cas_df_new.rds")

cas_df_new <- readRDS("data/process/cas_df_new.rds")
