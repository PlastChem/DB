
# Aurisano

aurisano_name2 <- subset(full_data, subset = in_aurisano == 1L & is.na(pubchem_cid) & !is.na(aurisano_substance_name))
aurisano_name2 <- unique(aurisano_name2$aurisano_substance_name)

aurisano_name2_cid <- pbapply::pblapply(
  aurisano_name2,
  FUN = function(x) {
    Sys.sleep(0.2)
    res <- pcapi::post_to_cid(x, format = "Name")
    res
  }
)

aurisano_name2_df <- data.frame(
  "name" = aurisano_name2,
  "pubchem_cid" = sapply(aurisano_name2_cid, \(x) x[[1]])
)

aurisano_name2_prop <- pcapi::post_to_property(
  unique(na.omit(aurisano_name2_df$pubchem_cid)),
  property = c("MolecularFormula", "MolecularWeight", "CanonicalSMILES", 
               "IsomericSMILES", "InChI", "InChIKey", "IUPACName", "Title", 
               "XLogP", "ExactMass", "MonoisotopicMass", "TPSA", "Complexity", 
               "Charge")
)
colnames(aurisano_name2_prop) <- c("pubchem_cid", "molecular_formula", "molecular_weight", 
                         "canonical_smiles", "isomeric_smiles", "inchi", 
                         "inchikey", "iupac_name", "exact_mass", 
                         "monoisotopic_mass", "tpsa", "complexity", "charge", "title", "xlogp")

aurisano_name2_df <- 
  merge(
    aurisano_name2_df,
    aurisano_name2_prop,
    by.x = "pubchem_cid",
    by.y = "pubchem_cid",
    all.x = TRUE
  )

full_data_new <- full_data
full_data_new_aurisano <- subset(full_data_new, subset = in_aurisano == 1L)
full_data_new_aurisano <- transform(
  full_data_new_aurisano,
  "identified_by" = ifelse(aurisano_substance_name %in% aurisano_name2_df$name, "name", identified_by)
) |> 
  merge(
    aurisano_name2_df,
    by.x = "aurisano_substance_name",
    by.y = "name",
    all.x = TRUE, 
  ) |> 
  transform(
    "pubchem_cid" = ifelse(is.na(pubchem_cid.x), pubchem_cid.y, pubchem_cid.x),
    "pubchem_molecular_formula" = ifelse(is.na(pubchem_molecular_formula), molecular_formula, pubchem_molecular_formula), 
    "pubchem_molecular_weight" = ifelse(is.na(pubchem_molecular_weight), molecular_weight, pubchem_molecular_weight), 
    "pubchem_canonical_smiles" = ifelse(is.na(pubchem_canonical_smiles), canonical_smiles, pubchem_canonical_smiles), 
    "pubchem_isomeric_smiles" = ifelse(is.na(pubchem_isomeric_smiles), isomeric_smiles, pubchem_isomeric_smiles), 
    "pubchem_inchi" = ifelse(is.na(pubchem_inchi), inchi, pubchem_inchi), 
    "pubchem_inchikey" = ifelse(is.na(pubchem_inchikey), inchikey, pubchem_inchikey), 
    "pubchem_iupac_name" = ifelse(is.na(pubchem_iupac_name), iupac_name, pubchem_iupac_name), 
    "pubchem_xlogp" = ifelse(is.na(pubchem_xlogp), xlogp, pubchem_xlogp), 
    "pubchem_exact_mass" = ifelse(is.na(pubchem_exact_mass), exact_mass, pubchem_exact_mass), 
    "pubchem_monoisotopic_mass" = ifelse(is.na(pubchem_monoisotopic_mass), monoisotopic_mass, pubchem_monoisotopic_mass), 
    "pubchem_tpsa" = ifelse(is.na(pubchem_tpsa), tpsa, pubchem_tpsa), 
    "pubchem_complexity" = ifelse(is.na(pubchem_complexity), complexity, pubchem_complexity), 
    "pubchem_charge" = ifelse(is.na(pubchem_charge), charge, pubchem_charge), 
    "pubchem_title" = ifelse(is.na(pubchem_title), title, pubchem_title)
  ) |> 
  transform("pubchem_cid.x" = pubchem_cid)

full_data_new_aurisano <- subset(full_data_new_aurisano, select = colnames(full_data))
full_data_new_aurisano <- full_data_new_aurisano[, colnames(full_data)]

full_data_new <- rbind(
  subset(full_data_new, subset = in_aurisano == 0L),
  full_data_new_aurisano,
  make.row.names = FALSE
)

## CPP

full_data_new_cpp <- subset(full_data_new, subset = in_cpp == 1L & is.na(pubchem_cid) & !is.na(cpp_name))

full_data_new_cpp_name_cid <- pbapply::pblapply(
  unique(full_data_new_cpp$cpp_name),
  FUN = function(x) {
    Sys.sleep(0.2)
    res <- pcapi::post_to_cid(x, format = "Name")
    res
  }
)

full_data_new_cpp_name_df <- data.frame(
  "name" = unique(full_data_new_cpp$cpp_name),
  "pubchem_cid" = sapply(full_data_new_cpp_name_cid, \(x) x[[1]])
)

full_data_new_cpp_prop <- pcapi::post_to_property(
  unique(na.omit(full_data_new_cpp_name_df$pubchem_cid)),
  property = c("MolecularFormula", "MolecularWeight", "CanonicalSMILES", 
               "IsomericSMILES", "InChI", "InChIKey", "IUPACName", "Title", 
               "XLogP", "ExactMass", "MonoisotopicMass", "TPSA", "Complexity", 
               "Charge")
)
colnames(full_data_new_cpp_prop) <- c("pubchem_cid", "molecular_formula", "molecular_weight", 
                                   "canonical_smiles", "isomeric_smiles", "inchi", 
                                   "inchikey", "iupac_name", "exact_mass", 
                                   "monoisotopic_mass", "tpsa", "complexity", "charge", "title", "xlogp")

cpp_name_df <- 
  merge(
    full_data_new_cpp_name_df,
    full_data_new_cpp_prop,
    by.x = "pubchem_cid",
    by.y = "pubchem_cid",
    all.x = TRUE
  )

# full_data_new <- full_data
full_data_new_cpp <- subset(full_data_new, subset = in_cpp == 1L)
full_data_new_cpp <- transform(
  full_data_new_cpp,
  "identified_by" = ifelse(cpp_name %in% cpp_name_df$name, "name", identified_by)
) |> 
  merge(
    cpp_name_df,
    by.x = "cpp_name",
    by.y = "name",
    all.x = TRUE, 
  ) |> 
  transform(
    "pubchem_cid" = ifelse(is.na(pubchem_cid.x), pubchem_cid.y, pubchem_cid.x),
    "pubchem_molecular_formula" = ifelse(is.na(pubchem_molecular_formula), molecular_formula, pubchem_molecular_formula), 
    "pubchem_molecular_weight" = ifelse(is.na(pubchem_molecular_weight), molecular_weight, pubchem_molecular_weight), 
    "pubchem_canonical_smiles" = ifelse(is.na(pubchem_canonical_smiles), canonical_smiles, pubchem_canonical_smiles), 
    "pubchem_isomeric_smiles" = ifelse(is.na(pubchem_isomeric_smiles), isomeric_smiles, pubchem_isomeric_smiles), 
    "pubchem_inchi" = ifelse(is.na(pubchem_inchi), inchi, pubchem_inchi), 
    "pubchem_inchikey" = ifelse(is.na(pubchem_inchikey), inchikey, pubchem_inchikey), 
    "pubchem_iupac_name" = ifelse(is.na(pubchem_iupac_name), iupac_name, pubchem_iupac_name), 
    "pubchem_xlogp" = ifelse(is.na(pubchem_xlogp), xlogp, pubchem_xlogp), 
    "pubchem_exact_mass" = ifelse(is.na(pubchem_exact_mass), exact_mass, pubchem_exact_mass), 
    "pubchem_monoisotopic_mass" = ifelse(is.na(pubchem_monoisotopic_mass), monoisotopic_mass, pubchem_monoisotopic_mass), 
    "pubchem_tpsa" = ifelse(is.na(pubchem_tpsa), tpsa, pubchem_tpsa), 
    "pubchem_complexity" = ifelse(is.na(pubchem_complexity), complexity, pubchem_complexity), 
    "pubchem_charge" = ifelse(is.na(pubchem_charge), charge, pubchem_charge), 
    "pubchem_title" = ifelse(is.na(pubchem_title), title, pubchem_title)
  ) |> 
  transform("pubchem_cid.x" = pubchem_cid)

full_data_new_cpp <- subset(full_data_new_cpp, select = colnames(full_data))
full_data_new_cpp <- full_data_new_cpp[, colnames(full_data)]

full_data_new <- rbind(
  subset(full_data_new, subset = in_cpp == 0L),
  full_data_new_cpp,
  make.row.names = FALSE
)

## ECHA

echa_name2 <- subset(full_data_new, subset = in_echa == 1L & is.na(pubchem_cid) & !is.na(echa_substance_name))
echa_name2 <- unique(echa_name2$echa_substance_name)

echa_name2_cid <- pbapply::pblapply(
  echa_name2,
  FUN = function(x) {
    Sys.sleep(0.2)
    res <- pcapi::post_to_cid(x, format = "Name")
    res
  }
)

echa_name2_df <- data.frame(
  "name" = echa_name2,
  "pubchem_cid" = sapply(echa_name2_cid, \(x) x[[1]])
) # none!

## FCC

fcc_name2 <- subset(full_data_new, subset = in_fcc == 1L & is.na(pubchem_cid) & !is.na(fcc_name))
fcc_name2 <- unique(fcc_name2$fcc_name)

fcc_name2_cid <- pbapply::pblapply(
  fcc_name2,
  FUN = function(x) {
    Sys.sleep(0.2)
    res <- pcapi::post_to_cid(x, format = "Name")
    res
  }
)

fcc_name2_df <- data.frame(
  "name" = fcc_name2,
  "pubchem_cid" = sapply(fcc_name2_cid, \(x) x[[1]])
)

fcc_name2_prop <- pcapi::post_to_property(
  unique(na.omit(fcc_name2_df$pubchem_cid)),
  property = c("MolecularFormula", "MolecularWeight", "CanonicalSMILES", 
               "IsomericSMILES", "InChI", "InChIKey", "IUPACName", "Title", 
               "XLogP", "ExactMass", "MonoisotopicMass", "TPSA", "Complexity", 
               "Charge")
)
colnames(fcc_name2_prop) <- c("pubchem_cid", "molecular_formula", "molecular_weight", 
                                      "canonical_smiles", "isomeric_smiles", "inchi", 
                                      "inchikey", "iupac_name", "xlogp", "exact_mass", 
                                      "monoisotopic_mass", "tpsa", "complexity", "charge", "title")

fcc_name2_df <- 
  merge(
    fcc_name2_df,
    fcc_name2_prop,
    by.x = "pubchem_cid",
    by.y = "pubchem_cid",
    all.x = TRUE
  )

full_data_new_fcc <- subset(full_data_new, subset = in_fcc == 1L)
full_data_new_fcc <- transform(
  full_data_new_fcc,
  "identified_by" = ifelse(fcc_name %in% fcc_name2_df$name, "name", identified_by)
) |> 
  merge(
    fcc_name2_df,
    by.x = "fcc_name",
    by.y = "name",
    all.x = TRUE, 
  ) |> 
  transform(
    "pubchem_cid" = ifelse(is.na(pubchem_cid.x), pubchem_cid.y, pubchem_cid.x),
    "pubchem_molecular_formula" = ifelse(is.na(pubchem_molecular_formula), molecular_formula, pubchem_molecular_formula), 
    "pubchem_molecular_weight" = ifelse(is.na(pubchem_molecular_weight), molecular_weight, pubchem_molecular_weight), 
    "pubchem_canonical_smiles" = ifelse(is.na(pubchem_canonical_smiles), canonical_smiles, pubchem_canonical_smiles), 
    "pubchem_isomeric_smiles" = ifelse(is.na(pubchem_isomeric_smiles), isomeric_smiles, pubchem_isomeric_smiles), 
    "pubchem_inchi" = ifelse(is.na(pubchem_inchi), inchi, pubchem_inchi), 
    "pubchem_inchikey" = ifelse(is.na(pubchem_inchikey), inchikey, pubchem_inchikey), 
    "pubchem_iupac_name" = ifelse(is.na(pubchem_iupac_name), iupac_name, pubchem_iupac_name), 
    "pubchem_xlogp" = ifelse(is.na(pubchem_xlogp), xlogp, pubchem_xlogp), 
    "pubchem_exact_mass" = ifelse(is.na(pubchem_exact_mass), exact_mass, pubchem_exact_mass), 
    "pubchem_monoisotopic_mass" = ifelse(is.na(pubchem_monoisotopic_mass), monoisotopic_mass, pubchem_monoisotopic_mass), 
    "pubchem_tpsa" = ifelse(is.na(pubchem_tpsa), tpsa, pubchem_tpsa), 
    "pubchem_complexity" = ifelse(is.na(pubchem_complexity), complexity, pubchem_complexity), 
    "pubchem_charge" = ifelse(is.na(pubchem_charge), charge, pubchem_charge), 
    "pubchem_title" = ifelse(is.na(pubchem_title), title, pubchem_title)
  ) |> 
  transform("pubchem_cid.x" = pubchem_cid)

full_data_new_fcc <- subset(full_data_new_fcc, select = colnames(full_data))
full_data_new_fcc <- full_data_new_fcc[, colnames(full_data)]

full_data_new <- rbind(
  subset(full_data_new, subset = in_fcc == 0L),
  full_data_new_fcc,
  make.row.names = FALSE
)

## FCCmigex

fccmigex_name2 <- subset(full_data_new, subset = in_fccmigex == 1L & is.na(pubchem_cid) & !is.na(fccmigex_fcc_main))
fccmigex_name2 <- unique(fccmigex_name2$fccmigex_fcc_main)

fccmigex_name2_cid <- pbapply::pblapply(
  fccmigex_name2,
  FUN = function(x) {
    Sys.sleep(0.2)
    res <- pcapi::post_to_cid(x, format = "Name")
    res
  }
)

fccmigex_name2_df <- data.frame(
  "name" = fccmigex_name2,
  "pubchem_cid" = sapply(fccmigex_name2_cid, \(x) x[[1]])
)

skimr::skim(fccmigex_name2_df)

fccmigex_name2_prop <- pcapi::post_to_property(
  unique(na.omit(fccmigex_name2_df$pubchem_cid)),
  property = c("MolecularFormula", "MolecularWeight", "CanonicalSMILES", 
               "IsomericSMILES", "InChI", "InChIKey", "IUPACName", "Title", 
               "XLogP", "ExactMass", "MonoisotopicMass", "TPSA", "Complexity", 
               "Charge")
)
colnames(fccmigex_name2_prop) <- c("pubchem_cid", "molecular_formula", "molecular_weight", 
                              "canonical_smiles", "isomeric_smiles", "inchi", 
                              "inchikey", "iupac_name", "xlogp", "exact_mass", 
                              "monoisotopic_mass", "tpsa", "complexity", "charge", "title")

fccmigex_name2_df <- 
  merge(
    fccmigex_name2_df,
    fccmigex_name2_prop,
    by.x = "pubchem_cid",
    by.y = "pubchem_cid",
    all.x = TRUE
  )

full_data_new_fccmigex <- subset(full_data_new, subset = in_fccmigex == 1L)
full_data_new_fccmigex <- transform(
  full_data_new_fccmigex,
  "identified_by" = ifelse(fccmigex_fcc_main %in% fccmigex_name2_df$name, "name", identified_by)
) |> 
  merge(
    fccmigex_name2_df,
    by.x = "fcc_name",
    by.y = "name",
    all.x = TRUE, 
  ) |> 
  transform(
    "pubchem_cid" = ifelse(is.na(pubchem_cid.x), pubchem_cid.y, pubchem_cid.x),
    "pubchem_molecular_formula" = ifelse(is.na(pubchem_molecular_formula), molecular_formula, pubchem_molecular_formula), 
    "pubchem_molecular_weight" = ifelse(is.na(pubchem_molecular_weight), molecular_weight, pubchem_molecular_weight), 
    "pubchem_canonical_smiles" = ifelse(is.na(pubchem_canonical_smiles), canonical_smiles, pubchem_canonical_smiles), 
    "pubchem_isomeric_smiles" = ifelse(is.na(pubchem_isomeric_smiles), isomeric_smiles, pubchem_isomeric_smiles), 
    "pubchem_inchi" = ifelse(is.na(pubchem_inchi), inchi, pubchem_inchi), 
    "pubchem_inchikey" = ifelse(is.na(pubchem_inchikey), inchikey, pubchem_inchikey), 
    "pubchem_iupac_name" = ifelse(is.na(pubchem_iupac_name), iupac_name, pubchem_iupac_name), 
    "pubchem_xlogp" = ifelse(is.na(pubchem_xlogp), xlogp, pubchem_xlogp), 
    "pubchem_exact_mass" = ifelse(is.na(pubchem_exact_mass), exact_mass, pubchem_exact_mass), 
    "pubchem_monoisotopic_mass" = ifelse(is.na(pubchem_monoisotopic_mass), monoisotopic_mass, pubchem_monoisotopic_mass), 
    "pubchem_tpsa" = ifelse(is.na(pubchem_tpsa), tpsa, pubchem_tpsa), 
    "pubchem_complexity" = ifelse(is.na(pubchem_complexity), complexity, pubchem_complexity), 
    "pubchem_charge" = ifelse(is.na(pubchem_charge), charge, pubchem_charge), 
    "pubchem_title" = ifelse(is.na(pubchem_title), title, pubchem_title)
  ) |> 
  transform("pubchem_cid.x" = pubchem_cid)

full_data_new_fccmigex <- subset(full_data_new_fccmigex, select = colnames(full_data))
full_data_new_fccmigex <- full_data_new_fccmigex[, colnames(full_data)]

full_data_new <- rbind(
  subset(full_data_new, subset = in_fccmigex == 0L),
  full_data_new_fccmigex,
  make.row.names = FALSE
)

## PlasticMAP

pmap_name2 <- subset(full_data_new, subset = in_plasticmap == 1L & is.na(pubchem_cid) & !is.na(plasticmap_name))
pmap_name2 <- unique(pmap_name2$plasticmap_name)

pmap_name2_cid <- pbapply::pblapply(
  pmap_name2,
  FUN = function(x) {
    Sys.sleep(0.2)
    res <- pcapi::post_to_cid(x, format = "Name")
    res
  }
)

pmap_name2_df <- data.frame(
  "name" = pmap_name2,
  "pubchem_cid" = sapply(pmap_name2_cid, \(x) x[[1]])
)

skimr::skim(pmap_name2_df)

pmap_name2_prop <- pcapi::post_to_property(
  unique(na.omit(pmap_name2_df$pubchem_cid)),
  property = c("MolecularFormula", "MolecularWeight", "CanonicalSMILES", 
               "IsomericSMILES", "InChI", "InChIKey", "IUPACName", "Title", 
               "XLogP", "ExactMass", "MonoisotopicMass", "TPSA", "Complexity", 
               "Charge")
)
colnames(pmap_name2_prop) <- c("pubchem_cid", "molecular_formula", "molecular_weight", 
                                   "canonical_smiles", "isomeric_smiles", "inchi", 
                                   "inchikey", "iupac_name", "xlogp", "exact_mass", 
                                   "monoisotopic_mass", "tpsa", "complexity", "charge", "title")

pmap_name2_df <- 
  merge(
    pmap_name2_df,
    pmap_name2_prop,
    by.x = "pubchem_cid",
    by.y = "pubchem_cid",
    all.x = TRUE
  )

full_data_new_pmap <- subset(full_data_new, subset = in_plasticmap == 1L)
full_data_new_pmap <- transform(
  full_data_new_pmap,
  "identified_by" = ifelse(plasticmap_name %in% pmap_name2_df$name, "name", identified_by)
) |> 
  merge(
    pmap_name2_df,
    by.x = "plasticmap_name",
    by.y = "name",
    all.x = TRUE, 
  ) |> 
  transform(
    "pubchem_cid" = ifelse(is.na(pubchem_cid.x), pubchem_cid.y, pubchem_cid.x),
    "pubchem_molecular_formula" = ifelse(is.na(pubchem_molecular_formula), molecular_formula, pubchem_molecular_formula), 
    "pubchem_molecular_weight" = ifelse(is.na(pubchem_molecular_weight), molecular_weight, pubchem_molecular_weight), 
    "pubchem_canonical_smiles" = ifelse(is.na(pubchem_canonical_smiles), canonical_smiles, pubchem_canonical_smiles), 
    "pubchem_isomeric_smiles" = ifelse(is.na(pubchem_isomeric_smiles), isomeric_smiles, pubchem_isomeric_smiles), 
    "pubchem_inchi" = ifelse(is.na(pubchem_inchi), inchi, pubchem_inchi), 
    "pubchem_inchikey" = ifelse(is.na(pubchem_inchikey), inchikey, pubchem_inchikey), 
    "pubchem_iupac_name" = ifelse(is.na(pubchem_iupac_name), iupac_name, pubchem_iupac_name), 
    "pubchem_xlogp" = ifelse(is.na(pubchem_xlogp), xlogp, pubchem_xlogp), 
    "pubchem_exact_mass" = ifelse(is.na(pubchem_exact_mass), exact_mass, pubchem_exact_mass), 
    "pubchem_monoisotopic_mass" = ifelse(is.na(pubchem_monoisotopic_mass), monoisotopic_mass, pubchem_monoisotopic_mass), 
    "pubchem_tpsa" = ifelse(is.na(pubchem_tpsa), tpsa, pubchem_tpsa), 
    "pubchem_complexity" = ifelse(is.na(pubchem_complexity), complexity, pubchem_complexity), 
    "pubchem_charge" = ifelse(is.na(pubchem_charge), charge, pubchem_charge), 
    "pubchem_title" = ifelse(is.na(pubchem_title), title, pubchem_title)
  ) |> 
  transform("pubchem_cid.x" = pubchem_cid)

full_data_new_pmap <- subset(full_data_new_pmap, select = colnames(full_data))
full_data_new_pmap <- full_data_new_pmap[, colnames(full_data)]

full_data_new <- rbind(
  subset(full_data_new, subset = in_plasticmap == 0L),
  full_data_new_pmap,
  make.row.names = FALSE
)

## PlasticMAP2

pmap2_name2 <- subset(full_data_new, subset = in_plasticmap2 == 1L & is.na(pubchem_cid) & !is.na(plasticmap2_name))
pmap2_name2 <- unique(pmap2_name2$plasticmap2_name)

pmap2_name2_cid <- pbapply::pblapply(
  pmap2_name2,
  FUN = function(x) {
    Sys.sleep(0.2)
    res <- pcapi::post_to_cid(x, format = "Name")
    res
  }
)

pmap2_name2_df <- data.frame(
  "name" = pmap2_name2,
  "pubchem_cid" = sapply(pmap2_name2_cid, \(x) x[[1]])
)

skimr::skim(pmap2_name2_df)

pmap2_name2_prop <- pcapi::post_to_property(
  unique(na.omit(pmap2_name2_df$pubchem_cid)),
  property = c("MolecularFormula", "MolecularWeight", "CanonicalSMILES", 
               "IsomericSMILES", "InChI", "InChIKey", "IUPACName", "Title", 
               "XLogP", "ExactMass", "MonoisotopicMass", "TPSA", "Complexity", 
               "Charge")
)
colnames(pmap2_name2_prop) <- c("pubchem_cid", "molecular_formula", "molecular_weight", 
                               "canonical_smiles", "isomeric_smiles", "inchi", 
                               "inchikey", "iupac_name", "xlogp", "exact_mass", 
                               "monoisotopic_mass", "tpsa", "complexity", "charge", "title")

pmap2_name2_df <- 
  merge(
    pmap2_name2_df,
    pmap2_name2_prop,
    by.x = "pubchem_cid",
    by.y = "pubchem_cid",
    all.x = TRUE
  )

full_data_new_pmap2 <- subset(full_data_new, subset = in_plasticmap2 == 1L)
full_data_new_pmap2 <- transform(
  full_data_new_pmap2,
  "identified_by" = ifelse(plasticmap2_name %in% pmap2_name2_df$name, "name", identified_by)
) |> 
  merge(
    pmap2_name2_df,
    by.x = "plasticmap2_name",
    by.y = "name",
    all.x = TRUE, 
  ) |> 
  transform(
    "pubchem_cid" = ifelse(is.na(pubchem_cid.x), pubchem_cid.y, pubchem_cid.x),
    "pubchem_molecular_formula" = ifelse(is.na(pubchem_molecular_formula), molecular_formula, pubchem_molecular_formula), 
    "pubchem_molecular_weight" = ifelse(is.na(pubchem_molecular_weight), molecular_weight, pubchem_molecular_weight), 
    "pubchem_canonical_smiles" = ifelse(is.na(pubchem_canonical_smiles), canonical_smiles, pubchem_canonical_smiles), 
    "pubchem_isomeric_smiles" = ifelse(is.na(pubchem_isomeric_smiles), isomeric_smiles, pubchem_isomeric_smiles), 
    "pubchem_inchi" = ifelse(is.na(pubchem_inchi), inchi, pubchem_inchi), 
    "pubchem_inchikey" = ifelse(is.na(pubchem_inchikey), inchikey, pubchem_inchikey), 
    "pubchem_iupac_name" = ifelse(is.na(pubchem_iupac_name), iupac_name, pubchem_iupac_name), 
    "pubchem_xlogp" = ifelse(is.na(pubchem_xlogp), xlogp, pubchem_xlogp), 
    "pubchem_exact_mass" = ifelse(is.na(pubchem_exact_mass), exact_mass, pubchem_exact_mass), 
    "pubchem_monoisotopic_mass" = ifelse(is.na(pubchem_monoisotopic_mass), monoisotopic_mass, pubchem_monoisotopic_mass), 
    "pubchem_tpsa" = ifelse(is.na(pubchem_tpsa), tpsa, pubchem_tpsa), 
    "pubchem_complexity" = ifelse(is.na(pubchem_complexity), complexity, pubchem_complexity), 
    "pubchem_charge" = ifelse(is.na(pubchem_charge), charge, pubchem_charge), 
    "pubchem_title" = ifelse(is.na(pubchem_title), title, pubchem_title)
  ) |> 
  transform("pubchem_cid.x" = pubchem_cid)

full_data_new_pmap2 <- subset(full_data_new_pmap2, select = colnames(full_data))
full_data_new_pmap2 <- full_data_new_pmap2[, colnames(full_data)]

full_data_new <- rbind(
  subset(full_data_new, subset = in_plasticmap2 == 0L),
  full_data_new_pmap2,
  make.row.names = FALSE
)

##

full_data_new_clean <- full_data_new[order(full_data_new$plastchem_id), colnames(full_data)]
row.names(full_data_new_clean) <- NULL
full_data_new_clean <- transform(
  full_data_new_clean,
  "identified_by" = ifelse(is.na(pubchem_cid), NA_character_, identified_by)
)

skimr::skim(full_data_new_clean)

saveRDS(full_data_new_clean, "data/plastchem_db_v0.5.rds")
writexl::write_xlsx(full_data_new_clean, "data/plastchem_db_v0.5.xlsx")
write.csv(full_data_new_clean, file = "data/plastchem_db_v0.5.csv", na = "", row.names = FALSE)
