# Aurisano et al. 

aurisano_raw <- openxlsx::read.xlsx(
  xlsxFile = "data/input/raw/Aurisano/1-s2.0-S2452223621000699-mmc2.xlsx", 
  cols = c(1:6),
  na.strings = c("", "-")
  )
colnames(aurisano_raw) <- c(
  "cas_number", "substance_name", "primary_function", "other_functions", 
  "polymer_type", "concentration_range_pct"
)
aurisano_raw <- cbind("aurisano_id" = 1:nrow(aurisano_raw), "cas_number" = aurisano_raw$cas_number, "ec" = NA_character_, aurisano_raw[, 2:6])
aurisano_raw <- transform(
  aurisano_raw,
  "ec" = ifelse(
    test = grepl("EC", cas_number),
    yes = trimws(gsub("EC:", "", cas_number)),
    no = ec
  )
)
skimr::skim(aurisano_raw)

# subset(aurisano_raw, grepl("[[:alpha:]]", cas_number)) |> View()

sapply(aurisano_raw, \(x) sum(grepl(";", x))) # cas_number, substance_name, other_functions
sapply(aurisano_raw, \(x) sum(grepl("\\|", x))) # substance_name, other_functions, polymer_type
sapply(aurisano_raw, \(x) sum(grepl(",", x))) # substance_name, other_functions

# subset(aurisano_raw, subset = grepl(";", cas_number)) |> View()

aurisano_raw <- transform(
  aurisano_raw,
  "polymer_type" = ifelse(
    test = concentration_range_pct == "PP", 
    yes = "PP", 
    no = polymer_type
  ),
  "concentration_range_pct" = gsub(pattern = "\\(|%|\\)|Pigment", replacement = "", concentration_range_pct)
)
aurisano_raw <- transform(
  aurisano_raw,
  "concentration_range_pct" = ifelse(
    test = concentration_range_pct == "PP", 
    yes = NA_character_, 
    no = concentration_range_pct
  )
)

aurisano_split <- split(aurisano_raw, f = ~ aurisano_id)

aurisano <- do.call(
  what = "rbind",
  args = c(
    pbapply::pblapply(
      aurisano_split,
      FUN = function(x) {
        ofcts <- paste(na.omit(trimws(unlist(strsplit(x$other_functions, split = ",|;|\\|"), use.names = FALSE))), collapse = ";")
        if(nchar(ofcts) == 0L) {
          ofcts <- NA_character_
        }
        ptp <- paste(na.omit(trimws(unlist(strsplit(x$polymer_type, split = "\\|"), use.names = FALSE))), collapse = ";")
        if(nchar(ptp) == 0L) {
          ptp <- NA_character_
        }
        crp <- paste(na.omit(as.double(trimws(unlist(strsplit(x$concentration_range_pct, split = "-|–"), use.names = FALSE)))), collapse = "-")
        if(nchar(crp) == 0L) {
          crp <- NA_character_
        }
        if(is.na(x$substance_name)) {
          sn <- NA_character_
        } else {
          sn <- paste(trimws(unlist(strsplit(x$substance_name, split = ";"))), collapse = ";")
        }
        res <- data.frame(
          "aurisano_id" = x$aurisano_id,
          "cas_number" = trimws(unlist(strsplit(x$cas_number, split = ";"))),
          "ec" = x$ec,
          "substance_name" = sn,
          "primary_function" = x$primary_function,
          "other_functions" = ofcts,
          "polymer_type" = ptp,
          "concentration_range_pct" = crp
        )
        res
      }
    ),
    make.row.names = FALSE
  )
)

skimr::skim(aurisano)

aurisano <- transform(
  aurisano,
  "cas_number" = ifelse(
    test = cleanventory:::.check_cas(cas_number),
    yes = cas_number,
    no = NA_character_
  )
)

# 6'031 entries
#   307 entries with invalid CAS Registry Numbers (5%)

skimr::skim(aurisano)

rm(aurisano_raw, aurisano_split)

saveRDS(aurisano, "data/input/clean/Aurisano/aurisano.rds")

aurisano_cas <- subset(aurisano, subset = !is.na(cas_number))
aurisano_no_cas <- subset(aurisano, subset = is.na(cas_number))
aurisano_cas_split <- split(aurisano_cas, f = ~ cas_number)
aurisano_no_cas_split <- split(aurisano_no_cas, f = ~ tolower(substance_name))

aurisano_cas_clean <- do.call(
  what = "rbind",
  args = c(
    pbapply::pblapply(
      aurisano_cas_split,
      FUN = function(x) {
        if(sum(is.na(x$ec)) == nrow(x)) {
          n_ec <- NA_character_
        } else {
          n_ec <- paste(unique(na.omit(x$ec)), collapse = ";")
        }
        if(sum(is.na(x$substance_name)) == nrow(x)) {
          n_substance_name <- NA_character_
        } else {
          n_substance_name <- paste(unique(na.omit(x$substance_name)), collapse = ";")
        }
        if(sum(is.na(x$primary_function)) == nrow(x)) {
          n_primary_function <- NA_character_
        } else {
          n_primary_function <- paste(unique(na.omit(x$primary_function)), collapse = ";")
        }
        if(sum(is.na(x$other_functions)) == nrow(x)) {
          n_other_functions <- NA_character_
        } else {
          n_other_functions <- paste(unique(na.omit(x$other_functions)), collapse = ";")
        }
        if(sum(is.na(x$polymer_type)) == nrow(x)) {
          n_polymer_type <- NA_character_
        } else {
          n_polymer_type <- paste(unique(na.omit(x$polymer_type)), collapse = ";")
        }
        if(sum(is.na(x$concentration_range_pct)) == nrow(x)) {
          n_concentration_range_pct <- NA_character_
        } else {
          n_concentration_range_pct <- paste(unique(na.omit(x$concentration_range_pct)), collapse = ";")
        }
        res <- data.frame(
          "aurisano_id" = paste(x$aurisano_id, collapse = ";"),
          "cas_number" = unique(x$cas_number),
          "ec" = n_ec,
          "substance_name" = n_substance_name,
          "primary_function" = n_primary_function,
          "other_functions" = n_other_functions,
          "polymer_type" = n_polymer_type,
          "concentration_range_pct" = n_concentration_range_pct,
          stringsAsFactors = FALSE
        )
        res
      }
    ),
    make.row.names = FALSE
  )
)

aurisano_no_cas_clean <- do.call(
  what = "rbind",
  args = c(
    pbapply::pblapply(
      aurisano_no_cas_split,
      FUN = function(x) {
        if(sum(is.na(x$ec)) == nrow(x)) {
          n_ec <- NA_character_
        } else {
          n_ec <- paste(unique(na.omit(x$ec)), collapse = ";")
        }
        if(sum(is.na(x$substance_name)) == nrow(x)) {
          n_substance_name <- NA_character_
        } else {
          n_substance_name <- paste(unique(na.omit(x$substance_name)), collapse = ";")
        }
        if(sum(is.na(x$primary_function)) == nrow(x)) {
          n_primary_function <- NA_character_
        } else {
          n_primary_function <- paste(unique(na.omit(x$primary_function)), collapse = ";")
        }
        if(sum(is.na(x$other_functions)) == nrow(x)) {
          n_other_functions <- NA_character_
        } else {
          n_other_functions <- paste(unique(na.omit(x$other_functions)), collapse = ";")
        }
        if(sum(is.na(x$polymer_type)) == nrow(x)) {
          n_polymer_type <- NA_character_
        } else {
          n_polymer_type <- paste(unique(na.omit(x$polymer_type)), collapse = ";")
        }
        if(sum(is.na(x$concentration_range_pct)) == nrow(x)) {
          n_concentration_range_pct <- NA_character_
        } else {
          n_concentration_range_pct <- paste(unique(na.omit(x$concentration_range_pct)), collapse = ";")
        }
        res <- data.frame(
          "aurisano_id" = paste(x$aurisano_id, collapse = ";"),
          "cas_number" = NA_character_,
          "ec" = n_ec,
          "substance_name" = n_substance_name,
          "primary_function" = n_primary_function,
          "other_functions" = n_other_functions,
          "polymer_type" = n_polymer_type,
          "concentration_range_pct" = n_concentration_range_pct,
          stringsAsFactors = FALSE
        )
        res
      }
    ),
    make.row.names = FALSE
  )
)

aurisano_clean <- rbind(aurisano_cas_clean, aurisano_no_cas_clean)
row.names(aurisano_clean) <- NULL

skimr::skim(aurisano_clean)

rm(aurisano_cas, aurisano_cas_split, aurisano_cas_clean, aurisano_no_cas, aurisano_no_cas_split, aurisano_no_cas_clean)

saveRDS(aurisano_clean, "data/input/clean/Aurisano/aurisano_clean.rds")

rm(aurisano)

aurisano <- readRDS("data/input/clean/Aurisano/aurisano.rds")
write.csv(aurisano, "data/aurisano_original_w_index.csv", na = "", row.names = FALSE)

aurisano_clean <- readRDS("data/input/clean/Aurisano/aurisano_clean.rds")
write.csv(aurisano_clean, "data/aurisano_w_index.csv", na = "", row.names = FALSE)
