# ECHA db

echa_raw <- openxlsx::read.xlsx(
  xlsxFile = "data/input/raw/ECHA_db/fcm-and-articles-regulation--annex-i---authorised-substances-export.xlsx",
  sheet = "results",
  startRow = 5,
  cols = c(1:3, 8:9),
  na.strings = c("", " ", "-", "-")
)
colnames(echa_raw) <- c(
  "substance_name", "ec_no", "cas_no", "additive_or_ppa", "use_as_monomer_macromolecule"
)
echa_raw <- transform(
  echa_raw,
  "ec_no" = ifelse(test = nchar(ec_no) < 9, yes = NA_character_, no = ec_no),
  "cas_no" = ifelse(test = nchar(cas_no) < 7, yes = NA_character_, no = cas_no),
  "additive_or_ppa" = ifelse(
    test = grepl("yes", additive_or_ppa), 
    yes = 1L, 
    no = ifelse(
      test = grepl("no", additive_or_ppa),
      yes = 0L,
      no = NA_integer_
    )
  ),
  "use_as_monomer_macromolecule" = ifelse(
    test = grepl("yes", use_as_monomer_macromolecule), 
    yes = 1L, 
    no = ifelse(
      test = grepl("no", use_as_monomer_macromolecule),
      yes = 0L,
      no = NA_integer_
    )
  )
)

sapply(echa_raw, \(x) sum(grepl(";", x))) # substance_name, cas_no

echa_raw <- cbind("echa_id" = 1:nrow(echa_raw), echa_raw)

skimr::skim(echa_raw)

echa_split <- split(echa_raw, f = ~ echa_id)

echa <- do.call(
  what = "rbind",
  args = c(
    pbapply::pblapply(
      echa_split,
      FUN = function(x) {
        if(is.na(x$cas_no)) {
          n_cas_no <- NA_character_
        } else {
          n_cas_no <- trimws(unlist(strsplit(x$cas_no, split = ";")))
        }
        res <- data.frame(
          "echa_id" = x$echa_id,
          "substance_name" = trimws(unlist(strsplit(x$substance_name, split = ";"))),
          "ec_no" = x$ec_no,
          "cas_no" = n_cas_no,
          "additive_or_ppa" = x$additive_or_ppa,
          "use_as_monomer_macromolecule" = x$use_as_monomer_macromolecule
        )
        res
      }
    ),
    make.row.names = FALSE
  )
)

skimr::skim(echa) # 262 + 265 missing # 788

echa <- transform(
  echa,
  "cas_no" = ifelse(
    test = cleanventory:::.check_cas(cas_no),
    yes = cas_no,
    no = NA_character_
  )
)

skimr::skim(echa)

# 1'074 entries
#   262 entries with missing CAS Registry Numbers (24%)

rm(echa_raw, echa_split)

saveRDS(echa, "data/input/clean/ECHA_db/ECHA_db_results.rds")

echa_cas <- subset(echa, subset = !is.na(cas_no))
echa_no_cas <- subset(echa, subset = is.na(cas_no))

sapply(split(echa_cas, f = ~ cas_no), FUN = \(x) nrow(x)) |> max()

echa_cas_split <- split(echa_cas, f = ~ cas_no)
echa_no_cas_split <- split(echa_no_cas, f = ~ tolower(substance_name))

echa_cas_clean <- do.call(
  what = "rbind",
  args = c(
    pbapply::pblapply(
      echa_cas_split,
      FUN = function(x) {
        if(sum(is.na(x$ec_no)) == nrow(x)) {
          n_ec_no <- NA_character_
        } else {
          n_ec_no <- paste(unique(na.omit(x$ec_no)), collapse = ";")
        }
        if(sum(is.na(x$substance_name)) == nrow(x)) {
          n_substance_name <- NA_character_
        } else {
          n_substance_name <- paste(unique(na.omit(x$substance_name)), collapse = ";")
        }
        
        if(sum(is.na(x$additive_or_ppa)) == nrow(x)) {
          n_additive_or_ppa <- NA_integer_
        } else {
          n_additive_or_ppa <- max(x$additive_or_ppa)
        }
        if(sum(is.na(x$use_as_monomer_macromolecule)) == nrow(x)) {
          n_use_as_monomer_macromolecule <- NA_integer_
        } else {
          n_use_as_monomer_macromolecule <- max(x$use_as_monomer_macromolecule)
        }
        res <- data.frame(
          "echa_id" = paste(unique(x$echa_id), collapse = ";"),
          "substance_name" = n_substance_name,
          "ec_no" = n_ec_no,
          "cas_no" = unique(x$cas_no),
          "additive_or_ppa" = n_additive_or_ppa,
          "use_as_monomer_macromolecule" = n_use_as_monomer_macromolecule,
          stringsAsFactors = FALSE
        )
        res
      }
    ),
    make.row.names = FALSE
  )
)

echa_no_cas_clean <- do.call(
  what = "rbind",
  args = c(
    pbapply::pblapply(
      echa_no_cas_split,
      FUN = function(x) {
        if(sum(is.na(x$ec_no)) == nrow(x)) {
          n_ec_no <- NA_character_
        } else {
          n_ec_no <- paste(unique(na.omit(x$ec_no)), collapse = ";")
        }
        if(sum(is.na(x$substance_name)) == nrow(x)) {
          n_substance_name <- NA_character_
        } else {
          n_substance_name <- paste(unique(na.omit(x$substance_name)), collapse = ";")
        }
        
        if(sum(is.na(x$additive_or_ppa)) == nrow(x)) {
          n_additive_or_ppa <- NA_integer_
        } else {
          n_additive_or_ppa <- max(x$additive_or_ppa)
        }
        if(sum(is.na(x$use_as_monomer_macromolecule)) == nrow(x)) {
          n_use_as_monomer_macromolecule <- NA_integer_
        } else {
          n_use_as_monomer_macromolecule <- max(x$use_as_monomer_macromolecule)
        }
        res <- data.frame(
          "echa_id" = paste(unique(x$echa_id), collapse = ";"),
          "substance_name" = paste(unique(x$substance_name), collapse = ";"),
          "ec_no" = n_ec_no,
          "cas_no" = NA_character_,
          "additive_or_ppa" = n_additive_or_ppa,
          "use_as_monomer_macromolecule" = n_use_as_monomer_macromolecule,
          stringsAsFactors = FALSE
        )
        res
      }
    ),
    make.row.names = FALSE
  )
)

echa_clean <- rbind(echa_cas_clean, echa_no_cas_clean)
row.names(echa_clean) <- NULL

saveRDS(echa_clean, "data/input/clean/ECHA_db/echa_clean.rds")

rm(echa_cas, echa_cas_split, echa_cas_clean, echa_no_cas, echa_no_cas_split, echa_no_cas_clean)

rm(echa)

echa <- readRDS("data/input/clean/ECHA_db/ECHA_db_results.rds")
write.csv(echa, "data/echa_original_w_index.csv", na = "", row.names = FALSE)


echa_clean <- readRDS("data/input/clean/ECHA_db/echa_clean.rds")

write.csv(echa_clean, "data/echa_w_index.csv", na = "", row.names = FALSE)
