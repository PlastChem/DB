# FCCdb

fcc <- openxlsx::read.xlsx(
  xlsxFile = "data/input/raw/FCCdb/FCCdb_201130_v5_Zenodo.xlsx",
  sheet = "FCCdb_FINAL_LIST",
  cols = c(2:3, 13:21),
  na.strings = c(" ", "-", "0", "0;")
) # 12'285
fcc <- cbind("fcc_id" = 1:nrow(fcc), fcc)
fcc <- subset(
  fcc,
  subset = S07 != "0" | S08 != "0" | S09 != "0" | S10 != "0" | S11 != "0" | 
    S12 != "0" | S13 != "0" | S14 != "0" | S15 != "0",
  select = -c(S07, S08, S09, S10, S11, S12, S13, S14, S15)
) # 3'543

colnames(fcc) <- c("fcc_id", "cas_number", "name")
fcc <- transform(
  fcc,
  "cas_number" = trimws(cas_number)
)

skimr::skim(fcc) # 3543

fcc <- transform(
  fcc,
  "cas_number" = ifelse(
    test = cleanventory:::.check_cas(cas_number),
    yes = cas_number,
    no = NA_character_
  )
)

skimr::skim(fcc)

# 3'543 entries
#     8 entries with invalid CAS Registry Numbers (<1%)

saveRDS(fcc, "data/input/clean/FCCdb/FCCdb_v5.rds")

fcc <- readRDS("data/input/clean/FCCdb/FCCdb_v5.rds")

write.csv(fcc, "data/fcc_w_index.csv", na = "", row.names = FALSE)
