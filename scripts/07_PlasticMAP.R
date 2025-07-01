# PlasticMAP

pmap_s8_raw <- openxlsx::read.xlsx(
  xlsxFile = "data/input/raw/PlasticMAP/PlasticMAP_S1_210611.xlsx",
  sheet = "S8 - PlasticMAP",
  startRow = 6,
  cols = c(3:4, 12:18, 24:34), # 6, 9, 11:12, 23:26) + 1,
  na.strings = c(" ", "-")
)
colnames(pmap_s8_raw) <- c(
  "casrn", "name", "uvcb", "organic", "contains_metal", 
  "contains_silicon", "contains_phosphorus", "contains_sulfur", 
  "contains_halogen", "use_function", "polymer", "industrial_sector", 
  "industrial_sector_food_contact", "number_of_regions", "regions", 
  "tonnage_eu", "tonnage_nordic_countries_spin", "tonnage_usa", "tonnage_oecd", 
  "tonnage_total"
)
pmap_s8_raw <- cbind("pmap_s8_id" = 1:nrow(pmap_s8_raw), pmap_s8_raw)
pmap_s8_raw <- transform(
  pmap_s8_raw,
  "casrn" = trimws(gsub("'", "", casrn)),
  "uvcb" = as.integer(uvcb),
  "organic" = ifelse(
    test = organic == 0.5, 
    yes = NA_integer_, 
    no = as.integer(organic)
  ),
  "contains_metal" = as.integer(contains_metal),
  "contains_silicon" = as.integer(contains_silicon),
  "contains_phosphorus" = as.integer(contains_phosphorus),
  "contains_sulfur" = as.integer(contains_sulfur),
  "contains_halogen" = as.integer(contains_halogen),
  "industrial_sector_food_contact" = as.integer(industrial_sector_food_contact),
  "number_of_regions" = as.integer(number_of_regions),
  "tonnage_eu" = ifelse(
    test = tonnage_eu == -1, 
    yes = NA_integer_,
    no = as.integer(tonnage_eu)
  ), 
  "tonnage_nordic_countries_spin" = as.integer(tonnage_nordic_countries_spin),
  "tonnage_usa" = ifelse(
    test = tonnage_usa == -1,
    yes = NA_integer_,
    no = as.integer(tonnage_usa)
  ), 
  "tonnage_oecd" = as.integer(tonnage_oecd),
  "tonnage_total" = ifelse(
    test = tonnage_total == -1,
    yes = NA_integer_,
    no = as.integer(tonnage_total)
  ) 
)

skimr::skim(pmap_s8_raw)

sapply(pmap_s8_raw, \(x) sum(grepl(",", x)))

pmap_s8_split <- split(pmap_s8_raw, f = ~ pmap_s8_id)

pmap_s8 <- do.call(
  what = "rbind",
  args = c(
    pbapply::pblapply(
      pmap_s8_split,
      FUN = function(x) {
        res <- transform(
          x,
          "use_function" = gsub(pattern = ", ", replacement = ";", use_function),
          "polymer" = gsub(pattern = ", ", replacement = ";", polymer),
          "industrial_sector" = gsub(pattern = ", ", replacement = ";", industrial_sector),
          "regions" = gsub(pattern = ", ", replacement = ";", regions)
          
        )
        res
      }
    ),
    make.row.names = FALSE
  )
)

skimr::skim(pmap_s8) # 10547

pmap_s8 <- transform(
  pmap_s8,
  "casrn" = ifelse(
    test = cleanventory:::.check_cas(casrn),
    yes = casrn,
    no = NA_character_
  )
)

# sapply(pmap_s8, \(x) sum(grepl(";", x)))
# subset(pmap_s8, grepl(";", name))

skimr::skim(pmap_s8)

# No CAS Registry Numbers were removed

rm(pmap_s8_raw, pmap_s8_split)

saveRDS(pmap_s8, "data/input/clean/PlasticMAP/PlasticMAP.rds")

# CPPdb List B

pmap_s9_raw <- openxlsx::read.xlsx(
  xlsxFile = "data/input/raw/PlasticMAP/PlasticMAP_S1_210611.xlsx",
  sheet = "S9 - SopC",
  startRow = 8,
  cols = c(3:4, 12:18, 43:79),
  na.strings = c(" ", "-")
)

colnames(pmap_s9_raw) <- c(
  "casrn", "name", "uvcb", "organic", "contains_metal", 
  "contains_silicon", "contains_phosphorus", "contains_sulfur", 
  "contains_halogen", "regulated", "relevant_regulations", "montreal", "us_ozone_depleting_substances",
  "eu_controlled_substances_montreal", "stockholm", "eu_pop_directive", "rotterdam",
  "eu_priority_informed_content", "reach_restriction", "reach_authorisation", 
  "reach_svhc", "rohs_directive", "toys_directive", "california_proposition_65",
  "cscl_class_i_restriction", "cscl_class_ii_authorization", "isha_restriction",
  "isha_authorization", "kr_reach_restriction", "kr_reach_authorisation", 
  "relevant_food_contact_positive_lists", "food_contact_positive_list",
  "eu_food_contact_materials_list", "usa_generally_recognized_as_safe", 
  "japan_food_contact_materials_list", 
  "use_function", "polymer", "industrial_sector", 
  "industrial_sector_food_contact", "number_of_regions", "regions", 
  "tonnage_eu", "tonnage_nordic_countries_spin", "tonnage_usa", "tonnage_oecd", 
  "tonnage_total"
)

pmap_s9_raw <- cbind("pmap_s9_id" = 1:nrow(pmap_s9_raw), pmap_s9_raw)
pmap_s9_raw <- transform(
  pmap_s9_raw,
  "casrn" = trimws(gsub("'", "", casrn))
)

skimr::skim(pmap_s9_raw)

sapply(pmap_s9_raw, \(x) sum(grepl(",", x))) # relevant_regulations, relevant_food_contact_positive_lists, use_function, polymer, industrial_sector, regions
sapply(pmap_s9_raw, \(x) sum(grepl(";", x)))
sapply(pmap_s9_raw, \(x) sum(grepl("\n", x)))
sapply(pmap_s9_raw, \(x) sum(grepl("0.5", x)))

pmap_s9_split <- split(pmap_s9_raw, f = ~ pmap_s9_id)

pmap_s9 <- do.call(
  what = "rbind",
  args = c(
    pbapply::pblapply(
      pmap_s9_split,
      FUN = function(x) {
        res <- x
        res <- transform(
          res,
          "uvcb" = as.integer(uvcb),
          "organic" = ifelse(organic == 0.5, NA_integer_, as.integer(organic)),
          "contains_metal" = as.integer(contains_metal),
          "contains_silicon" = as.integer(contains_silicon),
          "contains_phosphorus" = as.integer(contains_phosphorus),
          "contains_sulfur" = as.integer(contains_sulfur),
          "contains_halogen" = as.integer(contains_halogen),
          "regulated" = as.integer(regulated),
          "relevant_regulations" = ifelse(is.na(relevant_regulations), NA_character_, paste(trimws(unlist(strsplit(relevant_regulations, split = ","))), collapse = ";")),
          "montreal" = as.integer(montreal),
          "us_ozone_depleting_substances" = as.integer(us_ozone_depleting_substances),
          "eu_controlled_substances_montreal" = as.integer(eu_controlled_substances_montreal),
          "stockholm" = as.integer(stockholm),
          "eu_pop_directive" = as.integer(eu_pop_directive),
          "rotterdam" = as.integer(rotterdam),
          "eu_priority_informed_content" = as.integer(eu_priority_informed_content), 
          "reach_restriction" = as.integer(reach_restriction), 
          "reach_authorisation" = as.integer(reach_authorisation), 
          "reach_svhc" = as.integer(reach_svhc), 
          "rohs_directive" = as.integer(rohs_directive), 
          "toys_directive" = as.integer(toys_directive), 
          "california_proposition_65" = as.integer(california_proposition_65),
          "cscl_class_i_restriction" = as.integer(cscl_class_i_restriction), 
          "cscl_class_ii_authorization" = as.integer(cscl_class_ii_authorization), 
          "isha_restriction" = as.integer(isha_restriction),
          "isha_authorization" = as.integer(isha_authorization), 
          "kr_reach_restriction" = as.integer(kr_reach_restriction), 
          "kr_reach_authorisation" = as.integer(kr_reach_authorisation),
          "relevant_food_contact_positive_lists" = ifelse(is.na(relevant_food_contact_positive_lists), NA_character_, paste(trimws(unlist(strsplit(relevant_food_contact_positive_lists, split = ","))), collapse = ";")),
          "food_contact_positive_list" = as.integer(food_contact_positive_list),
          "eu_food_contact_materials_list" = as.integer(eu_food_contact_materials_list), 
          "usa_generally_recognized_as_safe" = as.integer(usa_generally_recognized_as_safe), 
          "japan_food_contact_materials_list" = as.integer(japan_food_contact_materials_list), 
          "use_function" = ifelse(is.na(use_function), NA_character_, paste(trimws(unlist(strsplit(use_function, split = ","))), collapse = ";")), 
          "polymer" = ifelse(is.na(polymer), NA_character_, paste(trimws(unlist(strsplit(polymer, split = ","))), collapse = ";")), 
          "industrial_sector" = ifelse(is.na(industrial_sector), NA_character_, paste(trimws(unlist(strsplit(industrial_sector, split = ","))), collapse = ";")), 
          "industrial_sector_food_contact" = as.integer(industrial_sector_food_contact), 
          "number_of_regions" = as.integer(number_of_regions), 
          "regions" = ifelse(is.na(regions), NA_character_, paste(trimws(unlist(strsplit(regions, split = ","))), collapse = ";")), 
          "tonnage_eu" = ifelse(tonnage_eu == -1, NA_integer_, as.integer(tonnage_eu)), 
          "tonnage_nordic_countries_spin" = ifelse(tonnage_nordic_countries_spin == 0.5, NA_integer_, as.integer(tonnage_nordic_countries_spin)), 
          "tonnage_usa" = ifelse(tonnage_usa == -1, NA_integer_, as.integer(tonnage_usa)), 
          "tonnage_oecd" = ifelse(tonnage_oecd == -1, NA_integer_, as.integer(tonnage_oecd)), 
          "tonnage_total" = ifelse(tonnage_total == -1, NA_integer_, as.integer(tonnage_total))
        )
        res
      }
    ),
    make.row.names = FALSE
  )
)

skimr::skim(pmap_s9) # 2486

pmap_s9 <- transform(
  pmap_s9,
  "casrn" = ifelse(
    test = cleanventory:::.check_cas(casrn),
    yes = casrn,
    no = NA_character_
  )
)
# pmap_s9[, c(8:26, 28, 30:48, 50:53)] <- as.logical(pmap_s9[, c(8:26, 28, 30:48, 50:53)])

skimr::skim(pmap_s9) # 2441

rm(pmap_s9_raw, pmap_s9_split)

saveRDS(pmap_s9, "data/input/clean/PlasticMAP/SopC.rds")

##

# pmap_s9_cas <- subset(pmap_s9, subset = !is.na(casrn))
# pmap_s9_no_cas <- subset(pmap_s9, subset = is.na(casrn))
# 
# rm(pmap_s9_cas, pmap_s9_no_cas)

##

sum(pmap_s9$casrn %in% pmap_s8$casrn)
sum(pmap_s9$name %in% pmap_s8$name)

pmap_s9_sub <- subset(pmap_s9, select = c("name", colnames(pmap_s9)[!(colnames(pmap_s9) %in% colnames(pmap_s8))]))
pmap_s9_sub <- subset(pmap_s9_sub, select = -c(pmap_s9_id))

pmap <- merge(
  pmap_s8,
  pmap_s9_sub,
  by.x = "name",
  by.y = "name",
  all.x = TRUE
)
# pmap <- pmap[order(pmap$pmap_s8_id), ]
row.names(pmap) <- NULL
colnames(pmap)[2] <- "pmap_id"

rm(pmap_s9_sub)

saveRDS(pmap, "data/input/clean/PlasticMAP/pmap.rds")

rm(pmap_s8, pmap_s9)

pmap <- readRDS("data/input/clean/PlasticMAP/pmap.rds")

write.csv(pmap, "data/pmap_w_index.csv", na = "", row.names = FALSE)
