# CPPdb List A

cpp_a_raw <- openxlsx::read.xlsx(
  xlsxFile = "data/input/raw/CPPdb/CPPdb_ListA_ListB_181009_ZenodoV1.xlsx",
  sheet = "CPPdb_ListA",
  cols = c(1:3, 5:7, 10:11),
  na.strings = c(" ", "-", "0")
)
colnames(cpp_a_raw) <- c(
  "cas", "ec_number", "name", "description_function", "used_or_found_in", 
  "use_levels_and_migration_potential", "associated_with_plastics", 
  "associated_with_plastic_packaging"
)
cpp_a_raw <- cbind("cpp_a_id" = 1:nrow(cpp_a_raw), cpp_a_raw)

sapply(cpp_a_raw, \(x) sum(grepl(";", x))) # ec_number, name, description_function, used_or_found_in, use_levels_and_migration_potential
sapply(cpp_a_raw, \(x) sum(grepl("\\|", x))) # description_function
sapply(cpp_a_raw, \(x) sum(grepl(",", x))) # description_function, used_or_found_in, use_levels_and_migration_potential

cpp_a_raw <- transform(
  cpp_a_raw,
  "cas" = gsub(pattern = "‎", replacement = "", cas),
  "associated_with_plastics" = ifelse(
    test = grepl("yes", associated_with_plastics), 
    yes = 1L, 
    no = ifelse(
      test = grepl("no", associated_with_plastics),
      yes = 0L,
      no = NA_integer_
    )
  ),
  "associated_with_plastic_packaging" = ifelse(
    test = grepl("yes", associated_with_plastic_packaging), 
    yes = 1L, 
    no = ifelse(
      test = grepl("no", associated_with_plastic_packaging),
      yes = 0L,
      no = NA_integer_
    )
  )
)

cpp_a_split <- split(cpp_a_raw, f = ~ cpp_a_id)
 
cpp_a <- do.call(
  what = "rbind",
  args = c(
    pbapply::pblapply(
      cpp_a_split,
      FUN = function(x) {
        if(is.na(x$ec_number)) {
          ecn <- NA_character_
        } else {
          ecn <- paste(na.omit(trimws(unlist(strsplit(x$ec_number, split = ";")))), collapse = ";")
          if(nchar(ecn) == 0L) {
            ecn <- NA_character_
          }
        }
        
        desfun <- paste(na.omit(trimws(unlist(strsplit(x$description_function, split = ";|\\|")))), collapse = ";")
        if(nchar(desfun) == 0L) {
          desfun <- NA_character_
        }
        
        uofi <- paste(na.omit(trimws(unlist(strsplit(x$used_or_found_in, split = ";")))), collapse = ";")
        if(nchar(uofi) == 0L) {
          uofi <- NA_character_
        }
        
        ulmp <- paste(na.omit(trimws(unlist(strsplit(x$use_levels_and_migration_potential, split = ";")))), collapse = ";")
        if(nchar(ulmp) == 0L) {
          ulmp <- NA_character_
        }
        n_name <- paste(na.omit(trimws(unlist(strsplit(x$name, split = ";")))), collapse = ";")
        res <- data.frame(
          "cpp_a_id" = x$cpp_a_id,
          "cas" = trimws(x$cas),
          "ec_number" = ecn,
          "name" = n_name,
          "description_function" = desfun,
          "used_or_found_in" = uofi,
          "use_levels_and_migration_potential" = ulmp,
          "associated_with_plastics" = x$associated_with_plastics,
          "associated_with_plastic_packaging" = x$associated_with_plastic_packaging,
          stringsAsFactors = FALSE
        )
        res
      }
    ),
    make.row.names = FALSE
  )
)

skimr::skim(cpp_a) # 902

cpp_a <- transform(
  cpp_a,
  "cas" = ifelse(
    test = cleanventory:::.check_cas(cas),
    yes = cas,
    no = NA_character_
  ),
  "description_function" = ifelse(description_function == "0", NA_character_, description_function)
)

skimr::skim(cpp_a)

# 902 entries
#   7 entries with invalid CAS Registry Numbers (1%)

rm(cpp_a_raw, cpp_a_split)

saveRDS(cpp_a, "data/input/clean/CPPdb/CPPdb_ListA.rds")

cpp_a <- readRDS("data/input/clean/CPPdb/CPPdb_ListA.rds")

cpp_a_cas <- subset(cpp_a, subset = !is.na(cas))
cpp_a_no_cas <- subset(cpp_a, subset = is.na(cas))

sapply(split(cpp_a_cas, f = ~ cas), FUN = \(x) nrow(x)) |> max()

rm(cpp_a_cas, cpp_a_no_cas)

# CPPdb List B

cpp_b_raw <- openxlsx::read.xlsx(
  xlsxFile = "data/input/raw/CPPdb/CPPdb_ListA_ListB_181009_ZenodoV1.xlsx",
  sheet = "CPPdb_ListB",
  cols = c(1:3, 5:7, 10:11),
  na.strings = c(" ", "-", "0", "?")
)
colnames(cpp_b_raw) <- c(
  "cas", "ec_number", "name", "description_function", "used_or_found_in", 
  "use_levels_and_migration_potential", "associated_with_plastics", 
  "associated_with_plastic_packaging"
)
cpp_b_raw <- cbind("cpp_b_id" = 1:nrow(cpp_b_raw), cpp_b_raw)

sapply(cpp_b_raw, \(x) sum(grepl(";", x))) # ec_number, name, description_function, used_or_found_in, use_levels_and_migration_potential, associated_with_plastics, associated_with_plastic_packaging
sapply(cpp_b_raw, \(x) sum(grepl("\\|", x))) # description_function

unique(cpp_b_raw$associated_with_plastics)
unique(cpp_b_raw$associated_with_plastic_packaging)

cpp_b_raw <- transform(
  cpp_b_raw,
  "cas" = gsub(pattern = "‎", replacement = "", cas),
  "associated_with_plastics" = ifelse(
    test = grepl("yes", associated_with_plastics), 
    yes = 1L, 
    no = ifelse(
      test = grepl("no", associated_with_plastics),
      yes = 0L,
      no = NA_integer_
    )
  ),
  "associated_with_plastic_packaging" = ifelse(
    test = grepl("yes", associated_with_plastic_packaging), 
    yes = 1L, 
    no = ifelse(
      test = grepl("no", associated_with_plastic_packaging),
      yes = 0L,
      no = NA_integer_
    )
  )
)

skimr::skim(cpp_b_raw)

cpp_b_split <- split(cpp_b_raw, f = ~ cpp_b_id)

cpp_b <- do.call(
  what = "rbind",
  args = c(
    pbapply::pblapply(
      cpp_b_split,
      FUN = function(x) {
        if(is.na(x$ec_number)) {
          ecn <- NA_character_
        } else {
          ecn <- paste(na.omit(trimws(unlist(strsplit(x$ec_number, split = ";")))), collapse = ";")
          if(nchar(ecn) == 0L) {
            ecn <- NA_character_
          }
        }
        
        desfun <- paste(na.omit(trimws(unlist(strsplit(x$description_function, split = ";|\\|")))), collapse = ";")
        if(nchar(desfun) == 0L) {
          desfun <- NA_character_
        }
        
        uofi <- paste(na.omit(trimws(unlist(strsplit(x$used_or_found_in, split = ";")))), collapse = ";")
        if(nchar(uofi) == 0L) {
          uofi <- NA_character_
        }
        
        ulmp <- paste(na.omit(trimws(unlist(strsplit(x$use_levels_and_migration_potential, split = ";")))), collapse = ";")
        if(nchar(ulmp) == 0L) {
          ulmp <- NA_character_
        }
        
        # awp <- paste(na.omit(trimws(unlist(strsplit(x$associated_with_plastics, split = ";")))), collapse = ";")
        # if(nchar(awp) == 0L) {
        #   awp <- NA_character_
        # }
        # 
        # awpp <- paste(na.omit(trimws(unlist(strsplit(x$associated_with_plastic_packaging, split = ";")))), collapse = ";")
        # if(nchar(awpp) == 0L) {
        #   awpp <- NA_character_
        # }
        
        n_name <- paste(na.omit(trimws(unlist(strsplit(x$name, split = ";")))), collapse = ";")
        
        res <- data.frame(
          "cpp_b_id" = x$cpp_b_id,
          "cas" = trimws(x$cas),
          "ec_number" = ecn,
          "name" = n_name,
          "description_function" = desfun,
          "used_or_found_in" = uofi,
          "use_levels_and_migration_potential" = ulmp,
          "associated_with_plastics" = x$associated_with_plastics,
          "associated_with_plastic_packaging" = x$associated_with_plastic_packaging,
          stringsAsFactors = FALSE
        )
        res
      }
    ),
    make.row.names = FALSE
  )
)

skimr::skim(cpp_b) # 3353

cpp_b <- transform(
  cpp_b,
  # "cas" = ifelse(
  #   test = cleanventory:::.check_cas(cas),
  #   yes = cas,
  #   no = NA_character_
  # ),
  "description_function" = ifelse(description_function == "0", NA_character_, description_function)
)

skimr::skim(cpp_b)

# 3'353 entries
#     0 entries with invalid CAS Registry Numbers (0%)

rm(cpp_b_raw, cpp_b_split)

saveRDS(cpp_b, "data/input/clean/CPPdb/CPPdb_ListB.rds")

cpp_b <- readRDS("data/input/clean/CPPdb/CPPdb_ListB.rds")

cpp_b_cas <- subset(cpp_b, subset = !is.na(cas))
cpp_b_no_cas <- subset(cpp_b, subset = is.na(cas))

sapply(split(cpp_b_cas, f = ~ cas), FUN = \(x) nrow(x)) |> max()

rm(cpp_b_cas, cpp_b_no_cas)


##

colnames(cpp_a)[1] <- "cpp_id"
colnames(cpp_b)[1] <- "cpp_id"

cpp_a <- transform(
  cpp_a, 
  "cpp_id" = sapply(
    cpp_id,
    FUN = function(x) {
      res <- paste0("A", paste(rep("_", times = 1 + (nchar(max(cpp_a$cpp_id)) - nchar(x))), collapse = ""), x)
      res
    },
    USE.NAMES = FALSE
  )
)

cpp_b <- transform(
  cpp_b, 
  "cpp_id" = sapply(
    cpp_id,
    FUN = function(x) {
      res <- paste0("B", paste(rep("_", times = 1 + (nchar(max(cpp_b$cpp_id)) - nchar(x))), collapse = ""), x)
      res
    },
    USE.NAMES = FALSE
  )
)

cpp <- rbind(cpp_a, cpp_b, make.row.names = FALSE)

skimr::skim(cpp)

saveRDS(cpp, "data/input/clean/CPPdb/CPPdb.rds")

cpp <- readRDS("data/input/clean/CPPdb/CPPdb.rds")

cpp_cas <- subset(cpp, subset = !is.na(cas))
cpp_no_cas <- subset(cpp, subset = is.na(cas))

sapply(split(cpp_cas, f = ~ cas), FUN = \(x) nrow(x)) |> max()

rm(cpp_cas, cpp_no_cas)

rm(cpp_a, cpp_b)

cpp <- readRDS("data/input/clean/CPPdb/CPPdb.rds")

cpp <- transform(
  cpp,
  cpp_id = ifelse(
    grepl("A_A_", cpp_id),
    gsub("A_A_", "A_", cpp_id),
    cpp_id
  )
)

write.csv(cpp, "data/cpp_w_index.csv", na = "", row.names = FALSE)
