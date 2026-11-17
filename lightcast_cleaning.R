# =============================================================================
# SWFL Stock Index - Long List Screen
# Lightcast business locations  ->  list of publicly traded companies + tickers
#
# Criteria:
#   - Industries (2-digit NAICS): 23, 44-45, 53, 71, 72, 81
#   - Counties: Manatee, Sarasota, Charlotte, Glades, Lee, Hendry, Collier, Monroe
#     (raw CSVs in NEW_Index are already limited to these 8 counties)
#   - Publicly traded (matched to SEC company_tickers_exchange.json)
#
# Output: output/long-list-public-companies.csv  (company, ticker)
# =============================================================================

# 1. SET WORKING DIRECTORY ----------------------------------------------------

setwd("~/Library/CloudStorage/OneDrive-FloridaGulfCoastUniversity/SWFL Index/NEW_Index/New Stock Index")

# 2. INSTALL AND LOAD PACKAGES -------------------------------------------------

packages <- c(
  "readr",
  "dplyr",
  "httr2",
  "jsonlite",
  "stringr",
  "tibble",
  "stringdist"   # for fuzzy name matching
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(packages, library, character.only = TRUE))

# 3. SETTINGS ------------------------------------------------------------------

target_naics_2 <- c("23", "44", "45", "53", "71", "72", "81")
include_otc    <- FALSE   # TRUE keeps OTC-traded companies
fuzzy_max_dist <- 0.03    # Jaro-Winkler cutoff for typo matching (lower = stricter)
output_dir     <- "output"

# MANUAL ALIASES - one-word store names that really are the public company.
# One-word matches are NOT made automatically, because words like "Southern",
# "Compass" or "Crown" start hundreds of unrelated local business names.
# Check output/review-single-word-candidates.csv after each run and add real
# ones here (first word of the Lightcast name, cleaned/uppercase -> ticker).
manual_aliases <- tribble(
  ~prefix,   ~alias_ticker,
  "WALMART",        "WMT",
  "LOWES",          "LOW",
  "LENNAR",         "LEN",
  "APPLE",          "AAPL",
  "CATO",           "CATO",
  "CROCS",          "CROX",
  "CUBESMART",      "CUBE",
  "LOVESAC",        "LOVE",
  "MARINEMAX",      "HZO",
  "MONRO",          "MNRO",
  "STARBUCKS",      "SBUX",
  "VALVOLINE",      "VVV",
  "XYLEM",          "XYL",
  "MICROSOFT",      "MSFT",
  "HARLEYDAVIDSON", "HOG",
  "WHIRLPOOL",      "WHR",
  "STERIS",         "STE",
  "ESAB",           "ESAB",
  "TARGET",         "TGT",
  "KOHLS",          "KSS",
  "TESLA",          "TSLA",
  "AUTONATION",     "AN",
  "CINTAS",         "CTAS",
  "ARAMARK",        "ARMK",
  "FIRSTSERVICE",   "FSV",
  "LAZBOY",         "LZB",
  "NIKE",           "NKE",
  "ARHAUS",         "ARHS",
  "SUNOCO",         "SUN",
  "SHELL",          "SHEL",
  "CHEVRON",        "CVX",
  "HUBBELL",        "HUBB",
  "COMPASS",        "COMP"
)

# EXCLUSIONS - matches reviewed and confirmed to be unrelated local businesses
# (first ticker listed for the company). Applied to the final list in step 8.
exclude_tickers <- c(
  "CWCO",  # Consolidated Waste      != Consolidated Water
  "MKL",   # Markle Group Inc        != Markel
  "MNTS",  # Momentous Llc           != Momentus
  "RDN",   # Radiant Group Llc       != Radian
  "TMDE",  # Tm Energy               != TMD Energy
  "HITI",  # High Tide Marine etc.   != High Tide Inc
  "TBCH",  # Turtle Beach Resort     != Turtle Beach Corp
  "LE",    # Lands End Marina        != Lands' End
  "BLBD",  # Blue Bird Salon         != Blue Bird Corp
  "COHN",  # Cohen & Cohen Catering  != Cohen & Co
  "STT",   # State Street Eating House != State Street
  "MFI",   # Mf International Restaurant != mF International
  "ATI",   # "Ati" (likely ATI Physical Therapy) != ATI Inc (specialty metals)
  "BOX",   # "The Box"               != Box Inc
  "WHD",   # "Cactus" (local)        != Cactus Inc (oilfield equipment)
  "MRLN",  # "Merlin Inc" (local)    != Merlin Inc (aerospace)
  "GIB"    # "Cgi Llc" (local)       != CGI Inc
)

# SEC requires a "Name email" user agent, stored in ~/.Renviron as SEC_USER_AGENT
sec_user_agent <- Sys.getenv("SEC_USER_AGENT")
if (sec_user_agent == "") stop("Set SEC_USER_AGENT in ~/.Renviron first.")

dir.create(output_dir, showWarnings = FALSE)

# 4. IMPORT LIGHTCAST CSV FILES ------------------------------------------------

data_dir <- ".."

csv_files <- list.files(
  path = data_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

print(basename(csv_files))

combined_data <- lapply(
  csv_files,
  read_csv,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  bind_rows()

dim(combined_data)

# Keep target industries, and only the two name columns
business_data <- combined_data %>%
  filter(str_sub(trimws(`Primary NAICS Code`), 1, 2) %in% target_naics_2) %>%
  select(`Business Name`, `Parent Company`)

# 5. LOAD SEC TICKER DATA ------------------------------------------------------

sec_data <- request(
  "https://www.sec.gov/files/company_tickers_exchange.json"
) %>%
  req_user_agent(sec_user_agent) %>%
  req_perform() %>%
  resp_body_json(simplifyVector = TRUE)

sec_tickers <- as_tibble(sec_data$data, .name_repair = "minimal")
names(sec_tickers) <- sec_data$fields   # cik, name, ticker, exchange

sec_tickers <- sec_tickers %>%
  mutate(across(everything(), as.character)) %>%
  filter(!is.na(exchange), exchange != "")

if (!include_otc) {
  sec_tickers <- sec_tickers %>% filter(exchange != "OTC")
}

# 6. NAME CLEANING -------------------------------------------------------------
# Uppercase, no punctuation, and ONLY legal endings removed (Inc, Corp, Co,
# LLC...). Descriptive words like GROUP, INTERNATIONAL, HOLDINGS, COMPANIES are
# kept: stripping them turned "American International Group" into "AMERICAN"
# and "Coffee Holding Co" into "COFFEE", which matched hundreds of local shops.

clean_name <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("&", " AND ") %>%
    str_replace_all("['\\-\\.]", "") %>%       # Wal-Mart -> WALMART, Lowe's -> LOWES
    str_replace_all("[^A-Z0-9 ]", " ") %>%
    str_replace_all(
      "\\b(THE|INC|LNC|INCORPORATED|CORP|CORPORATION|CO|LLC|LP|LTD|LIMITED|PLC|NV|SA|AG|DE|NEW)\\b",
      " "
    ) %>%
    str_squish()
}

# One row per cleaned SEC name (share classes like GOOG/GOOGL kept together)
sec_lookup <- sec_tickers %>%
  mutate(clean = clean_name(name)) %>%
  filter(clean != "") %>%
  group_by(clean) %>%
  summarise(
    company = first(name),
    ticker  = paste(unique(ticker), collapse = "; "),
    .groups = "drop"
  )

# Every distinct name in Lightcast (business names AND parent companies).
# Duplicates collapse here, so Walmart appearing 40 times is matched once.
names_to_match <- tibble(
  raw = c(business_data$`Business Name`, business_data$`Parent Company`)
) %>%
  mutate(raw = trimws(raw)) %>%
  filter(!is.na(raw), raw != "") %>%
  distinct(raw) %>%
  mutate(clean = clean_name(raw)) %>%
  filter(clean != "")

# 7. MATCH TO SEC --------------------------------------------------------------

# 7a. Exact match on cleaned name ("Walmart Inc" -> WMT)
exact_matches <- names_to_match %>%
  inner_join(sec_lookup, by = "clean") %>%
  mutate(match_type = "exact")

# 7b. Fuzzy match for typos ("Lennarr" -> LEN). Strict cutoff and names of
#     8+ characters only - looser settings matched "Bc Automotive" to
#     Group 1 Automotive and "Markle Inc" to Markel.
unmatched <- names_to_match %>%
  filter(!raw %in% exact_matches$raw, nchar(clean) >= 8)

fuzzy_idx <- amatch(unmatched$clean, sec_lookup$clean,
                    method = "jw", p = 0.1, maxDist = fuzzy_max_dist)

fuzzy_matches <- unmatched %>%
  mutate(sec_row = fuzzy_idx) %>%
  filter(!is.na(sec_row)) %>%
  mutate(
    company    = sec_lookup$company[sec_row],
    ticker     = sec_lookup$ticker[sec_row],
    match_type = "fuzzy"
  ) %>%
  select(-sec_row)

# 7c. Prefix match (2+ words) for store/location names:
#     "Lgi Homes - Cape Coral" -> "LGI HOMES" -> LGIH. Longest hit wins.
still_unmatched <- names_to_match %>%
  filter(!raw %in% c(exact_matches$raw, fuzzy_matches$raw))

prefix_matches <- bind_rows(lapply(2:4, function(k) {
  still_unmatched %>% mutate(n_words = k, prefix = word(clean, 1, k))
})) %>%
  filter(!is.na(prefix), prefix != clean) %>%
  inner_join(sec_lookup, by = c("prefix" = "clean")) %>%
  group_by(raw) %>%
  slice_max(n_words, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(match_type = "prefix") %>%
  select(-n_words, -prefix)

# 7d. One-word store names, ONLY through the manual alias list above:
#     "Walmart Supercenter" / "Walmart Deli" -> WALMART -> WMT
alias_lookup <- manual_aliases %>%
  left_join(sec_tickers %>% select(alias_ticker = ticker, name), by = "alias_ticker") %>%
  mutate(clean = clean_name(name)) %>%
  left_join(sec_lookup, by = "clean") %>%
  select(prefix, alias_ticker, company, ticker)

if (any(is.na(alias_lookup$company))) {
  warning("These alias tickers were not found in the SEC list: ",
          paste(alias_lookup$alias_ticker[is.na(alias_lookup$company)], collapse = ", "))
}

alias_matches <- still_unmatched %>%
  filter(!raw %in% prefix_matches$raw) %>%
  mutate(prefix = word(clean, 1, 1)) %>%
  inner_join(alias_lookup %>% filter(!is.na(company)), by = "prefix") %>%
  mutate(match_type = "manual alias") %>%
  select(raw, clean, company, ticker, match_type)

all_matches <- bind_rows(exact_matches, fuzzy_matches, prefix_matches, alias_matches)

count(all_matches, match_type)

# 8. FINAL LIST: ONE ROW PER COMPANY -------------------------------------------

public_companies <- all_matches %>%
  distinct(company, ticker) %>%
  filter(!str_extract(ticker, "^[^;]+") %in% exclude_tickers) %>%
  arrange(company)

nrow(public_companies)
print(public_companies, n = Inf)

# Review sheet: which Lightcast names produced each match. Use it to delete
# false positives. Exact matches are reliable; check fuzzy and prefix.
match_review <- all_matches %>%
  group_by(company, ticker) %>%
  summarise(
    best_match_type = case_when(
      any(match_type == "exact")        ~ "exact",
      any(match_type == "manual alias") ~ "manual alias",
      any(match_type == "prefix")       ~ "prefix",
      TRUE                              ~ "fuzzy"
    ),
    n_names         = n(),
    lightcast_names = paste(head(unique(raw), 5), collapse = " | "),
    .groups = "drop"
  ) %>%
  arrange(factor(best_match_type, levels = c("fuzzy", "prefix", "manual alias", "exact")),
          desc(n_names))

# One-word candidates NOT matched: first word of an unmatched Lightcast name
# equals a one-word public company name. Mostly noise (Southern, Compass,
# Crown...) - scan for real ones and add them to manual_aliases.
single_word_candidates <- still_unmatched %>%
  filter(!raw %in% c(prefix_matches$raw, alias_matches$raw)) %>%
  mutate(prefix = word(clean, 1, 1)) %>%
  filter(nchar(prefix) >= 4) %>%
  inner_join(sec_lookup, by = c("prefix" = "clean")) %>%
  group_by(prefix, company, ticker) %>%
  summarise(
    n_names  = n(),
    examples = paste(head(raw, 5), collapse = " | "),
    .groups  = "drop"
  ) %>%
  arrange(desc(n_names))

# 9. EXPORT --------------------------------------------------------------------
# Written to ./output (not "..") so the next run doesn't read these back in.

write_csv(public_companies,       file.path(output_dir, "long-list-public-companies.csv"))
write_csv(match_review,           file.path(output_dir, "long-list-match-review.csv"))
write_csv(single_word_candidates, file.path(output_dir, "review-single-word-candidates.csv"))
