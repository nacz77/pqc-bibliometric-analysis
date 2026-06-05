################################################################################
############################# Scopus Data Cleaning #############################
################################################################################

## Packages
library(dplyr)    # data manipulation
library(stringr)  # string operations


##### Load data #####

file <- file.choose() # choose raw Scopus export
df <- read.csv(file,
               # leaves column names as in the CSV
               check.names = FALSE)


##### Step 1: Remove missing DOIs and duplicates #####

df_clean <- df %>%
  filter(!is.na(DOI), DOI != "") %>%
  distinct(DOI, .keep_all = TRUE)


##### Step 2: Filter to Europe and Asia #####

# extract_countries() pulls the country from affiliation string.
# In Scopus exports, affiliations are separated by ";". Within each affiliation,
# the country is always the element after the last comma. Returns a character
# vector of unique countries for a single row.
extract_countries <- function(affiliation_string) {
  if (is.na(affiliation_string)) return(NA)
  affiliations <- str_split(affiliation_string, ";")[[1]]
  countries <- str_trim(str_extract(affiliations, "[^,]+$"))
  return(unique(countries))
}


europe <- c("ALBANIA", "ANDORRA", "AUSTRIA", "BELARUS", "BELGIUM",
            "BOSNIA AND HERZEGOVINA", "BULGARIA", "CROATIA", "CYPRUS",
            "CZECH REPUBLIC", "DENMARK", "ESTONIA", "FINLAND", "FRANCE",
            "GERMANY", "GREECE", "HUNGARY", "ICELAND", "IRELAND", "ITALY",
            "KOSOVO", "LATVIA", "LIECHTENSTEIN", "LITHUANIA", "LUXEMBOURG",
            "MALTA", "MOLDOVA", "MONACO", "MONTENEGRO", "NETHERLANDS",
            "NORTH MACEDONIA", "NORWAY", "POLAND", "PORTUGAL", "ROMANIA",
            "RUSSIAN FEDERATION", "SAN MARINO", "SERBIA", "SLOVAKIA",
            "SLOVENIA", "SPAIN", "SWEDEN", "SWITZERLAND", "UKRAINE",
            "UNITED KINGDOM")

asia <- c("AFGHANISTAN", "ARMENIA", "AZERBAIJAN", "BAHRAIN", "BANGLADESH",
          "BHUTAN", "BRUNEI DARUSSALAM", "CAMBODIA", "CHINA", "GEORGIA",
          "HONG KONG", "INDIA", "INDONESIA", "IRAN", "IRAQ", "ISRAEL", "JAPAN",
          "JORDAN", "KAZAKHSTAN", "KUWAIT", "KYRGYZSTAN", "LAOS", "LEBANON",
          "MACAU", "MALAYSIA", "MALDIVES", "MONGOLIA", "MYANMAR", "NEPAL",
          "NORTH KOREA", "OMAN", "PAKISTAN", "PALESTINE", "PHILIPPINES",
          "QATAR", "SAUDI ARABIA", "SINGAPORE", "SOUTH KOREA", "SRI LANKA",
          "SYRIAN ARAB REPUBLIC", "TAIWAN", "TAJIKISTAN", "THAILAND",
          "TIMOR-LESTE", "TURKEY", "TURKMENISTAN", "UNITED ARAB EMIRATES",
          "UZBEKISTAN", "VIET NAM", "YEMEN")

target_countries <- c(europe, asia)


# Keep a record if at least one of its affiliation countries is in the target
# list. Meaning that if a publication is e.g. by an author from France and
# an author from the US, it will still be included.
# str_to_upper() normalizes case before matching.
df_filtered <- df_clean %>%
  filter(sapply(Affiliations, function(x) {
    countries <- extract_countries(x)
    any(str_to_upper(countries) %in% target_countries)
  }))


##### Step 3: Export cleaned dataset for external data handling #####

write.csv(df_filtered, "cleaned_dataset.csv", row.names = FALSE)


##### Step 4: Filter original Scopus export using post-handling DOIs #####

# After external data handling, re-import the handled dataset.
file_handled <- file.choose()
df_handled <- read.csv(file_handled, check.names = FALSE)

# semi_join() keeps only rows in df_filtered whose DOI also appears in
# df_handled (like an inner join).
df_final <- df_filtered %>%
  semi_join(df_handled, by = "DOI")


##### Step 5: Export final dataset for analysis #####

write.csv(df_final, "final_dataset.csv", row.names = FALSE)