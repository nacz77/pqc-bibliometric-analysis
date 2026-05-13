################################################################################
########################## Bibliometric Data Analysis ########################## 
################################################################################

# Packages
library(bibliometrix)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggrepel)
library(stringr)
library(maps)
library(wordcloud)
library(RColorBrewer)

# Load data
file <- file.choose()
df <- convert2df(file = file,
                 dbsource = "scopus",
                 format = "csv")


# ==============================================================================
# ================= DATASET OVERVIEW — Descriptive Statistics ==================
# ==============================================================================

europe <- c(
  "ALBANIA", "ANDORRA", "AUSTRIA", "BELARUS", "BELGIUM",
  "BOSNIA AND HERZEGOVINA", "BULGARIA", "CROATIA", "CYPRUS", "CZECH REPUBLIC",
  "DENMARK", "ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY",
  "ICELAND", "IRELAND", "ITALY", "KOSOVO", "LATVIA", "LIECHTENSTEIN",
  "LITHUANIA", "LUXEMBOURG", "MALTA", "MOLDOVA", "MONACO", "MONTENEGRO",
  "NETHERLANDS", "NORTH MACEDONIA", "NORWAY", "POLAND", "PORTUGAL", "ROMANIA",
  "RUSSIAN FEDERATION", "SAN MARINO", "SERBIA", "SLOVAKIA", "SLOVENIA", "SPAIN",
  "SWEDEN", "SWITZERLAND", "UKRAINE", "UNITED KINGDOM")

asia <- c(
  "AFGHANISTAN", "ARMENIA", "AZERBAIJAN", "BAHRAIN", "BANGLADESH", "BHUTAN",
  "BRUNEI DARUSSALAM", "CAMBODIA", "CHINA", "GEORGIA", "HONG KONG", "INDIA",
  "INDONESIA", "IRAN", "IRAQ", "ISRAEL", "JAPAN", "JORDAN", "KAZAKHSTAN",
  "KUWAIT", "KYRGYZSTAN", "LAOS", "LEBANON", "MACAU", "MALAYSIA", "MALDIVES",
  "MONGOLIA", "MYANMAR", "NEPAL", "NORTH KOREA", "OMAN", "PAKISTAN",
  "PALESTINE", "PHILIPPINES", "QATAR", "SAUDI ARABIA", "SINGAPORE",
  "SOUTH KOREA", "SRI LANKA", "SYRIAN ARAB REPUBLIC", "TAIWAN", "TAJIKISTAN",
  "THAILAND", "TIMOR-LESTE", "TURKEY", "TURKMENISTAN", "UNITED ARAB EMIRATES",
  "UZBEKISTAN", "VIET NAM", "YEMEN")

target_countries <- unique(c(europe, asia))

# There are some incomplete Scopus entries that are missing a country at the
# end. The regex grabs whatever the last element is which turns out to be e.g.
# an organisation name.
# Whitelist: Only keep a value if it is a recognized country.
all_world_countries <- c(
  europe,
  asia,
  # Africa
  "ALGERIA", "ANGOLA", "BENIN", "BOTSWANA", "BURKINA FASO",
  "BURUNDI", "CABO VERDE", "CAMEROON", "CENTRAL AFRICAN REPUBLIC",
  "CHAD", "COMOROS", "DEMOCRATIC REPUBLIC OF CONGO", "DJIBOUTI",
  "EGYPT", "EQUATORIAL GUINEA", "ERITREA", "ESWATINI", "ETHIOPIA",
  "GABON", "GAMBIA", "GHANA", "GUINEA", "GUINEA-BISSAU",
  "IVORY COAST", "KENYA", "LESOTHO", "LIBERIA", "LIBYA",
  "MADAGASCAR", "MALAWI", "MALI", "MAURITANIA", "MAURITIUS",
  "MOROCCO", "MOZAMBIQUE", "NAMIBIA", "NIGER", "NIGERIA",
  "REPUBLIC OF CONGO", "RWANDA", "SAO TOME AND PRINCIPE", "SENEGAL",
  "SIERRA LEONE", "SOMALIA", "SOUTH AFRICA", "SOUTH SUDAN", "SUDAN",
  "TANZANIA", "TOGO", "TUNISIA", "UGANDA", "ZAMBIA", "ZIMBABWE",
  # America
  "ANTIGUA AND BARBUDA", "ARGENTINA", "BAHAMAS", "BARBADOS",
  "BELIZE", "BOLIVIA", "BRAZIL", "CANADA", "CHILE", "COLOMBIA",
  "COSTA RICA", "CUBA", "DOMINICA", "DOMINICAN REPUBLIC", "ECUADOR",
  "EL SALVADOR", "GRENADA", "GUATEMALA", "GUYANA", "HAITI",
  "HONDURAS", "JAMAICA", "MEXICO", "NICARAGUA", "PANAMA",
  "PARAGUAY", "PERU", "PUERTO RICO", "SAINT KITTS AND NEVIS",
  "SAINT LUCIA", "SAINT VINCENT AND THE GRENADINES", "SURINAME",
  "TRINIDAD AND TOBAGO", "UNITED STATES", "URUGUAY", "VENEZUELA",
  # Oceania
  "AUSTRALIA", "FIJI", "KIRIBATI", "MARSHALL ISLANDS", "MICRONESIA",
  "NAURU", "NEW ZEALAND", "PALAU", "PAPUA NEW GUINEA", "SAMOA",
  "SOLOMON ISLANDS", "TONGA", "TUVALU", "VANUATU",
  # Territories and special cases that appear in Scopus
  "CAYMAN ISLANDS", "FRENCH POLYNESIA", "SCOTLAND", "NEW CALEDONIA",
  "REUNION", "MARTINIQUE", "GUADELOUPE", "GIBRALTAR", "JERSEY",
  "FAROE ISLANDS", "GREENLAND", "WESTERN SAHARA"
)

# ----------- Country Extraction -----------

# Extract all unique countries from a single C1 (affiliation) string.
# Adapted from the extraction function from the data cleaning part.
extract_countries <- function(affiliation_string) {
  if (is.na(affiliation_string) || affiliation_string == "") return(character(0))
  affiliations <- str_split(affiliation_string, ";")[[1]]
  countries <- str_trim(str_extract(affiliations, "[^,]+$"))
  # There are some incomplete Scopus entries that are missing a country at the
  # end. The regex grabs whatever the last element is which turns out to be e.g.
  # an organisation name.
  # Whitelist filter: drops anything that is not a recognized name (e.g.
  # organisation name)
  countries <- countries[countries %in% all_world_countries]
  unique(countries)
}

# Apply country extraction function to every row. Result is a list-column where
# each element is a character vector of countries for that publication.
df$country_list <- lapply(df$C1, extract_countries)


# ----------- Region Classification -----------

# contains_region() returns TRUE if any country in the vector belongs to the
# given region.
contains_region <- function(countries_vec, region_vec) {
  if (length(countries_vec) == 0 || all(is.na(countries_vec))) return(FALSE)
  any(countries_vec %in% region_vec)
}

# Tag each row with a descriptive region.
df$is_europe <- sapply(df$country_list, contains_region, europe)
df$is_asia <- sapply(df$country_list, contains_region, asia)
# has_other: TRUE if at least one affiliation country falls outside of both
# the European and Asian lists.
df$has_other <- sapply(df$country_list, function(x) {
  if (length(x) == 0 || all(is.na(x))) return(FALSE)
  any(!x %in% target_countries)
})

df$region <- case_when(
  df$is_europe & !df$is_asia & !df$has_other ~ "Europe only",
  df$is_asia & !df$is_europe & !df$has_other ~ "Asia only",
  df$is_europe & df$is_asia & !df$has_other  ~ "Europe & Asia",
  df$has_other & (df$is_europe | df$is_asia) ~ "Mixed (incl. other regions)",
  TRUE                                       ~ "Other / unclassified"
)


######################### MAIN SUMMARY STATISTICS TABLE ########################

# ----------- Core Counts -----------
total_papers      <- nrow(df)
year_min          <- min(df$PY, na.rm = TRUE)
year_max          <- max(df$PY, na.rm = TRUE)
total_citations   <- sum(df$TC, na.rm = TRUE)
avg_citations     <- round(total_citations / total_papers, 1)
avg_papers_year   <- round(total_papers / length(year_min:year_max), 1)

# ----------- Unique Authors -----------
unique_authors <- df %>%
  filter(!is.na(AU), AU != "") %>%
  pull(AU) %>%
  strsplit(";") %>%
  unlist() %>%
  trimws() %>%
  unique() %>%
  length()

# ----------- Unique Institutions -----------
# Use AU_UN (cleaned institution names) if available; fall back to C1 if not.
affil_field <- if ("AU_UN" %in% colnames(df)) "AU_UN" else "C1"

unique_institutions <- df %>%
  filter(!is.na(.data[[affil_field]]), .data[[affil_field]] != "") %>%
  pull(affil_field) %>%
  strsplit(";") %>%
  unlist() %>%
  trimws() %>%
  unique() %>%
  length()

# ----------- Unique Countries -----------
unique_countries <- df %>%
  pull(country_list) %>%
  unlist() %>%
  na.omit() %>%
  unique() %>%
  length()

# ----------- Unique Sources -----------
unique_sources <- df$SO[!is.na(df$SO) & df$SO != ""] %>%
  unique() %>%
  length()

# ----------- Summary Table -----------
# Helper to format numbers with thousand separators
fmt <- function(x) format(x, big.mark = ",")

summary_stats <- data.frame(
  Indicator = c(
    "Total publications",
    "Time coverage",
    "Average publications per year",
    "Total unique authors",
    "Total unique institutions",
    "Total unique countries / territories",
    "Total unique publication sources",
    "Total citations",
    "Average citations per paper"
  ),
  Value = c(
    fmt(total_papers),
    paste0(year_min, "\u2013", year_max),
    avg_papers_year,
    fmt(unique_authors),
    fmt(unique_institutions),
    unique_countries,
    fmt(unique_sources),
    fmt(total_citations),
    avg_citations
  ),
  stringsAsFactors = FALSE
)

# Save as CSV
dir.create("tables", showWarnings = FALSE)
write.csv(summary_stats, "tables/01_summary_stats.csv", row.names = FALSE)


############################# DOCUMENT TYPE TABLE ##############################

doctype_table <- df %>%
  filter(!is.na(DT), DT != "") %>%
  count(DT, name = "Publications") %>%
  arrange(desc(Publications)) %>%
  mutate(
    Percentage = paste0(round(Publications / sum(Publications) * 100, 1), "%")
  ) %>%
  rename(`Document Type` = DT)

write.csv(doctype_table, "tables/02_document_types.csv", row.names = FALSE)


########################### REGIONAL BREAKDOWN TABLE ###########################
n_europe_only <- sum(df$region == "Europe only",                 na.rm = TRUE)
n_asia_only   <- sum(df$region == "Asia only",                   na.rm = TRUE)
n_eu_asia     <- sum(df$region == "Europe & Asia",               na.rm = TRUE)
n_mixed       <- sum(df$region == "Mixed (incl. other regions)", na.rm = TRUE)
n_other       <- sum(df$region == "Other / unclassified",        na.rm = TRUE)

regional_table <- data.frame(
  Region = c(
    "Europe only",
    "Asia only",
    "Cross-regional (Europe & Asia)",
    "Mixed (incl. other regions)",
    "Other / unclassified",
    "Total"),
  Papers = c(
    n_europe_only,
    n_asia_only,
    n_eu_asia,
    n_mixed,
    n_other,
    total_papers
  )
) %>%
  mutate(
    `Share (%)` = paste0(round(Papers / total_papers * 100, 1), "%")
  )

write.csv(regional_table, "tables/03_regional_breakdown.csv", row.names = FALSE)


############################# LANGUAGE DISTRIBUTION ############################

lang_table <- df %>%
  filter(!is.na(LA), LA != "") %>%
  count(LA, name = "Publications") %>%
  arrange(desc(Publications)) %>%
  mutate(
    `Share (%)` = paste0(round(Publications / sum(Publications) * 100, 1), "%")
  ) %>%
  rename(Language = LA)

write.csv(lang_table, "tables/04_languages.csv", row.names = FALSE)



# ==============================================================================
# ================= SQ1: Publication Volume and Temporal Trends ================
# ==============================================================================

########################### ANNUAL PUBLICATION COUNT ###########################

# ----------- Overall -----------
annual_total <- df %>%
  count(PY) %>%
  rename(Year = PY, Publications = n)

p_annual_total <- ggplot(annual_total,
                         aes(x = factor(Year), y = Publications)) +
  geom_col(fill = "grey", colour = "white", width = 0.7) +
  geom_smooth(aes(x = as.numeric(factor(Year))),
              method = "loess", span = 0.5, colour = "#C00000",
              linewidth = 0.9, linetype = "solid") +
  geom_text(aes(label = Publications), vjust = -0.5, size = 3.2,
            fontface = "bold", colour = "grey25") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    # title    = "Annual Publication Count (Europe and Asia)",
    x        = "Year",
    y        = "Number of Publications"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    panel.grid.major.x = element_blank(),
    axis.text.x   = element_text(angle = 45, hjust = 1)
  )

dir.create("figures", showWarnings = FALSE)
ggsave("figures/SQ1_01_annual_total.png",
       p_annual_total, width = 10, height = 5.5, dpi = 300)

# ----------- By Region -----------
annual_regional <- df %>%
  select(PY, is_europe, is_asia) %>%
  # Inclusive counting: publication with both European and Asian affiliations is
  # counted in both regions. pivot_longer() converts columns 'is_europe' and
  # 'is_asia' so that each publication appears twice, once for Europe and once
  # for Asia.
  pivot_longer(
    cols = c(is_europe, is_asia),
    names_to = "Region",
    values_to = "included"
  ) %>%
  filter(included == TRUE) %>%
  mutate(
    Region = case_when(
      Region == "is_europe" ~ "Europe",
      Region == "is_asia"   ~ "Asia"
    )
  ) %>%
  count(PY, Region) %>%
  rename(Year = PY, Publications = n)

p_annual_regional <- ggplot(annual_regional,
                            aes(x = factor(Year), y = Publications, fill = Region)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7,
           colour = "white") +
  geom_text(aes(label = Publications),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 2.8, fontface = "bold") +
  scale_fill_manual(values = c("Europe" = "#4472C4", "Asia" = "#ED7D31")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    # title = "Annual Publications by Region",
    # subtitle = "Inclusive counting: mixed collaborations are counted in each relevant region",
    x = "Year",
    y = "Number of Publications",
    fill = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("figures/SQ1_02_annual_regional.png",
       p_annual_regional, width = 11, height = 6, dpi = 300)


#################### CUMULATIVE PUBLICATION GROWTH BY REGION ###################

annual_regional_cum <- annual_regional %>%
  arrange(Region, Year) %>%
  group_by(Region) %>%
  mutate(Cumulative = cumsum(Publications)) %>%
  ungroup()

# Find the first year where Asia's cumulative count overtakes Europe's.
crossover <- annual_regional_cum %>%
  select(Year, Region, Cumulative) %>%
  pivot_wider(names_from = Region, values_from = Cumulative) %>%
  filter(!is.na(Europe), !is.na(Asia)) %>%
  mutate(
    diff = Asia - Europe,
    crossed = diff > 0 & lag(diff, default = first(diff)) <= 0
  ) %>%
  filter(crossed)

p_cumulative <- ggplot(annual_regional_cum,
                       aes(x = Year, y = Cumulative, colour = Region, group = Region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("Europe" = "#4472C4", "Asia" = "#ED7D31")) +
  scale_x_continuous(breaks = seq(2016, 2026, 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    # title = "Cumulative Publication Growth by Region",
    # subtitle = "Inclusive counting: mixed publications are counted in each relevant region",
    x = "Year",
    y = "Cumulative Publications",
    colour = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Add crossover annotation if Asia overtook europe at some point.
if (nrow(crossover) > 0) {
  p_cumulative <- p_cumulative +
    geom_vline(
      xintercept = crossover$Year[1],
      linetype = "dotted",
      colour = "grey40",
      linewidth = 0.8
    ) +
    annotate(
      "text",
      x = crossover$Year[1] + 0.2,
      y = max(annual_regional_cum$Cumulative) * 0.5,
      label = paste0("Asia overtakes Europe\n(", crossover$Year[1], ")"),
      hjust = 0,
      size = 3.2,
      colour = "grey30"
    )
}

ggsave("figures/SQ1_03_cumulative.png",
       p_cumulative, width = 10, height = 5.5, dpi = 300)


########################### COUNTRY-WISE DISTRIBUTION ########################## 

# Separated by regions
country_regional <- df %>%
  tidyr::unnest(country_list) %>%
  filter(country_list %in% c(europe, asia)) %>%
  mutate(
    country_region = if_else(country_list %in% europe, "Europe", "Asia")
  ) %>%
  count(country_region, country_list, name = "Publications") %>%
  group_by(country_region) %>%
  arrange(desc(Publications), .by_group = TRUE) %>%
  slice_head(n = 10) %>%
  ungroup()

p_top_countries_regional <- ggplot(country_regional,
                                   aes(
                                     x = reorder(country_list, Publications),
                                     y = Publications,
                                     fill = country_region)) +
  geom_col(show.legend = FALSE, colour = "white") +
  geom_text(aes(label = Publications), hjust = -0.2, size = 3) +
  coord_flip() +
  facet_wrap(~country_region, scales = "free_y") +
  scale_fill_manual(values = c("Europe" = "#4472C4", "Asia" = "#ED7D31")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    # title = "Top 10 Most Productive Countries by Region",
    x = NULL,
    y = "Number of Publications"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave("figures/SQ1_04_top_countries_regional.png",
       p_top_countries_regional, width = 12, height = 6, dpi = 300)



# ==============================================================================
# ============== SQ2: REGIONAL ACTORS AND COLLABORATION STRUCTURES =============
# ==============================================================================

df_europe <- df %>% filter(is_europe)
df_asia <- df %>% filter(is_asia)

############################ TOP AUTHORS BY REGION #############################

# Authors are counted per region based on the publication's regional assignment:
# a publication is attributed to a region if at least one of its affiliations
# belongs to that region. As a result, an author may appear in both regional
# rankings if they co-authored publications with affiliations in both regions,
# regardless of their own primary affiliation.
get_top_authors <- function(data, region_name, n = 10) {
  data %>%
    filter(!is.na(AU), AU != "") %>%
    pull(AU) %>%
    strsplit(";") %>%
    unlist() %>%
    trimws() %>%
    .[. != ""] %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    head(n) %>%
    as.data.frame() %>%
    setNames(c("author", "Publications")) %>%
    mutate(Region = region_name)
}

top_authors <- bind_rows(
  get_top_authors(df_europe, "Europe", 10),
  get_top_authors(df_asia,   "Asia",   10)
)

p_authors <- ggplot(top_authors,
                    aes(x = reorder_within(author, Publications, Region),
                        y = Publications,
                        fill = Region)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = Publications), hjust = -0.2, size = 3) +
  coord_flip() +
  facet_wrap(~Region, scales = "free") +
  scale_x_reordered() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c("Europe" = "#4472C4", "Asia" = "#ED7D31")) +
  labs(
    # title = "Top 10 Authors by Region",
    x     = NULL,
    y     = "Number of Publications"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave("figures/SQ2_01_top_authors_regional.png",
       p_authors, width = 12, height = 6, dpi = 300)


########################## TOP INSTITUTIONS BY REGION ##########################

# Using strict regional assignment; inclusion counting does not apply here.

# Table mapping each institution name to a region using C1, which contains both 
# institution and country.
institution_region_lookup <- df %>%
  filter(!is.na(C1), C1 != "") %>%
  pull(C1) %>%
  strsplit(";") %>%
  unlist() %>%
  trimws() %>%
  {
    data.frame(
      # Institution = second comma-separated element
      institution = toupper(trimws(sapply(strsplit(., ","), function(x) x[2]))),
      # Country = last comma-separated element
      country     = toupper(trimws(sapply(strsplit(., ","), function(x) x[length(x)])))
    )
  } %>%
  filter(!is.na(institution), institution != "") %>%
  mutate(
    region = case_when(
      country %in% europe ~ "Europe",
      country %in% asia   ~ "Asia",
      TRUE                ~ NA_character_
    )
  ) %>%
  # Drops institutions outside of the both regions (e.g. Florida Atlantic).
  filter(!is.na(region)) %>%
  distinct(institution, region)

# Keep only institutions that map to exactly one region
institution_region_lookup <- institution_region_lookup %>%
  group_by(institution) %>%
  filter(n() == 1) %>%
  ungroup()

# Count institutions from AU_UN and join with the lookup.
institution_counts <- df %>%
  filter(!is.na(AU_UN), AU_UN != "") %>%
  pull(AU_UN) %>%
  strsplit(";") %>%
  unlist() %>%
  trimws() %>%
  # some institutions are not extracted correctly:
  .[. != "" & . != "NOTREPORTED"] %>%
  table() %>%
  as.data.frame() %>%
  setNames(c("institution", "Publications")) %>%
  inner_join(institution_region_lookup, by = "institution") %>%
  mutate(Publications = as.integer(Publications))

# Take top 10 per region.
top_institutions <- institution_counts %>%
  group_by(region) %>%
  arrange(desc(Publications), .by_group = TRUE) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  rename(Region = region)

p_institutions <- ggplot(top_institutions,
                         aes(x = reorder_within(institution, Publications, Region),
                             y = Publications,
                             fill = Region)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = Publications), hjust = -0.2, size = 3) +
  coord_flip() +
  facet_wrap(~Region, scales = "free") +
  scale_x_reordered() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c("Europe" = "#4472C4", "Asia" = "#ED7D31")) +
  labs(
    # title = "Top 10 Institutions by Region",
    x     = NULL,
    y     = "Number of Publications"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave("figures/SQ2_02_top_institutions_regional.png",
       p_institutions, width = 12, height = 6, dpi = 300)


######################## CO-AUTHORSHIP NETWORK BY REGION #######################

# see Appendix 1
# Europe co-authorship network
NetMatrix_europe <- biblioNetwork(df_europe,
                                  analysis   = "collaboration",
                                  network    = "authors",
                                  sep        = ";")

png("figures/SQ2_03_coauthorship_network_europe.png",
    width = 1200, height = 1000, res = 150, bg = "white")
networkPlot(NetMatrix_europe,
            n             = 50,
            Title         = "Co-authorship Network (Europe)",
            size          = TRUE, 
            edgesize      = 3)
dev.off()

# Asia co-authorship network
NetMatrix_asia <- biblioNetwork(df_asia,
                                analysis = "collaboration",
                                network  = "authors",
                                sep      = ";")

png("figures/SQ2_03_coauthorship_network_asia.png",
    width = 1200, height = 1000, res = 150, bg = "white")
networkPlot(NetMatrix_asia,
            n             = 50,
            Title         = "Co-authorship Network (Asia)",
            size          = TRUE,
            edgesize      = 3)
dev.off()



########################### COUNTRY COLLABORATION MAP ##########################

world <- map_data("world")

# Uses capital cities from the maps package as the representative point for 
# each country.
country_coords <- world.cities %>%
  filter(capital == 1) %>%
  # Name corrections because of different Scopus country names.
  mutate(
    country_upper = toupper(country.etc),
    country_upper = case_when(
      country_upper == "UK"          ~ "UNITED KINGDOM",
      country_upper == "USA"         ~ "UNITED STATES",
      country_upper == "RUSSIA"      ~ "RUSSIAN FEDERATION",
      country_upper == "VIETNAM"     ~ "VIET NAM",
      country_upper == "KOREA SOUTH" ~ "SOUTH KOREA",
      country_upper == "KOREA NORTH" ~ "NORTH KOREA",
      TRUE                           ~ country_upper
    )
  ) %>%
  group_by(country_upper) %>%
  # Some countries have multiple entries in world.cities (e.g. Cyprus,
  # Costa Rica). Keep only the most populous entry per country.
  slice_max(pop, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(country_upper, lon = long, lat)

# Country-pair edge list where each pair of countries that co-appear on a
# publication produces one edge. Edges are tagged by collaboration type and 
# joined with coordinates for the plot.
map_edges <- df %>%
  mutate(paper_id = row_number()) %>%
  select(paper_id, country_list) %>%
  unnest(country_list) %>%
  filter(!is.na(country_list), country_list != "") %>%
  group_by(paper_id) %>%
  summarise(countries = list(unique(country_list)), .groups = "drop") %>%
  filter(lengths(countries) >= 2) %>%
  rowwise() %>%
  mutate(edges = list(as.data.frame(t(combn(countries, 2))))) %>%
  unnest(edges) %>%
  rename(country1 = V1, country2 = V2) %>%
  mutate(
    c1_region = case_when(
      country1 %in% europe ~ "Europe",
      country1 %in% asia   ~ "Asia",
      TRUE                 ~ "Other"
    ),
    c2_region = case_when(
      country2 %in% europe ~ "Europe",
      country2 %in% asia   ~ "Asia",
      TRUE                 ~ "Other"
    )
  ) %>%
  count(country1, country2, c1_region, c2_region, name = "weight") %>%
  # Keep only edges involving at least one European or Asian country.
  filter(
    c1_region %in% c("Europe", "Asia") |
      c2_region %in% c("Europe", "Asia")
  ) %>%
  mutate(
    edge_type = case_when(
      (c1_region == "Europe" & c2_region == "Asia") |
        (c1_region == "Asia" & c2_region == "Europe") ~ "Europe\u2013Asia",
      c1_region == "Europe" | c2_region == "Europe"   ~ "Europe\u2013Other",
      c1_region == "Asia"   | c2_region == "Asia"     ~ "Asia\u2013Other",
      TRUE                                            ~ "Other"
    )
  ) %>%
  left_join(country_coords, by = c("country1" = "country_upper")) %>%
  rename(lon1 = lon, lat1 = lat) %>%
  left_join(country_coords, by = c("country2" = "country_upper")) %>%
  rename(lon2 = lon, lat2 = lat) %>%
  filter(!is.na(lon1), !is.na(lat1), !is.na(lon2), !is.na(lat2))

# Plot
p_country_map <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(map_id = region),
    fill = "grey70", colour = "white", linewidth = 0.2
  ) +
  geom_curve(
    data = map_edges,
    aes(
      x = lon1, y = lat1, xend = lon2, yend = lat2,
      linewidth = weight, colour = edge_type
    ),
    curvature = 0.2, alpha = 0.6
  ) +
  coord_quickmap(ylim = c(-55, 80)) +
  scale_linewidth(range = c(0.2, 2)) +
  scale_colour_manual(values = c(
    "Europe\u2013Asia"  = "#2E7D32",
    "Europe\u2013Other" = "#4472C4",
    "Asia\u2013Other"   = "#ED7D31"
  )) +
  labs(
    # title     = "International Collaboration Network",
    # subtitle  = "Includes collaborations with external regions",
    linewidth = "Collaboration frequency",
    colour    = "Collaboration type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave("figures/SQ2_04_country_collaboration_map_global.png",
       p_country_map, width = 12, height = 7, dpi = 300, bg = "white")



# ==============================================================================
# ============ SQ3: DOMINANT RESEARCH THEMES AND THEMATIC PRIORITIES ===========
# ==============================================================================

################################## WORDCLOUDS ##################################

# Minimum number of times a keyword must appear to be included.
min_freq <- 5

# Function to get keyword frequencies (uses DE column, not ID)
get_keyword_freq <- function(data) {
  data %>%
    filter(!is.na(DE), DE != "") %>%
    pull(DE) %>%
    strsplit(";") %>%
    unlist() %>%
    trimws() %>%
    .[. != ""] %>%
    table() %>%
    as.data.frame() %>%
    setNames(c("keyword", "freq")) %>%
    filter(freq >= min_freq) %>%
    arrange(desc(freq))
}

# Europe
freq_europe <- get_keyword_freq(df_europe)

png("figures/SQ3_01_wordcloud_europe.png",
    width = 1200, height = 900, res = 150, bg = "white")
wordcloud(
  words  = freq_europe$keyword,
  freq   = freq_europe$freq,
  min.freq    = min_freq,
  max.words   = 100,
  random.order = FALSE,      # most frequent words in the centre
  rot.per = 0.3,
  colors      = brewer.pal(8, "Dark2"),
  scale       = c(2, 0.4)    # range of word sizes
)
dev.off()

# Asia
freq_asia <- get_keyword_freq(df_asia)

png("figures/SQ3_01_wordcloud_asia.png",
    width = 1200, height = 900, res = 150, bg = "white")
wordcloud(
  words  = freq_asia$keyword,
  freq   = freq_asia$freq,
  min.freq    = min_freq,
  max.words   = 100,
  random.order = FALSE,      # most frequent words in the centre
  rot.per = 0.3,
  colors      = brewer.pal(8, "Dark2"),
  scale       = c(2, 0.4)    # range of word sizes
)
dev.off()


################################# THEMATIC MAPS ################################

# Europe
thematicMap(
  df_europe,
  field = "DE",
  n = 250,
  minfreq = 3,
  stemming = FALSE,
  size = 0.5,
  n.labels = 3,
  repel = TRUE
)

png("figures/SQ3_02_thematic_map_europe.png", width = 1200, height = 900, res = 150)
thematicMap(
  df_europe,
  field = "DE",
  n = 250,
  minfreq = 3,
  stemming = FALSE,
  size = 0.5,
  n.labels = 3,
  repel = TRUE
)
dev.off()

# Asia
thematicMap(
  df_asia,
  field = "DE",
  n = 250,
  minfreq = 3,
  stemming = FALSE,
  size = 0.5,
  n.labels = 3,
  repel = TRUE
)

png("figures/SQ3_02_thematic_map_asia.png", width = 1200, height = 900, res = 150)
thematicMap(
  df_asia,
  field = "DE",
  n = 250,
  minfreq = 3,
  stemming = FALSE,
  size = 0.5,
  n.labels = 3,
  repel = TRUE
)
dev.off()


########################### PQC APPROACHES OVER TIME ###########################

# List of associated keywords for each PQC approach
pqc_keywords <- list(
  "Lattice-based" = c(
    "LATTICE", "LATTICE-BASED", "LATTICE BASED",
    "LEARNING WITH ERRORS", "LWE", "RING-LWE", "RLWE",
    "MODULE-LWE", "MLWE", "KYBER", "CRYSTALS-KYBER",
    "DILITHIUM", "CRYSTALS-DILITHIUM", "NTRU", "SABER"
  ),
  "Code-based" = c(
    "CODE-BASED", "CODE BASED", "MCELIECE", "CLASSIC MCELIECE",
    "GOPPA", "ERROR-CORRECTING CODE", "ERROR CORRECTING CODE"
  ),
  "Hash-based" = c(
    "HASH-BASED", "HASH BASED", "HASH SIGNATURE",
    "HASH-BASED SIGNATURE", "SPHINCS", "SPHINCS+",
    "XMSS", "LMS", "MERKLE"
  ),
  "Multivariate" = c(
    "MULTIVARIATE", "MULTIVARIATE CRYPTOGRAPHY",
    "MULTIVARIATE PUBLIC KEY CRYPTOGRAPHY", "MPKC", "RAINBOW"
  ),
  "Isogeny-based" = c(
    "ISOGENY", "ISOGENY-BASED", "ISOGENY BASED",
    "SUPERSINGULAR ISOGENY", "SIDH", "SIKE"
  )
)

# Function to see which PQC approaches a publication covers based on DE
detect_pqc <- function(de_string) {
  if (is.na(de_string) || de_string == "") return(character(0))
  keyword_string <- toupper(de_string)
  matched <- names(pqc_keywords)[
    sapply(pqc_keywords, function(patterns) {
      any(str_detect(keyword_string, fixed(patterns, ignore_case = TRUE)))
    })
  ]
  unique(matched)
}

# Assign PQC approaches to directly from DE.
df$pqc_approaches <- lapply(df$DE, detect_pqc)

# Refresh regional subsets to include pqc_approaches
df_europe <- df %>% filter(is_europe)
df_asia   <- df %>% filter(is_asia)

# Build a publication count by year, region, and PQC approach.
# (inclusive counting applies again)
pqc_trends <- df %>%
  filter(!is.na(PY), PY >= year_min, PY <= year_max) %>%
  select(PY, pqc_approaches, is_europe, is_asia) %>%
  tidyr::unnest(pqc_approaches) %>%
  filter(!is.na(pqc_approaches), pqc_approaches != "") %>%
  pivot_longer(
    cols      = c(is_europe, is_asia),
    names_to  = "Region",
    values_to = "included"
  ) %>%
  filter(included == TRUE) %>%
  mutate(
    Region = case_when(
      Region == "is_europe" ~ "Europe",
      Region == "is_asia"   ~ "Asia"
    )
  ) %>%
  count(PY, Region, pqc_approaches, name = "Publications") %>%
  rename(Year = PY, Approach = pqc_approaches)

# pqc_trends only contains rows where at least one publication exists.
# Fills missing combinations with 0 so all lines.
pqc_trends_complete <- pqc_trends %>%
  filter(Year < 2026) %>%
  tidyr::complete(
    Year     = year_min:2025,
    Region   = c("Europe", "Asia"),
    Approach = names(pqc_keywords),
    fill     = list(Publications = 0)
  )

# Plot
p_pqc_trends <- ggplot(
  pqc_trends_complete,
  aes(x = Year, y = Publications, colour = Approach, group = Approach)
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~Region) +
  scale_x_continuous(breaks = seq(year_min, 2025, 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    # title    = "PQC Approaches Over Time by Region",
    # subtitle = "Inclusive counting: mixed publications are counted in each relevant region",
    x        = "Year",
    y        = "Number of Publications",
    colour   = "PQC approach"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1),
    strip.text      = element_text(face = "bold", size = 14)
  )

ggsave("figures/SQ3_03_pqc_approaches_over_time.png",
       p_pqc_trends, width = 13, height = 7, dpi = 300, bg = "white")