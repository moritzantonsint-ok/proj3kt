# =============================================================================
# EU-SILC 2017 Querschnitt — Österreich
# Streng nach Codebuch EU-SILC 2017 panel (GESIS, ver_2020_03)
# =============================================================================

rm(list = ls()); gc()
packages <- c("DBI", "RPostgres", "tidyverse")
sapply(packages, library, character.only = TRUE)

# 1. VERBINDUNG ================================================================
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  port     = NULL,
  options  = Sys.getenv("DB_OPTIONS")
)

# 2. DOWNLOAD DER 4 DATEIEN (c17p, c17r, c17h, c17d) =========================

# --- P-DATA ---
df_p <- tbl(con, "c17p") %>%
  filter(pb020 == "AT") %>%
  collect(n = Inf)

# --- P-REG ---
df_r <- tbl(con, "c17r") %>%
  filter(rb020 == "AT") %>%
  collect(n = Inf)

# --- HH-DATA ---
df_h <- tbl(con, "c17h") %>%
  filter(hb020 == "AT") %>%
  collect(n = Inf)

# --- HH-REG ---
df_d <- tbl(con, "c17d") %>%
  filter(db020 == "AT") %>%
  collect(n = Inf)

DBI::dbDisconnect(con)

# 3. MERGE =====================================================================
# P-DATA + P-REG über Personen-ID
silc <- left_join(df_p, df_r, by = c("pb030" = "rb030"))

# + HH-DATA über Haushalts-ID (px030 = hb030)
silc <- left_join(silc, df_h, by = c("px030" = "hb030"))

# + HH-REG über Haushalts-ID (px030 = db030)
silc <- left_join(silc, df_d, by = c("px030" = "db030"))

# Nur gewünschte Variablen behalten (die tatsächlich existieren)
vars_to_keep <- c(
  # IDs & Jahr
  "pb010", "pb020", "pb030", "px030", "rb040",
  # Gewichte (nur die vorhandenen)
  "pb050", "pb050_f", "rb060", "rb060_f", "db095", "db095_f",
  # Einkommen
  "py010g", "py010g_f", "py010g_i",
  # Bildung & Beruf
  "pe040", "pe040_f", "pl031", "pl031_f", "pl051", "pl051_f",
  "pl060", "pl060_f", "pl140", "pl140_f", "pl200", "pl200_f",
  # Demografie
  "rb080", "rb080_f", "rb090", "rb090_f", "pb190", "pb190_f",
  # Gesundheit & Status
  "ph010", "ph010_f", "rb210", "rb210_f",
  # Haushalt
  "hh021", "hh021_f", "hx040", "hx050", "hx090", "hx100",
  "db100", "db100_f"
)

silc <- silc %>% select(any_of(vars_to_keep))

# 4. VARIABLEN-DOKUMENTATION ==================================================
# ┌────┬────────────┬──────────────────────────────────────────────┬───────────┐
# │ #  │ Variable   │ Inhalt (Codebuch)                           │ Datei     │
# ├────┼────────────┼──────────────────────────────────────────────┼───────────┤
# │  0 │ py010g     │ Employee cash or near cash income (gross)   │ P-DATA    │
# │  1 │ pe040      │ Highest ISCED level attained                │ P-DATA    │
# │  2 │ pl031      │ Self-defined current economic status        │ P-DATA    │
# │  3 │ pl051      │ Occupation (ISCO-08)                        │ P-DATA    │
# │  5 │ rb080      │ Year of birth                               │ P-REG     │
# │  6 │ rb090      │ Sex (1=male, 2=female)                      │ P-REG     │
# │  7 │ pl060      │ Number of hours usually worked per week     │ P-DATA    │
# │  8 │ pl200      │ Number of years spent in paid work          │ P-DATA    │
# │  9 │ pl140      │ Type of contract (1=unbefr., 2=befr.)      │ P-DATA    │
# │ 13 │ pb190      │ Marital status                              │ P-DATA    │
# │ 14 │ ph010      │ General health (1–5)                        │ P-DATA    │
# │ 15 │ rb210      │ Basic activity status                       │ P-REG     │
# │ 18 │ hh021      │ Tenure status                               │ HH-DATA   │
# │ 19 │ db100      │ Degree of urbanisation                      │ HH-REG    │
# │ 20 │ hx040      │ Household size                              │ HH-DATA   │
# ├────┼────────────┼──────────────────────────────────────────────┼───────────┤
# │    │ pb050      │ Personal base weight (Querschnitt)          │ P-DATA    │
# │    │ rb060      │ Personal base weight (Panel)                │ P-REG     │
# │    │ db095      │ Household longitudinal weight               │ HH-REG    │
# ├────┴────────────┴──────────────────────────────────────────────┴───────────┤
# │ NICHT IM CODEBUCH:                                                        │
# │   PL111 (NACE), PL130 (Betriebsgröße), PB210 (Geburtsland),              │
# │   PB220A (Staatsbürgerschaft), HB110 (n/a), HB120 (n/a)                  │
# └───────────────────────────────────────────────────────────────────────────────┘

# 5. ÜBERBLICK =================================================================
cat("\n=== EU-SILC 2017 Querschnitt — Österreich ===\n")
cat("Zeilen: ", nrow(silc), "\n")
cat("Spalten:", ncol(silc), "\n\n")
glimpse(silc)

# 6. SPEICHERN =================================================================
saveRDS(silc, "silc_at_2017.rds")
#:)