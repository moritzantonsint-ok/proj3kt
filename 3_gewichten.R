

# 3. GEWICHTE NACHLADEN aus Datenbank =========================================
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  port     = NULL,
  options  = Sys.getenv("DB_OPTIONS")
)

# Personengewicht (cross-sectional)
weights_p <- tbl(con, "c17p") %>%
  filter(pb020 == "AT") %>%
  select(pb030, pb040, pb060) %>%
  collect(n = Inf) %>%
  mutate(pb060 = suppressWarnings(as.numeric(pb060)))   # chr → num

# Haushaltsgewicht (cross-sectional)
weights_d <- tbl(con, "c17d") %>%
  filter(db020 == "AT") %>%
  select(db030, db090) %>%
  collect(n = Inf)

DBI::dbDisconnect(con)

# 4. silcw ERSTELLEN & GEWICHTE ZUWEISEN ======================================
silcw <- silc %>%
  left_join(weights_p, by = c("Pid_pb030" = "pb030")) %>%
  left_join(weights_d, by = c("Hid_px030" = "db030")) %>%
  mutate(
    pw          = pb040,                                     # Person, Querschnitt
    pw_selected = if_else(!is.na(pb060), pb060, pb040),      # Selected Resp. (PH010)
    hw          = db090                                      # Haushalt, Querschnitt
  )

# 5. SURVEY-DESIGNS ============================================================
design_person   <- svydesign(ids = ~1, weights = ~pw,
                              data = silcw %>% filter(!is.na(pw)))
design_selected <- svydesign(ids = ~1, weights = ~pw_selected,
                              data = silcw %>% filter(!is.na(pw_selected)))
design_hh       <- svydesign(ids = ~1, weights = ~hw,
                              data = silcw %>% distinct(Hid_px030, .keep_all = TRUE) %>%
                                              filter(!is.na(hw)))

# 6. ÜBERBLICK & SPEICHERN =====================================================
cat("=== silcw ===\n")
cat("Zeilen:", nrow(silcw), " Spalten:", ncol(silcw), "\n")
cat("Hochrechnung Bevölkerung (pw):",
    format(round(sum(silcw$pw, na.rm = TRUE)), big.mark = "."), "\n")
cat("Hochrechnung Haushalte (hw):",
    format(round(sum(silcw %>% distinct(Hid_px030, .keep_all = TRUE) %>% pull(hw),
                     na.rm = TRUE)), big.mark = "."), "\n")

saveRDS(silcw, "silcw.rds")
