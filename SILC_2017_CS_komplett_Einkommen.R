
# =============================================================================
# EU-SILC 2017 Cross-Sectional — Österreich
# Download → Merge → Eltern-Verknüpfung → Faktorisierung → Imputation →
# 3x Random Forest (Netto-Kombi, Nettolohn, Bruttolohn) → Variable Importance
# Streng nach Codebuch EU-SILC 2017 cross-sec (GESIS)
# =============================================================================

rm(list = ls()); gc()
 packages <- c("DBI","RPostgres","tidyverse","ranger","mice","caret","patchwork")
sapply(packages, library, character.only = TRUE)
set.seed(42)

# =============================================================================
# 1. DOWNLOAD (c17p, c17r, c17h, c17d)
# =============================================================================
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  port     = NULL,
  options  = Sys.getenv("DB_OPTIONS")
)

cat("Lade c17p ...\n")
df_p <- tbl(con, "c17p") %>% filter(pb020 == "AT") %>% collect(n = Inf)
cat("Lade c17r ...\n")
df_r <- tbl(con, "c17r") %>% filter(rb020 == "AT") %>% collect(n = Inf)
cat("Lade c17h ...\n")
df_h <- tbl(con, "c17h") %>% filter(hb020 == "AT") %>% collect(n = Inf)
cat("Lade c17d ...\n")
df_d <- tbl(con, "c17d") %>% filter(db020 == "AT") %>% collect(n = Inf)
DBI::dbDisconnect(con)

cat("P:", nrow(df_p), "×", ncol(df_p),
    " R:", nrow(df_r), "×", ncol(df_r),
    " H:", nrow(df_h), "×", ncol(df_h),
    " D:", nrow(df_d), "×", ncol(df_d), "\n\n")

# Spalten auflisten (Diagnose)
cat("P-DATA Spalten:\n"); print(names(df_p))
cat("\nR-DATA Spalten:\n"); print(names(df_r))
cat("\nH-DATA Spalten:\n"); print(names(df_h))
cat("\nD-DATA Spalten:\n"); print(names(df_d))

# =============================================================================
# 2. MERGE
# =============================================================================
silc <- df_p %>%
  left_join(df_r %>% select(-any_of(c("rb010","rb020"))),
            by = c("pb030" = "rb030")) %>%
  left_join(df_h %>% select(-any_of(c("hb010","hb020"))),
            by = c("px030" = "hb030")) %>%
  left_join(df_d %>% select(-any_of(c("db010","db020"))),
            by = c("px030" = "db030"))

cat("Merged:", nrow(silc), "×", ncol(silc), "\n\n")

# =============================================================================
# 3. VARIABLEN AUSWÄHLEN (streng nach Codebuch 2017 cross-sec)
# =============================================================================
vars_wanted <- c(
  # --- IDs ---
  "pb010","pb020","pb030","px030",
  # --- Gewichte (Codebuch 2017 CS) ---
  "pb040","pb040_f",                   # Personal CS weight
  "pb060","pb060_f",                   # Personal CS weight selected respondent
  "rb050","rb050_f",                   # Personal CS weight (R-file)
  "db090","db090_f",                   # Household CS weight
  # --- Demografie ---
  "pb190","pb190_f",                   # Marital status
  "pb210","pb210_f",                   # Country of birth (LOC/EU/OTH)
  "pb220a","pb220a_f",                 # Citizenship
  # --- Bildung ---
  "pe040","pe040_f",                   # Highest ISCED
  # --- Arbeitsmarkt ---
  "pl031","pl031_f",                   # Self-defined econ. status
  "pl040","pl040_f",                   # Status in employment
  "pl051","pl051_f",                   # ISCO-08
  "pl060","pl060_f",                   # Hours/week
  "pl111","pl111_f",                   # NACE Rev.2
  "pl130","pl130_f",                   # Firm size
  "pl150","pl150_f",                   # Managerial position
  "pl200","pl200_f",                   # Years paid work
  # --- Gesundheit ---
  "ph010","ph010_f",                   # General health
  # --- Einkommen Person NETTO ---
  "py010n","py010n_f","py010n_i",      # Employee cash net
  "py020n","py020n_f","py020n_i",      # Non-cash employee net
  "py050n","py050n_f","py050n_i",      # Self-employment net
  # --- Einkommen Person BRUTTO ---
  "py010g","py010g_f","py010g_i",      # Employee cash gross
  # --- Einkommen Haushalt ---
  "hy040n","hy040n_f","hy040n_i",      # Rental income net
  "hy090n","hy090n_f","hy090n_i",      # Interest/dividends net
  "hy120n","hy120n_f",                 # Taxes on wealth
  # --- Register ---
  "rb080","rb080_f",                   # Year of birth
  "rb090","rb090_f",                   # Sex
  "rb210","rb210_f",                   # Basic activity status
  "rb220","rb220_f",                   # Father ID
  "rb230","rb230_f",                   # Mother ID
  # --- Childcare (RL-Modul) ---
  "rl030","rl030_f",                   # Centre-based h/week
  "rl040","rl040_f",                   # Day-care h/week
  "rl050","rl050_f",                   # Prof. child-minder h/week
  "rl060","rl060_f",                   # Grandparents h/week
  # --- Wohnung ---
  "hh010","hh010_f",                   # Dwelling type
  "hh030","hh030_f",                   # Number rooms
  "hh050","hh050_f",                   # Keep home warm
  "hh060","hh060_f",                   # Current rent
  # --- Region & Urbanisierung ---
  "db040","db040_f",                   # Region (AT1/AT2/AT3)
  "db100","db100_f",                   # Urbanisation
  # --- Haushalt abgeleitet ---
  "hx040","hx050","hx060","hx090"
)

silc <- silc %>% select(any_of(vars_wanted))
cat("Variablen ausgewählt:", ncol(silc), "\n")
cat("Fehlend:", paste(setdiff(vars_wanted, names(silc)), collapse = ", "), "\n\n")

# =============================================================================
# 4. GEWICHTE & NUMERISCHE KONVERTIERUNGEN
# =============================================================================
silc <- silc %>%
  mutate(
    across(any_of("pb060"), ~suppressWarnings(as.numeric(.))),
    pw          = coalesce(pb040, rb050),
    pw_selected = coalesce(suppressWarnings(as.numeric(pb060)), pb040, rb050),
    hw          = db090
  )

cat("pw  mean:", round(mean(silc$pw, na.rm=T), 1),
    "| sum:", format(round(sum(silc$pw, na.rm=T)), big.mark="."), "\n")
cat("hw  mean:", round(mean(silc$hw, na.rm=T), 1), "\n\n")

# =============================================================================
# 5. ZIELVARIABLEN ERSTELLEN
# =============================================================================
# Numerisch sicherstellen
inc_vars <- c("py010n","py020n","py050n","hy040n","hy090n","py010g")
for (v in inc_vars) {
  if (v %in% names(silc)) silc[[v]] <- suppressWarnings(as.numeric(silc[[v]]))
}

silc <- silc %>%
  mutate(
    # Zielvariable 1: Netto-Kombi
    Y_netto_kombi = rowSums(across(any_of(c("py010n","py020n","py050n","hy040n","hy090n")),
                                    ~replace_na(., 0))),
    # Zielvariable 2: Nettolohn
    Y_netto = py010n,
    # Zielvariable 3: Bruttolohn
    Y_brutto = py010g,
    # Alter
    Age = 2017 - rb080
  )

cat("Y_netto_kombi: mean =", round(mean(silc$Y_netto_kombi, na.rm=T)),
    "| median =", round(median(silc$Y_netto_kombi, na.rm=T)), "\n")
cat("Y_netto:       mean =", round(mean(silc$Y_netto, na.rm=T)),
    "| median =", round(median(silc$Y_netto, na.rm=T)), "\n")
cat("Y_brutto:      mean =", round(mean(silc$Y_brutto, na.rm=T)),
    "| median =", round(median(silc$Y_brutto, na.rm=T)), "\n\n")

# =============================================================================
# 6. ELTERN-KIND-VERKNÜPFUNG (via RB220 = Vater-ID, RB230 = Mutter-ID)
# =============================================================================
cat("=== Eltern-Kind-Verknüpfung ===\n")

# Info pro Person für Eltern-Lookup
person_lookup <- silc %>%
  select(pid = pb030,
         any_of(c("pe040","py010n","py010g","pl051","pl111",
                   "Y_netto_kombi","Y_netto","Y_brutto")))

# --- VATER ---
if ("rb220" %in% names(silc)) {
  vater <- person_lookup %>%
    rename_with(~paste0("vater_", .), -pid) %>%
    rename(vater_id = pid)
  
  silc <- silc %>% left_join(vater, by = c("rb220" = "vater_id"))
  cat("Vater-Match:", sum(!is.na(silc$vater_Y_netto), na.rm=T), "von", nrow(silc), "\n")
}

# --- MUTTER ---
if ("rb230" %in% names(silc)) {
  mutter <- person_lookup %>%
    rename_with(~paste0("mutter_", .), -pid) %>%
    rename(mutter_id = pid)
  
  silc <- silc %>% left_join(mutter, by = c("rb230" = "mutter_id"))
  cat("Mutter-Match:", sum(!is.na(silc$mutter_Y_netto), na.rm=T), "von", nrow(silc), "\n")
}
cat("\n")

# =============================================================================
# 7. FAKTORISIERUNG (streng nach Codebuch 2017 CS)
# =============================================================================
cat("Faktorisiere ...\n")
safe_factor <- function(df, col, levels, labels) {
  if (col %in% names(df)) df[[col]] <- factor(df[[col]], levels = levels, labels = labels)
  df
}

# PE040: ISCED (2014+ Kodierung)
silc <- safe_factor(silc, "pe040",
  c(0,100,200,300,344,354,400,450,500,600,700,800),
  c("Less than primary","Primary","Lower secondary","Upper secondary",
    "Upper sec with tertiary access","Upper sec general with tertiary",
    "Post-secondary non-tertiary","Short-cycle tertiary","Tertiary",
    "Bachelor","Master","Doctorate"))

# PL031: Employment status
silc <- safe_factor(silc, "pl031", 1:11,
  c("Employee FT","Employee PT","Self-empl FT","Self-empl PT",
    "Unemployed","Student","Retired","Disabled",
    "Military","Domestic/care","Other inactive"))

# PL040: Status in employment
silc <- safe_factor(silc, "pl040", c(1,2,3),
  c("Self-employed with employees","Self-employed without employees","Employee"))

# PL111: NACE Rev.2 (letter codes in 2017 CS!)
silc <- safe_factor(silc, "pl111",
  c("a","b-e","f","g","h","i","j","k","l-n","o","p","q","r-u"),
  c("Agriculture","Industry","Construction","Wholesale/Retail",
    "Transport","Accommodation/Food","Information/Comm","Finance/Insurance",
    "Professional/Admin","Public admin","Education","Health/Social",
    "Arts/Other services"))

# PL130: Firm size (Codes 1-15 lt. Codebuch)
silc <- safe_factor(silc, "pl130", 1:15,
  c("1 person","2-4","5-9","10","11-19","20-49",
    "50-99","100-149","150-199","200-499","500+",
    "11-19 persons","20-49 persons","50+ persons",
    "Don't know"))

# PL150: Managerial position
silc <- safe_factor(silc, "pl150", c(1,2), c("Supervisory","Non-supervisory"))

# PB190: Marital status
silc <- safe_factor(silc, "pb190", c(1,2,3,4,5),
  c("Never married","Married","Separated","Widowed","Divorced"))

# PB210: Country of birth
silc <- safe_factor(silc, "pb210", c("LOC","EU","OTH"),
  c("Born in AT","Born in EU","Born outside EU"))

# RB090: Sex
silc <- safe_factor(silc, "rb090", c(1,2), c("Male","Female"))

# RB210: Activity status
silc <- safe_factor(silc, "rb210", c(1,2,3,4),
  c("At work","Unemployed","Retired","Other inactive"))

# PH010: General health
silc <- safe_factor(silc, "ph010", 1:5,
  c("Very good","Good","Fair","Bad","Very bad"))

# DB100: Urbanisation
silc <- safe_factor(silc, "db100", c(1,2,3),
  c("Densely populated","Intermediate","Thinly populated"))

# DB040: Region (AT-spezifisch)
silc <- safe_factor(silc, "db040", c("AT1","AT2","AT3"),
  c("Ostösterreich","Südösterreich","Westösterreich"))

# HH010: Dwelling type
silc <- safe_factor(silc, "hh010", c(1,2,3,4),
  c("Detached house","Semi-detached","Apartment <10","Apartment 10+"))

# HH050: Keep warm
silc <- safe_factor(silc, "hh050", c(1,2), c("Yes","No"))

# PL051: ISCO-08 → 1-stellig
if ("pl051" %in% names(silc)) {
  silc$isco_1digit <- factor(
    case_when(
      silc$pl051 == 0                          ~ 0L,
      silc$pl051 >= 1  & silc$pl051 < 20       ~ 1L,
      silc$pl051 >= 20 & silc$pl051 < 30       ~ 2L,
      silc$pl051 >= 30 & silc$pl051 < 40       ~ 3L,
      silc$pl051 >= 40 & silc$pl051 < 50       ~ 4L,
      silc$pl051 >= 50 & silc$pl051 < 60       ~ 5L,
      silc$pl051 >= 60 & silc$pl051 < 70       ~ 6L,
      silc$pl051 >= 70 & silc$pl051 < 80       ~ 7L,
      silc$pl051 >= 80 & silc$pl051 < 90       ~ 8L,
      silc$pl051 >= 90 & silc$pl051 < 100      ~ 9L,
      TRUE ~ NA_integer_),
    levels = 0:9,
    labels = c("Armed forces","Managers","Professionals","Technicians",
               "Clerical","Service/Sales","Agriculture skilled",
               "Craft","Machine operators","Elementary"))
}

cat("Faktorisierung abgeschlossen.\n\n")

# =============================================================================
# 8. RF-DATEN VORBEREITEN
# =============================================================================
safe_col <- function(df, col) if (col %in% names(df)) df[[col]] else NA

rf_base <- silc %>%
  filter(rb210 == "At work") %>%
  transmute(
    # --- 3 Zielvariablen ---
    Y_netto_kombi = Y_netto_kombi,
    Y_netto       = Y_netto,
    Y_brutto      = Y_brutto,
    # --- Demografie ---
    Age            = Age,
    Gender         = safe_col(., "rb090"),
    Marital        = safe_col(., "pb190"),
    Country_Birth  = safe_col(., "pb210"),
    Citizenship    = safe_col(., "pb220a"),
    Health         = safe_col(., "ph010"),
    # --- Bildung ---
    Educ           = safe_col(., "pe040"),
    # --- Arbeitsmarkt ---
    Empl_Status    = safe_col(., "pl031"),
    Empl_Type      = safe_col(., "pl040"),
    Occupation     = safe_col(., "isco_1digit"),
    Branch_NACE    = safe_col(., "pl111"),
    Hours          = safe_col(., "pl060"),
    FirmSize       = safe_col(., "pl130"),
    Managerial     = safe_col(., "pl150"),
    Experience     = safe_col(., "pl200"),
    # --- Eltern (direkte Verknüpfung) ---
    Income_Father  = safe_col(., "vater_Y_netto"),
    Income_Mother  = safe_col(., "mutter_Y_netto"),
    Educ_Father    = safe_col(., "vater_pe040"),
    Educ_Mother    = safe_col(., "mutter_pe040"),
    Occup_Father   = safe_col(., "vater_pl051"),
    Occup_Mother   = safe_col(., "mutter_pl051"),
    Branch_Father  = safe_col(., "vater_pl111"),
    Branch_Mother  = safe_col(., "mutter_pl111"),
    # --- Haushalt ---
    Urbanisation   = safe_col(., "db100"),
    Region         = safe_col(., "db040"),
    Dwelling       = safe_col(., "hh010"),
    Rooms          = safe_col(., "hh030"),
    KeepWarm       = safe_col(., "hh050"),
    Rent           = safe_col(., "hh060"),
    Tax_Wealth     = safe_col(., "hy120n"),
    HH_Size        = safe_col(., "hx040"),
    HH_Type        = safe_col(., "hx060"),
    # --- Childcare ---
    Childcare_Centre = safe_col(., "rl030"),
    Childcare_Daycare = safe_col(., "rl040"),
    Childcare_Minder = safe_col(., "rl050"),
    Childcare_Grandp = safe_col(., "rl060"),
    # --- Gewicht ---
    weight         = pw
  )

# Komplett leere Spalten entfernen
all_na <- sapply(rf_base, function(x) all(is.na(x)))
if (any(all_na)) {
  cat("Nicht vorhanden (entfernt):\n  ", paste(names(all_na[all_na]), collapse="\n  "), "\n\n")
  rf_base <- rf_base[, !all_na]
}

cat("RF-Basis:", nrow(rf_base), "×", ncol(rf_base), "\n")
cat("Variablen im Modell:\n  ",
    paste(setdiff(names(rf_base), c("Y_netto_kombi","Y_netto","Y_brutto","weight")),
          collapse="\n  "), "\n\n")

# =============================================================================
# 9. IMPUTATION (alles AUSSER Zielvariablen & Gewicht)
# =============================================================================
cat("=== Imputation ===\n")

targets_and_weight <- c("Y_netto_kombi","Y_netto","Y_brutto","weight")
imp_data <- rf_base %>% select(-all_of(targets_and_weight))

missing_pct <- colMeans(is.na(imp_data))
vars_too_much <- names(missing_pct[missing_pct >= 0.8])
vars_to_impute <- names(missing_pct[missing_pct > 0 & missing_pct < 0.8])

cat("Komplett:",       sum(missing_pct == 0), "\n")
cat("Zu imputieren:",  length(vars_to_impute), "\n")
cat(">80% Missing:",   length(vars_too_much), " →",
    paste(vars_too_much, collapse=", "), "\n")

rf_base <- rf_base %>% select(-any_of(vars_too_much))
imp_data <- imp_data %>% select(-any_of(vars_too_much))

if (length(vars_to_impute) > 0) {
  imp <- mice(imp_data, m = 1, method = "pmm", maxit = 5,
               printFlag = FALSE, seed = 42)
  imp_complete <- complete(imp, 1)
  rf_base[, names(imp_complete)] <- imp_complete
  cat("Imputation abgeschlossen.\n\n")
} else {
  cat("Keine Imputation nötig.\n\n")
}

# =============================================================================
# 10. DREI RANDOM FORESTS TRAINIEREN
# =============================================================================
# Prädiktoren (alles ausser Zielvariablen und Gewicht)
predictors <- setdiff(names(rf_base), targets_and_weight)

run_rf <- function(data, target_name, predictors) {
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("RF:", target_name, "\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  
  # Filtern: Zielvariable > 0 und nicht NA
  df <- data %>%
    filter(.data[[target_name]] > 0, !is.na(.data[[target_name]])) %>%
    drop_na(weight)
  
  cat("n =", nrow(df), "\n")
  
  # Train/Test
  idx <- createDataPartition(df[[target_name]], p = 0.8, list = FALSE)
  train <- df[idx, ]
  test  <- df[-idx, ]
  
  # Formel
  f <- as.formula(paste(target_name, "~", paste(predictors, collapse = " + ")))
  
  # Random Forest
  rf <- ranger(
    formula        = f,
    data           = train,
    num.trees      = 500,
    mtry           = floor(sqrt(length(predictors))),
    min.node.size  = 5,
    importance     = "permutation",
    case.weights   = train$weight,
    seed           = 42
  )
  
  # Performance
  pred <- predict(rf, data = test)$predictions
  r2   <- 1 - sum((test[[target_name]] - pred)^2) /
              sum((test[[target_name]] - mean(test[[target_name]]))^2)
  rmse <- sqrt(mean((test[[target_name]] - pred)^2))
  mae  <- mean(abs(test[[target_name]] - pred))
  
  cat("R² (Test):", round(r2, 3), "\n")
  cat("R² (OOB): ", round(rf$r.squared, 3), "\n")
  cat("RMSE:     ", format(round(rmse), big.mark="."), "EUR\n")
  cat("MAE:      ", format(round(mae), big.mark="."), "EUR\n")
  
  list(model = rf, r2 = r2, rmse = rmse, mae = mae,
       train = train, test = test, pred = pred, target = target_name)
}

# Die 3 Modelle
res_kombi  <- run_rf(rf_base, "Y_netto_kombi", predictors)
res_netto  <- run_rf(rf_base, "Y_netto",       predictors)
res_brutto <- run_rf(rf_base, "Y_brutto",      predictors)

# =============================================================================
# 11. VARIABLE IMPORTANCE — VERGLEICH ALLER 3 MODELLE
# =============================================================================
cat("\n\n=== VARIABLE IMPORTANCE ===\n\n")

make_imp_df <- function(res) {
  tibble(
    Variable   = names(res$model$variable.importance),
    Importance = res$model$variable.importance
  ) %>%
    arrange(desc(Importance)) %>%
    mutate(
      Rank    = row_number(),
      Rel_Pct = round(Importance / sum(Importance) * 100, 1),
      Category = case_when(
        Variable %in% c("Age","Gender","Marital","Health",
                         "Country_Birth","Citizenship")       ~ "Demografie & Herkunft",
        Variable %in% c("Educ")                               ~ "Bildung",
        Variable %in% c("Hours","Experience","Occupation",
                         "Branch_NACE","FirmSize","Managerial",
                         "Empl_Status","Empl_Type")            ~ "Arbeitsmarkt",
        Variable %in% c("Income_Father","Income_Mother",
                         "Educ_Father","Educ_Mother",
                         "Occup_Father","Occup_Mother",
                         "Branch_Father","Branch_Mother")      ~ "Eltern",
        Variable %in% c("Urbanisation","Region","Dwelling",
                         "Rooms","KeepWarm","Rent","Tax_Wealth",
                         "HH_Size","HH_Type")                  ~ "Haushalt & Wohnung",
        Variable %in% c("Childcare_Centre","Childcare_Daycare",
                         "Childcare_Minder","Childcare_Grandp") ~ "Kinderbetreuung",
        TRUE                                                    ~ "Sonstiges"
      ),
      Model = res$target
    )
}

imp_kombi  <- make_imp_df(res_kombi)
imp_netto  <- make_imp_df(res_netto)
imp_brutto <- make_imp_df(res_brutto)

# Tabellen anzeigen
cat("--- Netto-Kombi (Top 15) ---\n")
print(imp_kombi %>% select(Rank, Variable, Category, Rel_Pct) %>% head(15))
cat("\n--- Nettolohn (Top 15) ---\n")
print(imp_netto %>% select(Rank, Variable, Category, Rel_Pct) %>% head(15))
cat("\n--- Bruttolohn (Top 15) ---\n")
print(imp_brutto %>% select(Rank, Variable, Category, Rel_Pct) %>% head(15))

# =============================================================================
# 12. PLOTS
# =============================================================================
dir.create("plots", showWarnings = FALSE)

# --- 12a. Importance pro Modell ---
plot_importance <- function(imp_df, title_suffix, r2) {
  top <- imp_df %>% slice_max(Importance, n = 20)
  ggplot(top, aes(x = reorder(Variable, Importance), y = Rel_Pct, fill = Category)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(Rel_Pct, "%")), hjust = -0.1, size = 3) +
    coord_flip() +
    scale_fill_brewer(palette = "Set2") +
    expand_limits(y = max(top$Rel_Pct) * 1.15) +
    labs(title = paste("Variable Importance:", title_suffix),
         subtitle = paste0("R² = ", round(r2, 3)),
         x = NULL, y = "Relative Importance (%)", fill = "Kategorie") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

p_kombi  <- plot_importance(imp_kombi,  "Netto-Kombi (PY010N+PY020N+PY050N+HY040N+HY090N)", res_kombi$r2)
p_netto  <- plot_importance(imp_netto,  "Nettolohn (PY010N)", res_netto$r2)
p_brutto <- plot_importance(imp_brutto, "Bruttolohn (PY010G)", res_brutto$r2)

print(p_kombi)
print(p_netto)
print(p_brutto)

ggsave("plots/vimp_netto_kombi.png", p_kombi, width=12, height=8, dpi=300, bg="white")
ggsave("plots/vimp_netto.png",       p_netto, width=12, height=8, dpi=300, bg="white")
ggsave("plots/vimp_brutto.png",      p_brutto, width=12, height=8, dpi=300, bg="white")

# --- 12b. Importance nach Kategorie (alle 3 Modelle nebeneinander) ---
cat_all <- bind_rows(imp_kombi, imp_netto, imp_brutto) %>%
  group_by(Model, Category) %>%
  summarise(Pct = sum(Rel_Pct), .groups = "drop") %>%
  mutate(Model = case_when(
    Model == "Y_netto_kombi" ~ "Netto-Kombi",
    Model == "Y_netto"       ~ "Nettolohn",
    Model == "Y_brutto"      ~ "Bruttolohn"
  ))

p_cat <- ggplot(cat_all, aes(x = reorder(Category, Pct), y = Pct, fill = Model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c("Netto-Kombi" = "steelblue",
                                "Nettolohn" = "darkorange",
                                "Bruttolohn" = "darkgreen")) +
  labs(title = "Importance nach Kategorie — Vergleich aller 3 Modelle",
       x = NULL, y = "Anteil (%)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p_cat)
ggsave("plots/vimp_kategorie_vergleich.png", p_cat, width=12, height=7, dpi=300, bg="white")

# --- 12c. Top 10 Vergleich nebeneinander ---
top10_all <- bind_rows(imp_kombi, imp_netto, imp_brutto) %>%
  group_by(Model) %>%
  slice_max(Importance, n = 10) %>%
  mutate(Model = case_when(
    Model == "Y_netto_kombi" ~ "Netto-Kombi",
    Model == "Y_netto"       ~ "Nettolohn",
    Model == "Y_brutto"      ~ "Bruttolohn"
  ))

p_top10 <- ggplot(top10_all, aes(x = reorder(Variable, Rel_Pct), y = Rel_Pct)) +
  geom_segment(aes(xend = Variable, y = 0, yend = Rel_Pct, color = Category),
               linewidth = 1.2) +
  geom_point(aes(color = Category), size = 3) +
  geom_text(aes(label = paste0(Rel_Pct, "%")), hjust = -0.3, size = 2.8) +
  coord_flip() +
  facet_wrap(~Model, scales = "free_y") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Top 10 Prädiktoren — Vergleich der 3 Lohnmodelle",
       subtitle = "EU-SILC 2017 AT | Random Forest mit Survey-Gewichtung (PB040)",
       x = NULL, y = "Relative Importance (%)", color = "Kategorie") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

print(p_top10)
ggsave("plots/vimp_top10_vergleich.png", p_top10, width=16, height=8, dpi=300, bg="white")

# =============================================================================
# 13. PERFORMANCE-VERGLEICH
# =============================================================================
perf <- tibble(
  Modell = c("Netto-Kombi", "Nettolohn (PY010N)", "Bruttolohn (PY010G)"),
  `R² (Test)` = c(res_kombi$r2, res_netto$r2, res_brutto$r2),
  `R² (OOB)`  = c(res_kombi$model$r.squared, res_netto$model$r.squared, res_brutto$model$r.squared),
  RMSE = c(res_kombi$rmse, res_netto$rmse, res_brutto$rmse),
  MAE  = c(res_kombi$mae, res_netto$mae, res_brutto$mae),
  n    = c(nrow(res_kombi$train) + nrow(res_kombi$test),
           nrow(res_netto$train) + nrow(res_netto$test),
           nrow(res_brutto$train) + nrow(res_brutto$test))
)

cat("\n\n=== PERFORMANCE-VERGLEICH ===\n")
print(perf)

# =============================================================================
# 14. SPEICHERN
# =============================================================================
saveRDS(silc, "silc_2017_CS_AT.rds")
saveRDS(list(kombi = res_kombi, netto = res_netto, brutto = res_brutto), "rf_models_2017.rds")
saveRDS(list(kombi = imp_kombi, netto = imp_netto, brutto = imp_brutto), "vimp_2017.rds")

cat("\n✓ Gespeichert:\n")
cat("  silc_2017_CS_AT.rds    — Haupttabelle\n")
cat("  rf_models_2017.rds     — 3 RF-Modelle\n")
cat("  vimp_2017.rds          — Variable Importance\n")
cat("  plots/vimp_*.png       — Alle Plots\n")
