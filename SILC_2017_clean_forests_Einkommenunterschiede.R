# =============================================================================
# Einkommensunterschiede erklären — ohne Eltern-Verknüpfung
# Voraussetzung: SILC_2017_CS_komplett.R wurde ausgeführt → silc existiert
# =============================================================================

library(tidyverse)
library(ranger)
library(caret)
library(patchwork)
set.seed(42)

cat(paste(rep("=", 60), collapse=""), "\n")
cat("3x Random Forest: Was erklärt Einkommensunterschiede?\n")
cat("Ohne Eltern-Verknüpfung\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

# =============================================================================
# 1. DATEN VORBEREITEN
# =============================================================================
safe_col <- function(df, col) if (col %in% names(df)) df[[col]] else NA

rf_clean <- silc %>%
  filter(Y_netto_kombi > 1200 | Y_netto > 1200 | Y_brutto > 1200) %>%
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
    # --- Haushalt & Wohnung ---
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

# Leere Spalten und >80% Missing entfernen
all_na <- sapply(rf_clean, function(x) all(is.na(x)))
if (any(all_na)) {
  cat("Komplett leer (entfernt):", paste(names(all_na[all_na]), collapse=", "), "\n")
  rf_clean <- rf_clean[, !all_na]
}

targets_and_weight <- c("Y_netto_kombi","Y_netto","Y_brutto","weight")
missing_pct <- colMeans(is.na(rf_clean %>% select(-all_of(targets_and_weight))))
vars_drop <- names(missing_pct[missing_pct >= 0.8])
if (length(vars_drop) > 0) {
  cat(">80% Missing (entfernt):", paste(vars_drop, collapse=", "), "\n")
  rf_clean <- rf_clean %>% select(-all_of(vars_drop))
}

rf_clean <- rf_clean %>% drop_na()

predictors <- setdiff(names(rf_clean), targets_and_weight)
cat("n =", nrow(rf_clean), "| Prädiktoren =", length(predictors), "\n")
cat("Variablen:\n  ", paste(predictors, collapse="\n  "), "\n\n")

# =============================================================================
# 2. DREI RANDOM FORESTS
# =============================================================================
run_rf <- function(data, target_name, predictors) {
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("RF:", target_name, "\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  
  df <- data %>%
    filter(.data[[target_name]] > 1200, !is.na(.data[[target_name]])) %>%
    drop_na(weight)
  
  cat("n =", nrow(df), "\n")
  
  idx <- createDataPartition(df[[target_name]], p = 0.8, list = FALSE)
  train <- df[idx, ]
  test  <- df[-idx, ]
  
  f <- as.formula(paste(target_name, "~", paste(predictors, collapse = " + ")))
  
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

res_kombi  <- run_rf(rf_clean, "Y_netto_kombi", predictors)
res_netto  <- run_rf(rf_clean, "Y_netto",       predictors)
res_brutto <- run_rf(rf_clean, "Y_brutto",      predictors)

# =============================================================================
# 3. VARIABLE IMPORTANCE
# =============================================================================
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

cat("\n\n--- Netto-Kombi (Top 15) ---\n")
print(imp_kombi %>% select(Rank, Variable, Category, Rel_Pct) %>% head(15))
cat("\n--- Nettolohn (Top 15) ---\n")
print(imp_netto %>% select(Rank, Variable, Category, Rel_Pct) %>% head(15))
cat("\n--- Bruttolohn (Top 15) ---\n")
print(imp_brutto %>% select(Rank, Variable, Category, Rel_Pct) %>% head(15))

# =============================================================================
# 4. PERFORMANCE
# =============================================================================
perf <- tibble(
  Modell      = c("Netto-Kombi", "Nettolohn (PY010N)", "Bruttolohn (PY010G)"),
  `R² (Test)` = c(res_kombi$r2, res_netto$r2, res_brutto$r2),
  `R² (OOB)`  = c(res_kombi$model$r.squared, res_netto$model$r.squared, res_brutto$model$r.squared),
  RMSE        = c(res_kombi$rmse, res_netto$rmse, res_brutto$rmse),
  MAE         = c(res_kombi$mae, res_netto$mae, res_brutto$mae),
  n           = c(nrow(res_kombi$train)+nrow(res_kombi$test),
                  nrow(res_netto$train)+nrow(res_netto$test),
                  nrow(res_brutto$train)+nrow(res_brutto$test))
)

cat("\n=== PERFORMANCE ===\n")
print(perf)

# =============================================================================
# 5. PLOTS
# =============================================================================
dir.create("plots", showWarnings = FALSE)

# --- 5a. Importance pro Modell ---
plot_imp <- function(imp_df, title_suffix, r2, n) {
  top <- imp_df %>% slice_max(Importance, n = 20)
  ggplot(top, aes(x = reorder(Variable, Importance), y = Rel_Pct, fill = Category)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(Rel_Pct, "%")), hjust = -0.1, size = 3) +
    coord_flip() +
    scale_fill_brewer(palette = "Set2") +
    expand_limits(y = max(top$Rel_Pct) * 1.15) +
    labs(title = paste("Was erklärt Einkommensunterschiede?", title_suffix),
         subtitle = paste0("R² = ", round(r2, 3), " | n = ", n,
                            " | ohne Eltern-Verknüpfung"),
         x = NULL, y = "Relative Importance (%)", fill = "Kategorie") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

n_of <- function(res) nrow(res$train) + nrow(res$test)

p1 <- plot_imp(imp_kombi,  "— Netto-Kombi",      res_kombi$r2,  n_of(res_kombi))
p2 <- plot_imp(imp_netto,  "— Nettolohn PY010N",  res_netto$r2,  n_of(res_netto))
p3 <- plot_imp(imp_brutto, "— Bruttolohn PY010G", res_brutto$r2, n_of(res_brutto))

print(p1); print(p2); print(p3)

ggsave("plots/clean_vimp_netto_kombi.png", p1, width=12, height=8, dpi=300, bg="white")
ggsave("plots/clean_vimp_netto.png",       p2, width=12, height=8, dpi=300, bg="white")
ggsave("plots/clean_vimp_brutto.png",      p3, width=12, height=8, dpi=300, bg="white")

# --- 5b. Importance nach Kategorie ---
cat_all <- bind_rows(imp_kombi, imp_netto, imp_brutto) %>%
  group_by(Model, Category) %>%
  summarise(Pct = sum(Rel_Pct), .groups = "drop") %>%
  mutate(Model = case_when(
    Model == "Y_netto_kombi" ~ "Netto-Kombi",
    Model == "Y_netto"       ~ "Nettolohn",
    Model == "Y_brutto"      ~ "Bruttolohn"
  ))

p4 <- ggplot(cat_all, aes(x = reorder(Category, Pct), y = Pct, fill = Model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c("Netto-Kombi" = "steelblue",
                                "Nettolohn" = "darkorange",
                                "Bruttolohn" = "darkgreen")) +
  labs(title = "Einkommensunterschiede nach Kategorie",
       subtitle = "Welche Faktoren erklären wie viel?",
       x = NULL, y = "Anteil (%)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p4)
ggsave("plots/clean_vimp_kategorie.png", p4, width=12, height=7, dpi=300, bg="white")

# --- 5c. Top 10 faceted ---
top10_all <- bind_rows(imp_kombi, imp_netto, imp_brutto) %>%
  group_by(Model) %>%
  slice_max(Importance, n = 10) %>%
  mutate(Model = case_when(
    Model == "Y_netto_kombi" ~ "Netto-Kombi",
    Model == "Y_netto"       ~ "Nettolohn",
    Model == "Y_brutto"      ~ "Bruttolohn"
  ))

p5 <- ggplot(top10_all, aes(x = reorder(Variable, Rel_Pct), y = Rel_Pct)) +
  geom_segment(aes(xend = Variable, y = 0, yend = Rel_Pct, color = Category),
               linewidth = 1.2) +
  geom_point(aes(color = Category), size = 3) +
  geom_text(aes(label = paste0(Rel_Pct, "%")), hjust = -0.3, size = 2.8) +
  coord_flip() +
  facet_wrap(~Model, scales = "free_y") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Top 10 — Was erklärt Einkommensunterschiede?",
       subtitle = "EU-SILC 2017 AT | ohne Eltern-Verknüpfung | gewichtet mit PB040",
       x = NULL, y = "Relative Importance (%)", color = "Kategorie") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

print(p5)
ggsave("plots/clean_vimp_top10.png", p5, width=16, height=8, dpi=300, bg="white")

# =============================================================================
# 6. SPEICHERN
# =============================================================================
saveRDS(list(kombi = res_kombi, netto = res_netto, brutto = res_brutto),
        "rf_models_clean.rds")

cat("\n✓ Gespeichert:\n")
cat("  rf_models_clean.rds       — 3 RF-Modelle\n")
cat("  plots/clean_vimp_*.png    — Alle Plots\n")
