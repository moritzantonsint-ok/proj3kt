# =============================================================================
# Random Forest: Vorhersage Bruttolohn (PY010G) für Erwerbstätige
# EU-SILC 2017 Querschnitt — Österreich
# =============================================================================

install.packages("ranger")
install.packages("caret")
library(tidyverse)
library(ranger)        # schneller Random Forest
library(caret)         # für Train/Test Split & Evaluation

set.seed(42)

# 1. DATEN VORBEREITEN =========================================================

# Filter: nur Erwerbstätige mit positivem Lohn
rf_data <- silcw %>%
  filter(Employment2_rb210 == "At work",
         Lohnb_py010g > 0,
         !is.na(Lohnb_py010g)) %>%
  transmute(
    # Zielvariable
    Lohn        = Lohnb_py010g,
    # Prädiktoren
    Educ        = Educ_pe040,
    Empl        = Empl_pl031,
    Branch      = Branch_pl051,
    Hours       = Hoursperweek_pl060,
    Experience  = Experienceinyears_pl200,
    Age         = 2017 - Birthday_rb080,           # Alter statt Geburtsjahr
    Gender      = Gender_rb090,
    Maternal    = Maternal_pb190,
    Health      = Health_ph010,
    OwnorRent   = OwnorRent_hh021,
    Degruba     = Degruba_db100,
    # Gewicht für Modell
    weight      = pw
  ) %>%
  drop_na()                                         # nur vollständige Fälle

cat("=== Datensatz für Random Forest ===\n")
cat("Beobachtungen (At work, Lohn>0, complete cases):", nrow(rf_data), "\n")
cat("Mittlerer Lohn:", round(mean(rf_data$Lohn), 0), "EUR\n\n")

# 2. TRAIN / TEST SPLIT ========================================================
train_idx <- createDataPartition(rf_data$Lohn, p = 0.8, list = FALSE)
train     <- rf_data[train_idx, ]
test      <- rf_data[-train_idx, ]

cat("Training:", nrow(train), " Test:", nrow(test), "\n\n")

# 3. RANDOM FOREST TRAINIEREN ==================================================
rf_model <- ranger(
  formula        = Lohn ~ Educ + Empl + Branch + Hours + Experience +
                          Age + Gender + Maternal + Health + OwnorRent + Degruba,
  data           = train,
  num.trees      = 500,
  mtry           = 4,                  # ~ sqrt(11 Prädiktoren)
  min.node.size  = 5,
  importance     = "permutation",      # für Variable-Importance
  case.weights   = train$weight,       # Survey-Gewichte berücksichtigen
  seed           = 42
)

print(rf_model)

# 4. VORHERSAGE & EVALUATION ===================================================
pred <- predict(rf_model, data = test)$predictions

rmse <- sqrt(mean((test$Lohn - pred)^2))
mae  <- mean(abs(test$Lohn - pred))
r2   <- 1 - sum((test$Lohn - pred)^2) / sum((test$Lohn - mean(test$Lohn))^2)

cat("\n=== Modell-Performance (Test-Set) ===\n")
cat("RMSE:", round(rmse, 0), "EUR\n")
cat("MAE: ", round(mae, 0), "EUR\n")
cat("R²:  ", round(r2, 3), "\n")

# 5. VARIABLE IMPORTANCE =======================================================
imp <- data.frame(
  Variable   = names(rf_model$variable.importance),
  Importance = rf_model$variable.importance
) %>%
  arrange(desc(Importance))

cat("\n=== Variable Importance ===\n")
print(imp)

# Plot
ggplot(imp, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance — Random Forest",
       subtitle = "Vorhersage Bruttolohn EU-SILC 2017 AT (At work, Lohn > 0)",
       x = NULL, y = "Permutation Importance") +
  theme_minimal(base_size = 12)

# 6. PRED VS. ACTUAL PLOT ======================================================
plot_df <- data.frame(actual = test$Lohn, predicted = pred)

ggplot(plot_df, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Vorhergesagter vs. tatsächlicher Lohn",
       subtitle = sprintf("R² = %.3f, RMSE = %.0f EUR", r2, rmse),
       x = "Tatsächlicher Bruttolohn (EUR)",
       y = "Vorhergesagter Bruttolohn (EUR)") +
  theme_minimal(base_size = 12)

# 7. MODELL SPEICHERN ==========================================================
saveRDS(rf_model, "rf_lohn_model.rds")
cat("\n✓ Modell gespeichert: rf_lohn_model.rds\n")
