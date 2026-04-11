# ============================================================
# df_r + df_p zusammenführen → dat
# Join-Key: person_id + land + erhebungsjahr
# ============================================================

library(dplyr)

# ── 1. Doppelte Variablen identifizieren ─────────────────────
# Spalten die in BEIDEN Tabellen vorkommen (außer den Join-Keys)
join_keys <- c("person_id")

gemeinsame_vars <- intersect(names(df_r), names(df_p))
doppelte_vars   <- setdiff(gemeinsame_vars, join_keys)

cat("Doppelte Variablen die aus df_p entfernt werden:\n")
print(doppelte_vars)

# ── 2. Doppelte Spalten aus df_p entfernen ───────────────────
# Wir behalten die Version aus df_r (Register = Referenzquelle)
df_p_clean <- df_p |>
  select(-all_of(doppelte_vars))

# ── 3. Join ──────────────────────────────────────────────────
dat <- df_r |>
  left_join(df_p_clean, by = join_keys)

# ── 4. Überblick ─────────────────────────────────────────────
cat("\n── Ergebnis ──────────────────────────────────────────\n")
cat("Zeilen df_r:    ", nrow(df_r),      "\n")
cat("Zeilen df_p:    ", nrow(df_p),      "\n")
cat("Zeilen dat:     ", nrow(dat),       "\n\n")
cat("Spalten df_r:   ", ncol(df_r),      "\n")
cat("Spalten df_p:   ", ncol(df_p),      "\n")
cat("Doppelte vars:  ", length(doppelte_vars), "\n")
cat("Spalten dat:    ", ncol(dat),       "\n\n")

# Prüfen ob Join sauber war (keine unerwarteten Duplikate)
cat("Eindeutige Personen in dat:", n_distinct(dat$person_id), "\n")
cat("Zeilen = Personen?", nrow(dat) == n_distinct(dat$person_id), "\n")
