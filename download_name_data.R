# DOWNLOAD SILC FOR MULTIPLE YEARS ----------------------------------------------------------------
# the following code lets you download the harmonized SILC files for multiple years and countries
# note, that you can also ask Statistik Austria for SILC files for more recent years

# 0. PREP -----------------------------------------------------------------------------------------

# clean working environment
rm(list = ls())
gc()

# packages - maybe you need to install them first
# you can do this in the console by typing install.packages("DBI")
packages <- c("DBI", "RPostgreSQL", "RPostgres", "tidyverse")
sapply(packages, library, character.only = T)

# 1. GET THE DATA ---------------------------------------------------------------------------------

# 1.1. Establish a connection to the server -----------------------------------

con <- DBI::dbConnect(
  RPostgres::Postgres(), 
  dbname   = Sys.getenv("DB_NAME"), 
  host     = Sys.getenv("DB_HOST"), 
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  port     = NULL,
  options  = Sys.getenv("DB_OPTIONS")
)

# 1.2. See which datasets are available ---------------------------------------

tables <- DBI::dbListTables(con)
print(tables)

# remember the structure of the SILC files
# D = household register
# H = household questionnaire
# P = personal questionnaire
# R = Personal register

# also, remember that there are cross-sectional and longitudinal files
# c is for crosssection

# 1.3. Determine what you want to download ------------------------------------
# Which year
# Which countries?
# Which datafile?
# Also think about the Variables you want to download

# 1.4. Example ----------------------------------------------------------------
# this example code downloads:
### year 2017 --> so you have to pick a dataset with 17
### personal register --> so this is the r file
### Austria --> we have to filter. go to the codebook to see which variables are the country variable
df_r <- tbl(con, "c17r") %>% 
  filter(rb020 == "AT") %>% 
  collect(n = Inf)

# ============================================================
# EU-SILC 2024 – Register R (Personal Register)
# Umbenennung & Typzuweisung nach Eurostat Methodological
# Guidelines 2024 (v7)
# ============================================================

library(dplyr)

# ── 1. Spalten umbenennen ────────────────────────────────────
df_r <- df_r |>
  rename(
    # ── Identifikatoren & Gewichte ───────────────────────────
    erhebungsjahr          = rb010,   # Reference year (Erhebungsjahr)
    land                   = rb020,   # Country (Ländercode)
    person_id              = rb030,   # Personal ID
    haushalt_id            = rb031,   # Household ID (neu in 2021+)
    haushalt_id_f          = rb031_f, # Flag: Household ID
    personengewicht        = rb050,   # Personal cross-sectional weight
    personengewicht_f      = rb050_f, # Flag: Personal weight
    
    # ── Geburts- & Demographische Angaben ───────────────────
    geburtsjahr            = rb070,   # Year of birth
    geburtsjahr_f          = rb070_f, # Flag: Year of birth
    geburtsmonat           = rb080,   # Month of birth
    geburtsmonat_f         = rb080_f, # Flag: Month of birth
    geschlecht             = rb090,   # Sex (1 = männlich, 2 = weiblich)
    geschlecht_f           = rb090_f, # Flag: Sex
    
    # ── Aufenthaltsstatus ────────────────────────────────────
    aufenthaltsstatus      = rb200,   # Residential status (1 = im HH, 2 = nicht im HH)
    aufenthaltsstatus_f    = rb200_f, # Flag: Residential status
    
    # ── Aktivitätsstatus ────────────────────────────────────
    aktivitaet_aktuell     = rb210,   # Current activity status
    aktivitaet_aktuell_f   = rb210_f, # Flag: Current activity status
    aktivitaet_jan         = rb220,   # Activity Jan (Selbstständ./Arbeitnehmer)
    aktivitaet_jan_f       = rb220_f, # Flag: Activity Jan
    aktivitaet_feb_mrz     = rb230,   # Activity Feb–Mar
    aktivitaet_feb_mrz_f   = rb230_f, # Flag: Activity Feb–Mar
    aktivitaet_apr_jun     = rb240,   # Activity Apr–Jun
    aktivitaet_apr_jun_f   = rb240_f, # Flag: Activity Apr–Jun
    aktivitaet_jul_sep     = rb245,   # Activity Jul–Sep
    aktivitaet_jul_sep_f   = rb245_f, # Flag: Activity Jul–Sep
    aktivitaet_okt_dez     = rb250,   # Activity Oct–Dec
    aktivitaet_okt_dez_f   = rb250_f, # Flag: Activity Oct–Dec
    
    # ── Staatsbürgerschaft & Geburtsland ────────────────────
    staatsangehoerigkeit   = rb260,   # Citizenship
    staatsangehoerigkeit_f = rb260_f, # Flag: Citizenship
    geburtsland            = rb270,   # Country of birth
    geburtsland_f          = rb270_f, # Flag: Country of birth
    
    # ── Kinder-/Betreuungsangaben (RL – Child Register) ─────
    kinderbetreuung_u1     = rl010,   # Care: Kinder unter 1 Jahr
    kinderbetreuung_u1_f   = rl010_f, # Flag
    kinderbetreuung_1_2    = rl020,   # Care: 1–2 Jahre
    kinderbetreuung_1_2_f  = rl020_f, # Flag
    kinderbetreuung_3_5    = rl030,   # Care: 3–5 Jahre (vor Schulpflicht)
    kinderbetreuung_3_5_f  = rl030_f, # Flag
    kinderbetreuung_6_11   = rl040,   # Care: 6–11 Jahre
    kinderbetreuung_6_11_f = rl040_f, # Flag
    kinderbetreuung_12_17  = rl050,   # Care: 12–17 Jahre
    kinderbetreuung_12_17_f= rl050_f, # Flag
    kinderbetreuung_18plus = rl060,   # Care: 18+ (behinderte Angehörige)
    kinderbetreuung_18plus_f= rl060_f,# Flag
    betreuung_stunden      = rl070,   # Stunden formelle Kinderbetreuung/Woche
    betreuung_stunden_f    = rl070_f, # Flag
    
    # ── Kollektive / Derived (RC – Collected/Computed) ──────
    alter                  = rc010t,  # Alter zum Erhebungszeitpunkt (berechnet)
    alter_f                = rc010t_f,# Flag: Alter
    haushaltsgroesse       = rc020t,  # Haushaltsgröße (berechnet)
    haushaltsgroesse_f     = rc020t_f,# Flag: Haushaltsgröße
    aequivalenzgewicht     = rc030t,  # OECD-Äquivalenzskala (berechnet)
    aequivalenzgewicht_f   = rc030t_f,# Flag: Äquivalenzgewicht
    
    # ── Abgeleitete Hilfsvariablen (RX) ─────────────────────
    alter_klasse           = rx010,   # Altersgruppe (kategorisiert)
    ausbildung_jahre       = rx020,   # Ausbildungsjahre (ISCED-basiert)
    erwerbsstatus_akt      = rx030,   # Aktueller Erwerbsstatus (abgeleitet)
    besch_vollzeit         = rx040,   # Vollzeit-/Teilzeit-Status (abgeleitet)
    selbst_unselbst        = rx050,   # Selbst-/Unselbstständig (abgeleitet)
    arbeitszeit_monate     = rx060,   # Monate in Beschäftigung im Ref.-Jahr
    berufsgruppe           = rx070    # Berufsgruppe (ISCO, abgeleitet)
  )


# ── 2. Flags als Integer belassen (NA-Codes laut Eurostat) ──
#    Flags kodieren: 0 = gefüllt, 1–9 = verschiedene Missings
flag_vars <- names(df_r)[endsWith(names(df_r), "_f")]
df_r[flag_vars] <- lapply(df_r[flag_vars], as.integer)


# ── 3. Faktoren ──────────────────────────────────────────────
df_r <- df_r |>
  mutate(
    # Geschlecht
    geschlecht = factor(geschlecht,
                        levels = c(1, 2),
                        labels = c("Männlich", "Weiblich")),
    
    # Aufenthaltsstatus
    aufenthaltsstatus = factor(aufenthaltsstatus,
                               levels = c(1, 2),
                               labels = c("Im Haushalt wohnhaft",
                                          "Nicht im Haushalt wohnhaft")),
    
    # Aktueller Aktivitätsstatus (rb210)
    aktivitaet_aktuell = factor(aktivitaet_aktuell,
                                levels = 1:9,
                                labels = c("Vollzeitbeschäftigt",
                                           "Teilzeitbeschäftigt",
                                           "Selbstständig",
                                           "Arbeitslos",
                                           "Rente/Pension",
                                           "Andere Inaktivität",
                                           "In Ausbildung",
                                           "Dauerhafte Erwerbsunfähigkeit",
                                           "Sonstiges")),
    
    # Monatliche Aktivitätsstatus-Variablen (rb220–rb250)
    across(c(aktivitaet_jan, aktivitaet_feb_mrz,
             aktivitaet_apr_jun, aktivitaet_jul_sep,
             aktivitaet_okt_dez),
           ~ factor(.x,
                    levels = 1:11,
                    labels = c("Vollzeitbeschäftigt",
                               "Teilzeitbeschäftigt",
                               "Selbstständig Vollzeit",
                               "Selbstständig Teilzeit",
                               "Arbeitslos",
                               "Rente/Pension",
                               "Vorruhestand",
                               "Erwerbsunfähigkeit",
                               "Hausarbeit/Pflege",
                               "In Ausbildung",
                               "Sonstiges"))),
    
    # Altersgruppe (rx010)
    alter_klasse = factor(alter_klasse,
                          levels = c(1, 2, 3, 4, 5, 6),
                          labels = c("0–15 Jahre",
                                     "16–24 Jahre",
                                     "25–49 Jahre",
                                     "50–64 Jahre",
                                     "65–74 Jahre",
                                     "75+ Jahre")),
    
    # Erwerbsstatus aktuell abgeleitet (rx030)
    erwerbsstatus_akt = factor(erwerbsstatus_akt,
                               levels = c(1, 2, 3),
                               labels = c("Erwerbstätig",
                                          "Arbeitslos",
                                          "Nicht erwerbstätig")),
    
    # Vollzeit/Teilzeit (rx040)
    besch_vollzeit = factor(besch_vollzeit,
                            levels = c(1, 2),
                            labels = c("Vollzeit", "Teilzeit")),
    
    # Selbst-/Unselbstständig (rx050)
    selbst_unselbst = factor(selbst_unselbst,
                             levels = c(1, 2),
                             labels = c("Arbeitnehmer/in",
                                        "Selbstständig")),
    
    # Ländercode als Faktor
    land = factor(land)
  )


# ── 4. Numerische Variablen ──────────────────────────────────
df_r <- df_r |>
  mutate(
    across(c(erhebungsjahr, geburtsjahr, geburtsmonat,
             alter, haushaltsgroesse, ausbildung_jahre,
             arbeitszeit_monate), as.integer),
    across(c(personengewicht, aequivalenzgewicht), as.numeric),
    across(starts_with("kinderbetreuung"), as.integer),
    betreuung_stunden = as.numeric(betreuung_stunden)
  )


# ── 5. Überblick ─────────────────────────────────────────────
cat("Struktur des umbenannten Datensatzes:\n")
str(df_r)
cat("\nAnzahl Beobachtungen:", nrow(df_r), "\n")
cat("Anzahl Variablen:", ncol(df_r), "\n")

df_p <- tbl(con, "c17p") %>% 
  filter(pb020 == "AT") %>% 
  collect(n = Inf)

# ============================================================
# EU-SILC – Personal Questionnaire (P-File)
# Umbenennung & Typzuweisung nach Eurostat Methodological
# Guidelines 2024 (v7)
# ============================================================

library(dplyr)

# ── 1. Spalten umbenennen ────────────────────────────────────
df_p <- df_p |>
  rename(
    
    # ── PB: Persönliche Basisdaten ───────────────────────────
    erhebungsjahr              = pb010,    # Reference year
    land                       = pb020,    # Country
    person_id                  = pb030,    # Personal ID
    querschnittsgewicht        = pb040,    # Cross-sectional weight
    querschnittsgewicht_f      = pb040_f,
    laengsschnittgewicht       = pb060,    # Longitudinal weight
    laengsschnittgewicht_f     = pb060_f,
    interview_monat            = pb100,    # Month of interview
    interview_monat_f          = pb100_f,
    interview_jahr             = pb110,    # Year of interview
    interview_jahr_f           = pb110_f,
    interview_art              = pb120,    # Mode of interview (face-to-face, tel., etc.)
    interview_art_f            = pb120_f,
    interview_dauer            = pb130,    # Duration of interview (minutes)
    interview_dauer_f          = pb130_f,
    proxy_interview            = pb140,    # Proxy interview (1=ja, 2=nein)
    proxy_interview_f          = pb140_f,
    geschlecht                 = pb150,    # Sex (1=männlich, 2=weiblich)
    geschlecht_f               = pb150_f,
    partner_im_hh              = pb160,    # Partner in household (1=ja, 2=nein)
    partner_im_hh_f            = pb160_f,
    partner_person_id          = pb170,    # Person ID des Partners
    partner_person_id_f        = pb170_f,
    mutter_im_hh               = pb180,    # Mutter im Haushalt (1=ja, 2=nein)
    mutter_im_hh_f             = pb180_f,
    mutter_person_id           = pb190,    # Person ID der Mutter
    mutter_person_id_f         = pb190_f,
    vater_im_hh                = pb200,    # Vater im Haushalt (1=ja, 2=nein)
    vater_im_hh_f              = pb200_f,
    vater_person_id            = pb210,    # Person ID des Vaters
    vater_person_id_f          = pb210_f,
    geburtsland                = pb220a,   # Country of birth
    geburtsland_f              = pb220a_f,
    
    # ── PE: Bildung ──────────────────────────────────────────
    bildung_isced              = pe010,    # Highest ISCED level attained
    bildung_isced_f            = pe010_f,
    bildung_abschluss_jahr     = pe020,    # Year highest level completed
    bildung_abschluss_jahr_f   = pe020_f,
    bildung_in_ausbildung      = pe030,    # Currently in education (1=ja, 2=nein)
    bildung_in_ausbildung_f    = pe030_f,
    bildung_ausbildungsfeld    = pe040,    # Field of education (ISCED-F)
    bildung_ausbildungsfeld_f  = pe040_f,
    
    # ── PL: Arbeitsmarkt ─────────────────────────────────────
    erwerbsstatus_selbstdef    = pl031,    # Self-defined econ. status (Hauptstatus)
    erwerbsstatus_selbstdef_f  = pl031_f,
    erwerbsstatus_ilo          = pl035,    # ILO employment status
    erwerbsstatus_ilo_f        = pl035_f,
    arbeit_gesucht_12m         = pl015,    # Job search in past 12 months
    arbeit_gesucht_12m_f       = pl015_f,
    arbeit_verfuegbar          = pl020,    # Available for work within 2 weeks
    arbeit_verfuegbar_f        = pl020_f,
    arbeit_gesucht_4w          = pl025,    # Job search in past 4 weeks
    arbeit_gesucht_4w_f        = pl025_f,
    stellung_im_beruf          = pl040,    # Status in employment (AN/Selbst.)
    stellung_im_beruf_f        = pl040_f,
    beruf_isco                 = pl051,    # Occupation (ISCO-08, 2-digit)
    beruf_isco_f               = pl051_f,
    woechentliche_arbeitsstd   = pl060,    # Hours per week in main job
    woechentliche_arbeitsstd_f = pl060_f,
    ueberstunden_vollzeit      = pl073,    # Full-time/Part-time indicator (1=VZ)
    ueberstunden_vollzeit_f    = pl073_f,
    vollzeit_teilzeit          = pl074,    # Full-time or part-time (reason)
    vollzeit_teilzeit_f        = pl074_f,
    teilzeit_grund             = pl075,    # Reason for part-time work
    teilzeit_grund_f           = pl075_f,
    befristet_unbefristet      = pl076,    # Permanent/temporary contract
    befristet_unbefristet_f    = pl076_f,
    arbeitgeber_typ            = pl080,    # Supervisor/employee (manager status)
    arbeitgeber_typ_f          = pl080_f,
    branche_nace               = pl085,    # NACE industry branch (main job)
    branche_nace_f             = pl085_f,
    unternehmensgroesse        = pl086,    # Size of local unit (Betriebsgröße)
    unternehmensgroesse_f      = pl086_f,
    arbeit_von_zuhause         = pl087,    # Work from home (frequency)
    arbeit_von_zuhause_f       = pl087_f,
    befristung_grund           = pl088,    # Reason for temporary contract
    befristung_grund_f         = pl088_f,
    mehrere_jobs               = pl089,    # More than one job (1=ja, 2=nein)
    mehrere_jobs_f             = pl089_f,
    nebenjob_stunden           = pl090,    # Hours per week in second job
    nebenjob_stunden_f         = pl090_f,
    gesamte_arbeitsstunden     = pl100,    # Total hours worked per week (all jobs)
    gesamte_arbeitsstunden_f   = pl100_f,
    beruf_isco_nebenjob        = pl111,    # Occupation second job (ISCO-08)
    beruf_isco_nebenjob_f      = pl111_f,
    monate_vollzeit            = pl120,    # Months in full-time employment
    monate_vollzeit_f          = pl120_f,
    monate_teilzeit            = pl130,    # Months in part-time employment
    monate_teilzeit_f          = pl130_f,
    monate_selbst_vollzeit     = pl140,    # Months self-empl. full-time
    monate_selbst_vollzeit_f   = pl140_f,
    monate_selbst_teilzeit     = pl150,    # Months self-empl. part-time
    monate_selbst_teilzeit_f   = pl150_f,
    monate_arbeitslos          = pl160,    # Months unemployed
    monate_arbeitslos_f        = pl160_f,
    monate_rente               = pl170,    # Months in retirement
    monate_rente_f             = pl170_f,
    monate_erwerbsunfaehig     = pl180,    # Months in disability
    monate_erwerbsunfaehig_f   = pl180_f,
    monate_sonstige_inaktiv    = pl190,    # Months other inactive
    monate_sonstige_inaktiv_f  = pl190_f,
    monate_ausbildung          = pl200,    # Months in education/training
    monate_ausbildung_f        = pl200_f,
    
    # PL211: Monatliche Aktivitätskalen (a–l = Jan–Dez)
    aktivitaet_m01             = pl211a,   # Januar
    aktivitaet_m01_f           = pl211a_f,
    aktivitaet_m02             = pl211b,   # Februar
    aktivitaet_m02_f           = pl211b_f,
    aktivitaet_m03             = pl211c,   # März
    aktivitaet_m03_f           = pl211c_f,
    aktivitaet_m04             = pl211d,   # April
    aktivitaet_m04_f           = pl211d_f,
    aktivitaet_m05             = pl211e,   # Mai
    aktivitaet_m05_f           = pl211e_f,
    aktivitaet_m06             = pl211f,   # Juni
    aktivitaet_m06_f           = pl211f_f,
    aktivitaet_m07             = pl211g,   # Juli
    aktivitaet_m07_f           = pl211g_f,
    aktivitaet_m08             = pl211h,   # August
    aktivitaet_m08_f           = pl211h_f,
    aktivitaet_m09             = pl211i,   # September
    aktivitaet_m09_f           = pl211i_f,
    aktivitaet_m10             = pl211j,   # Oktober
    aktivitaet_m10_f           = pl211j_f,
    aktivitaet_m11             = pl211k,   # November
    aktivitaet_m11_f           = pl211k_f,
    aktivitaet_m12             = pl211l,   # Dezember
    aktivitaet_m12_f           = pl211l_f,
    
    # ── PH: Gesundheit ───────────────────────────────────────
    gesundheit_allgemein       = ph010,    # Self-perceived health (1=sehr gut … 5=sehr schlecht)
    gesundheit_allgemein_f     = ph010_f,
    chronisch_krank            = ph020,    # Chronic illness/condition (1=ja, 2=nein)
    chronisch_krank_f          = ph020_f,
    aktivitaet_eingeschraenkt  = ph030,    # Activity limitation (1=stark … 3=keine)
    aktivitaet_eingeschraenkt_f= ph030_f,
    arzt_konsultiert           = ph040,    # GP consultation in past 4 weeks (1=ja, 2=nein)
    arzt_konsultiert_f         = ph040_f,
    arzt_anzahl_besuche        = ph050,    # Number of GP visits in past 4 weeks
    arzt_anzahl_besuche_f      = ph050_f,
    spezialist_konsultiert     = ph060,    # Specialist consultation (1=ja, 2=nein)
    spezialist_konsultiert_f   = ph060_f,
    spezialist_anzahl_besuche  = ph070,    # Number of specialist visits
    spezialist_anzahl_besuche_f= ph070_f,
    
    # ── PY: Persönliche Einkommen (netto) ────────────────────
    lohn_netto                 = py010n,   # Employee cash income (net)
    lohn_netto_f               = py010n_f,
    lohn_netto_i               = py010n_i, # Imputationsflag
    krankenversicherung_netto  = py020n,   # Non-cash employee income (net)
    krankenversicherung_netto_f= py020n_f,
    krankenversicherung_netto_i= py020n_i,
    firmenwagen_netto          = py021n,   # Company car (net value)
    firmenwagen_netto_f        = py021n_f,
    firmenwagen_netto_i        = py021n_i,
    krankentaggeld_netto       = py035n,   # Sick pay (net)
    krankentaggeld_netto_f     = py035n_f,
    krankentaggeld_netto_i     = py035n_i,
    selbst_einkommen_netto     = py050n,   # Self-employment income (net)
    selbst_einkommen_netto_f   = py050n_f,
    selbst_einkommen_netto_i   = py050n_i,
    pension_altersrente_netto  = py080n,   # Private pension plan (net)
    pension_altersrente_netto_f= py080n_f,
    pension_altersrente_netto_i= py080n_i,
    al_geld_netto              = py090n,   # Unemployment benefits (net)
    al_geld_netto_f            = py090n_f,
    al_geld_netto_i            = py090n_i,
    altersrente_netto          = py100n,   # Old-age pension (net)
    altersrente_netto_f        = py100n_f,
    altersrente_netto_i        = py100n_i,
    hinterbliebenenrente_netto = py110n,   # Survivors' pension (net)
    hinterbliebenenrente_netto_f= py110n_f,
    hinterbliebenenrente_netto_i= py110n_i,
    invalidenrente_netto       = py120n,   # Sickness/disability pension (net)
    invalidenrente_netto_f     = py120n_f,
    invalidenrente_netto_i     = py120n_i,
    bildungsbeihilfe_netto     = py130n,   # Education-related allowances (net)
    bildungsbeihilfe_netto_f   = py130n_f,
    bildungsbeihilfe_netto_i   = py130n_i,
    sonstige_transfers_netto   = py140n,   # Other social transfers (net)
    sonstige_transfers_netto_f = py140n_f,
    sonstige_transfers_netto_i = py140n_i,
    
    # ── PY: Persönliche Einkommen (brutto) ───────────────────
    lohn_brutto                = py010g,   # Employee cash income (gross)
    lohn_brutto_f              = py010g_f,
    lohn_brutto_i              = py010g_i,
    krankenversicherung_brutto = py020g,
    krankenversicherung_brutto_f= py020g_f,
    krankenversicherung_brutto_i= py020g_i,
    firmenwagen_brutto         = py021g,
    firmenwagen_brutto_f       = py021g_f,
    firmenwagen_brutto_i       = py021g_i,
    ag_sv_beitrag              = py030g,   # Employer's social insurance contrib. (gross)
    ag_sv_beitrag_f            = py030g_f,
    ag_sv_beitrag_i            = py030g_i,
    an_sv_beitrag              = py031g,   # Employee's social insurance contrib. (gross)
    an_sv_beitrag_f            = py031g_f,
    an_sv_beitrag_i            = py031g_i,
    krankentaggeld_brutto      = py035g,
    krankentaggeld_brutto_f    = py035g_f,
    krankentaggeld_brutto_i    = py035g_i,
    selbst_einkommen_brutto    = py050g,
    selbst_einkommen_brutto_f  = py050g_f,
    selbst_einkommen_brutto_i  = py050g_i,
    pension_privat_brutto      = py080g,
    pension_privat_brutto_f    = py080g_f,
    pension_privat_brutto_i    = py080g_i,
    al_geld_brutto             = py090g,
    al_geld_brutto_f           = py090g_f,
    al_geld_brutto_i           = py090g_i,
    altersrente_brutto         = py100g,
    altersrente_brutto_f       = py100g_f,
    altersrente_brutto_i       = py100g_i,
    hinterbliebenenrente_brutto= py110g,
    hinterbliebenenrente_brutto_f= py110g_f,
    hinterbliebenenrente_brutto_i= py110g_i,
    invalidenrente_brutto      = py120g,
    invalidenrente_brutto_f    = py120g_f,
    invalidenrente_brutto_i    = py120g_i,
    bildungsbeihilfe_brutto    = py130g,
    bildungsbeihilfe_brutto_f  = py130g_f,
    bildungsbeihilfe_brutto_i  = py130g_i,
    sonstige_transfers_brutto  = py140g,
    sonstige_transfers_brutto_f= py140g_f,
    sonstige_transfers_brutto_i= py140g_i,
    einkommenssteuer_brutto    = py200g,   # Tax on income (gross)
    einkommenssteuer_brutto_f  = py200g_f,
    einkommenssteuer_brutto_i  = py200g_i,
    
    # ── PY: Aufgesplittete Sozialleistungen (g = brutto) ─────
    # Arbeitslosengeld-Komponenten (py091g–py094g)
    al_geld_brutto_k1          = py091g,
    al_geld_brutto_k1_f        = py091g_f,
    al_geld_brutto_k2          = py092g,
    al_geld_brutto_k2_f        = py092g_f,
    al_geld_brutto_k3          = py093g,
    al_geld_brutto_k3_f        = py093g_f,
    al_geld_brutto_k4          = py094g,
    al_geld_brutto_k4_f        = py094g_f,
    # Altersrente-Komponenten (py101g–py104g)
    altersrente_brutto_k1      = py101g,
    altersrente_brutto_k1_f    = py101g_f,
    altersrente_brutto_k2      = py102g,
    altersrente_brutto_k2_f    = py102g_f,
    altersrente_brutto_k3      = py103g,
    altersrente_brutto_k3_f    = py103g_f,
    altersrente_brutto_k4      = py104g,
    altersrente_brutto_k4_f    = py104g_f,
    # Hinterbliebenenrente-Komponenten (py111g–py114g)
    hinterbl_brutto_k1         = py111g,
    hinterbl_brutto_k1_f       = py111g_f,
    hinterbl_brutto_k2         = py112g,
    hinterbl_brutto_k2_f       = py112g_f,
    hinterbl_brutto_k3         = py113g,
    hinterbl_brutto_k3_f       = py113g_f,
    hinterbl_brutto_k4         = py114g,
    hinterbl_brutto_k4_f       = py114g_f,
    # Invalidenrente-Komponenten (py121g–py124g)
    invalidenrente_brutto_k1   = py121g,
    invalidenrente_brutto_k1_f = py121g_f,
    invalidenrente_brutto_k2   = py122g,
    invalidenrente_brutto_k2_f = py122g_f,
    invalidenrente_brutto_k3   = py123g,
    invalidenrente_brutto_k3_f = py123g_f,
    invalidenrente_brutto_k4   = py124g,
    invalidenrente_brutto_k4_f = py124g_f,
    # Bildungsbeihilfe-Komponenten (py131g–py134g)
    bildungsbh_brutto_k1       = py131g,
    bildungsbh_brutto_k1_f     = py131g_f,
    bildungsbh_brutto_k2       = py132g,
    bildungsbh_brutto_k2_f     = py132g_f,
    bildungsbh_brutto_k3       = py133g,
    bildungsbh_brutto_k3_f     = py133g_f,
    bildungsbh_brutto_k4       = py134g,
    bildungsbh_brutto_k4_f     = py134g_f,
    # Sonstige Transfers-Komponenten (py141g–py144g)
    sonst_transfer_brutto_k1   = py141g,
    sonst_transfer_brutto_k1_f = py141g_f,
    sonst_transfer_brutto_k2   = py142g,
    sonst_transfer_brutto_k2_f = py142g_f,
    sonst_transfer_brutto_k3   = py143g,
    sonst_transfer_brutto_k3_f = py143g_f,
    sonst_transfer_brutto_k4   = py144g,
    sonst_transfer_brutto_k4_f = py144g_f,
    
    # ── PD: Materielle Entbehrung ─────────────────────────────
    entbehrung_urlaub          = pd020,    # Can afford 1 week holiday (1=ja, 2=nein)
    entbehrung_urlaub_f        = pd020_f,
    entbehrung_fleisch         = pd030,    # Can afford meat/fish every 2nd day
    entbehrung_fleisch_f       = pd030_f,
    entbehrung_unerwartete_ausg= pd050,    # Can afford unexpected expense
    entbehrung_unerwartete_ausg_f= pd050_f,
    entbehrung_telefon         = pd060,    # Can afford telephone
    entbehrung_telefon_f       = pd060_f,
    entbehrung_tv              = pd070,    # Can afford colour TV
    entbehrung_tv_f            = pd070_f,
    entbehrung_waschmaschine   = pd080,    # Can afford washing machine
    entbehrung_waschmaschine_f = pd080_f,
    
    # ── PH (erweitert): Gesundheitsversorgung ────────────────
    arzt_verzicht_kosten       = ph080,    # Unmet need for medical exam (cost)
    arzt_verzicht_kosten_f     = ph080_f,
    arzt_verzicht_wartezeit    = ph090,    # Unmet need (waiting time)
    arzt_verzicht_wartezeit_f  = ph090_f,
    arzt_verzicht_entfernung   = ph100,    # Unmet need (distance/travel)
    arzt_verzicht_entfernung_f = ph100_f,
    zahnarzt_verzicht_kosten   = ph110,    # Unmet dental need (cost)
    zahnarzt_verzicht_kosten_f = ph110_f,
    zahnarzt_verzicht_wartezeit= ph120,    # Unmet dental need (waiting time)
    zahnarzt_verzicht_wartezeit_f= ph120_f,
    zahnarzt_verzicht_entfernung= ph130,   # Unmet dental need (distance)
    zahnarzt_verzicht_entfernung_f= ph130_f,
    mental_verzicht_kosten     = ph140,    # Unmet mental health need (cost)
    mental_verzicht_kosten_f   = ph140_f,
    mental_verzicht_wartezeit  = ph150,    # Unmet mental health need (waiting)
    mental_verzicht_wartezeit_f= ph150_f,
    
    # ── PX: Abgeleitete Hilfsvariablen ───────────────────────
    alter                      = px010,    # Age at interview
    aequivalenzgewicht         = px020,    # Equivalised household size
    einkommensquintil          = px030,    # Income quintile (abgeleitet)
    bildung_3gruppen           = px040,    # Education (3 groups: low/mid/high)
    erwerbsstatus_5gruppen     = px050     # Activity status (5 groups, abgeleitet)
  )


# ── 2. Flags & Imputationsflags als Integer ──────────────────
flag_vars <- names(df_p)[endsWith(names(df_p), "_f")]
imp_vars  <- names(df_p)[endsWith(names(df_p), "_i")]
df_p[c(flag_vars, imp_vars)] <- lapply(df_p[c(flag_vars, imp_vars)], as.integer)


# ── 3. Faktoren ──────────────────────────────────────────────
df_p <- df_p |>
  mutate(
    
    land = factor(land),
    
    geschlecht = factor(geschlecht,
                        levels = c(1, 2),
                        labels = c("Männlich", "Weiblich")),
    
    interview_art = factor(interview_art,
                           levels = 1:5,
                           labels = c("Face-to-face (CAPI)",
                                      "Face-to-face (Papier)",
                                      "Telefon (CATI)",
                                      "Selbstausfüllen (CASI/Web)",
                                      "Sonstiges")),
    
    proxy_interview = factor(proxy_interview,
                             levels = c(1, 2),
                             labels = c("Proxy-Interview", "Selbstauskunft")),
    
    # Bildung (ISCED 2011)
    bildung_isced = factor(bildung_isced,
                           levels = 0:8,
                           labels = c("ISCED 0 – Vorschule",
                                      "ISCED 1 – Primarstufe",
                                      "ISCED 2 – Sekundarstufe I",
                                      "ISCED 3 – Sekundarstufe II",
                                      "ISCED 4 – Post-sekundär, nicht-tertiär",
                                      "ISCED 5 – Kurzes tertiäres Programm",
                                      "ISCED 6 – Bachelor",
                                      "ISCED 7 – Master",
                                      "ISCED 8 – Doktorat")),
    
    bildung_in_ausbildung = factor(bildung_in_ausbildung,
                                   levels = c(1, 2),
                                   labels = c("Ja", "Nein")),
    
    # Arbeitsmarkt
    erwerbsstatus_selbstdef = factor(erwerbsstatus_selbstdef,
                                     levels = 1:11,
                                     labels = c("Vollzeitbeschäftigt",
                                                "Teilzeitbeschäftigt",
                                                "Selbstständig Vollzeit",
                                                "Selbstständig Teilzeit",
                                                "Arbeitslos",
                                                "Rente/Pension",
                                                "Vorruhestand",
                                                "Dauerhafte Erwerbsunfähigkeit",
                                                "Hausarbeit/Pflege",
                                                "In Ausbildung",
                                                "Sonstiges")),
    
    erwerbsstatus_ilo = factor(erwerbsstatus_ilo,
                               levels = 1:4,
                               labels = c("Erwerbstätig",
                                          "Arbeitslos (ILO)",
                                          "Im Ruhestand",
                                          "Sonstige Nichterwerbspersonen")),
    
    stellung_im_beruf = factor(stellung_im_beruf,
                               levels = 1:4,
                               labels = c("Arbeitnehmer/in",
                                          "Selbstständig ohne Mitarbeiter",
                                          "Selbstständig mit Mitarbeitern",
                                          "Mithelfende/r Familienangehörige/r")),
    
    vollzeit_teilzeit = factor(vollzeit_teilzeit,
                               levels = c(1, 2),
                               labels = c("Vollzeit", "Teilzeit")),
    
    teilzeit_grund = factor(teilzeit_grund,
                            levels = 1:7,
                            labels = c("Eigene Krankheit/Behinderung",
                                       "Betreuungspflichten Kinder",
                                       "Betreuungspflichten Erwachsene",
                                       "Ausbildung",
                                       "Kein Vollzeitjob gefunden",
                                       "Eigener Wunsch",
                                       "Sonstiges")),
    
    befristet_unbefristet = factor(befristet_unbefristet,
                                   levels = c(1, 2),
                                   labels = c("Unbefristet", "Befristet")),
    
    befristung_grund = factor(befristung_grund,
                              levels = 1:5,
                              labels = c("Probezeit/Ausbildungsvertrag",
                                         "Kein unbefristeter Job gefunden",
                                         "Eigener Wunsch",
                                         "Subventionierter Arbeitsplatz",
                                         "Sonstiges")),
    
    mehrere_jobs = factor(mehrere_jobs,
                          levels = c(1, 2),
                          labels = c("Ja – mehrere Jobs", "Nein – ein Job")),
    
    arbeit_von_zuhause = factor(arbeit_von_zuhause,
                                levels = 1:5,
                                labels = c("Nie",
                                           "Selten (< 1 Tag/Woche)",
                                           "Manchmal (1–2 Tage/Woche)",
                                           "Meist (3–4 Tage/Woche)",
                                           "Immer (5 Tage/Woche)")),
    
    # Monatliche Aktivitätskalender (pl211a–l)
    across(starts_with("aktivitaet_m"),
           ~ factor(.x,
                    levels = 1:11,
                    labels = c("Vollzeitbeschäftigt",
                               "Teilzeitbeschäftigt",
                               "Selbstständig Vollzeit",
                               "Selbstständig Teilzeit",
                               "Arbeitslos",
                               "Rente/Pension",
                               "Vorruhestand",
                               "Erwerbsunfähigkeit",
                               "Hausarbeit/Pflege",
                               "In Ausbildung",
                               "Sonstiges"))),
    
    # Gesundheit
    gesundheit_allgemein = factor(gesundheit_allgemein,
                                  levels = 1:5,
                                  labels = c("Sehr gut", "Gut", "Mittelmäßig", "Schlecht", "Sehr schlecht")),
    
    chronisch_krank = factor(chronisch_krank,
                             levels = c(1, 2),
                             labels = c("Ja", "Nein")),
    
    aktivitaet_eingeschraenkt = factor(aktivitaet_eingeschraenkt,
                                       levels = 1:3,
                                       labels = c("Stark eingeschränkt",
                                                  "Eingeschränkt (aber nicht stark)",
                                                  "Nicht eingeschränkt")),
    
    across(c(arzt_konsultiert, spezialist_konsultiert,
             partner_im_hh, mutter_im_hh, vater_im_hh),
           ~ factor(.x, levels = c(1, 2), labels = c("Ja", "Nein"))),
    
    # Materielle Entbehrung (alle 1=ja/2=nein)
    across(starts_with("entbehrung_"),
           ~ factor(.x, levels = c(1, 2), labels = c("Ja – leistbar", "Nein – nicht leistbar"))),
    
    # Ungedeckter Bedarf Gesundheit
    across(c(arzt_verzicht_kosten, arzt_verzicht_wartezeit,
             arzt_verzicht_entfernung, zahnarzt_verzicht_kosten,
             zahnarzt_verzicht_wartezeit, zahnarzt_verzicht_entfernung,
             mental_verzicht_kosten, mental_verzicht_wartezeit),
           ~ factor(.x, levels = c(1, 2), labels = c("Ja – verzichtet", "Nein"))),
    
    # Abgeleitete PX-Variablen
    einkommensquintil = factor(einkommensquintil,
                               levels = 1:5,
                               labels = c("1. Quintil (niedrigstes)",
                                          "2. Quintil",
                                          "3. Quintil (Median)",
                                          "4. Quintil",
                                          "5. Quintil (höchstes)")),
    
    bildung_3gruppen = factor(bildung_3gruppen,
                              levels = 1:3,
                              labels = c("Niedrig (ISCED 0–2)",
                                         "Mittel (ISCED 3–4)",
                                         "Hoch (ISCED 5–8)")),
    
    erwerbsstatus_5gruppen = factor(erwerbsstatus_5gruppen,
                                    levels = 1:5,
                                    labels = c("Erwerbstätig",
                                               "Arbeitslos",
                                               "Im Ruhestand",
                                               "In Ausbildung",
                                               "Sonstige Inaktive"))
  )


# ── 4. Numerische Variablen ──────────────────────────────────
df_p <- df_p |>
  mutate(
    across(c(erhebungsjahr, interview_monat, interview_jahr,
             interview_dauer, alter,
             monate_vollzeit, monate_teilzeit,
             monate_selbst_vollzeit, monate_selbst_teilzeit,
             monate_arbeitslos, monate_rente,
             monate_erwerbsunfaehig, monate_sonstige_inaktiv,
             monate_ausbildung,
             arzt_anzahl_besuche, spezialist_anzahl_besuche,
             bildung_abschluss_jahr), as.integer),
    across(c(woechentliche_arbeitsstd, gesamte_arbeitsstunden,
             nebenjob_stunden), as.numeric),
    across(c(querschnittsgewicht, laengsschnittgewicht,
             aequivalenzgewicht), as.numeric),
    # Alle Einkommensvariablen als numeric
    across(matches("^(lohn|krankenversicherung|firmenwagen|krankentaggeld|
                    selbst_einkommen|pension|al_geld|altersrente|
                    hinterbl|invalidenrente|bildungsbeihilfe|
                    sonstige_transfers|einkommenssteuer|ag_sv|an_sv|
                    bildungsbh|sonst_transfer|hinterbliebenenrente)"),
           as.numeric)
  )


# ── 5. Überblick ─────────────────────────────────────────────
cat("Struktur des umbenannten df_p:\n")
str(df_p)
cat("\nAnzahl Beobachtungen:", nrow(df_p), "\n")
cat("Anzahl Variablen:",     ncol(df_p), "\n")

