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
### household register --> so this is the d file
### Austria --> we have to filter. go to the codebook to see which variables are the country variable

df_d <- tbl(con, "c17d") %>% 
  filter(db020 == "AT") %>% 
  collect(n = Inf)
# test Lara
# test boris 
x<-"test"
x
y<-"HALLO"
y

