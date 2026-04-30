
varnames <- c("Yearsurvey_pb010","Country_pb020","Pid_pb030","Hid_px030","Lohnb_py010g","w_Lohnb_py010g_f","ImputationFactor_py010g_i","Educ_pe040","w_Educ_pe040_f","Empl_pl031","w_Empl_pl031_f","Branch_pl051","w_Branch_pl051_f","Hoursperweek_pl060","w_Hoursperweek_pl060_f","Typeempl_pl140","w_Typeempl_pl140_f","Experienceinyears_pl200","w_Experienceinyears_pl200_f","Birthday_rb080","w_Birthday_rb080_f","Gender_rb090","w_Gender_rb090_f","Maternal_pb190","w_Maternal_pb190_f","Health_ph010","w_Health_ph010_f","Employment2_rb210","w_Employment2_rb210_f","OwnorRent_hh021","w_OwnorRent_hh021_f","Hsize_hx040","Equivalisedhouseholdsize_HX050 - Equivalised household size","Equivaliseddisposableincome_hx090","degruba_db100","w_degruba_db100_f")

colnames(silc) <- varnames
colnames(silc)

silc$Educ_pe040 <- factor(silc$Educ_pe040,
                          levels=sort(c(300,354,500,200,400,450,344,100)),
                          labels=c("Primary education",
"Lower secondary education",
"Upper secondary education","level completion, with direct access to tertiary
education",
"level completion, with direct access to tertiary
education",
"Post-secondary non-tertiary education (not
further specified)","Vocational education",
"Short cycle tertiary "))

silc$Empl_pl031 <- factor(silc$Empl_pl031,levels=sort(c(1,7, 10,  2,  3,  4,  6,  8,  5,  9,11)),labels=c("Employee working full-time","Employee working part-time ","Self-employed working full-time (including
family worker)","Self-employed working part-time (including
family worker)
","Unemployed","Pupil, student, further training, unpaid work
experience","In retirement or in early retirement or has
given up business","Permanently disabled or/and unfit to work","In compulsory military community or service","Fulfilling domestic tasks and care
responsibilities","Other inactive person"))



silc$Branch_pl051 <- factor(
  case_when(
    silc$Branch_pl051 == 0 ~ 0L,
    silc$Branch_pl051 >= 1 & silc$Branch_pl051 < 20 ~ 1L,
    silc$Branch_pl051 >= 20 & silc$Branch_pl051 < 30 ~ 2L,
    silc$Branch_pl051 >= 30 & silc$Branch_pl051 < 40 ~ 3L,
    silc$Branch_pl051 >= 40 & silc$Branch_pl051 < 50 ~ 4L,
    silc$Branch_pl051 >= 50 & silc$Branch_pl051 < 60 ~ 5L,
    silc$Branch_pl051 >= 60 & silc$Branch_pl051 < 70 ~ 6L,
    silc$Branch_pl051 >= 70 & silc$Branch_pl051 < 80 ~ 7L,
    silc$Branch_pl051 >= 80 & silc$Branch_pl051 < 90 ~ 8L,
    silc$Branch_pl051 >= 90 & silc$Branch_pl051 < 100 ~ 9L,
    TRUE ~ NA_integer_
  ),
  levels = 0:9,
  labels = c(
    "Armed forces",                                  # 0
    "Managers",                                      # 1
    "Professionals",                                 # 2
    "Technicians and associate professionals",       # 3
    "Clerical support workers",                      # 4
    "Service and sales workers",                     # 5
    "Skilled agricultural, forestry and fishery workers", # 6
    "Craft and related trades workers",              # 7
    "Plant and machine operators, and assemblers",   # 8
    "Elementary occupations"                         # 9
  )
)




# --- PL140: Type of contract ---

silc$Typeempl_pl140 <- factor(silc$Typeempl_pl140,
                        levels = c(1, 2),
                        labels = c(
                          "Permanent job: work contract of unlimited duration",
                          "Temporary job: work contract of limited duration"
                        )
)

# --- RB090: Sex ---

silc$Gender_rb090 <- factor(silc$Gender_rb090,
                      levels = c(1, 2),
                      labels = c("Male", "Female")
)

# --- PB190: Marital status ---


silc$Maternal_pb190 <- factor(silc$Maternal_pb190,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c(
                          "Never married",
                          "Married",
                          "Separated",
                          "Widowed",
                          "Divorced")
)


# --- PH010: General health ---

silc$Health_ph010 <- factor(silc$Health_ph010,
                      levels = c(1, 2, 3, 4, 5),
                      labels = c(
                        "Very good",
                        "Good",
                        "Fair",
                        "Bad",
                        "Very bad"
                      )
)

# --- RB210: Basic activity status ---
silc$Employment2_rb210 = factor(silc$Employment2_rb210,
                           levels = c(1, 2, 3, 4),
                           labels = c(
                             "At work",
                             "Unemployed",
                             "In retirement or early retirement",
                             "Other inactive person"
                           )
)

# --- HH021: Tenure status ---
silc$OwnorRent_hh021 <- factor(silc$OwnorRent_hh021,
                         levels = c(1, 2, 3, 4, 5),
                         labels = c(
                           "Outright owner",
                           "Owner paying mortgage",
                           "Tenant/subtenant paying rent at prevailing or market rate",
                           "Accommodation is rented at a reduced rate",
                           "Accommodation is provided free"
                         )
)

# --- DB100: Degree of urbanisation ---
silc$degruba_db100 <- factor(silc$degruba_db100,levels=c(1,2,3), labels = c(
"Densely populated area",
"Intermediate area",
"Thinly populated area")
)
