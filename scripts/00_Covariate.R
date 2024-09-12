
## required covariates
# Sex, Race, Center, Education
# Smoking, Drinking
# APOE

## required outcomes
# Fat mass, TG, TC, HDL-C, LDL-C, BMI
# Diabetes, Hypertension, Heart failure, myocardial infarction
# Fall, Frailty, overall cognition, SPPB, depression

## Set data working path
setwd('/Users/gsn/Library/CloudStorage/OneDrive-JohnsHopkins/【JHU】Aging-RA/ARIC_Data/')

# Extract Data
## Sex, Race, Center, Education
dat.1 = read_dta('Data/ARIC/V1/derive13.dta') |>
  select(id, gender, racegrp, center, elevel02) |>
  rename(subjectid = id,
         Race = racegrp,
         Center = center,
         Education = elevel02)

## Smoking, Drinking
dat.2 = read_dta('Data/ARIC/V7/derive71.dta') |>
  select(id, cigt72, drnkr71) |>
  rename(subjectid = id,
         Smoking = cigt72,
         Drinking = drnkr71)

## APOE
dat.3 = read_dta('Data/ARIC/Genetics/apoe_new_p.dta') |>
  select(id, final_apoe) |>
  rename(subjectid = id,
         APOE = final_apoe) |>
  mutate(APOE = case_when(grepl('4', APOE) ~ 'Risk',
                          grepl('2', APOE) ~ 'Protect',
                          grepl('3', APOE) ~ 'Homo',
                          TRUE~NA)) |>
  mutate(APOE = case_when(APOE %in% c('Homo', 'Protect') ~ 'No ε4 allele',
                          is.na(APOE) ~ NA,
                          TRUE ~ '≥1 APOE ε4 allelel'))

## Fat mass
dat.4 = read_dta('Data/ARIC/V9/ant_220622.dta') |>
  select(SubjectID, ANT7) |>
  rename(Fat_mass = ANT7,
         subjectid = SubjectID)

## TG, TC, HDL-C, LDL-C
dat.5 = read_dta('Data/ARIC/V9/longlabv1v10_240508_np.dta') |>
  filter(visit == 9) |>
  select(subjectid, value_tc, value_tg, value_hdl, value_ldl) |>
  rename(TC = value_tc,
         TG = value_tg,
         HDL_C = value_hdl,
         LDL_C = value_ldl)

## BMI, frailty, SPPB
dat.6 = read_dta('Data/ARIC/V9/derive91_230719_np.dta') |>
  select(id, frailty91b, bmi91, sppb91) |>
  rename(subjectid = id,
         frailty = frailty91b,
         BMI = bmi91,
         SPPB = sppb91)
  
## Cognition
dat.7 = read_dta('Data/ARIC/V9/v2_v9_cnfa_np_220511.dta') |>
  filter(vtype == 'V9NCS') |>
  select(subjectid, GLOBZ_NCTS, GLOBALFS1) |>
  rename(overall = GLOBZ_NCTS)

## Depression
dat.8 = read_dta('Data/ARIC/V9/derive91_220622.dta') |>
  select(ID, CESD91) |>
  rename(subjectid = ID,
         `CES-D` = CESD91)

# Combine Covariate and Outcome together
library(dplyr)
merge(dat.1, dat.2)
list_of_dfs = list(dat.1, dat.2, dat.3, dat.4, dat.5,
                   dat.6, dat.7, dat.8)
merged_df <- reduce(list_of_dfs, full_join, by = "subjectid")
