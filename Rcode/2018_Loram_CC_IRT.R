# Specify packages
# devtools::install_github("Lngtax/misinformation")

library(misinformation)
library(psych)
library(ltm)
library(tidyverse)
library(here)
library(corrr)
library(dplyr)
library(ggpubr)
library(GPArotation)
library(tableone)
library(apaTables)
library(MBESS)


# Data validatation and filtration ----------------------------------------

# The following commented code has been pre-run to clean and verify the data. 
# This code removes some variables inrelated to this scale. 
# 
# # Specify data location
# data <- here("data", "2018measures_data.csv")
# 
# dat <- read_qualtrics(data)
# 
# # Stitch metadata names
# meta <- read_csv(here("data", "2018measures_metadata.csv")) %>% 
#   janitor::clean_names() %>% 
#   mutate(variable_id = tolower(variable_id),
#          variable_label = tolower(variable_label),
#          reverse = as.logical(reverse))
# 
# dat <- dat %>% rename_at(meta$variable_id, ~ meta$variable_label)
# 
# # Remove irrelevant columns
# 
# dat <- select(dat, -x123, -x124, 
#               -end_date, -status, -progress, -duration_seconds, -finished, 
#               -recorded_date, -response_id, -distribution_channel, 
#               -user_language, -vax01, -vax02, -vax03, -vax04, -vax05, -vax06, 
#               -vax07, -vax08, -vax09, -vax10, -vax11, -vax12, -vax13, -vax14, 
#               -vax15, -vax16, -vax17, -vax18, -vax19, -vax20, -vax21, -vax22, 
#               -vax23, -vax24, -vax25, -vax26, -vax27, -psm01, -psm02, -psm03, 
#               -psm04, -psm05)
# 
# write_csv(dat, here("data", "2018_Loramdata.csv"))

# Project initiation ------------------------------------------------------

dat <-  read_csv(here("data", "2018_Loramdata.csv"))

# data cleaning  # fix ages --------------------------------

dat <- dat %>% 
  mutate(age = case_when(
    age == 1994 ~ 24,
    age == 1981 ~ 37, 
    TRUE ~ as.numeric(age)
))

dat <- dat %>% 
  mutate(
    # Convergent measures totals
    # trust in science 
    trust_tot = trust01 + trust02 +trust03 + trust04 + trust05 + trust06 + trust07 + trust08 + trust09 + trust10 + trust11 + 
           trust12 + trust13 + trust14,
    # ASC, reverse code items: 3, 5, 6, 7, 10, 11, 15, 16, 17
         asc03 = 8 - asc03,
         asc05 = 8 - asc05,
         asc06 = 8 - asc06,
         asc07 = 8 - asc07,
         asc10 = 8 - asc10,
         asc11 = 8 - asc11,
         asc15 = 8 - asc15,
         asc16 = 8 - asc16,
         asc17 = 8 - asc17,
         asc_tot = asc01 + asc02 + asc03 + asc04 + asc05 + asc06 + asc07 + asc08 + asc09 + asc10 + asc11 + asc12 + 
           asc13 + asc14 + asc15 + asc16 + asc17+ asc18,
    #reverse code and total SDO scores
         sdo03 = 8 - sdo03,
         sdo04 = 8 - sdo04,
         sdo07 = 8 - sdo07, 
         sdo08 = 8 - sdo08,
         sdo_tot = sdo01 + sdo02 + sdo03 + sdo04 + sdo05 + sdo06 + sdo07 + sdo08,
    # conspiracy total
          cons_tot = cons01 + cons02 + cons03 + cons04 + cons05 + cons06 + cons07 + 
           cons08 + cons09 + cons10 + cons11 + cons12 + cons13 + cons14 + cons15,
    # Recode CCD items, accounting for reversed items 5, 10, 11, 14, 15
         ccd01 = 3 - ccd01,
         ccd02 = 3 - ccd02,
         ccd03 = 3 - ccd03,
         ccd04 = 3 - ccd04,
         ccd06 = 3 - ccd06,
         ccd07 = 3 - ccd07,
         ccd08 = 3 - ccd08,
         ccd09 = 3 - ccd09,
         ccd12 = 3 - ccd12,
         ccd13 = 3 - ccd13,
         ccd16 = 3 - ccd16,
         ccd17 = 3 - ccd17,
         ccd18 = 3 - ccd18
  )
# IRT Model -----------------

# makes a new object containing only denial items, and minuses 1 from them all

ccd <- dat %>% 
  dplyr::select(starts_with("ccd")) %>% 
  mutate_all(funs(.-1))

out <- ltm(ccd~z1)

# Plot model

plot(out)

IRTplot(out)

# Spreads out the x axis

IRTplot(out) + coord_cartesian(xlim = c(-1.5, 3))

out

# Refinining CCD measure

ccdrefined02 <- dat %>% 
  dplyr::select("ccd05", "ccd18", "ccd11", "ccd13", "ccd08", "ccd06", "ccd09", "ccd16") %>% 
  mutate_all(funs(.-1))

out_refined02 <- ltm(ccdrefined02~z1)  

plot(out_refined02)

IRTplot(out_refined02)

# Spreads out the x axis

IRTplot(out_refined02) + coord_cartesian(xlim = c(-.8, 3))

# Estimate participant scores -----------------------

o2 <- factor.scores.ltm(object = out_refined02, resp.patterns = ccdrefined02)  

fscores2 <- o2$score.dat[, "z1"]

fscores2

describe(fscores2)

# Add the scores into a column in the dataframe

dat <- dat %>% 
  mutate(ccd2 = fscores2)

# correlations of convergent measures -------------

datcorCI <- select(dat, "trust_tot", "sdo_tot", "ccd2", "asc_tot", "cons_tot") %>% 
  apa.cor.table()

cor.test(dat$cons_tot, dat$ccd2)

# Reliability Of convergent measures -----

trust <- dat[c("trust01", 
               "trust02",
               "trust03",
               "trust04",
               "trust05",
               "trust06",
               "trust07",
               "trust08",
               "trust09",
               "trust10",
               "trust11",
               "trust12",
               "trust13",
               "trust14")]

ci.reliability(data=trust, type="omega", conf.level = 0.95,interval.type="bca", B=1000)

asc <- dat[c("asc01",
             "asc02",
             "asc03",
             "asc04",
             "asc05",
             "asc06",
             "asc07",
             "asc08",
             "asc09",
             "asc10",
             "asc11",
             "asc12",
             "asc13",
             "asc14",
             "asc15",
             "asc16",
             "asc17",
             "asc18")]

ci.reliability(data=asc, type="omega", conf.level = 0.95,interval.type="bca", B=1000)

sdo <- dat[c("sdo01",
             "sdo02",
             "sdo03",
             "sdo04",
             "sdo05",
             "sdo06",
             "sdo07",
             "sdo08")]

ci.reliability(data=sdo, type="omega", conf.level = 0.95,interval.type="bca", B=1000)

cons <- dat[c("cons01",
              "cons02",
              "cons03",
              "cons04",
              "cons05",
              "cons06",
              "cons07",
              "cons08",
              "cons09",
              "cons10",
              "cons11",
              "cons12",
              "cons13",
              "cons14",
              "cons15")]

ci.reliability(data=cons, type="omega", conf.level = 0.95,interval.type="bca", B=1000)

# of CCD Measure

ci.reliability(data=ccdrefined02, type="omega", conf.level = 0.95,interval.type="bca", B=1000)

# Creating tables --------------------------


tableonevars <- select(dat, age, gender, location, education, trust_tot, asc_tot, sdo_tot, cons_tot) 
catvars <- select(tableonevars, gender, education)
catvars1 <- c("gender", "education")
tab2 <- CreateTableOne(data = tableonevars, factorVars = catvars)
tab2
tab3 <- CreateTableOne(strata = "trt" , data = tableonevars, factorVars = catvars)
tableonevarsnames <- c("age", "gender", "location", "education", "trust_tot", "asc_tot", "sdo_tot", "cons_tot")
tab4 <- CreateTableOne(vars = tableonevarsnames, data = dat, factorVars = catvars1)
print(tab4)
print(tab4, quote = TRUE, noSpaces = TRUE)

