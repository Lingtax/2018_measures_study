#devtools::install_github("lingtax/Qualtrics")
library(psych)
library(ltm)
library(tidyverse)
library(here)
library(corrr)
library(misinformation)

# specify data location
data <-  here("data", "2018measures_data_clean.csv")

df <- read_csv(data)

# Prepare the data --------------------------------------------------------
df1 <- df %>% 
  mutate(sdo01 = sdo01,
         sdo02 = sdo02, 
         sdo03 = 8 - sdo03, 
         sdo04 = 8 - sdo04, 
         sdo05 = sdo05, 
         sdo06 = sdo06, 
         sdo07 = 8 - sdo07, 
         sdo08 = 8 - sdo08,
         sdo_tot = sdo01 + sdo02 + sdo03 + sdo04 + sdo05 + sdo06 + sdo07 + sdo08,
         vax01 = vax01,
         vax02 = 3 - vax02,
         vax03 = 3 - vax03,
         vax04 = 3 - vax04,
         vax05 = 3 - vax05,
         vax06 = vax06,
         vax07 = vax07,
         vax08 = 3 - vax08,
         vax09 = 3 - vax09,
         vax10 = 3 - vax10,
         vax11 = 3 - vax11,
         vax12 = 3 - vax12,
         vax13 = 3 - vax13,
         vax14 = 3 - vax14,
         vax15 = 3 - vax15,
         vax16 = 3 - vax16,
         vax17 = vax17,
         vax18 = vax18,
         vax19 = vax19,
         vax20 = vax20,
         vax21 = vax21,
         vax22 = vax22,
         vax23 = vax23,
         vax24 = 3 - vax24,
         vax25 = 3 - vax25,
         vax26 = vax26,
         vax27 = 3 - vax27,
         trust01 = 8 - trust01,
         trust02 = 8 - trust02,
         trust03 = 8 - trust03,
         trust04 = 8 - trust04,
         trust05 = 8 - trust05,
         trust06 = 8 - trust06,
         trust07 = 8 - trust07,
         trust08 = 8 - trust08,
         trust09 = 8 - trust09,
         trust10 = 8 - trust10,
         trust11 = 8 - trust11,
         trust12 = 8 - trust12,
         trust13 = 8 - trust13,
         trust14 = 8 - trust14,
         trust_tot = trust01 + trust02 + trust03 + trust04 + 
           trust05 + trust06 + trust07 + trust08 + trust09 + 
           trust10 + trust11 + trust12 + trust13 + trust14
         )
rm(df, meta)
# Run the IRT model -------------------------------------------------------

vax <- df1 %>% 
  dplyr::select(starts_with("vax")) %>% 
  mutate_all(funs(.-1))

out <- ltm(vax~z1)

IRTplot(out)


o1 <- factor.scores.ltm(out, vax)  
fscores <- o1$score.dat[, "z1"]

df1 <- df1 %>% 
  mutate(vax = fscores)

correlate(df1[, c("vax", "trust_tot")])

# Reduced item pool -------------------------------------------------------

vax1 <- df1 %>% 
  dplyr::select(vax02, vax04, vax09, vax10, vax18, vax21, vax22) %>% 
  mutate_all(funs(.-1))
out1 <- ltm(vax1~z1)
o2 <- factor.scores.ltm(out1, vax1)  
fscores1 <- o2$score.dat[, "z1"]

df1 <- df1 %>% 
  mutate(vax1 = fscores1)

correlate(df1[, c("vax1", "trust_tot")])

irt2 <-  function(x, diff, disc) {
  1 / (1 + exp(-disc*(x-diff)))
}

df <-  out1 %>%
  coef() %>%
  as.data.frame()

df <-  df %>%
  dplyr::transmute(item = row.names(df),
                   diff = Dffclt,
                   disc = Dscrmn) %>%
  tidyr::crossing(x = seq(-5, 5, .1)) %>%
  dplyr::mutate(y = irt2(x, diff, disc))

ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, colour = item)) +
  ggplot2::geom_line(size = 1.5) +
  ggplot2::theme_classic() + ggplot2::xlab("Difficulty") + ggplot2::ylab("Probability of endorsement") +
  ggplot2::labs(colour= "Item") + geom_hline(yintercept=.5) + theme(text = element_text(size = 15))

i1 <-  IRTplot(out1) 
i1 + geom_hline(yintercept=.5)

# Climate change ----------------------------------------------------------
# Q6_1 - Q6_18  = CCD

ccd <- df %>% 
  dplyr::select(starts_with("Q6_")) %>% 
  mutate_all(funs(.-1))

out <- ltm(ccd~z1)

plot(out)

o1 <- factor.scores.ltm(out, ccd)  
fscores <- o1$score.dat[, 31]

df <- df %>% 
  mutate(ccd = fscores)



# Run a model! ------------------------------------------------------------

describe(df[, c("ccd", "sdo_tot")])
correlate(df[, c("ccd", "sdo_tot")])

ccd_mod <- lm(ccd ~ sdo_tot, data = df)
summary(ccd_mod)

df %>% 
  select(q5_1, q5_2, q5_3, q5_4) %>% 
  apaTables::apa.cor.table()
