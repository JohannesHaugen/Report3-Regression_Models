# Load packages

library(tidyverse)
library(readr)
library(rstatix)
library(kableExtra)

download.file(url = "https://ndownloader.figstatic.com/files/14702420", 
              destfile = "./data/hypertrophy.csv") # Laster ned datafilen.

hypertrophy <- read_csv("./data/hypertrophy.csv")  # Laster inn datafilen og kobler den til objektet hypertrophy.

#### Part 1

hypertrophy %>%
  select(SUB_ID, CLUSTER, AVG_CSA_T1) %>% # Velger hvilke variabler vi vil ha med
  filter(!is.na(CLUSTER)) %>% # Filtrer vekk forsøkspersoner uten CLUSTER
  group_by(CLUSTER) # Grupper etter variabelen CLUSTER

  

stat.test <- hypertrophy %>% # Lager et nytt objekt
  t_test(AVG_CSA_T1 ~ CLUSTER, var.equal = TRUE) %>% # Gjennomfører t-testen med valgte variabler med en varians som er registrert som lik.
  add_significance()
stat.test


m1 <- lm(AVG_CSA_T1 ~ CLUSTER, data = hypertrophy)

tidy(m1) %>%
  kable(col.names = c("", "Estimate", "SE", "t-statistic", "p-value"), 
        digits = c(NA, 1, 1, 2, 3))


