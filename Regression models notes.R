# Load packages

library(tidyverse)
library(readr)
library(rstatix)
library(kableExtra)
library(flextable)
library(ggpubr)

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


tidy(model1) %>%
  flextable() %>%
  colformat_num(digits = 3) %>%
  set_header_labels(estimate = "Estimate", # Endrer navn 
                    std.error = "Standard Error", 
                    statistic = "Statistic",
                    p.value = "P-value",
                    term = "Term") %>%
  set_table_properties( width = 1, layout = "autofit") %>% # Gjør tabellen penere.
  add_header_row(values = "Tabell 1", colwidths = 5) %>% # Legger til en overskrift.
  fontsize(part = "header", size = 12) # Endrer størrelsen på overskriftene.

### Part 2


glimpse(hypertrophy)

model2 <- lm(AVG_CSA_T1 ~ Squat_3RM_kg, data = hypertrophy)
model2

plot(model2, pch = 16, col = "blue") #Plot the results
abline(lmTemp)

tidy(model2) %>%
  flextable() %>%
  colformat_num(digits = 3) %>%
  set_header_labels(estimate = "Estimate", # Endrer navn 
                    std.error = "Standard Error", 
                    statistic = "Statistic",
                    p.value = "P-value",
                    term = "Term") %>%
  set_table_properties( width = 1, layout = "autofit") %>% # Gjør tabellen penere.
  add_header_row(values = "Tabell 1", colwidths = 5) %>% # Legger til en overskrift.
  fontsize(part = "header", size = 12) # Endrer størrelsen på overskriftene.



### Morro ######

glimpse(hypertrophy) %>%
  scatter.smooth(x=hypertrophy$FAST_CSA_T1, y=hypertrophy$Squat_3RM_kg, main="Dist ~ Speed")  # scatterplot

hypertrophy %>%
  ggplot(aes(FAST_CSA_T1, Squat_3RM_kg)) + geom_point() + theme_minimal()

cor.test(hypertrophy$FAST_CSA_T1, hypertrophy$Squat_3RM_kg, method = "spearman") # Korrelasjonstest

shapiro.test(hypertrophy$FAST_CSA_T1)

ggqqplot(hypertrophy$FAST_CSA_T1)

model3 <- lm(Squat_3RM_kg ~ FAST_CSA_T1, data = hypertrophy)
tidy(model3) %>%
  flextable() %>%
  colformat_num(digits = 3) %>%
  set_header_labels(estimate = "Estimate", # Endrer navn 
                    std.error = "Standard Error", 
                    statistic = "Statistic",
                    p.value = "P-value",
                    term = "Term") %>%
  set_table_properties( width = 1, layout = "autofit") %>% # Gjør tabellen penere.
  add_header_row(values = "Tabell 1", colwidths = 5) %>% # Legger til en overskrift.
  fontsize(part = "header", size = 12) # Endrer størrelsen på overskriftene.
