# Load packages

library(tidyverse)
library(readr)
library(rstatix)
library(kableExtra)
library(flextable)
library(ggpubr)
library(ggplot2)

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


hyp %>%
  mutate(cluster = if_else(CLUSTER == "HIGH", 0, 1)) %>%
  ggplot(aes(CLUSTER, AVG_CSA_T1)) + 
  geom_abline(slope = coef(model1)[2], intercept = coef(model1)[1]) +
  geom_point()

### Part 2

model2 <- lm(Squat_3RM_kg ~ AVG_CSA_T1 + T1_BODY_MASS, data = hypertrophy) # Lager en regresjonsmodell hvor vi tester både kroppsvekt og tverrsnittsareal.
summary(model2) # Oppsummerer tallene fra modellen.

tmodel2 <- tidy(model2) # Gjør tallene fra modellen penere og lagrer det i et nytt objekt.

cfmodel2 <- confint(model2) # Finner konfidensintervallene til modellen.

flextable(cbind(tmodel2, cfmodel2)) %>% # Binder sammen konfidensintervallene og regresjonsmodellen til en tabell
  colformat_num(digits = 4) %>%
  set_header_labels(estimate = "Estimat", # Endrer navn 
                    std.error = "Standard Error", 
                    statistic = "T-Statistic",
                    p.value = "P-verdi",
                    term = "Term") %>%
  set_table_properties( width = 1, layout = "autofit") %>% # Gjør tabellen penere.
  add_header_row(values = "Tabell 2", colwidths = 7) %>% # Legger til en overskrift.
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


ggplot(hypertrophy, aes(T1_BODY_MASS, Squat_3RM_kg)) + geom_point() + geom_smooth(method=lm)

ggplot(hypertrophy, aes(AVG_CSA_T1, Squat_3RM_kg)) + geom_point() + geom_smooth(method=lm)

ggqqplot(hypertrophy$T1_BODY_MASS)

ggqqplot(hypertrophy$AVG_CSA_T1)


cor.test(hypertrophy$Squat_3RM_kg, hypertrophy$AVG_CSA_T1)

cor.test(hypertrophy$Squat_3RM_kg, hypertrophy$T1_BODY_MASS)


hyp <- hypertrophy %>%
  select(SUB_ID, Squat_3RM_kg, AVG_CSA_T1) # Velger hvilke variabler vi vil ha med

boxplot(hyp)
  
model0 <- lm(Squat_3RM_kg ~ AVG_CSA_T1, data = hypertrophy)
summary(model0)

tidy(model0)

boxplot(model0)$out

######## JÆJÆJÆJÆJJÆJÆÆJÆ

cor.test(hypertrophy$Squat_3RM_kg, hypertrophy$AVG_CSA_T1)

model0 <- lm(Squat_3RM_kg ~ AVG_CSA_T1, data = hypertrophy)
summary(model0)


model1 <- lm(Squat_3RM_kg ~ T1_BODY_MASS, data = hypertrophy)
summary(model1)

par(mfrow = c(2,2))
plot(model1)


model2 <- lm(Squat_3RM_kg ~ AVG_CSA_T1 + T1_BODY_MASS, data = hypertrophy)
summary(model2)
plot(model2)

cbind(broom::tidy(model2), broom::tidy(confint(model2))) # kombinerer konfidensintervallene og regresjonsanalysen

tmodel2 <- tidy(model2)

cfmodel2 <- confint(model2)

flextable(cbind(tmodel2, cfmodel2)) %>%
  colformat_num(digits = 4) %>%
  set_header_labels(estimate = "Estimate", # Endrer navn 
                    std.error = "Standard Error", 
                    statistic = "T-Statistic",
                    p.value = "P-value",
                    term = "Term") %>%
  set_table_properties( width = 1, layout = "autofit") %>% # Gjør tabellen penere.
  add_header_row(values = "Tabell 1", colwidths = 7) %>% # Legger til en overskrift.
  fontsize(part = "header", size = 12) # Endrer størrelsen på overskriftene.
