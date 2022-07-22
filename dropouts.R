library(tidyverse)
library(mfx)
options(scipen=999)

load('data/export/df_new.RData')

dfwide <- 
  df %>%
  pivot_wider(id_cols = quest:area,
              names_from = wave,
              values_from = c(age:overpolicing)) %>%
  mutate(missingt2 = is.na(panelid_2),
         missingt3 = is.na(panelid_3))

# predicting dropouts
t2 <- glm(missingt2 ~ fairness_1 + overpolicing_1 + legitimacy_1 + effectiveness_1 + male + white + class + age_1,
          family = binomial(link = 'logit'), dfwide)
t3 <- glm(missingt3 ~ fairness_1 + overpolicing_1 + legitimacy_1 + effectiveness_1 + male + white + class + age_1,
          family = binomial(link = 'logit'), dfwide)

logitmfx(t2, data = dfwide, atmean = F)
logitmfx(t3, data = dfwide, atmean = F)
