library(tidyverse)
library(mfx)
options(scipen=999)

load('data/df.RData')

dfwide <- df %>%
  reshape(timevar = 'wave',
          idvar = 'quest',
          direction = 'wide',
          sep = '_') %>%
  dplyr::select(-c('class_2', 'white_2', 'male_2', 'area_2', 'gun.dynamics_2', 'stop.dynamics_2',
                   'class_3', 'white_3', 'male_3', 'area_3', 'gun.dynamics_3', 'stop.dynamics_3')) %>%
  rename('class' = 'class_1',
         'white' = 'white_1',
         'male' = 'male_1',
         'area' = 'area_1',
         'gun.dynamics' = 'gun.dynamics_1',
         'stop.dynamics' = 'stop.dynamics_1') %>%
  mutate(missingt2 = is.na(panelid_2),
         missingt3 = is.na(panelid_3))

# predicting dropouts
t2 <- glm(missingt2 ~ fairness_1 + overpolicing_1 + cynicism_1 + male + white + class + age_1,
          family = binomial(link = 'logit'), dfwide)
t3 <- glm(missingt3 ~ fairness_1 + overpolicing_1 + cynicism_1 + male + white + class + age_1,
          family = binomial(link = 'logit'), dfwide)

logitmfx(t2, data = dfwide, atmean = F)
logitmfx(t3, data = dfwide, atmean = F)
