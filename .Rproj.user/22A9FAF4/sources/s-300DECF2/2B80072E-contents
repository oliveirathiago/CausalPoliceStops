library(tidyverse)
library(ltm)

load('data/dflong.Rdata')

####################################################

### Measurement models

## 1) Cynicism about police protection

irt_und <- grm(dflong[, c('und1', 'und2', 'und3')])#, Hessian = T)#sconstrained = F)#start.val = c(rep(1, 16)))
irt_und

irt_und_scores <- factor.scores.grm(irt_und, resp.patterns = dflong[, c('und1', 'und2', 'und3')])
dflong$cynicism <- -irt_und_scores$score.dat$z1


## 2) Perceptions of overpolicing

irt_ovp <- grm(dflong[, c('ovp1', 'ovp2')])
irt_ovp

irt_ovp_scores <- factor.scores.grm(irt_ovp, resp.patterns = dflong[, c('ovp1', 'ovp2')])
dflong$overpolicing <- irt_ovp_scores$score.dat$z1


## 3) Perceptions of police fairness

irt_pj <- grm(dflong[, c('pj1', 'pj2', 'pj3', 'pj4')])
irt_pj

irt_pj_scores <- factor.scores.grm(irt_pj, resp.patterns = dflong[, c('pj1', 'pj2', 'pj3', 'pj4')])
dflong$fairness <- irt_pj_scores$score.dat$z1

####################################################

### Assessing trends

dfwide <- dflong %>%
  reshape(timevar = 'wave',
          idvar = 'quest',
          direction = 'wide',
          sep = '_') %>%
  dplyr::select(-c('class_2', 'white_2', 'male_2', 'area_2',
                   'class_3', 'white_3', 'male_3', 'area_3')) %>%
  rename('class' = 'class_1',
         'white' = 'white_1',
         'male' = 'male_1',
         'area' = 'area_1') %>%
  mutate(stopall_1 = ifelse(stop_1==T,1,0),
         stopall_2 = ifelse(stop_2==T,1,0),
         stopall_3 = ifelse(stop_3==T,1,0)) %>%
  replace_na(list(gun_1 = 0,
                  gun_2 = 0,
                  gun_3 = 0,
                  stopall_1 = 0,
                  stopall_2 = 0,
                  stopall_3 = 0))

dfwide <- dfwide %>%
  mutate(gun.dynamics = ifelse(dfwide$gun_1 == 1 & dfwide$gun_2 == 1 & dfwide$gun_3 == 1, "1-1-1",
                               ifelse(dfwide$gun_1 == 0 & dfwide$gun_2 == 1 & dfwide$gun_3 == 1, "0-1-1",
                                      ifelse(dfwide$gun_1 == 0 & dfwide$gun_2 == 0 & dfwide$gun_3 == 1, "0-0-1",
                                             ifelse(dfwide$gun_1 == 0 & dfwide$gun_2 == 0 & dfwide$gun_3 == 0, "0-0-0",
                                                    ifelse(dfwide$gun_1 == 1 & dfwide$gun_2 == 0 & dfwide$gun_3 == 1, "1-0-1",
                                                           ifelse(dfwide$gun_1 == 1 & dfwide$gun_2 == 1 & dfwide$gun_3 == 0, "1-1-0",
                                                                  ifelse(dfwide$gun_1 == 1 & dfwide$gun_2 == 0 & dfwide$gun_3 == 0, "1-0-0",
                                                                         ifelse(dfwide$gun_1 == 0 & dfwide$gun_2 == 1 & dfwide$gun_3 == 0, "0-1-0",
                                                                                "NA"))))))))) %>%
  mutate(stop.dynamics = ifelse(dfwide$stopall_1 == 1 & dfwide$stopall_2 == 1 & dfwide$stopall_3 == 1, "1-1-1",
                                ifelse(dfwide$stopall_1 == 0 & dfwide$stopall_2 == 1 & dfwide$stopall_3 == 1, "0-1-1",
                                       ifelse(dfwide$stopall_1 == 0 & dfwide$stopall_2 == 0 & dfwide$stopall_3 == 1, "0-0-1",
                                              ifelse(dfwide$stopall_1 == 0 & dfwide$stopall_2 == 0 & dfwide$stopall_3 == 0, "0-0-0",
                                                     ifelse(dfwide$stopall_1 == 1 & dfwide$stopall_2 == 0 & dfwide$stopall_3 == 1, "1-0-1",
                                                            ifelse(dfwide$stopall_1 == 1 & dfwide$stopall_2 == 1 & dfwide$stopall_3 == 0, "1-1-0",
                                                                   ifelse(dfwide$stopall_1 == 1 & dfwide$stopall_2 == 0 & dfwide$stopall_3 == 0, "1-0-0",
                                                                          ifelse(dfwide$stopall_1 == 0 & dfwide$stopall_2 == 1 & dfwide$stopall_3 == 0, "0-1-0",
                                                                                 "NA")))))))))

df <- dfwide %>%
  reshape(timevar = 'wave',
          idvar = 'quest',
          direction = 'long',
          sep = '_',
          varying = c(6:83)) %>% 
  filter(!is.na(panelid)) %>%
  arrange(wave)


save(df, file = 'data/df.Rdata')