library(tidyverse)
library(ltm)

load('data/df.legitimacy.RData')

load('data/df.Rdata')

dflong <-
  dflong %>%
  left_join(df.legitimacy %>% dplyr::select(panelid, legitimacy), by = 'panelid') %>%
  mutate(comp = comp1 + comp2 + comp3 + comp4 + comp5)

dflong %>% group_by(area) %>% summarise(m = mean(legitimacy, na.rm = T))

####################################################

### Measurement models

## 1) Perceptions of police fairness

irt_pj_scores <- 
  dflong %>%
  dplyr::select(pj1:pj4) %>%
  grm %>%
  factor.scores.grm(resp.patterns = 
                      dflong %>%
                      dplyr::select(pj1:pj4)
  )

## 2) Perceptions of police effectiveness

irt_eff_scores <- 
  dflong %>%
  dplyr::select(eff1:eff6) %>%
  grm %>%
  factor.scores.grm(resp.patterns = 
                      dflong %>%
                      dplyr::select(eff1:eff6)
  )

## 3) Perceptions of overpolicing

irt_ovp_scores <- 
  dflong %>%
  dplyr::select(ovp1:ovp2) %>%
  grm %>%
  factor.scores.grm(resp.patterns = 
                      dflong %>%
                      dplyr::select(ovp1:ovp2)
  )

### Deriving new variables

df <-
  dflong %>%
  mutate(fairness = irt_pj_scores$score.dat$z1,
         effectiveness = -irt_eff_scores$score.dat$z1,
         overpolicing = irt_ovp_scores$score.dat$z1)

save(df, file = 'data/export/df_new.Rdata')

####################################################

### Assessing trends

dfwide <-
  dflong %>%
  dplyr::select(quest, wave, gun, stop) %>%
  pivot_wider(id_cols = quest,
              names_from = wave,
              values_from = c('stop', 'gun')) %>%
  mutate(stop_1 = ifelse(stop_1 == T, 1, 0),
         stop_2 = ifelse(stop_2 == T, 1, 0),
         stop_3 = ifelse(stop_3 == T, 1, 0)) %>%
  replace_na(list(gun_1 = 0,
                  gun_2 = 0,
                  gun_3 = 0,
                  stop_1 = 0,
                  stop_2 = 0,
                  stop_3 = 0)) %>%
  mutate(gun.dynamics = case_when(
    gun_1 == 1 & gun_2 == 1 & gun_3 == 1 ~ '1-1-1',
    gun_1 == 0 & gun_2 == 1 & gun_3 == 1 ~ '0-1-1',
    gun_1 == 0 & gun_2 == 0 & gun_3 == 1 ~ '0-0-1',
    gun_1 == 0 & gun_2 == 0 & gun_3 == 0 ~ '0-0-0',
    gun_1 == 1 & gun_2 == 0 & gun_3 == 1 ~ '1-0-1',
    gun_1 == 1 & gun_2 == 1 & gun_3 == 0 ~ '1-1-0',
    gun_1 == 1 & gun_2 == 0 & gun_3 == 0 ~ '1-0-0',
    gun_1 == 0 & gun_2 == 1 & gun_3 == 0 ~ '0-1-0'
  ),
  stop.dynamics = case_when(
    stop_1 == 1 & stop_2 == 1 & stop_3 == 1 ~ '1-1-1',
    stop_1 == 0 & stop_2 == 1 & stop_3 == 1 ~ '0-1-1',
    stop_1 == 0 & stop_2 == 0 & stop_3 == 1 ~ '0-0-1',
    stop_1 == 0 & stop_2 == 0 & stop_3 == 0 ~ '0-0-0',
    stop_1 == 1 & stop_2 == 0 & stop_3 == 1 ~ '1-0-1',
    stop_1 == 1 & stop_2 == 1 & stop_3 == 0 ~ '1-1-0',
    stop_1 == 1 & stop_2 == 0 & stop_3 == 0 ~ '1-0-0',
    stop_1 == 0 & stop_2 == 1 & stop_3 == 0 ~ '0-1-0'
  ))

table(dfwide$stop.dynamics)
table(dfwide$gun.dynamics)