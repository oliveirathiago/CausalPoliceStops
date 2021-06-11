library(tidyverse)
library(did)
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
  replace_na(list(gun_1 = 0,
                  gun_2 = 0,
                  gun_3 = 0,
                  stop_1 = 0,
                  stop_2 = 0,
                  stop_3 = 0)) %>%
  mutate(first.treated.stop = ifelse(stop_1 == T, 1,
                                     ifelse(stop_2 == T, 2,
                                            ifelse(stop_3 == T, 3,
                                                   0))),
         first.treated.gun = ifelse(gun_1 == 1, 1,
                                     ifelse(gun_2 == 1, 2,
                                            ifelse(gun_3 == 1, 3,
                                                   0))))

df <- dfwide %>%
  reshape(timevar = 'wave',
          idvar = 'quest',
          direction = 'long',
          sep = '_',
          varying = c(8:85)) %>% 
  filter(!is.na(panelid)) %>%
  arrange(wave)

#criteria.stop <- df$wave == 1 & df$stop == T
#df.stop <- df[criteria.stop == F,]

df.stop <- df[df$first.treated.stop != 1,] %>%
  mutate(quest = as.numeric(quest),
         wave = as.integer(wave),
         stop = as.logical(stop),
         gun = as.logical(gun)) #%>%
  #mutate(first_treated.stop = ifelse(wave == 2 & stop == T, 2,
  #                                   ifelse(wave == 3 & stop == T, 3,
  #                                          0)))


did.fairness.stop <- att_gt(yname = "fairness",
                            tname = "wave",
                            idname = "quest",
                            gname = "first.treated.stop",
                            xformla = ~ white + class + area + age,
                            data = df.stop,
                            control_group = 'nevertreated',
                            allow_unbalanced_panel = T
)

did.overpolicing.stop <- att_gt(yname = "overpolicing",
                                tname = "wave",
                                idname = "quest",
                                gname = "first.treated.stop",
                                xformla = ~ white + class + area + age,
                                data = df.stop,
                                control_group = 'nevertreated',
                                allow_unbalanced_panel = T
)

did.cynicism.stop <- att_gt(yname = "cynicism",
                            tname = "wave",
                            idname = "quest",
                            gname = "first.treated.stop",
                            xformla = ~ white + class + area + age,
                            data = df.stop,
                            control_group = 'nevertreated',
                            allow_unbalanced_panel = T
)

# summarize the results
summary(did.fairness.stop)
summary(did.overpolicing.stop)
summary(did.cynicism.stop)



df.gun <- df[df$first.treated.gun != 1,] %>%
  mutate(quest = as.numeric(quest),
         wave = as.integer(wave),
         stop = as.logical(stop),
         gun = as.logical(gun))

did.fairness.gun <- att_gt(yname = "fairness",
                            tname = "wave",
                            idname = "quest",
                            gname = "first.treated.gun",
                            xformla = ~ white + class + area + age,
                            data = df.gun,
                            allow_unbalanced_panel = T,
                            control_group = 'nevertreated',
                            est_method = 'dl'
)

did.overpolicing.gun <- att_gt(yname = "overpolicing",
                                tname = "wave",
                                idname = "quest",
                                gname = "first.treated.gun",
                                xformla = ~ white + class + area + age,
                                data = df.gun,
                                control_group = 'nevertreated',
                                allow_unbalanced_panel = T
)

did.cynicism.gun <- att_gt(yname = "cynicism",
                            tname = "wave",
                            idname = "quest",
                            gname = "first.treated.gun",
                            xformla = ~ white + class + area + age,
                            data = df.gun,
                            control_group = 'nevertreated',
                            allow_unbalanced_panel = T
)

# summarize the results
summary(did.fairness.gun)
summary(did.overpolicing.gun)
summary(did.cynicism.gun)

results.fairness.stop.aggte <- aggte(did.fairness.stop, type = 'dynamic')
results.overpolicing.stop.aggte <- aggte(did.overpolicing.stop, type = 'dynamic')
results.cynicism.stop.aggte <- aggte(did.cynicism.stop, type = 'dynamic')
results.fairness.gun.aggte <- aggte(did.fairness.gun, type = 'dynamic')
results.overpolicing.gun.aggte <- aggte(did.overpolicing.gun, type = 'dynamic')
results.cynicism.gun.aggte <- aggte(did.cynicism.gun, type = 'dynamic')


summary(results.fairness.stop.aggte)
summary(results.overpolicing.stop.aggte)
summary(results.cynicism.stop.aggte)
summary(results.fairness.gun.aggte)
summary(results.overpolicing.gun.aggte)
summary(results.cynicism.gun.aggte)

ggdid(results.fairness.gun.aggte)


dataplot <- data.frame(outcome = c('cynicism_contact', 'overpolicing_contact', 'fairness_contact',
                                   'cynicism_gun', 'overpolicing_gun', 'fairness_gun'),
                       treatment = c(rep('contact', 3), rep('gun', 3)),
                       coef = c(results.cynicism.stop.aggte$overall.att, results.overpolicing.stop.aggte$overall.att, results.fairness.stop.aggte$overall.att,
                                results.cynicism.gun.aggte$overall.att, results.overpolicing.gun.aggte$overall.att, results.fairness.gun.aggte$overall.att),
                       ci_low = c(results.cynicism.stop.aggte$overall.att - 1.96 * results.cynicism.stop.aggte$overall.se, 
                                  results.overpolicing.stop.aggte$overall.att - 1.96 * results.overpolicing.stop.aggte$overall.se, 
                                  results.fairness.stop.aggte$overall.att - 1.96 * results.fairness.stop.aggte$overall.se,
                                  results.cynicism.gun.aggte$overall.att - 1.96 * results.cynicism.gun.aggte$overall.se,
                                  results.overpolicing.gun.aggte$overall.att - 1.96 * results.overpolicing.gun.aggte$overall.se,
                                  results.fairness.gun.aggte$overall.att - 1.96 * results.fairness.gun.aggte$overall.se),
                       ci_upp = c(results.cynicism.stop.aggte$overall.att + 1.96 * results.cynicism.stop.aggte$overall.se, 
                                  results.overpolicing.stop.aggte$overall.att + 1.96 * results.overpolicing.stop.aggte$overall.se, 
                                  results.fairness.stop.aggte$overall.att + 1.96 * results.fairness.stop.aggte$overall.se,
                                  results.cynicism.gun.aggte$overall.att + 1.96 * results.cynicism.gun.aggte$overall.se,
                                  results.overpolicing.gun.aggte$overall.att + 1.96 * results.overpolicing.gun.aggte$overall.se,
                                  results.fairness.gun.aggte$overall.att + 1.96 * results.fairness.gun.aggte$overall.se))



didgt_plot <- ggplot(dataplot, aes(x = coef, y = outcome, colour = treatment, xmin = -.4, xmax = .5)) + 
  geom_point(size = 3) + 
  geom_vline(aes(xintercept = 0), size = 1) + 
  geom_errorbar(aes(xmin = ci_low, xmax = ci_upp), size = 1.25, width = .2) + 
  scale_color_brewer(palette = "Pastel1", name = "Treatment \n variable", labels = c("Police stop", "Police stop \n at gunpoint")) + 
  labs(y="", x="") +
  scale_y_discrete(limits = c('fairness_contact', 'fairness_gun',
                              'overpolicing_contact', 'overpolicing_gun',
                              'cynicism_contact', 'cynicism_gun'),
                   breaks = c('fairness_contact', 'fairness_gun',
                              'overpolicing_contact', 'overpolicing_gun',
                              'cynicism_contact', 'cynicism_gun'),
                   labels = c("", "Perceptions of                           \n police fairness                          ",
                              "", "Perceptions of                           \n overpolicing                             ",
                              "", "Cynicism about                           \n police protection                        ")) +
  coord_flip() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.line = element_line(colour = "black"))


pdf('plots/didgt_plot.pdf')
didgt_plot
dev.off()

