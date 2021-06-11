library(tidyverse)
library(texreg)
library(estimatr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)


load('data/df.Rdata')

####################################################

### Two-way fixed-effect models

## Fairness
twfe_pj_contact <- lm_robust(fairness ~ stop, df, fixed_effects = ~ quest + wave, clusters = quest, se_type = 'stata')
twfe_pj_gun     <- lm_robust(fairness ~ gun, df, fixed_effects = ~ quest + wave, clusters = quest, se_type = 'stata')

# Overpolicing
twfe_ovp_contact <- lm_robust(overpolicing ~ stop, df, fixed_effects = ~ quest + wave, clusters = quest, se_type = 'stata')
twfe_ovp_gun     <- lm_robust(overpolicing ~ gun, df, fixed_effects = ~ quest + wave, clusters = quest, se_type = 'stata')

## Cynicism
twfe_cyn_contact <- lm_robust(cynicism ~ stop, df, fixed_effects = ~ quest + wave, clusters = quest, se_type = 'stata')
twfe_cyn_gun     <- lm_robust(cynicism ~ gun, df, fixed_effects = ~ quest + wave, clusters = quest, se_type = 'stata')

# Outputs:
screenreg(list(twfe_pj_contact, twfe_pj_gun, twfe_ovp_contact, twfe_ovp_gun, twfe_cyn_contact, twfe_cyn_gun))
texreg(list(twfe_pj_contact, twfe_pj_gun, twfe_ovp_contact, twfe_ovp_gun, twfe_cyn_contact, twfe_cyn_gun))



## Plotting:
dataplot <- data.frame(outcome = c('cynicism_contact', 'overpolicing_contact', 'fairness_contact',
                                   'cynicism_gun', 'overpolicing_gun', 'fairness_gun'),
                       treatment = c(rep('contact', 3), rep('gun', 3)),
                       coef = c(twfe_cyn_contact$coefficients, twfe_ovp_contact$coefficients, twfe_pj_contact$coefficients,
                                twfe_cyn_gun$coefficients, twfe_ovp_gun$coefficients, twfe_pj_gun$coefficients),
                       ci_low = c(twfe_cyn_contact$conf.low, twfe_ovp_contact$conf.low, twfe_pj_contact$conf.low,
                                  twfe_cyn_gun$conf.low, twfe_ovp_gun$conf.low, twfe_pj_gun$conf.low),
                       ci_upp = c(twfe_cyn_contact$conf.high, twfe_ovp_contact$conf.high, twfe_pj_contact$conf.high,
                                  twfe_cyn_gun$conf.high, twfe_ovp_gun$conf.high, twfe_pj_gun$conf.high))

#####

twfe_plot <- ggplot(dataplot, aes(x = coef, y = outcome, colour = treatment, group = treatment, xmin = -.4, xmax = .4)) + 
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

pdf('plots/twfe_plots.pdf')
twfe_plot
dev.off()
