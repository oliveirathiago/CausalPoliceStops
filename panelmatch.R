library(tidyverse)
library(PanelMatch)
options(scipen=999)

load('data/export/df_new.RData')

df <- df %>%
  mutate(quest = as.numeric(quest),
         wave = as.integer(wave),
         stop = as.integer(stop),
         gun = as.integer(gun))

# CHECKING FOR COVARIATE BALANCE

## TREATMENT: POLICE STOP

### perceived police fairness

m.fairness.stop.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                treatment = "stop", refinement.method = "mahalanobis", 
                                                data = df, match.missing = TRUE, 
                                                covs.formula = ~ age + I(lag(fairness, 1:1)) + fearpolgen + I(lag(comp, 1:1)),
                                                size.match = 5, qoi = "att", outcome.var = "fairness",
                                                lead = 0, forbid.treatment.reversal = F, 
                                                use.diagonal.variance.matrix = F)

get_covariate_balance(m.fairness.stop.cont.matching_1ag$att,
                      data = df, covariates = c("age", "fairness", "fearpolgen", "comp"),
                      plot = F)

### perceived police effectiveness

m.effectiveness.stop.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                     treatment = "stop", refinement.method = "mahalanobis", 
                                                     data = df, match.missing = TRUE, 
                                                     covs.formula = ~ age + I(lag(effectiveness, 1:1)) + fearpolgen + I(lag(comp, 1:1)),
                                                     size.match = 5, qoi = "att", outcome.var = "effectiveness",
                                                     lead = 0, forbid.treatment.reversal = F, 
                                                     use.diagonal.variance.matrix = F)

get_covariate_balance(m.effectiveness.stop.cont.matching_1ag$att,
                      data = df, covariates = c("age", "effectiveness", "fearpolgen", "comp"),
                      plot = F)

### perceived overpolicing

m.overpolicing.stop.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                    treatment = "stop", refinement.method = "mahalanobis", 
                                                    data = df, match.missing = TRUE, 
                                                    covs.formula = ~ age + I(lag(overpolicing, 1:1)) + fearpolgen + I(lag(comp, 1:1)),
                                                    size.match = 5, qoi = "att", outcome.var = "overpolicing",
                                                    lead = 0, forbid.treatment.reversal = F, 
                                                    use.diagonal.variance.matrix = F)

get_covariate_balance(m.overpolicing.stop.cont.matching_1ag$att,
                      data = df, covariates = c("age", "overpolicing", "fearpolgen", "comp"),
                      plot = F)

### legitimacy

m.legitimacy.stop.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                  treatment = "stop", refinement.method = "mahalanobis", 
                                                  data = df, match.missing = TRUE, 
                                                  covs.formula = ~ age + I(lag(legitimacy, 1:1)) + fearpolgen + I(lag(comp, 1:1)),
                                                  size.match = 5, qoi = "att", outcome.var = "legitimacy",
                                                  lead = 0, forbid.treatment.reversal = F, 
                                                  use.diagonal.variance.matrix = F)

get_covariate_balance(m.overpolicing.stop.cont.matching_1ag$att,
                      data = df, covariates = c("age", "legitimacy", "fearpolgen", "comp"),
                      plot = F)

########################

## TREATMENT: POLICE STOP at gun point

### perceived police fairness

m.fairness.gun.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                               treatment = "gun", refinement.method = "mahalanobis", 
                                               data = df, match.missing = TRUE, 
                                               covs.formula = ~ age + I(lag(fairness, 1:1)) + fearpolgen + I(lag(comp, 1:1)),
                                               size.match = 5, qoi = "att", outcome.var = "fairness",
                                               lead = 0, forbid.treatment.reversal = F, 
                                               use.diagonal.variance.matrix = F)

get_covariate_balance(m.fairness.gun.cont.matching_1ag$att,
                      data = df, covariates = c("age", "fairness", "fearpolgen", "comp"),
                      plot = F)

### perceived police effectiveness

m.effectiveness.gun.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                    treatment = "gun", refinement.method = "mahalanobis", 
                                                    data = df, match.missing = TRUE, 
                                                    covs.formula = ~ age + I(lag(effectiveness, 1:1)) + fearpolgen + I(lag(comp, 1:1)),
                                                    size.match = 5, qoi = "att", outcome.var = "effectiveness",
                                                    lead = 0, forbid.treatment.reversal = F, 
                                                    use.diagonal.variance.matrix = F)


get_covariate_balance(m.effectiveness.gun.cont.matching_1ag$att,
                      data = df, covariates = c("age", "effectiveness", "fearpolgen", "comp"),
                      plot = F)

### perceived overpolicing

m.overpolicing.gun.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                   treatment = "gun", refinement.method = "mahalanobis", 
                                                   data = df, match.missing = TRUE, 
                                                   covs.formula = ~ age + I(lag(overpolicing, 1:1)) + fearpolgen + I(lag(comp, 1:1)),
                                                   size.match = 5, qoi = "att", outcome.var = "overpolicing",
                                                   lead = 0, forbid.treatment.reversal = F, 
                                                   use.diagonal.variance.matrix = F)

get_covariate_balance(m.overpolicing.gun.cont.matching_1ag$att,
                      data = df, covariates = c("age", "overpolicing", "fearpolgen", "comp"),
                      plot = F)

### perceived legitimacy

m.legitimacy.gun.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                 treatment = "gun", refinement.method = "mahalanobis", 
                                                 data = df, match.missing = TRUE, 
                                                 covs.formula = ~ age + I(lag(legitimacy, 1:1)) + fearpolgen + I(lag(comp, 1:1)),
                                                 size.match = 5, qoi = "att", outcome.var = "legitimacy",
                                                 lead = 0, forbid.treatment.reversal = F, 
                                                 use.diagonal.variance.matrix = F)

get_covariate_balance(m.legitimacy.gun.cont.matching_1ag$att,
                      data = df, covariates = c("age", "legitimacy", "fearpolgen", "comp"),
                      plot = F)

########

m.offending.gun.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                treatment = "gun", refinement.method = "mahalanobis", 
                                                data = df, match.missing = TRUE, 
                                                covs.formula = ~ age + fearpolgen + I(lag(comp, 1:1)),
                                                size.match = 5, qoi = "att", outcome.var = "comp",
                                                lead = 0, forbid.treatment.reversal = F, 
                                                use.diagonal.variance.matrix = F)
get_covariate_balance(m.offending.gun.cont.matching_1ag$att,
                      data = df, covariates = c("age", "legitimacy", "comp"),
                      plot = F)
results.offending.stop <- PanelEstimate(sets = m.offending.gun.cont.matching_1ag, data = df)
summary(results.offending.stop)
##########################

# ESTIMATE ATT
set.seed(1234)

## TREATMENT: POLICE STOP

results.fairness.stop <- PanelEstimate(sets = m.fairness.stop.cont.matching_1ag, data = df)
results.effectiveness.stop <- PanelEstimate(sets = m.effectiveness.stop.cont.matching_1ag, data = df)
results.overpolicing.stop <- PanelEstimate(sets = m.overpolicing.stop.cont.matching_1ag, data = df)
results.legitimacy.stop <- PanelEstimate(sets = m.legitimacy.stop.cont.matching_1ag, data = df)
summary(results.fairness.stop)
summary(results.effectiveness.stop)
summary(results.overpolicing.stop)
summary(results.legitimacy.stop)

## TREATMENT: POLICE STOP AT GUN POINT

results.fairness.gun <- PanelEstimate(sets = m.fairness.gun.cont.matching_1ag, data = df)
results.effectiveness.gun <- PanelEstimate(sets = m.effectiveness.gun.cont.matching_1ag, data = df)
results.overpolicing.gun <- PanelEstimate(sets = m.overpolicing.gun.cont.matching_1ag, number.iterations = 1000, data = df)
results.legitimacy.gun <- PanelEstimate(sets = m.legitimacy.gun.cont.matching_1ag, number.iterations = 1000, data = df)
summary(results.fairness.gun)
summary(results.effectiveness.gun)
summary(results.overpolicing.gun)
summary(results.legitimacy.gun)

##########################


dataplot <- data.frame(outcome = c('fairness_contact', 'effectiveness_contact', 'overpolicing_contact', 'legitimacy_contact',
                                   'fairness_gun', 'effectiveness_gun', 'overpolicing_gun', 'legitimacy_gun'),
                       treatment = c(rep('contact', 4), rep('gun', 4)),
                       coef = c(results.fairness.stop$estimates, results.effectiveness.stop$estimates, results.overpolicing.stop$estimates, results.legitimacy.stop$estimates,
                                results.fairness.gun$estimates, results.effectiveness.gun$estimates, results.overpolicing.gun$estimates, results.legitimacy.gun$estimates),
                       ci_low = c(quantile(results.fairness.stop$bootstrapped.estimates, 0.025), 
                                  quantile(results.effectiveness.stop$bootstrapped.estimates, 0.025), 
                                  quantile(results.overpolicing.stop$bootstrapped.estimates, 0.025), 
                                  quantile(results.legitimacy.stop$bootstrapped.estimates, 0.025),
                                  #
                                  quantile(results.fairness.gun$bootstrapped.estimates, 0.025), 
                                  quantile(results.effectiveness.gun$bootstrapped.estimates, 0.025), 
                                  quantile(results.overpolicing.gun$bootstrapped.estimates, 0.025), 
                                  quantile(results.legitimacy.gun$bootstrapped.estimates, 0.025)),
                       ci_upp = c(quantile(results.fairness.stop$bootstrapped.estimates, 0.975), 
                                  quantile(results.effectiveness.stop$bootstrapped.estimates, 0.975), 
                                  quantile(results.overpolicing.stop$bootstrapped.estimates, 0.975), 
                                  quantile(results.legitimacy.stop$bootstrapped.estimates, 0.975),
                                  #
                                  quantile(results.fairness.gun$bootstrapped.estimates, 0.975), 
                                  quantile(results.effectiveness.gun$bootstrapped.estimates, 0.975), 
                                  quantile(results.overpolicing.gun$bootstrapped.estimates, 0.975),
                                  quantile(results.legitimacy.gun$bootstrapped.estimates, 0.975)))

matching_plot_stop <- ggplot(dataplot %>% filter(treatment == 'contact'), aes(x = coef, y = outcome, xmin = -.7, xmax = .7)) + 
  geom_point(size = 3, color = 'deepskyblue4') + 
  geom_vline(aes(xintercept = 0), size = .7, color = 'darkgray') + 
  geom_errorbar(aes(xmin = ci_low, xmax = ci_upp), size = 1.25, width = .2, color = 'deepskyblue4') + 
  labs(y="", x="") + 
  coord_flip() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_discrete(limits = c('fairness_contact',
                              'effectiveness_contact',
                              'overpolicing_contact',
                              'legitimacy_contact'),
                   breaks = c('fairness_contact',
                              'effectiveness_contact',
                              'overpolicing_contact',
                              'legitimacy_contact'),
                   labels = c("Procedural fairness",
                              "Police effectiveness",
                              "Overpolicing",
                              "Police legitimacy"))

pdf('plots/matching_plot_stop.pdf', width = 8, height = 6)
matching_plot_stop
dev.off()

matching_plot_gun <- ggplot(dataplot %>% filter(treatment == 'gun'), aes(x = coef, y = outcome, xmin = -.7, xmax = .7)) + 
  geom_point(size = 3, color = 'firebrick4') + 
  geom_vline(aes(xintercept = 0), size = .7, color = 'darkgray') + 
  geom_errorbar(aes(xmin = ci_low, xmax = ci_upp), size = 1.25, width = .2, color = 'firebrick4') + 
  labs(y="", x="") + 
  coord_flip() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_discrete(limits = c('fairness_gun',
                              'effectiveness_gun',
                              'overpolicing_gun',
                              'legitimacy_gun'),
                   breaks = c('fairness_gun',
                              'effectiveness_gun',
                              'overpolicing_gun',
                              'legitimacy_gun'),
                   labels = c("Procedural fairness",
                              "Police effectiveness",
                              "Overpolicing",
                              "Police legitimacy"))

pdf('plots/matching_plot_gun.pdf', width = 8, height = 6)
matching_plot_gun
dev.off()
