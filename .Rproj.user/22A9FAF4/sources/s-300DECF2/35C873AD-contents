library(tidyverse)
library(PanelMatch)
options(scipen=999)

load('data/df.RData')

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
                                                covs.formula = ~ age + I(lag(fairness, 1:1)),
                                                size.match = 3, qoi = "att", outcome.var = "fairness",
                                                lead = 0, forbid.treatment.reversal = F, 
                                                use.diagonal.variance.matrix = F)

get_covariate_balance(m.fairness.stop.cont.matching_1ag$att,
                      data = df, covariates = c("age", "fairness"),
                      plot = F)

### overpolicing

m.overpolicing.stop.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                treatment = "stop", refinement.method = "mahalanobis", 
                                                data = df, match.missing = TRUE, 
                                                covs.formula = ~ age + I(lag(overpolicing, 1:1)),
                                                size.match = 3, qoi = "att", outcome.var = "overpolicing",
                                                lead = 0, forbid.treatment.reversal = F, 
                                                use.diagonal.variance.matrix = F)

get_covariate_balance(m.overpolicing.stop.cont.matching_1ag$att,
                      data = df, covariates = c("age", "overpolicing"),
                      plot = F)

### cynicism

m.cynicism.stop.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                    treatment = "stop", refinement.method = "mahalanobis", 
                                                    data = df, match.missing = TRUE, 
                                                    covs.formula = ~ age + I(lag(cynicism, 1:1)),
                                                    size.match = 3, qoi = "att", outcome.var = "cynicism",
                                                    lead = 0, forbid.treatment.reversal = F, 
                                                    use.diagonal.variance.matrix = F)

get_covariate_balance(m.cynicism.stop.cont.matching_1ag$att,
                      data = df, covariates = c("age", "cynicism"),
                      plot = F)

########################

## TREATMENT: POLICE STOP at gun point

### perceived police fairness

m.fairness.gun.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                treatment = "gun", refinement.method = "mahalanobis", 
                                                data = df, match.missing = TRUE, 
                                                covs.formula = ~ age + I(lag(fairness, 1:1)),
                                                size.match = 1, qoi = "att", outcome.var = "fairness",
                                                lead = 0, forbid.treatment.reversal = F, 
                                                use.diagonal.variance.matrix = F)

get_covariate_balance(m.fairness.gun.cont.matching_1ag$att,
                      data = df, covariates = c("age", "fairness"),
                      plot = F)

### overpolicing

m.overpolicing.gun.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                    treatment = "gun", refinement.method = "mahalanobis", 
                                                    data = df, match.missing = TRUE, 
                                                    covs.formula = ~ age + I(lag(overpolicing, 1:1)),
                                                    size.match = 1, qoi = "att", outcome.var = "overpolicing",
                                                    lead = 0, forbid.treatment.reversal = F, 
                                                    use.diagonal.variance.matrix = F)


get_covariate_balance(m.overpolicing.gun.cont.matching_1ag$att,
                      data = df, covariates = c("age", "overpolicing"),
                      plot = F)
m.cynicism.gun.cont.matching_1ag <- PanelMatch(lag = 1, time.id = "wave", unit.id = "quest", 
                                                treatment = "gun", refinement.method = "mahalanobis", 
                                                data = df, match.missing = TRUE, 
                                                covs.formula = ~ age + I(lag(cynicism, 1:1)),
                                                size.match = 1, qoi = "att", outcome.var = "cynicism",
                                                lead = 0, forbid.treatment.reversal = F, 
                                                use.diagonal.variance.matrix = F)

get_covariate_balance(m.cynicism.gun.cont.matching_1ag$att,
                      data = df, covariates = c("age", "cynicism"),
                      plot = F)

##########################

# ESTIMATE ATT

## TREATMENT: POLICE STOP

results.fairness.stop <- PanelEstimate(sets = m.fairness.stop.cont.matching_1ag, data = df)
results.overpolicing.stop <- PanelEstimate(sets = m.overpolicing.stop.cont.matching_1ag, data = df)
results.cynicism.stop <- PanelEstimate(sets = m.cynicism.stop.cont.matching_1ag, data = df)
summary(results.fairness.stop)
summary(results.overpolicing.stop)
summary(results.cynicism.stop)

## TREATMENT: POLICE STOP AT GUN POINT

results.fairness.gun <- PanelEstimate(sets = m.fairness.gun.cont.matching_1ag, data = df)
results.overpolicing.gun <- PanelEstimate(sets = m.overpolicing.gun.cont.matching_1ag, number.iterations = 1000, data = df)
results.cynicism.gun <- PanelEstimate(sets = m.cynicism.gun.cont.matching_1ag, data = df)
summary(results.fairness.gun)
summary(results.overpolicing.gun)
summary(results.cynicism.gun)

##########################


dataplot <- data.frame(outcome = c('cynicism_contact', 'overpolicing_contact', 'fairness_contact',
                                   'cynicism_gun', 'overpolicing_gun', 'fairness_gun'),
                       treatment = c(rep('contact', 3), rep('gun', 3)),
                       coef = c(results.cynicism.stop$estimates, results.overpolicing.stop$estimates, results.fairness.stop$estimates,
                                results.cynicism.gun$estimates, results.overpolicing.gun$estimates, results.fairness.gun$estimates),
                       ci_low = c(quantile(results.cynicism.stop$bootstrapped.estimates, 0.025),quantile(results.overpolicing.stop$bootstrapped.estimates, 0.025),
                                  quantile(results.fairness.stop$bootstrapped.estimates, 0.025), quantile(results.cynicism.gun$bootstrapped.estimates, 0.025),
                                  quantile(results.overpolicing.gun$bootstrapped.estimates, 0.025), quantile(results.fairness.gun$bootstrapped.estimates, 0.025)),
                       ci_upp = c(quantile(results.cynicism.stop$bootstrapped.estimates, 0.975), quantile(results.overpolicing.stop$bootstrapped.estimates, 0.975),
                                  quantile(results.fairness.stop$bootstrapped.estimates, 0.975), quantile(results.cynicism.gun$bootstrapped.estimates, 0.975),
                                  quantile(results.overpolicing.gun$bootstrapped.estimates, 0.975), quantile(results.fairness.gun$bootstrapped.estimates, 0.975)))

matching_plot <- ggplot(dataplot, aes(x = coef, y = outcome, colour = treatment, xmin = -.5, xmax = .4)) + 
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

pdf('plots/matching_plots.pdf')
matching_plot
dev.off()