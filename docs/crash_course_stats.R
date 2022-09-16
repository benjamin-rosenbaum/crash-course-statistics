# Crash course statistics
# Benjamin Rosenbaum
# 11.04.2022


# Hypothesis testing #### ------------------------------------------------------

# example: 2 sample t-test

rm(list=ls())

library(palmerpenguins)
data = as.data.frame(penguins)
str(data)

x = subset(data, species=="Chinstrap")$flipper_length_mm
y = subset(data, species=="Adelie")$flipper_length_mm
boxplot(x,y)

# test for difference in sample means
t.test(x,y)

# example plot
curve(dt(x, df=119.68), from=0, to=6, lwd=3, col=4)
abline(v=qt(p=0.95, df=119.68), lty=2  )
points(5.7804, 0, col=2, pch=16, cex=1.5)


# Regression #### --------------------------------------------------------------

rm(list=ls())

library(palmerpenguins)
data=as.data.frame(penguins)
data = subset(data, species=="Chinstrap")

# models with 2 continuous predictors. models are nested
mod1 = lm(flipper_length_mm ~ body_mass_g * bill_length_mm, data)
mod2 = lm(flipper_length_mm ~ body_mass_g + bill_length_mm, data)

# F-test for difference in residuals 
anova(mod1, mod2)

# example plot
curve(df(x, df1=1, df2=64), from=0, to=4, col=4, lwd=2)
abline(v=qf(p=0.95, df1=1, df2=64), lty=2)
points(0.3563, 0, col=2, pch=16, cex=1.5)

# overall F-test for regression
# mod1 = lm(flipper_length_mm ~ body_mass_g * bill_length_mm, data)
summary(mod1) # same as summary.lm(mod1)

# example plot for F-statistic
curve(df(x,3,64), from=0, to=17, col=4, lwd=2)
abline(v=qf(p=0.95, df1=3, df2=64), lty=2)
points(16.91, 0, col=2, pch=16, cex=1.5)

# single effects t-tests 
summary(mod1)

# example plot for t-test of body_mass_g:bill_length_mm
curve(dt(x,64), from=-4, to=4, col=4, lwd=2)
abline(v=qt(p=0.975, df=64), lty=2) # two-sided: alpha/2 = 2.5%
points(0.597, 0, col=2, pch=16, cex=1.5) 

# Type I anova() table 
anova(mod1) # same as summary.aov(mod1)

# information critera
AIC(mod1, mod2)
BIC(mod1, mod2)

# One-way ANOVA #### -----------------------------------------------------------

rm(list=ls())

library(palmerpenguins)
data = as.data.frame(penguins)
boxplot(flipper_length_mm ~ species, data=data)

# or use ggplot if preferred
library(ggplot2)
ggplot(data=data, aes(y=flipper_length_mm, x=species)) + 
  geom_boxplot()

# ANOVA model
mod1 = lm(flipper_length_mm ~ species, data=data)

# ANOVA table
summary.aov(mod1)

# dummy-coded effects
summary.lm(mod1)

# get group-level means with aov() and model.tables()
aov1 = aov(flipper_length_mm ~ species, data=data)
model.tables(aov1, "means")

# contrasts (pairwise differences)
TukeyHSD(aov1)

# plot
library("sjPlot")
plot_model(mod1, type="pred", show.data = TRUE, jitter=0.5)

# alternative:
# get group-level means with emmeans()
library(emmeans)
# mod1 = lm(flipper_length_mm ~ species, data=data)
emmeans(mod1, ~1)
emmeans(mod1, ~species)

# contrasts (pairwise differences)
pairs(emmeans(mod1, ~species))

# plot
plot(emmeans(mod1, ~species), horizontal=FALSE)
# or
emmip(mod1, ~species, CIs=TRUE) +
  geom_jitter( aes(x=species, y=flipper_length_mm), 
               data=data, width=0.1, alpha=0.2, color="red")

# One-way ANOVA #### -----------------------------------------------------------

rm(list=ls())

library(palmerpenguins)
data = as.data.frame(penguins)
data = subset(data, !is.na(sex))

library(ggplot2)
ggplot(data = data, aes(y=flipper_length_mm, x=species, colour=sex)) + 
  geom_boxplot()

# additive model
mod1 = lm(flipper_length_mm ~ species + sex, data=data)
summary.lm(mod1)

# interaction model
mod2 = lm(flipper_length_mm ~ species * sex, data=data)
summary.lm(mod2)

# Type I anova table
summary.aov(mod2) # same as anova(mod2)

# Type II anova table
library(car)
Anova(mod2) # same as default: Anova(mod2, type=2)

# predicted means
library(emmeans)
emmeans(mod2, ~species*sex)
emmeans(mod2, ~species|sex) # same, just ordered
emmeans(mod2, ~sex|species) # same, just ordered the other way

# marginal means
emmeans(mod2, ~species)
emmeans(mod2, ~sex)

# all contrasts
pairs( emmeans(mod2, ~species*sex) )

# all contrasts as table
pwpm( emmeans(mod2, ~species*sex) )

# in-group contrasts only
pairs( emmeans(mod2, ~species|sex) )
pairs( emmeans(mod2, ~sex|species) )

# marginal contrasts
pairs( emmeans(mod2, ~sex) )
pairs( emmeans(mod2, ~species) )

# tidy-style version: code is more readable
# mod2 |> emmeans(~sex) |> pairs()
# mod2 |> emmeans(~species) |> pairs()

# plot
emmip(mod2, ~species|sex, CIs=TRUE) +
  geom_jitter( aes(x=species, y=flipper_length_mm), 
               data=data, width=0.1, alpha=0.2, color="red")
emmip(mod2, ~sex|species, CIs=TRUE) +
  geom_jitter( aes(x=sex, y=flipper_length_mm), 
               data=data, width=0.1, alpha=0.2, color="red")


# ANCOVA #### -----------------------------------------------------------

rm(list=ls())

library(palmerpenguins)
data = as.data.frame(penguins)

library(ggplot2)
ggplot( aes(x=body_mass_g, y=flipper_length_mm, color=species), data=data) + 
  geom_point()

# scale the continuous predictor
data$mass_z = as.numeric( scale(data$body_mass_g) )

# ANCOVA model, varying intercepts
mod1 = lm(flipper_length_mm ~ mass_z + species, data=data)
summary.lm(mod1)

# plot
library("sjPlot")
plot_model(mod1, 
           type="pred", 
           terms=c("mass_z", "species"), 
           show.data = TRUE) + ylim(170, 240)

# ANCOVA model, varying intercepts & slopes
mod2 = lm(flipper_length_mm ~ mass_z * species, data=data)
summary.lm(mod2)

# plot
plot_model(mod2, 
           type="pred", 
           terms=c("mass_z", "species"), 
           show.data = TRUE) + ylim(170, 240)

library(car)
Anova(mod2) 

# estimated group-level slopes
library("emmeans")
emtrends(mod2, ~species, var="mass_z")

# contrasts
pairs( emtrends(mod2, ~species, var="mass_z") )


# Mixed-effects models #### ----------------------------------------------------

rm(list=ls())

library(palmerpenguins)
data = as.data.frame(penguins)
data = subset(data, !is.na(sex))
# scale the continuous predictor
data$mass_z = as.numeric( scale(data$body_mass_g) )

library(lme4)

# STEP 1: maximal fixed effects model
lmmax1 = lmer( flipper_length_mm ~ mass_z * species + 
                 (1+mass_z|sex), data=data )

# STEP 2: test random effects structure
library("lmerTest")
ranova(lmmax1)

lmmax2 = lmer( flipper_length_mm ~ mass_z * species + 
                 (1|sex), data=data )
ranova(lmmax2)

# STEP 3: test fixed effects structure (REML=FALSE)
lmm1 = lmer( flipper_length_mm ~ mass_z * species + (1|sex), data=data, REML=FALSE )
lmm2 = lmer( flipper_length_mm ~ mass_z + species + (1|sex), data=data, REML=FALSE )
anova(lmm1, lmm2)

# STEP 4: refit best model with REML=TRUE (default)
lmm1 = lmer( flipper_length_mm ~ mass_z * species + (1|sex), data=data)
summary(lmm1)

# extract only fixed effects or only random effects
fixef(lmm1) 
ranef(lmm1) 

# plot
library("sjPlot")
plot_model(lmm1, 
           type="pred", 
           terms=c("mass_z", "species"), 
           show.data = TRUE) 

# model selection: automatize steps 2&3 with step() function
# STEP 1
lmmax1 = lmer( flipper_length_mm ~ mass_z * species + (1+mass_z|sex), data=data )
# STEPS 2&3
step(lmmax1)
# flipper_length_mm ~ mass_z + species + (1 | sex) + mass_z:species, is SAME as
# flipper_length_mm ~ mass_z * species + (1|sex)
# STEP 4
lmm1 = lmer( flipper_length_mm ~ mass_z * species + (1|sex), data=data)

# group-level slopes (fixed effects) with emmeans as before (ANCOVA)
library("emmeans")
emtrends(lmm1, ~species, var="mass_z")
pairs( emtrends(lmm1, ~species, var="mass_z") )
