library(survival)
str(pbc)

y ~ x1 + x2 + x1:x2 + I(x3^2) + x4*x5

y = b0 + b1*x1 + b2*x2 + b3 * x1:x2 + b4*(x3^2) + b5*x4 + b6*x5 + b7* x4:x5

if x1=0 and x2 = 0, then y = b0
if x1=1 and x2 = 0, then y = b0 + b1
if x1=0 and x2 = 1, then y = b0 + b2
if x1=1 and x2 = 1, then y = b0 + b1 + b2 + b3
########

m <- lm(chol ~ ascites + spiders + ascites:spiders + I(bili^2)+
     factor(trt)*factor(hepato), data= pbc)

mod_f <- lm(chol ~ factor(stage), data=pbc)
mod <- lm(chol ~ stage, data=pbc)

White, Black, Asian, Hispanic, Native American

dat <- tibble(y = rnorm(20), race = sample(c('White','Black','Asian','Hispanic','NatAm'), 20, replace=T))
dat <- dat %>% mutate(race = as.factor(race))

lm(y ~ race, data=dat) # Asian is the reference category

dat <- dat %>%
  mutate(race = fct_relevel(race, 'White')) # change base (1st) level to "White"

lm(y~race, data=dat) # White is the reference category

glm # generalized linear model

g1 <- glm(spiders ~ albumin + bili + chol,data = pbc,
    family = binomial) # determines logistic regression
tidy(g1)

predict(g1, type = 'response')

library(survival)
library(survminer)

s <- survfit(Surv(time, status==2)~1, data= pbc)
ggsurvplot(s) + labs(x = 'Time (days)')

pbc1 <- pbc %>%
  mutate(across(c(trt, ascites, hepato, spiders, stage),
                as.factor))
cr <- coxph(Surv(time, status==2) ~ trt + age + sex + hepato +
        spiders + ascites + stage, data = pbc1)


library(gtsummary)
tbl_regression(cr, exponentiate = T)







