pacman::p_load(mlbench, visdat, skimr, tidyverse, gtsummary, broom, rio)
data("PimaIndiansDiabetes2")

skimr::skim(PimaIndiansDiabetes2)
visdat::vis_dat(PimaIndiansDiabetes2)

model_dat <- PimaIndiansDiabetes2 %>%
  select(-insulin, -triceps, -glucose)

logist_reg <- glm(diabetes ~ ., data = model_dat,
                  family=binomial, na.action = na.omit)
summary(logist_reg)

gtsummary::tbl_regression(logist_reg, exponentiate = TRUE)

tidy(logist_reg) %>%
  mutate(lcb = estimate - 2*std.error,
         ucb = estimate + 2*std.error) %>%
  mutate(across(c(estimate, lcb, ucb), exp)) %>%
  filter(term != '(Intercept)') %>%
  ggplot(aes(x = term, y = estimate, ymin = lcb, ymax = ucb))+
  geom_pointrange(size=0.5) +
  geom_hline(yintercept = 1, linetype=2) +
  labs(x = 'Predictor', y = 'Odds ratio') +
  coord_flip()

tidy(logist_reg) %>%
  mutate(lcb = estimate - 2*std.error,
         ucb = estimate + 2*std.error) %>%
  mutate(across(c(estimate, lcb, ucb), exp)) %>%
  filter(term != '(Intercept)') %>%
  ggplot(aes(x = term, y = estimate, ymin = lcb, ymax = ucb))+
  geom_linerange()+
  geom_point(size=1) +
  geom_hline(yintercept = 1, linetype=2) +
  labs(x = 'Predictor', y = 'Odds ratio') +
  coord_flip()


pres <- import_list('data/American Presidency Project - Approval Ratings for POTUS.xlsx')
pres_approvals <- bind_rows(pres, .id = 'president') %>% janitor::clean_names()

ggplot(pres_approvals,
       aes(x = end_date, y = approving))+
  geom_line(aes(color = president), show.legend=F)

inaug <- tibble(
  year = c(1941, 1945, 1953, 1961, 1965, 1969, 1974, 1977, 1981, 1989, 1993,
           2001, 2009, 2017)
) %>%
  mutate(dt = as.Date(paste0(year, '-01-20')))

ggplot(pres_approvals,
       aes(x = as.Date(end_date), y = unsure_no_data/100))+
  geom_line(aes(color = president), show.legend=F)+
  geom_vline(xintercept = inaug$dt, linetype=2)+
  labs(x = '', y = 'Percent unsure')+
  scale_y_continuous(labels = scales::percent_format())


pres_approvals <- pres_approvals %>%
  mutate(president = as.factor(president)) %>%
  mutate(president = fct_reorder(president, start_date))
tbl_summary(pres_approvals %>% select(-ends_with('date')),
            by='president')
