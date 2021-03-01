# --------------------------------------------------------------------  
# Script to use the officer package to create PowerPoint decks for tables and
# graphs, utilizing the AZ PPTX template
#
# Author: Abhijit Dasgupta (abhijit.dasgupta@astrazeneca.com)
# 
# Title: 3i over time updates: EAGLE
# --------------------------------------------------------------------- 


# Setup -------------------------------------------------------------------

source(here::here('setup.R'))
pacman::p_load(tidyverse, here, officer, fs, survival, survminer) 
datadir <- '~/Box Sync/2019_09_20_CentraliseF1Studies/2021_01_29_MYSTIC_EAGLE_over_time'
study <- 'EAGLE'

path_to_data <- file.path(datadir, 'data', study)

# Ingest data -------------------------------------------------------------

load(file.path(path_to_data, 'eagle_scored_data.rda'))


# Utilities for visualization in ggplot2 ----------------------------------

stat_box_data <- function(y, upper_limit = max(inc_overall_scored$F1, na.rm=T)*1.15){
  # see https://medium.com/@gscheithauer/how-to-add-number-of-observations-to-a-ggplot2-boxplot-b22710f7ef80
  return(
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('N =', length(y))
    )
  )
}

flag_status = c(
  high="Baseline F1: high",
  low = "Baseline F1: low"
)
theme_set(theme_bw())

plot_surv_week <- function(wk){
  compare_base <- inc_overall_scored %>% 
    filter(week == wk) %>% 
    filter(!is.na(flag), !is.na(flag_base)) %>% 
    mutate(grp = paste(flag_base, flag, sep='-'))
  pvals <- compare_base %>% 
    nest(data = c(-ARM)) %>% 
    mutate(mod = map(data, ~survdiff(Surv(survtime/7, status)~grp, data=.))) %>% 
    mutate(pval = map_dbl(mod, ~pchisq(.$chisq, sum(.$exp>0)-1, lower=F))) %>% 
    select(ARM, pval)
  ggsurvplot(survfit(Surv(survtime/7, status)~grp,
                     data = compare_base),
             data = compare_base,
             facet.by = 'ARM')+
    geom_text(data = pvals, 
              aes(x=100, y=0.9, 
                  label = paste('p =', signif(pval,2))))+
    ggtitle(glue::glue('Comparing week {wk} with baseline'))
}

plot_surv_week2 <- function(wk){
  
  compare_base <- inc_overall_scored %>% 
    filter(week == wk) %>% 
    filter(!is.na(flag), !is.na(flag_base)) %>% 
    mutate(grp = paste(flag_base, flag, sep='-'))
  pvals <- compare_base %>%
    nest(data = c(-grp)) %>%
    mutate(mod = map(data, ~try(survdiff(Surv(survtime/7, status)~ARM, data=.), silent=T))) %>%
    mutate(pval = map_dbl(mod, ~ifelse(class(.)=='try-error', 
                                       NA_real_, 
                                       pchisq(.$chisq,sum(.$exp>0)-1, lower=F)))) %>%
    select(grp, pval)
  
  ggsurvplot(survfit(Surv(survtime/7, status)~ARM,
                     data = compare_base),
             data = compare_base,
             facet.by = 'grp')+
    geom_text(data = pvals,
              aes(x=100, y=0.9,
                  label = paste('p =', signif(pval,2))))+
    ggtitle(glue::glue('Comparing week {wk} with baseline'))
}

# Wrapper functions for officer -------------------------------------------

az_slide_create <- function(title, author, date, 
                            template=here::here("docs/AZ_blank_template.pptx") ) {
  require(officer)
  doc <- read_pptx(template)
  n_template <- length(doc)
  doc <- doc %>% 
    add_slide(layout  = "Title Slide", master='AstraZeneca Standard Template') %>% 
    ph_with(value = title,
            location = ph_location_type(type='title')) %>%
    ph_with(value = author, type='body',
            location = ph_location_label('Text Placeholder 9')) %>% 
    ph_with(value = date, 
            location = ph_location_type(type='dt')) %>% 
    ph_with(value = "Strictly Confidential",
            location = ph_location_label("Text Placeholder 7"))
  for(i in 1:n_template) remove_slide(doc, 1)
  return(doc)
}
  
slide_add_gg <- function(doc, gg){
  return(
    doc %>% 
      add_slide(layout='Blank', master = 'AstraZeneca Standard Template') %>% 
      ph_with(value = gg, location = ph_location_fullsize())
  )
}

slide_add_section <- function(doc, title){
  return(
    doc %>% 
    add_slide(layout = 'Section Divider', master = 'AstraZeneca Standard Template') %>% 
    ph_with(value=title, location = ph_location_type('title'))
  )
}

# Creating graphs ---------------------------------------------------------

inc_overall_scored <- inc_overall_scored %>% 
  filter(week %in% c(-1, seq(2,36,by=2)))

plt1 <- ggplot(filter(inc_overall_scored, !is.na(flag_base), !is.na(F1)), 
               aes(x = factor(week),
                   y = F1,
                   fill = ARM))+
  geom_boxplot(varwidth=FALSE, outlier.size = 0.5) +
  geom_jitter(width=0.1, alpha=0.5)+
  stat_summary(
    fun.data=stat_box_data,
    geom = 'text',
    hjust=0.5,
    vjust=0.9,
    size=2) +
  facet_grid(ARM ~ flag_base, labeller = labeller( flag_base=flag_status))+
  labs(x = 'Weeks') +
  expand_limits(x = c(0, 20))

plt2 <- ggplot(filter(inc_overall_scored,!is.na(F1), !is.na(flag_base)),
               aes(x = week,
                   y = F1,
                   color = ARM))+
  geom_line(aes(group=USUBJID), alpha = 0.3) +
  geom_hline(yintercept = 0.69, linetype=2)+
  facet_grid(ARM ~ flag_base, labeller = labeller(flag_base=flag_status)) +
  scale_x_continuous('Weeks',breaks = unique(inc_overall_scored$week))

plt3 <- ggplot(filter(inc_overall_scored,!is.na(F1), !is.na(flag_base)),
               aes(x = week,
                   y = F1,
                   color = ARM))+
  geom_line(aes(group=USUBJID), alpha=0.3)+
  geom_smooth(color='black') + 
  facet_grid(ARM~flag_base, labeller=labeller(flag_base=flag_status))+
  scale_x_continuous('Week', breaks = unique(inc_overall_scored$week))

plt4 <- ggplot(filter(inc_overall_scored,!is.na(F1), !is.na(flag_base)),
       aes(x = week,
           y = F1,
           color = ARM))+
  geom_line(aes(group=USUBJID), alpha=0.3)+
  geom_smooth(aes(color = ARM)) + 
  geom_hline(yintercept = 0.69, linetype=2) + 
  facet_grid(~flag_base, labeller=labeller(flag_base=flag_status))+
  scale_x_continuous('Week', breaks = unique(inc_overall_scored$week))
# Building the PPTX -------------------------------------------------------

doc1 <- az_slide_create(title = '3i over time update: EAGLE', 
                        author = 'Abhijit Dasgupta, for the modeling team',
                        date = '26 February, 2021') %>% 
  slide_add_section('F1 over 36 weeks') %>% 
  slide_add_gg(plt1) %>% 
  slide_add_gg(plt2) %>% 
  slide_add_gg(plt3) %>% 
  slide_add_section('Survival by F1 status') %>% 
  slide_add_gg(plot_surv_week(4)) %>% 
  slide_add_gg(plot_surv_week(8)) %>% 
  slide_add_gg(plot_surv_week(12)) %>% 
  slide_add_section('Survival by study arm') %>% 
  slide_add_gg(plot_surv_week2(4)) %>% 
  slide_add_gg(plot_surv_week2(8)) %>% 
  slide_add_gg(plot_surv_week2(12))


print(doc1,target=here('docs/eagle_eda_slides_20210226.pptx'))

  
  
