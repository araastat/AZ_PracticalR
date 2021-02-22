library(tidyverse)

table1
table2
table3

table2 %>%
  pivot_wider(names_from = 'type',
              values_from = 'count')

table3 %>%
  separate(col = rate, into = c('cases','population'),
           sep = '/') %>% #, convert = TRUE) # %>%
  # mutate(across(cases:population, as.numeric))
  # mutate(across(3:4, as.numeric))
  mutate(across(c(cases, population), as.numeric))

weather_dat <- read_csv(here::here('slides/lectures/data','weather.csv'))

# Always make longer before you make wider

w1 <- weather_dat %>%
  pivot_longer(names_to = 'day', # what's the name of the new column
               values_to = 'temperature', # what's the name of the new column
               cols = starts_with('d')) %>% # Which columns will I transform
# pivot_longer transforms multiple columns into 2 columns
  pivot_wider(names_from = "element", # which column provides new column headers
              values_from =  "temperature") %>%
  mutate(day = parse_number(day)) %>%
  # mutate(day = as.numeric(str_remove(day, 'd')))
  # separate(day, c('dummy','day'), sep = 1, convert=T) %>%
  # select(-dummy)
  filter(!is.na(tmax) | !is.na(tmin))
  # filter(complete.cases(.))


# Data visualization ------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(hrbrthemes)
beaches <- rio::import(here::here('slides/lectures/data','sydneybeaches3.csv'))
# beaches <- rio::import("/Users/ksrd721/Teaching/AZ_PracticalR/slides/lectures/data/sydneybeaches3.csv")

mytheme <- theme_classic()+
  theme(axis.title=element_text(size=20))

ggplot(
  data = beaches,
  mapping = aes(
    x = temperature,
    y = rainfall
  )
) +
  geom_point(aes(color = season_name, size=enterococci)) +
  facet_wrap(~season_name) + # You need the ~ to grab data from the dataset
  theme_classic()+
  labs(x = 'Temperature (C)',
       y = 'Rainfall (mm)',
       title = 'Rainfall',
       subtitle = 'Sydney beaches',
       caption = "Credits: Dr. D. Navarro",
       color = 'Season',
       size = 'Bacterial concentration')

ggplot(
  data = beaches,
  mapping = aes(
    x = day_num,
    y = temperature
  )) +
    geom_line(color = 'yellow', size = 2)+
    geom_point(color = 'red')

ggsave('example1.pdf')

pacman::p_load(ggpubr,cowplot,patchwork)

library(palmerpenguins)
head(penguins)

plt1 <- ggplot(penguins,
       aes(x = species, y = body_mass_g, fill = species))+
  geom_boxplot(color = 'white') +
  theme_dark()

plt2 <- ggplot(penguins,
               aes(x = bill_length_mm, y = body_mass_g,
                   color = species)) +
  geom_point()

plt3 <- ggplot(penguins,
               aes(x = bill_length_mm, y = flipper_length_mm,
                   color = species))+
  geom_smooth(se = FALSE)


ggpubr::ggarrange(plt1, plt2, plt3, ncol=2, nrow=2, common.legend=TRUE)

cowplot::plot_grid(plt1, plt2, plt3, nrow=2, ncol=2,
                   labels = c('A','B','C'))

(plt1 + plt2) / plt3
