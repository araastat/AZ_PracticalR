
# Setup -------------------------------------------------------------------

pacman::p_load(tidyverse, janitor, patchwork, ggpubr, cowplot)



# Tidying -----------------------------------------------------------------

table1
table2
table3
table4a # cases
table4b # population

table3 %>% separate(rate, c('cases','population'), sep='/')
table3 %>% separate(rate, c('cases','population'), sep='/', convert = TRUE)

weather_dat <- read_csv(here::here('slides/lectures/data','weather.csv'))

weather_dat %>%
  pivot_longer(names_to = 'day', values_to = 'temp', cols = starts_with('d')) %>%
  mutate(day = parse_number(day)) %>%
  filter(!is.na(temp)) %>%
  pivot_wider(names_from = 'element', values_from = 'temp')


# Plots -------------------------------------------------------------------

beaches <- read_csv(here::here('slides/lectures/data','sydneybeaches3.csv'))

ggplot(
  data = beaches,
  mapping = aes(
    x = temperature,
    y = rainfall
  )) +
  geom_point(
    mapping = aes(color = season_name),
    size = 4,
    show.legend=FALSE
  ) +
  facet_wrap(~ season_name) +
  theme_classic() +
  labs(x = 'Temperature (C)',
       y = 'Rainfall (mm)',
       title = 'Sydney weather by season',
       subtitle = 'Data from 2013 to 2018',)
