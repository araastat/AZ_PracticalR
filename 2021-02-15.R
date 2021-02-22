library(tidyverse)
gapminder_w <- read_csv('~/Downloads/gapminder_w.csv')

# Make all the years into a column
# Make the elements of Variable into separate columns

gapm <- gapminder_w %>%
  pivot_longer(names_to = 'year',     # many-to-two operation
               values_to = 'value',
               cols = `1952`:`2007`) %>%
  pivot_wider(names_from = 'Variable',
              values_from = 'value')

# Can I find the column indices of columns which have
# "numeric" values
gap2%>%
  select(matches('\\d{4}')) %>%
  head()
# gap2 <- read.csv('~/Downloads/gapminder_w.csv')
#
# gap2 %>%
#   pivot_longer(names_to = 'year',
#                values_to = 'value',
#                cols = X1952:X2007) %>%
#   mutate(year = str_remove(year, 'X'))
ggplot(data = gapm,
       mapping = aes(x = year, y = pop, group=country))+
  geom_line(aes(color = continent), show.legend=F)+
  scale_y_log10()

ggplot(data = gapm,
       mapping = aes(x = year, y = pop, group=country))+
  geom_line()+
  facet_wrap(~continent) +
  scale_y_log10('Population', labels = scales::label_number_si()) +
  labs(x = 'Year',  y = 'Population')

ggplot(gapm, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(size = pop, color = continent)) + # since population is in gapm
  scale_x_log10(labels = scales::label_dollar()) +
  scale_y_continuous(breaks = c(40, 60, 80), labels = paste(c(40,60,80), 'years'))+
  scale_size_continuous(labels = scales::label_number_si(accuracy = 0.1)) +
  facet_wrap(~year) +
  labs(x = 'Per capita GDP', y = 'Life expectancy',
       size = 'Population', color = 'Continent')+
  theme(axis.text.x = element_text(angle=45, hjust =1)) +
  expand_limits(y = c(0,150))


