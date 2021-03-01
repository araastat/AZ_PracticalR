library(officer)
library(tidyverse)
library(flextable)

beaches <- rio::import('../data/sydneybeaches3.csv')
plt <- ggplot(beaches,
              aes(x=temperature, y = rainfall))+
  geom_point()+
  labs(x = 'Temperature (C)',
       y = 'Rainfall (mm)',
       title = 'Weather in Sydney beaches')

ft <- flextable(head(iris)) %>% autofit()

read_docx(path=here::here('slides/lectures/week6/examples/Meeting Minutes.docx')) %>%
  body_add_par('Research meeting', style='heading 1') %>%
  body_add_par('1 March, 2021', style = 'heading 1') %>%
  body_add_par('This is an example of a table', style='Normal') %>%
  body_add_flextable(ft) %>%
  body_add_break() %>%

  body_add_par(value='Plotting example', style='heading 1') %>%
  body_add_gg(value = plt) %>%

  print(target = here::here('slides/lectures/week6/examples','word_officer_example.docx'))
