library(officer)
library(tidyverse)
library(survival)
pacman::p_load(survminer)

# Wrapper functions -------------------------------------------------------

az_slide_create <- function(title, author, date,
                            template = here::here("AZ_blank_template.pptx")) {
  require(officer)
  doc <- read_pptx(template)
  n_template <- length(doc)
  doc <- doc %>%
    add_slide(layout = "Title Slide", master = "AstraZeneca Standard Template") %>%
    ph_with(
      value = title,
      location = ph_location_type(type = "title")
    ) %>%
    ph_with(
      value = author, type = "body",
      location = ph_location_label("Text Placeholder 9")
    ) %>%
    ph_with(
      value = date,
      location = ph_location_type(type = "dt")
    ) %>%
    ph_with(
      value = "Strictly Confidential",
      location = ph_location_label("Text Placeholder 7")
    )
  for (i in 1:n_template) remove_slide(doc, 1)
  return(doc)
}

slide_add_gg <- function(doc, gg) {
  return(
    doc %>%
      add_slide(layout = "Blank", master = "AstraZeneca Standard Template") %>%
      ph_with(value = gg, location = ph_location_fullsize())
  )
}

slide_add_section <- function(doc, title) {
  return(
    doc %>%
      add_slide(layout = "Section Divider", master = "AstraZeneca Standard Template") %>%
      ph_with(value = title, location = ph_location_type("title"))
  )
}


# Make presentation -------------------------------------------------------

mpg1 <- mpg %>%
  filter(year == 2008) %>%
  mutate(
    manufacturer = str_to_title(manufacturer),
    manufacturer = factor(manufacturer),
    manufacturer = fct_infreq(manufacturer)
  )
(plt0 <- ggplot(
  mpg1,
  aes(x = manufacturer)
) +
  geom_bar() +
  labs(x = "Manufacturer", y = "Frequency") +
  theme_bw() +
  coord_flip())


plt1 <- ggplot(mpg, aes(x = fct_reorder(class, cty), y = cty, fill = factor(year))) +
  geom_boxplot() +
  labs(
    x = "Manufacturer",
    y = "City mileage (mpg)",
    fill = "Year"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


beaches <- rio::import("../../data/sydneybeaches3.csv")
(plt2 <- ggplot(
  beaches,
  aes(x = date, y = temperature)
) +
  geom_line(size = 0.5) +
  geom_point(aes(color = fct_relevel(season_name, "Summer", "Autumn", "Winter", "Spring")), size = 2) +
  facet_wrap(year ~ ., ncol = 1, scales = "free_x", dir = "v") +
  theme_bw() +
  labs(x = "Date", y = "Temperature (C)", color = "Season")
)

(sp1 <- ggsurvplot(
  survfit(Surv(time, status == 2) ~ trt, data = pbc),
  facet.by = "sex"
)
)

doc1 <- az_slide_create(
  title = "Examples of plots",
  author = "Abhijit Dasgupta",
  date = "1 March, 2021",
  template = "AZ_blank_template.pptx"
) %>%
  slide_add_section("Univariate plots") %>%
  slide_add_gg(ggplot(iris, aes(x = Sepal.Length)) +
    geom_histogram()) %>%
  slide_add_gg(plt0) %>%
  slide_add_section("Bivariate plots") %>%
  slide_add_gg(plt1) %>%
  slide_add_gg(plt2) %>%
  slide_add_section("Survival curves") %>%
  slide_add_gg(sp1)

print(doc1, target = "pptx_officer_example_az.pptx")

pltt <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  theme_bw() +
  labs(x = "Sepal Length", y = "Sepal Width")

pacman::p_load(gtsummary, flextable)
doc2 <- read_pptx("pptx_droplets.pptx") %>%
  remove_slide() %>%
  add_slide(layout = "Title Slide", master = "Droplet") %>%
  ph_with("A PPTX example", location = ph_location_type(type = "ctrTitle")) %>%
  ph_with("Abhijit Dasgupta", location = ph_location_type(type = "subTitle")) %>%
  add_slide(layout = "Title and Content", master = "Droplet") %>%
  ph_with("A summary table", location = ph_location(type = "ctrTitle")) %>%
  ph_with(tbl_summary(iris) %>% as_flex_table() %>% autofit() %>% theme_alafoli(),
    location = ph_location_right()
  ) %>%
  add_slide(layout = "Title and Content", master = "Droplet") %>%
  ph_with("A plot", location = ph_location_type(type = "title")) %>%
  ph_with(pltt, location = ph_location_type(type = "body"))

print(doc2, "pptx_example_officer.pptx")
