# Class coding
#

library(tidyverse)
library(readxl)

# Joins -------------------------------------------------------------------

clinical <- read_excel('data/BreastCancer_Clinical.xlsx') %>%
  janitor::clean_names()

proteome <- read_excel('data/BreastCancer_Expression.xlsx') %>%
  janitor::clean_names()

dim(clinical)
d1 <- clinical[1:20,1:5]
d2 <- clinical[1:20,8:20]
d3 <- clinical[21:50, 1:5]
d4 <- clinical[21:50, 6:10]

dim(d1); dim(d2); dim(d3); dim(d4)

cbind(d1, d2)
rbind(d1, d3)
rbind(d1, d4) # Point out this error

## To join, we need to make sure the ID variables are unique

any(duplicated(clinical$complete_tcga_id))
any(duplicated(proteome$tcga_id))

### Keep 1st instance for proteome data

proteome <- proteome %>% filter(!duplicated(tcga_id))
any(duplicated(proteome$tcga_id))

## inner join (p. 15)

common_rows <- inner_join(clinical[,1:6], proteome,
                          by=c('complete_tcga_id'='tcga_id'))
left_rows <- left_join(clinical[,1:6], proteome, by = c('complete_tcga_id'='tcga_id')) # p17
right_rows <- right_join(clinical[,1:6], proteome, by=c('complete_tcga_id'='tcga_id')) # p19
full_rows <- full_join(clinical[,1:6], proteome, by=c('complete_tcga_id'='tcga_id')) # p21

final_data <- clinical %>%
  inner_join(proteome, by = c('complete_tcga_id'='tcga_id')) %>%
  filter(gender=='FEMALE') %>%
  select(complete_tcga_id, age_at_initial_pathologic_diagnosis, er_status, starts_with('NP'))

## p25


# Group by ----------------------------------------------------------------

data(diamonds)

data(nycflights13)

# Map ---------------------------------------------------------------------

sites <- c('Brain','Colon','Esophagus','Lung','Oral')
data <- rio::import(file.path(paste0(sites,'.csv')), skip=4, check.names=TRUE)
names(dats)

map(dats,
    function(d){
      d %>% slice(-1) %>%
        mutate(across(where(is.character), as.numeric))
    })

map(dats, ~.x %>% slice(-1) %>% mutate(across(where(is.character), as.numeric)))

fn <- function(d){
  d %>% slice(-1) %>%
    mutate(across(where(is.character), as.numeric))
}

map(dats, fn)

map(dats, janitor::clean_names)

dats_all <- map(dats, select, year_of_diagnosis, ends_with('sexes'))
dats_male <- map(dats, select, year_of_diagnosis, ends_with('_males'))
dats_female <- map(dats, select, year_of_diagnosis, ends_with('females'))

for(n in sites){
  names(dats_all[[n]]) <- str_replace(names(dats_all[[n]]), 'both_sexes',n)
  names(dats_male[[n]]) <- str_replace(names(dats_male[[n]]), 'male',n)
  names(dats_female[[n]]) <- str_replace(names(dats_female[[n]]), 'female',n)
}
names(dats_all[['Esophagus']])

joined_all <- dats_all[['Brain']]
for(n in setdiff(names(dats2_all), 'Brain')){
  joined_all <- joined %>% left_join(dats_all[['n']])
}

joined_all <- Reduce(left_join, dats_all)

## Fix names so separate works on _
names(joined_all) <- str_replace(names(joined_all), 'all_races','allraces')
names(joined_male) <- str_replace(names(joined_male), 'all_races','allraces')
names(joined_female) <- str_replace(names(joined_female), 'all_races','allraces')

joined <- list('both'=joined_all, 'male'=joined_male, 'female'=joined_female)
joined <- map(joined,
              function(d){
                d %>%
                  pivot_longer(names_to='variable', values_to = 'rate',
                               cols=c(-year_of_diagnosis)) %>%
                  separate(variable, c('race','site'), sep='_')
              })
str(joined[['both']])


pltlist <- joined[['both']] %>% group_split(race) %>%
  map(function(d) {ggplot(d,
                          aes(x = year_of_diagnosis,
                              y = rate,
                              color=site))+
      geom_point(show.legend = F) })
cowplot::plot_grid(plotlist=pltlist, ncol=1,
                   labels=c('All','Whites','Blacks'))



# summaries ---------------------------------------------------------------

summary(brca[,-1])

tableone::CreateTableOne(data = brca[,-1])
print(tableone::CreateTableOne(data = brca[,-1]), nonnormal=names(brca)[-1])

brca_combined = clinical %>% left_join(proteome, by=c('complete_tcga_id'='tcga_id'))

brca_combined %>% group_by(er_status) %>% summarise(across(starts_with('np'), ~mean(.x, na.rm=T)))
tableone::CreateTableOne(vars = variables, strata = 'er_status', data=brca_combined)

