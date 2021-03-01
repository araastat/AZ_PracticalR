library(tidyverse)
library(reticulate)

reticulate::use_condaenv('ds_teaching')

reticulate::repl_python()
import pandas as pd
beaches = pd.read_csv('slides/lectures/data/sydneybeaches3.csv')

b1 <- py$beaches
