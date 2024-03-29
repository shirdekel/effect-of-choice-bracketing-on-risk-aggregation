library(conflicted)
library(dotenv)
library(targets)
library(tarchetypes)
library(magrittr)
library(dplyr)
library(purrr)
library(rlang)
library(stringr)
library(forcats)
library(ggplot2)
library(bookdown)
library(knitr)
library(papaja)
library(magick)
library(here)
library(yaml)
library(afex)
library(janitor)
library(snakecase)
library(emmeans)
library(printy)
library(broom)
library(MOTE)
library(broom.mixed)
library(pwr)

# Custom packages
library(aggregation1)
library(aggregation2)
library(shirthesis)

conflict_prefer("filter", "dplyr")
conflict_prefer("set_names", "rlang")
