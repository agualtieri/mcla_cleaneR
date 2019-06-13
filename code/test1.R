## MCLA 2019 Data Cleaning Script
## REACH Yemen
## 12 June 2019
## V1


## Install packages
# devtools::install_github("mabafaba/xlsformfill", force = T)
require("xlsformfill")
require("cleaninginspectoR")
require("tidyverse")
require("readr")

# Upload kobo tool and fill it with fake data
# load questionnaire
kobo_questions_v1 <- read.csv("data/tool/mcla_questions.csv", stringsAsFactors = F)
kobo_choices_v1 <- read.csv("data/tool/mcla_choices.csv", stringsAsFactors = F)

# generate fake data with `n` rows
fake_dataset <- xlsform_fill(kobo_questions_v1, kobo_choices_v1, n = 10)
