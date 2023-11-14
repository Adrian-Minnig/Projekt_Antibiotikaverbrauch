# This is the main / master script for the project Antibiotikaverbrauch
# It is used to source the other scripts
# Author: Adrian Minnig

setwd("~/GitHub/Projekt_Antibiotikaverbrauch")
install.packages(c("dplyr", "tidyverse", "lubridate", "usethis", "gitcreds", 
                   "here", "medicaldata", "cowplot", "readr", "ggplot2" ))

library(dplyr)
library(tidyverse)
library(lubridate)
library(usethis)
library(gitcreds)
library(here)
library(medicaldata)
library(cowplot)
library(readr)
library(ggplot2)


