# This is the main / master script for the project Antibiotikaverbrauch
# It is used to source the other scripts
# Author: Adrian Minnig

setwd("~/GitHub/Projekt_Antibiotikaverbrauch")
install.packages(c("dplyr", "tidyverse", "lubridate", "usethis", "gitcreds", 
                   "here", "medicaldata", "cowplot", "readr", "ggplot2", "remotes",
                   "rstatix", "gtsummary", "reshape2", "janitor"))

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
library(remotes)
library(rstatix)
library(gtsummary)
library(reshape2)
library(janitor)


# Set system language & options ####
Sys.setenv(LANG = "en")
rm(list=ls())                  # clear current workspace
options(scipen=999)            # avoid the use of scientific notation


file.edit("R/AMR_descriptive_statistics.R")
file.edit("R/AMR_analysis.R")
