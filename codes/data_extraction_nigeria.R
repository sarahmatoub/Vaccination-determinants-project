# extracted data for Nigeria
library(dplyr)
library(data.table)
library(readr)

# This script is made to build a database listing all household having small ruminants
# the database would give for each household, the demographic and economic charactertistics

# Nigeria Household survey 2018-2019
#setwd("D:/Mes Donnees/Stage-M2-Biostatistiques/data/GHS Nigeria/General Household Survey 2018-2019/Data")
# in ddi-documentation-english_microdata-3557-cAD pages 14-

# household identification information
household_inf = read.csv(file = "sect1_plantingw4.csv",
                         header = TRUE, sep = ",")

household_inf1 = read.csv("sect1_plantingw4.csv",
                          header = TRUE, sep = ",")

household_inf2 = read.csv(file = "sect1_harvestw4.csv",
                          header = TRUE, sep = ",")

# Economic activities outside the household farm
sec3 = read.csv(file = "sect3_plantingw4.csv",
                header = TRUE, sep = ",")

# Education
educ = read.csv(file = "sect2_harvestw4.csv",
                header = TRUE, sep = ",")

# Banking and savings

# Insurance (if they have any)