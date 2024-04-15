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

savings = read.csv("sect4a1_plantingw4.csv")

# Insurance (if they have any)

#Mobile phone and internet
phone = read.csv("sect4b_plantingw4.csv")

#Extension services
services = read.csv("sect11l1_plantingw4.csv")

#Livestock
livestock1 = read.csv("sect11i_plantingw4.csv")
livestock2 = read.csv("sect11j_plantingw4.csv")
livestock3 = read.csv("sect11k1_plantingw4.csv")
livestock4 = read.csv("sect11k2_plantingw4.csv")




