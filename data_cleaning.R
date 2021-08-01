##################################################################################################
#  This is the data cleaning code for "Interest rate effects and finite horizons", bachelor thesis 
#  by Nirai Alexander Tomass. This code is adapted from that of the publicly available code from 
#  Chen et al. (2019). 

##################################################################################################

library(data.table)
library(readxl)
library(tidyverse)

code_path = "C:/Users/nirai/Desktop/Bachelor's thesis/Code"
data_path = "C:/Users/nirai/Desktop/Bachelor's thesis/Data"
output_path = "C:/Users/nirai/Desktop/Bachelor's thesis/Output"

###--------------------------------------------------------------------------------------------------------
##Load in 

#Dataset from Aizenman et al. (2019)
data = data.table(read_excel(file.path(data_path, "saving_master_data2.xlsx")))

###--------------------------------------------------------------------------------------------------------
##Clean Aizenman data set

data = data[year %in% c(1995:2014)] #authors use 1995-2014 time window 

sample = c("AUS", "AUT", "BEL", "CAN", "DNK", "FIN",
           "FRA", "DEU", "GRC", "ISL", "IRL", "ITA",
           "JPN", "MLT", "NLD", "NZL", "NOR", "PRT",
           "ESP", "SWE", "CHE", "GBR", "USA", "ALB", 
           "DZA", "AGO", "ATG", "ARG", "ARM", "AZE",
           "BHS", "BHR", "BGD", "BRB", "BLR", "BLZ", 
           "BEN", "BOL", "BWA", "BRA", "BGR", "BFA",
           "BDI", "CIV", "CMR", "CAF", "TCD", "CHL",
           "CHN", "COL", "COM", "ZAR", "COG", "CRI",
           "HRV", "CYP", "CZE", "DOM", "ECU", "EGY",
           "SLV", "EST", "FJI", "GAB", "GMB", "GEO",
           "GHA", "GRD", "GNB", "HKG", "HUN", "IND",
           "IDN", "ISR", "JAM", "JOR", "KAZ", "KEN",
           "KOR", "KWT", "KGZ", "LAO", "LVA", "LBN",
           "LTU", "MDG", "MWI", "MYS", "MDV", "MLI",
           "MUS", "MEX", "MDA", "MNG", "MAR", "MOZ",
           "MMR", "NAM", "NPL", "NER", "NGA", "OMN",
           "PAK", "PAN", "PRY", "PER", "PHL", "POL",
           "QAT", "ROM", "RUS", "RWA", "SEN", "SYC",
           "SLE", "SGP", "SVK", "SVN", "ZAF", "LKA",
           "LCA", "VCT", "SWZ", "TJK", "TZA", "THA",
           "TGO", "TTO", "TUN", "TUR", "VEN") #authors use a sample of 131 countries (see appendix), although they say 135 (136)
data = data[ccode %in% sample]

data = data[, c("ccode", "year", "p_saving", "gbb", "pcgdp_g", "hp_pcgdp", "lrypc_ppp", 
                "real", "rypc_g_un", "depend_old", "depend_young", "healthcare", "ka_open", 
                "rypc_g_un_sd5", "ldc", "idc")]

saveRDS(data, file.path(data_path, "data_aizenman.rds"))



