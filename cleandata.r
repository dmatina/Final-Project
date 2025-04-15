library(haven)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)


# Read the .dta files
df1 <- read_dta("Data/Raw/Municipality/surnames_municipality.dta")
df2 <- read_dta("Data/Raw/Municipality/Migration2004-2010.dta")
df3 <- read_dta("Data/Raw/Municipality/corruptionANAC2015-2023.dta")
df4 <- read_dta("Data/Raw/Municipality/15-24unempl2018comuni.dta")

# Merge them step by step using full_join to keep all idcoms
merged_df <- df1 %>%
  full_join(df2, by = "idcom") %>%
  full_join(df3, by = "idcom") %>%
  full_join(df4, by = "idcom")

# Remove redundant/useless columns
merged_df <- merged_df %>%
  select(-comune, -com_name.y,-territorio,-tipo_dato_cens_pop,-tipodato,-v10,-prov_dest_z,-luogodidestinazione)