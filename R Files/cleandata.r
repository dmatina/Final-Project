library(haven)
library(dplyr)



# Read the .dta files
df1 <- read_dta("Data/Raw/surnames_municipality.dta")
df2 <- read_dta("Data/Raw/Migration2004-2010.dta")
df3 <- read_dta("Data/Raw/corruptionANAC2015-2023.dta")
df4 <- read_dta("Data/Raw/15-24unempl2018comuni.dta")

# Merge them step by step using full_join to keep all idcoms
merged_df <- df1 %>%
  full_join(df2, by = "idcom") %>%
  full_join(df3, by = "idcom") %>%
  full_join(df4, by = "idcom")

# Remove redundant/useless columns
merged_df <- merged_df %>%
  select(-comune, -com_name.y,-territorio,-tipo_dato_cens_pop,-tipodato,-v10,-prov_dest_z,-luogodidestinazione,-eta1,-time)

# Translate variables from Italian

  merged_df<-rename(merged_df,
    homebody_1524y = casalinghi,
    laborforce_1524y = forzalavoro.y,
    others_1524y = altracondizione,
    unemployed_1524y = disoccupati.y,
    inactive_1524y = inattivi,
    employed_1524y = occupati.y,
    pensioniers_1524y = ricpensioni,
    students_1524y = studenti,
    total_1524y = totale,
    inact_rate_1524y=inact_1524,
    unempl_rate_1524y=unempl_1524,
    municipality = com_name.x,
    surface_rev = superficie_rev,
    mountain = montuoso,
    employed = occupati.x,
    unemployed = disoccupati.x,
    laborforce = forzalavoro.x,
    college = laurea, 
    net_internal_migration = internal_migration,
    net_abroad_migration = abroad_migration,
    net_migration = migration,
    people_moving_internally = cancellatiperlinterno,
    people_moving_abroad = cancellatiperlestero,
    pop2004=popolazioneinizioperiodo
  )

# Generate new variables
merged_df <- merged_df %>%
  mutate(fam_ties=-entropy,net_migration_rate = (net_internal_migration+net_abroad_migration) / pop2004*100,gross_migration_rate=(people_moving_internally+people_moving_abroad)/pop2004*100)

merged_df$unempl_rate_1524y <- merged_df$unempl_rate_1524y * 100
merged_df$inact_rate_1524y <- merged_df$inact_rate_1524y * 100

write_dta(merged_df, "Data/Clean/municipalities.dta")






