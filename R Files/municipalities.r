library(fixest)
library(haven)
library(dplyr)
library(modelsummary)

# Load Municipality data
data <- read_dta("Data/Clean/municipalities.dta")

# Standardize family ties measure
df$fam_ties <- scale(df$fam_ties)


# Create custom set of controls
geo <-  c("litoraneo",  "density"  ,"alt_com","river" ,"lake" ,"sea_distance" ,"sh_mountain" , "d_alt")
educ <- c("high_school" ,"college")


#---------------------------------------------------------------------------------------------------
# REGRESSION ANALYSIS
#---------------------------------------------------------------------------------------------------


# OLS

## Unemployment
model1 <- lm(reformulate(c("fam_ties", geo,educ), response = "unempl_rate_1524y"), data = data)
summary(model1)
## Inactivity
model2 <- lm(reformulate(c("fam_ties", geo,educ), response = "inact_rate_1524y"), data = data)
summary(model2)
## Mobility
model3 <- lm(reformulate(c("fam_ties", geo,educ), response = "net_migration_rate"), data = data)
summary(model3)



#FE + IV

## Unemployment
model7 <- feols(unempl_rate_1524y ~  density + alt_com + river + lake 
               + sea_distance + sh_mountain + d_alt + pop_res_2007 + litoraneo + high_school + college | idsll | fam_ties ~ rug_med , data = data)
summary(model7)
## Inactivity
model8 <- feols(inact_rate_1524y ~  density + alt_com + river + lake 
               + sea_distance + sh_mountain + d_alt + pop_res_2007 + litoraneo + high_school + college | idsll | fam_ties ~ rug_med , data = data)
summary(model8)
## Mobility
model9 <- feols(net_migration_rate ~  density + alt_com + river + lake 
               + sea_distance + sh_mountain + d_alt + pop_res_2007 + litoraneo + high_school + college | idsll | fam_ties ~ rug_med , data = data)
summary(model9)



# Custom column names
model_list_ols <- list(
  "Youth Unemployment" = model1,
  "Youth Inactivity" = model2,
  "Net Migration" = model3
)


model_list_feiv <- list(
  "Youth Unemployment" = model7,
  "Youth Inactivity" = model8,
  "Net Migration" = model9
)


# OLS
modelsummary(model_list_ols,
             output = "Output/Tables/ols_table.tex",
             stars = TRUE,
             title = "OLS Estimates")


# FE-IV
modelsummary(model_list_feiv,
             output = "Output/Tables/feiv_table.tex",
             stars = TRUE,
             title = "FE-IV Estimates")
           



