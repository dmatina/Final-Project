library(fixest)
library(haven)
library(dplyr)
library(modelsummary)

# Load Municipality data
data <- read_dta("Data/Clean/municipalities.dta")

# Standardize family ties measure
data$surname_concentration <- scale(data$surname_concentration)


# Create custom set of controls
geo <-  c( "density"  ,"alt_com","river" ,"lake" ,"sea_distance" ,"sh_mountain" , "d_alt","pop_res_2007","litoraneo" )
educ <- c("high_school" ,"college")


#---------------------------------------------------------------------------------------------------
# REGRESSION ANALYSIS
#---------------------------------------------------------------------------------------------------


# OLS

## Unemployment
model1 <- lm(reformulate(c("surname_concentration", geo,educ), response = "unempl_rate_1524y"), data = data)
summary(model1)
## Inactivity
model2 <- lm(reformulate(c("surname_concentration", geo,educ), response = "inact_rate_1524y"), data = data)
summary(model2)
## Net Migration
model3 <- lm(reformulate(c("surname_concentration", geo,educ), response = "net_migration_rate"), data = data)
summary(model3)



#FE + IV

## Unemployment
model7 <- feols(unempl_rate_1524y ~  density + alt_com + river + lake 
               + sea_distance + sh_mountain + d_alt + pop_res_2007 + litoraneo + high_school + college | idsll | surname_concentration ~ rug_med , data = data)
summary(model7)
## Inactivity
model8 <- feols(inact_rate_1524y ~  density + alt_com + river + lake 
               + sea_distance + sh_mountain + d_alt + pop_res_2007 + litoraneo + high_school + college | idsll | surname_concentration ~ rug_med , data = data)
summary(model8)
## Net Migration
model9 <- feols(net_migration_rate ~  density + alt_com + river + lake 
               + sea_distance + sh_mountain + d_alt + pop_res_2007 + litoraneo + high_school + college | idsll | surname_concentration ~ rug_med , data = data)
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

custom_stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01)


controls<- data.frame(
  term = c("Other Geo Controls", "HC Controls"),
   `Model 1` = c("Yes", "Yes"),
  `Model 2` = c("Yes", "Yes"),
  `Model 3` = c("Yes", "Yes")
)


FEs <- data.frame(
  term = c("Other Geo Controls", "HC Controls","SLL Fixed Effects"),
  `Model 1` = c("Yes", "Yes", "Yes"),
  `Model 2` = c("Yes", "Yes", "Yes"),
  `Model 3` = c("Yes", "Yes", "Yes")
)

attr(controls, "position") <- c(11,12)
attr(FEs, "position") <- c(11,12)
# OLS output
modelsummary(model_list_ols,
             output = "Output/Tables/ols_table.png",
             title = "OLS Estimates",
             coef_omit = "^(?!surname_concentration|density|alt_com|river|sea_distance)",
             gof_map = c("nobs", "r.squared"),
             stars = custom_stars,
             add_rows=controls
)


# FE-IV
modelsummary(model_list_feiv,
             output = "Output/Tables/feiv_table.png",
             title = "FE-IV Estimates",
             coef_omit = "^(?!fit_surname_concentration|density|alt_com|river|sea_distance)",
             gof_map = c("nobs", "r.squared"),
             stars = custom_stars,
             add_rows=FEs
)






