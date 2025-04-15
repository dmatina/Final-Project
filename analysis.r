library(sf)
library(dplyr)
library(ggplot2)
library(haven)
library(tidyr)
library(scales)
library(RColorBrewer)


data <- data %>%
  filter(is.finite(fam_ties))
data <- data %>% drop_na(fam_ties)

# Load Shape file
comuni_sf <- read_sf("Data/Shape Files/Municipality/Com01012018_g_WGS84.shp")

# Load Municipality data
data <- read_dta("Data/Clean/municipalities.dta")

# Ensure both ID columns are character type
data$idcom <- as.character(data$idcom)
comuni_sf$PRO_COM <- as.character(comuni_sf$PRO_COM)

# Merge on the municipal ID
map_data <- left_join(comuni_sf, data, by = c("PRO_COM" = "idcom"))


# Create Map

colour_breaks <- c(-10, -9, -8)
colours <- c("darkblue", "lightblue", "yellow")


reds=colorRampPalette(c("darkred", "white"))
blues=colorRampPalette(c("white", "navyblue"))



ggplot(map_data) +
  geom_sf(aes(fill = fam_ties), color = NA) +
 scale_fill_gradientn(colors=c(blues(30),reds(20) ))+
        
  theme_minimal() +
  labs(
    title = "Family Ties Index by Italian Municipality",
    fill = "Family Ties"
  )

# Save the plot as a PNG
ggsave("Output/Figures/family_ties_map_municipality.png")
