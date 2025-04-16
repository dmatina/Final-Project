library(sf)
library(dplyr)
library(ggplot2)
library(haven)
library(tidyr)
library(scales)
library(RColorBrewer)
library(patchwork)

# Load Municipality data
data <- read_dta("Data/Clean/municipalities.dta")

data <- data %>%
  filter(is.finite(surname_concentration))
data <- data %>% drop_na(surname_concentration)

# Load Shape file
comuni_sf <- read_sf("Data/Shape Files/Com01012018_g_WGS84.shp")



# Ensure both ID columns are character type
data$idcom <- as.character(data$idcom)
comuni_sf$PRO_COM <- as.character(comuni_sf$PRO_COM)

# Merge on the municipal ID
map_data <- left_join(comuni_sf, data, by = c("PRO_COM" = "idcom"))


# Create Map (Municipality)

colour_breaks <- c(-10, -9, -8)
colours <- c("darkblue", "lightblue", "yellow")


reds=colorRampPalette(c("white","darkred"))
blues=colorRampPalette(c("navyblue","white" ))



plot1<- ggplot(map_data) +
  geom_sf(aes(fill = surname_concentration), color = NA) +
 scale_fill_gradientn(colors=c(blues(120),reds(100) ))+
        
  theme_minimal() +
  labs(
    fill = "Surname Conc."
  )+ theme_void()+ theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  ) 



plot2<- ggplot(map_data) +
  geom_sf(aes(fill = unempl_rate_1524y), color = NA) +
 scale_fill_gradientn(colors=c(blues(60),reds(120) ))+
        
  theme_minimal() +
  labs(
    fill = "Youth Unempl."
  )+ theme_void()+ theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  ) 

  
combined_plot <- plot1 + plot2  +
  plot_annotation(title = "Surname Concentration and Youth Unemployment Across Italian Municipalities",
  ,
    theme = theme(
      plot.title = element_text( hjust = 0.5))) # side by side

print(combined_plot)
# Save the plot as a PNG
ggsave("Output/Figures/fam_unempl_municipality.png",plot = combined_plot)








