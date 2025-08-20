## WDPA Data Exploration and per State Calculations
### Maya Hall
#### 8/19/2025

library(dplyr)
library(tidyr)
library(tidyverse)
library(terra)
library(sf)
setwd("C:/Users/mlhall6/Desktop")

# I. Explore CSV WDPA Data -------------------------------------------------------------------------

# bring in wdpa data in csv format
wdpa_data <- read.csv("WDPA_WDOECM_Aug2025_Public_USA_csv.csv")
wdpa_filter <- wdpa_data%>%filter(ISO3=="USA") #filter to USA only
wdpa_usa <- wdpa_filter%>%filter(MARINE == "0") #filter to terrestrial only
wdpa_data_separate <- wdpa_usa%>%separate_longer_delim(SUB_LOC, ";") #separate multi-state
wdpa_data_separate$SUB_LOC <- gsub(" ", "", wdpa_data_separate$SUB_LOC) #remove spaces in column

wdpa_state <- wdpa_data_separate%>%group_by(SUB_LOC)%>%summarise_at(c("REP_AREA", "GIS_AREA"), sum, na.rm=TRUE) #sum each column

# remove scientific notation for ease of interpretation
wdpa_state$REP_AREA <- format(wdpa_state$REP_AREA, scientific = FALSE)
wdpa_state$GIS_AREA <- format(wdpa_state$GIS_AREA, scientific = FALSE)

# II. Explore Shapefile WDPA Data -------------------------------------------------------------------------

# bring in wdpa data in shapefile format
shp1 <- st_read("C:/Users/mlhall6/Downloads/WDPA_WDOECM_Aug2025_Public_USA_shp/WDPA_WDOECM_Aug2025_Public_USA_shp_0/WDPA_WDOECM_Aug2025_Public_USA_shp-polygons.shp")
shp2 <- st_read("C:/Users/mlhall6/Downloads/WDPA_WDOECM_Aug2025_Public_USA_shp/WDPA_WDOECM_Aug2025_Public_USA_shp_1/WDPA_WDOECM_Aug2025_Public_USA_shp-polygons.shp")
shp3 <- st_read("C:/Users/mlhall6/Downloads/WDPA_WDOECM_Aug2025_Public_USA_shp/WDPA_WDOECM_Aug2025_Public_USA_shp_2/WDPA_WDOECM_Aug2025_Public_USA_shp-polygons.shp")

# merge the separated shapefiles (even split of data due to size)
all_shp <- list(shp1, shp2, shp3)
merged_wdpa <- do.call(rbind, all_shp)

merged_filter <- merged_wdpa%>%filter(ISO3 == "USA")
merged_usa <- merged_filter%>%filter(MARINE == "0")
merged_data_separate <- merged_usa%>%separate_longer_delim(SUB_LOC, ";")
merged_data_separate$SUB_LOC <- gsub(" ", "", merged_data_separate$SUB_LOC)
tmp <- merged_data_separate
merged_data_state <- merged_data_separate%>%group_by(SUB_LOC)%>%mutate_at(c("REP_AREA", "GIS_AREA"), sum, na.rm=TRUE)
head(merged_data_state)
# write shapefile to use in QGIS or ArcGIS 
# st_write(test,
#          "test_wdpa_cleaned.shp", driver = "ESRI Shapefile")


# III. Calculate PA Area by State -------------------------------------------------------------------------

# had to export to QGIS and calculate polygon area
## used $area function in QGIS field calculator 

updated_wdpa <- st_read("wdpa_area_calculated.shp")
clean_wdpa_df <- as.data.frame(updated_wdpa)
clean_wdpa <- clean_wdpa_df%>%group_by(SUB_LOC)%>%summarise_at(c("REP_AREA", "GIS_AREA", "pa_area_km"), sum, na.rm=TRUE)

# bring in state areas
state_areas <- read.csv("state_areas.csv")
colnames(state_areas) <- c("ID", "State", "Total_SqMi", "Total_SqKm", "Land_SqMi", "Land_SqKm", "long", "lat")

# merge necessary columns
colnames(clean_wdpa)<-c("ID", "REP_AREA", "GIS_AREA", "pa_area_km")
clean_wdpa <- clean_wdpa[-c(1), ]

states_and_wdpa <- left_join(clean_wdpa, state_areas, by = "ID")

# IV. Calculate Percentage of PA Area per State -------------------------------------------------------------------------

# calculate percentage of state covered by PAs
## use Land sq Km column for calculation
states_and_wdpa$Land_SqKm <- gsub(",", "", states_and_wdpa$Land_SqKm)
states_and_wdpa$Land_SqKm <- as.numeric(states_and_wdpa$Land_SqKm)
wdpa_with_perc <- states_and_wdpa%>%mutate(pa_percentage = (GIS_AREA/Land_SqKm)*100)

# V. Add Total Number of PAs per State -------------------------------------------------------------------------
w1 <- wdpa_data_separate%>%mutate(count = 1)
w2 <- w1%>%group_by(SUB_LOC)%>%summarise(total_pa = sum(count))
w3 <- w2[-c(1:2), ]
colnames(w3) <- c("ID", "total_pa_count")

final <- left_join(wdpa_with_perc, w3, by = "ID")

# VI. Create Comparison Tables -------------------------------------------------------------------------

library(flextable)

pa_by_count <- final%>%arrange(desc(total_pa_count))%>%slice(1:10)
pa_by_area <- final%>%arrange(desc(GIS_AREA))%>%slice(1:10)
pa_by_percent <- final%>%arrange(desc(pa_percentage))%>%slice(1:10)


pa_by_count_table <- flextable(pa_by_count, col_keys = c("ID", "State","total_pa_count"))
print(pa_by_count_table)

pa_by_area_table <- flextable(pa_by_area, col_keys = c("ID", "State", "GIS_AREA"))
print(pa_by_area_table)

pa_by_percent_table <- flextable(pa_by_percent, col_keys = c("ID", "State", "pa_percentage"))
print(pa_by_percent_table)
