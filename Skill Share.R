#load libraries needed for analysis
library(here)
library(tidyverse)
library(plotly)
library(ggmap)
library(leaflet)
library(tidycensus)
library(tigris)
library(sf)
library(htmltools)
library(broom)
library(RColorBrewer)
library(DT)
options(tigris_use_cache = TRUE)

#understand census variables
v_list18 <- load_variables(2018, "acs5", cache = TRUE)
View(v_list18)
#search "median household income"
#result is B19013_001

#retrieve income data for CA counties in 2018
CA_inc_18 <- get_acs(geography = "county", 
          variables = c(med_inc = "B19013_001"), 
          state = "CA",
          year = 2018,
          geometry = TRUE)
View(CA_inc_18)
#map it
ggplot(CA_inc_18, aes(fill = estimate, color =estimate)) +
  geom_sf()

# Map through ACS estimates to see how income by CA county changes through the years
CA_inc_yrs <- map_df(2016:2018, function(x) {
  get_acs(geography = "county", 
          variables = c(med_inc = "B19013_001"), 
          state = "CA",
          geometry = TRUE,
          year = x) %>%
    mutate(year = x)})
View(CA_inc_yrs)

#get income data for CA pumas
CAinc <- get_acs(geography = "public use microdata area",
                    variables = c(med_inc = "B19013_001"),
                    year = 2018,
                    output = "wide",
                    state = "CA",
                    geometry = FALSE)

#get CA pumas shapefile and convert to sf object
CApumas <- pumas(state = "CA") %>% 
  as(., "sf")
plot(CApumas)

#join data to shapefiles and transform to 4326 projection
CAjoin <- left_join(CApumas, CAinc, by = c("GEOID10" = "GEOID"))
st_crs(CAjoin)
CAjoin_map <- st_transform(CAjoin, crs = "+init=epsg:4326")

#filter for LA County Only
LACounty <- CAjoin_map %>% 
  filter(str_detect(NAME, "Los Angeles County"))
#plot(LACounty)

#create simple boundary map with basemap
map1 <- leaflet() %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addPolygons(data = LACounty,
              weight = 1,
              color = "black",
              label = ~paste0("PUMA: ", NAME))
map1

#create cool color palette bins for mapping
display.brewer.all()
cool_pal <-colorBin(palette = "YlGnBu", domain = LACounty$med_incE, bins = c(0,50000,75000,100000,500000))

#build bins cloropleth map
map2 <- leaflet() %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addPolygons(data = LACounty,
              weight = 1,
              color = "black",
              popup = ~paste0("<strong>PUMA: </strong>", NAME, "<br><strong>2018 Median Income: </strong>", scales::dollar(med_incE)),
              fillColor = ~cool_pal(med_incE),
              fillOpacity = 0.7) %>% 
  addLegend(position = "bottomleft",
            pal = cool_pal,
            values = LACounty$med_incE,
            labFormat = labelFormat(prefix = "$"),
            title = "Median Income")
map2


#create warm continuous palette to map
warm_pal <- colorNumeric(palette = "RdYlGn", domain = LACounty$med_incE)

#build continuous cloropleth map
map3 <- leaflet() %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addPolygons(data = LACounty,
              weight = 1,
              color = "black",
              popup = ~paste0("<strong>PUMA: </strong>", NAME, "<br><strong>2018 Median Income: </strong>", scales::dollar(med_incE)),
              fillColor = ~warm_pal(med_incE),
              fillOpacity = 0.7) %>% 
  addLegend(position = "bottomleft",
            pal = warm_pal,
            values = LACounty$med_incE,
            labFormat = labelFormat(prefix = "$"),
            title = "Median Income")
map3


#get median income and median home value by tx school district
v_list17 <- load_variables(2017, "acs5", cache = TRUE)
#View(v_list17)
tx_dist <- get_acs(geography = "school district (unified)",
                   variables = c(med_inc = "B19013_001", med_home_val = "B25077_001"),
                   year = 2017,
                   output = "wide",
                   state = "TX",
                   geometry = FALSE)
#View(tx_dist)

#make a simple plot to show data
plot1 <- ggplot(data = tx_dist,
       aes(x = med_incE, y = med_home_valE)) +
  geom_point() +
  geom_smooth()
plot1
ggplotly(plot1)

#bring in ratings data
tx_ratings <- read.csv("tx_schools.csv")
#View(tx_ratings)

#try to align field names to join
tx_dist <- tx_dist %>%
  mutate(short_name = str_replace(NAME, "Independent School District, Texas", "Isd"))

#join data and see missing
joined_schools <- left_join(tx_dist, tx_ratings, by = c("short_name" = "District"))
#View(joined_schools)
sum(is.na(joined_schools$Achievement))
nrow(joined_schools)

#rework variables to be in proper format
joined_schools_complete <- joined_schools %>%
  na.omit() %>% 
  mutate(new_rating = as.character(Rating)) %>% 
  mutate(real_ach = as.numeric(as.character(Achievement)))
#View(joined_schools_complete)

#make a plot to show districts with colors for ratings
plot2 <- ggplot(data = joined_schools_complete,
                aes(x = med_incE, y = med_home_valE, color = new_rating)) +
  geom_point()
plot2

#make a plot to show colors as numberic scores
plot3 <- ggplot(data = joined_schools_complete,
                aes(x = med_incE, y = med_home_valE, text = paste0("District : ",short_name), color = real_ach)) +
  geom_point() +
  scale_color_continuous(high = "red", low = "yellow") +
  theme_bw() +
  scale_x_continuous(name = "Median HH Income", labels = scales::dollar) +
  scale_y_continuous(name = "Median Home Value", labels = scales::dollar )
plot3

#interactive plot to hover on different districts
ggplotly(plot3)

#interactive data table
tx_table <- joined_schools_complete %>% 
  select(short_name, med_home_valE, med_incE, new_rating, real_ach) %>% 
  rename("District Name" = short_name, "Median Home Value" = med_home_valE, "Median Income" = med_incE, "District Grade" = new_rating, "Achievement Score" = real_ach)
datatable(tx_table, options = list(
  columnDefs = list(list(className = "dt-center", targets = 2:5))
)) %>% 
  formatCurrency(c("Median Home Value", "Median Income"), digits = 0)

#regression analysis with easy output
tx_reg <- lm(med_home_valE ~ real_ach + med_incE, data = joined_schools_complete)
summary(tx_reg)
tx_reg_summary <- tidy(tx_reg)
#View(tx_reg_summary)
write.csv(tx_reg_summary, "tx_regression_schools.csv")


#other stuff - simple geocoding
steamboat <- geocode("Steamboat, CO", output = "more", source = "google")
#map the point with view centered
leaflet (options = leafletOptions(maxZoom = 20)) %>% 
  addProviderTiles("CartoDB") %>% 
  setView(lng = steamboat$lon,
          lat = steamboat$lat,
          zoom = 5) %>% 
  addMarkers(steamboat, lng = steamboat$lon,
             lat = steamboat$lat,
             popup = steamboat$address)

