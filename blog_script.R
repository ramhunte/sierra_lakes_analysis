
library(tidyverse)
library(janitor)
library(lubridate)
library(vegan)
library(sf)
library(ggspatial)
library(ggmap)
library(janitor)
library(patchwork)
library(gridExtra)
library(ggpubr)
library(sjPlot)
library(leaflet)


setwd("/Users/rayhunter/Documents/Bren/Past_Courses/EDS_222/sierra_lakes_analysis")

################ map data #################

#google map base layer
register_google(key = "AIzaSyAMlAe4fbAxjFToWnpW-28exZtSUC6bXL8")
api_key <- "AIzaSyAMlAe4fbAxjFToWnpW-28exZtSUC6bXL8"
basemap <- get_googlemap(center = c(lon = -119.9, lat = 37.715161),
                         zoom = 7,
                         maptype = "terrain",
                         key = api_key)

#sierra nevada boundary
snb <- read_sf("./data/SNV_boundary/Sierra_Nevada_Conservancy_Boundary.shp")

# #wilderness boundaries
# wilderness <- read_sf("./data/wilderness/Wilderness_Areas_122721.shp") %>%
#   st_transform(crs = st_crs(snb)) %>%
#   clean_names() %>%
#   select(name, state) %>%
#   filter(state == "CA") %>%
#   st_filter(y = snb, .predicate = st_within)

snb <- snb %>% st_transform(4326)
# wilderness <- wilderness %>% st_transform(4326)


# ############################# reading in data ##################################
# fish sample data
fish <- read.csv("data/Fish.csv") %>%
  clean_names()

#lakes data
lake <- read.csv("data/Lake.csv") %>%
  clean_names() %>%
  #lake id code names
  left_join(data.frame(lake_type_code = c(1,3,4,5,9,10,11,12), lake_code_name = c("perennial", "unmapped pond", "stream widening", "marsh", "part of another lake", "unmapped marsh", "ephemeral", "meadow")))

#survey information
survey <- read.csv("data/Survey.csv")

#zooplankton sample sums
zp_samp_sum <- read.csv("data/ZooplanktonSampleSums.csv")

#zooplankton counts
zp_count <- read.csv("data/ZooplanktonSpeciesCounts.csv") %>%
  clean_names()

#zooplankton species names
zp_name <- read.csv("data/ZooplanktonSpeciesName.csv") %>%
  clean_names()

############################# processing data ##################################

# organizing zoo plankton diversity data frame
zp_mod <- zp_count %>%
  group_by(lake_id, survey_date, subsample) %>%
  summarise(species_id = species_id, number = number, diversity = vegan::diversity(number)) %>%
  group_by(lake_id, survey_date) %>%
  summarise(mean_div = mean(diversity), richness = length(unique(species_id))) %>%
  left_join(lake) %>%
  select(-c(lake_drainage_name, lake_quad_name, lake_county_name, lake_juris_name, lake_wilderness_name))

# #fish 
fish_mod <- fish %>%
  group_by(lake_id, survey_date, fish_species) %>%
  summarise(mean_length = mean(fish_length), mean_weight = mean(fish_weight)) %>%
  left_join(lake) %>%
  select(-c(lake_drainage_name, lake_quad_name, lake_county_name, lake_juris_name, lake_wilderness_name)) %>%
  mutate(lake_status = ifelse(any(fish_species %in% c("BK", "BN")), "non-native", "native"))

# #full join of fish and zp 
zp_fish <- left_join(zp_mod, fish_mod)

# #fish presence df
fish_actual <- survey %>%
  select(lake_id, actual_fish_presence)

#final join with fish presence
total <- left_join(zp_fish, fish_actual) %>%
  mutate(year = factor(year(mdy(survey_date)), levels = c("1996", "1997", "2000", "2001"))) %>%
mutate(lake_code_name = factor(lake_code_name))


############ correlation of independent variables ####################
ind_cor <- cor(log(total$lake_area_nbr), total$lake_elevation_nbr)


############# calcualting correlations of totals 
cors = total %>%
  group_by(actual_fish_presence) %>%
  summarise(
        cor_da = round(cor(mean_div, log(lake_area_nbr)), 2),
         cor_de = round(cor(mean_div, lake_elevation_nbr), 2),
         cor_ra = round(cor(richness, log(lake_area_nbr)), 2),
         cor_re = round(cor(richness, lake_elevation_nbr), 2))

# ##################.  diversity model ##############
m_div <- lm(mean_div ~ log(lake_area_nbr) + lake_elevation_nbr + actual_fish_presence + log(lake_area_nbr):actual_fish_presence + year, data = total)
# m_div_sum <- summary(m_div)


#richness model
m_rich <- lm(richness ~ log(lake_area_nbr) + lake_elevation_nbr + actual_fish_presence + log(lake_area_nbr):actual_fish_presence +  year, data = total)
# summary(m_rich)

################ qq plots #####################

#diversity model residuals
m_div_res <- residuals(m_div)
#richness model residuals
m_rich_res <- residuals(m_rich)


#qq plot for diversity
div_qq <-  ggplot( m_div, aes(sample = .resid)) +
  geom_qq(color = "lightblue4") +
  geom_qq_line() +
  theme_classic() +
  labs( x = "Theoretical Quantiles", y = "Sample Quantiles")

#qq plot for richness
rich_qq <-  ggplot( m_rich, aes(sample = .resid)) +
  geom_qq(color = "lightblue4") +
  geom_qq_line() +
  theme_classic() +
  labs( x = "Theoretical Quantiles", y = "")

#joining plots
qq <- div_qq + rich_qq +
plot_annotation(tag_levels = "A")

qq

ggsave("figures/residuals.png", qq, height = 5, width = 8, unit = 'in')

















