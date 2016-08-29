# produce cartogram of west dunbartonshire 

rm(list = ls())


#install.packages("pacman")

require(pacman)
p_load(
  readr, stringr, car,
  purrr, tidyr, dplyr, 
  ggplot2, RColorBrewer,
  lattice,
  tmap, rgeos, sp, maptools,
  xlsx
)



# west dun cart -----------------------------------------------------------

attribute_data <- read_csv("data/W Dund Age File.csv", col_types = "cidcd")

west_dun_lookup <- read_csv("data/2001_DZs_with_CC_final_lookup.csv")

# West Dun attribute data only 
attribute_wd <- west_dun_lookup %>% left_join(attribute_data)

# shapefile? 

scot_cart <- read_shape(file = "shapefiles/cartogram/Scotland_2001_population_cartogram.shp", current.projection = "longlat")


# Now append this, 
# and remove all non-joined datazones 

wd_cart <- append_data(shp = scot_cart, data = west_dun_lookup, key.shp = "zonecode", key.data = "Datazone", ignore.na = T)

wd_cart <- wd_cart[!is.na(wd_cart@data$Datazone),]

plot(wd_cart)

# Age distribution of population 

wd_cart_fort <- fortify(wd_cart, region = "zonecode") %>% 
  tbl_df %>% rename(zonecode = id)


attribute_data %>% 
  filter(str_detect(Indicator, "^Age")) %>% 
  mutate(proportion = Numerator / Denominator) %>% 
  mutate(age_group = str_replace(Indicator, "^Age_", "")) %>% 
  filter(Time_Period == 2011) %>% 
  select(datazone = Datazone, age_group, proportion) %>% 
  spread(age_group, proportion)  %>% 
  right_join(wd_cart_fort, by =c("datazone"="zonecode")) -> wd_cart_agegrp_fort

wd_cart_agegrp_long <- wd_cart_agegrp_fort  %>% 
  gather(key = "age_group", value = "proportion", `0_to_15`:`75Plus`) %>% 
  mutate(age_group = recode(age_group, 
                            "
'0_to_15' = '0 to 15';
'16_to_64' = '16 to 64';
'65_to_74' = '65 to 74';
'75Plus' = '75 or older'
                            "
                            )
         )


theme_clean <- function(base_size = 12) { 
  require(grid)
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      axis.ticks.margin = unit(0, "cm"), 
      panel.margin = unit(0, "lines"),
      plot.margin = unit(c(0,0,0,0), "lines"),
      complete = TRUE
    )
} 

ggplot(wd_cart_agegrp_long, 
       aes(x = long, y = lat, group = group)
       ) +
  geom_polygon(aes(fill = proportion)) + 
  scale_fill_gradientn(
    colours = colorRampPalette(brewer.pal(12, "Paired"))(200), 
    limits = c(0, 1),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
    ) + 
  facet_wrap(~age_group) + 
  theme_clean() + 
  labs(title = "Proportion of population by age group", fill = "")

ggsave("maps/cart_proportion_by_age.png", width = 20, height = 20, units = "cm", dpi = 300)



# Income deprived proportion 

wd_cart_fort <- fortify(wd_cart, region = "zonecode") %>% 
  tbl_df %>% rename(zonecode = id)

attribute_data %>% 
  filter(str_detect(Indicator, "income")) %>% 
  mutate(proportion = Numerator / Denominator) %>% 
  select(datazone = Datazone, proportion) %>% 
  right_join(wd_cart_fort, by =c("datazone"="zonecode")) -> wd_cart_income_deprived



ggplot(wd_cart_income_deprived, 
       aes(x = long, y = lat, group = group)
) +
  geom_polygon(aes(fill = proportion)) + 
  scale_fill_gradientn(
    colours = colorRampPalette(brewer.pal(12, "Paired"))(200), 
    limits = c(0, 1),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
  ) + 
  theme_clean() + 
  labs(title = "Proportion of population Income Deprived", fill = "")

ggsave("maps/cart_proportion_income_deprived.png", width = 15, height = 15, units = "cm", dpi = 300)



# Now to do for all indicators other than age 

attribute_data %>% 
  filter(!str_detect(Indicator, "^Age_")) %>% 
  mutate(proportion = Numerator / Denominator) %>% 
  select(datazone = Datazone, Indicator, proportion) %>% 
  spread(Indicator, proportion)  %>% 
  right_join(wd_cart_fort, by =c("datazone"="zonecode")) -> wd_cart_nondemo_fort

wd_cart_nondemo_long <- wd_cart_nondemo_fort  %>% 
  gather(key = "indicator", value = "proportion", `Adults with qualifications at higher level and above`:`Young people not in employment, education or training`) 

ggplot(wd_cart_nondemo_long, 
       aes(x = long, y = lat, group = group)
) +
  geom_polygon(aes(fill = proportion)) + 
  scale_fill_gradientn(
    colours = colorRampPalette(brewer.pal(12, "Paired"))(200), 
    limits = c(0, 1),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
  ) + 
  facet_wrap(~indicator) + 
  theme_clean() + 
  labs(title = "Proportion by attribute", fill = "")

ggsave("maps/cart_proportion_by_nondemographic_attributes.png", width = 50, height = 30, units = "cm", dpi = 300)



# Tasks now  --------------------------------------------------------------


# 1. Show UR class on a map --------------------------------------------------

shp <- read_shape(file = "shapefiles/scotland_2001_datazones/scotland_dz_2001.shp")

shp_wd_join <- append_data(
  shp = shp, data = west_dun_lookup, key.shp = "zonecode", key.data = "Datazone",
  ignore.na = TRUE
                          )
shp_wd_join <- shp_wd_join[!is.na(shp_wd_join$Datazone),]


# tm_shape(shp_wd_join, borders = NULL) + 
#   tm_fill("CC_Name", title = "Community Council") + 
#   tm_legend(
#     title.size = 2.0,
#     text.size = 1.5
#   ) -> this_map

# does not quite work as too many community councils 

# will try to join and label instead

shp_wd_cc <- unionSpatialPolygons(shp_wd_join, IDs = shp_wd_join$CC_Name)

# Note unionSpatialPolygons returns a SpatialPolygons object not a spatialpolygons DF
# to convert back
#http://gis.stackexchange.com/questions/61633/r-convert-a-spatial-polygon-objet-to-spatial-polygon-data-frame
tmp_df <- data.frame(id = getSpPPolygonsIDSlots(shp_wd_cc))
row.names(tmp_df) <- getSpPPolygonsIDSlots(shp_wd_cc)

shp_wd_cc <- SpatialPolygonsDataFrame(shp_wd_cc, data = tmp_df)
rm(tmp_df)

tm_shape(shp_wd_cc) + 
  tm_borders(lwd = 1) + 
  tm_legend(
    title.size = 2.0,
    text.size = 1.5
  ) + 
  tm_text("id", shadow = T, remove.overlap = T) -> cc_map

save_tmap(cc_map, filename = "maps/wd_by_cc.png", height = 15, width = 15, units = "cm", dpi = 300)

# This works! 
# Now to try to create a cartogram using cc geographies 


attribute_data  %>% 
  filter(Time_Period == 2014)  %>% 
  filter(str_detect(Indicator, "^Age_"))  %>% 
  select(datazone = Datazone, population = Denominator)   %>% 
  inner_join(west_dun_lookup, by = c("datazone" = "Datazone")) %>% 
  group_by(CC_Name) %>% 
  distinct() %>% 
  summarise(population = sum(population)) %>% 
  append_data(
    shp = shp_wd_cc, data = ., 
    key.shp = "id", key.data = "CC_Name", ignore.na = T
    ) -> wd_cc_pop

write_shape(wd_cc_pop, file = "shapefiles/pop_by_cc/pop_by_cc.shp")

# Read this back in from scapetoad

cc_cart_by_pop <- read_shape(file = "shapefiles/cc_cart_by_pop/cc_by_pop.shp", current.projection = "longlat")


tm_shape(cc_cart_by_pop) + 
  tm_borders(lwd = 1) + 
  tm_legend(
    title.size = 2.0,
    text.size = 1.5
  ) + 
  tm_text("CC_Name", shadow = T, remove.overlap = T) -> cc_cart_pop

save_tmap(cc_cart_pop, filename = "maps/wd_by_cc_popcart.png", height = 15, width = 15, units = "cm", dpi = 300)


# Cartogram by cc


