# produce cartogram of west dunbartonshire 

rm(list = ls())


install.packages("pacman")

require(pacman)
p_load(
  readr, stringr, car,
  purrr, tidyr, dplyr, 
  ggplot2, 
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

wd_cart_agegrp_fort <- fortify(wd_cart_agegrp, region = "zonecode") %>% 
  tbl_df %>% rename(zonecode = id)


attribute_data %>% 
  filter(str_detect(Indicator, "^Age")) %>% 
  mutate(proportion = Numerator / Denominator) %>% 
  mutate(age_group = str_replace(Indicator, "^Age_", "")) %>% 
  filter(Time_Period == 2011) %>% 
  select(datazone = Datazone, age_group, proportion) %>% 
  spread(age_group, proportion)  %>% 
  right_join(wd_cart_agegrp_fort, by =c("datazone"="zonecode")) -> wd_cart_agegrp_fort

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








# Specific tasks : 

# Load cartogram for scotland 
# remove dzs not in west dun
# view and resave






# Tasks now  --------------------------------------------------------------


# 1. Show UR class on a map --------------------------------------------------

ur_class <- read_csv("data/derived/dz_2001_by_ur_6fold_class.csv")
ur_class %>% 
  mutate(ur_label = factor(
    ur_class, 
    levels = c(1, 2, 3, 5, 6), 
    labels = c("large_urban", "other_urban", "accessible small towns", "accessible_rural", "remote_rural")
  )
  ) -> ur_class

shp <- read_shape(file = "data/shp/scotland_2001_datazones/scotland_dz_2001.shp")

shp_ur_join <- append_data(shp = shp, data = ur_class, key.shp = "zonecode", key.data = "dz_2001")



tm_shape(shp_ur_join, borders = NULL) + 
  tm_fill("ur_label", title = "Urban Rural Class") + # Note - important to add title at this stage rather than later
  tm_legend(
    title.size = 2.0,
    text.size = 1.5
  ) -> this_map

save_tmap(this_map, 
          filename = "maps/Scotland_urbanrural_standard.png", 
          width = 20, height = 35, units = "cm", dpi=300
)
