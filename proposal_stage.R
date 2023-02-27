#library(WHO)
library(tidyverse)
library(forcats)
library(janitor)
library(openxlsx)
library(countrycode)
options(scipen = 999)

#y <- get_data("LIFE_0000000031")
#saveRDS(y, "WHOData_downloaded_1_30_23.RDS")

data <- readRDS("data/WHOData_downloaded_1_30_23.RDS")
data <- data %>%
  mutate(agegroup = factor(agegroup),
         agegroup = factor(agegroup, levels = c(levels(agegroup)[1:2], levels(agegroup)[11], levels(agegroup)[3:10], levels(agegroup)[12:19])),
         prob = (value/100000)*100) %>%
  filter(!is.na(country),
         !is.na(agegroup))

countries <- read.csv("data/country_names.csv")
data <- left_join(data, countries %>% select(DisplayString, ISO), by = c("country" = "DisplayString"))

#useless plot, plotting individual survival traces for all countries for 2019
ggplot(data[data$year == 2019 & data$sex != "Both sexes",])+
  geom_line(aes(x = agegroup, y = prob, group = interaction(country, sex), color = sex), alpha = .2)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))

#####################
#Research Question 1#
#####################

#Did survival rate estimates change over the last samples?

#May be worth splitting by Region.
RQ1 <- data %>%
  filter(sex == "Both sexes") %>%
  group_by(agegroup, year) %>%
  summarize(M = mean(prob),
            SD = sd(prob),
            N = n(),
            SE = SD/sqrt(N),
            CI = SE*qt(.025, N-1, lower.tail = FALSE))

ggplot(RQ1) +
  geom_line(aes(x  = agegroup, y = M, group = year, color = year))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))

pos <- position_dodge(width = .6)
ggplot(RQ1, aes(x  = agegroup, y = M, group = year, color = year)) +
  geom_line(position = pos, lwd = 1)+
  geom_errorbar(aes(ymin = M - CI, ymax = M + CI), width = 0, position = pos, lwd = 1)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

RQ1_text <- RQ1[RQ1$agegroup == "85+ years",]
RQ1_text$y <- RQ1_text$M - mean(RQ1_text$M)
RQ1_text$y <- RQ1_text$y*1.5 + RQ1_text$M

pos <- position_dodge(width = .6)
ggplot(RQ1, aes(x  = agegroup, y = M, group = year, color = year)) +
  geom_line(position = pos, lwd = 1)+
  geom_errorbar(aes(ymin = M - CI, ymax = M + CI), width = 0, position = pos, lwd = 1)+
  #geom_label_repel(data = RQ1[RQ1$agegroup == "85+ years",], aes(label = year), direction = "y", position = position_nudge_repel(x = 2))+
  geom_text(data = RQ1_text, aes(label = year, y = y), position = pos, hjust = -0.4)+
  #geom_text_repel(data = RQ1_text, aes(label = year,), position = pos, hjust = -0.2, direction = "y")+
  labs(title = "Younger generations live longer.",
       subtitle = paste0("Data averaged across ", RQ1$N[1], " countries"),
       x = "Age Group",
       y = "Percentage alive",
       color = "Sample Year",
       caption = "Data from the WHO\nErrorbars represent confidence intervals.")+
  scale_y_continuous(limits = c(0, 100), labels = function(y) paste0(y, " %"))+
  scale_color_viridis_c(option = "plasma", 
                        guide = guide_colourbar(direction = "horizontal", 
                                                 title.position = "top"), 
                        end = .9)+
  expand_limits(x = c(0, 22))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.1, 0.2),
        legend.key.width = unit(1, 'cm'),
        legend.key.height = unit(0.4, 'cm'),
        legend.justification = "left")


#BY REGION

RQ1_regions <- data %>%
  filter(sex == "Both sexes") %>%
  group_by(agegroup, region, year) %>%
  summarize(M = mean(prob),
            SD = sd(prob),
            N = n(),
            SE = SD/sqrt(N),
            CI = SE*qt(.025, N-1, lower.tail = FALSE))

RQ1_regions_text <- RQ1_regions[RQ1_regions$agegroup == "85+ years",]
RQ1_regions_text <- RQ1_regions_text %>%
  group_by(region) %>%
  mutate(y = seq(10, 40, length.out = 5))

pos <- position_dodge(width = 1)
ggplot(RQ1_regions, aes(x  = agegroup, y = M, group = year, color = year)) +
  geom_line(position = pos, lwd = 1)+
  geom_errorbar(aes(ymin = M - CI, ymax = M + CI), width = 0, position = pos)+
  geom_label(data = RQ1_regions_text[RQ1_regions_text$region == "Africa",], 
             aes(label = year, x = 1, y = y + 20),
             label.size = NA, 
             hjust = 0,
             #fill = rgb(0.7,0.7,0.7)
             fontface = "bold")+
  geom_label(data = RQ1_regions_text %>% group_by(region) %>% slice(1), 
             aes(label = sprintf("italic(N) == %d", N), x = 18, y = 95, group = region),
             label.size = NA, 
             #fill = rgb(0.7,0.7,0.7)
             parse = TRUE)+
  facet_wrap(~region)+
  labs(title = "Recent generations live longer across Regions.",
       x = "Age Group",
       y = "Percentage alive",
       color = "Sample Year",
       caption = "Data from the WHO\nErrorbars represent confidence intervals.")+
  scale_y_continuous(limits = c(NA, 100), labels = function(y) paste0(y, " %"))+
  scale_color_viridis_c(option = "plasma", 
                        guide = guide_colourbar(direction = "horizontal", 
                                                title.position = "top", 
                                                title.hjust = 0.5), 
                        end = .8)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        legend.key.width = unit(3, 'cm'),
        legend.key.height = unit(0.7, 'cm'),
        )

#https://www.datalorax.com/post/alluvial-plots-with-ggforce/

# RQ1_alluv <- RQ1 %>%
#   filter(year == 2019) %>%
#   mutate(dead = 100 - M)
# 
# RQ1_alluv <- RQ1_alluv %>% pivot_longer(cols = c(M, dead))
# RQ1_alluv %>% pivot_wider(names_from = agegroup, values_from = value, id_cols = name)
# 
# library(ggforce)
# 
# gather_set_data(ready, 2:20) %>%
#   ggplot(aes(x = x, id = id, split = y, value = ))

# 
# https://www.alexcernat.com/visualizing-transitions-in-time-using-r/
# RQ1_alluv <- RQ1 %>%
#   filter(year == 2019) %>%
#   mutate(dead = 100 - M) %>% 
#   pivot_longer(cols = c(M, dead))
# 
# ggplot(RQ1_alluv, aes(x = agegroup, y = value, stratum = name, alluvium = name))+
#   geom_stratum()
#   geom_flow()
# 

#####################

#####################
#Research Question 2#
#####################

#get map data
map <- map_data("world")
map$ISO <- countrycode(map$region, "country.name", "iso3c")

#data that would correspond to research question 2
RQ2 <- data %>%
  filter(sex == "Both sexes" & agegroup == "85+ years") %>%
  group_by(country, ISO) %>%
  summarize(M = mean(prob))

minmax <- RQ2 %>%
  ungroup() %>%
  arrange(M) %>%
  slice(1:5, (n() - 4) : n())

minmax_countries <- minmax$country

RQ2 <- left_join(map, RQ2)

ggplot(RQ2, aes(x = long, y = lat, group = group, fill = M))+
  geom_polygon(color = "black")+
  scale_fill_viridis_c(option = "plasma",
                       guide = guide_colourbar(title.position = "top", 
                                               title.hjust = 0.5),
                       labels = function(y) paste0(y, " %"))+
  labs(fill = "Survival Rate at 85+ years")+
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(3, 'cm'),
        legend.key.height = unit(0.7, 'cm'))


#####################
#Research Question 3#
#####################

#data that would correspond to research question 3
RQ3 <- data %>%
  filter(sex != "Both sexes" & agegroup == "85+ years" & year == 2019) %>%
  group_by(country, ISO) %>%
  summarize(age_diff = prob[sex == "Female"] - prob[sex == "Male"])

minmax <- RQ3 %>%
  ungroup() %>%
  arrange(age_diff) %>%
  slice(1:5, (n() - 4) : n())

minmax_countries <- minmax$country

RQ3 <- left_join(map, RQ3)

ggplot(RQ3, aes(x = long, y = lat, group = group, fill = age_diff))+
  geom_polygon(color = "black")+
  scale_fill_gradient2(low = "blue", mid = "white", midpoint = 0, high = "red")+
  # scale_fill_viridis_c(option = "plasma",
  #                      guide = guide_colourbar(title.position = "top", 
  #                                              title.hjust = 0.5),
  #                      labels = function(y) paste0(y, " %"))+
  labs(fill = "Survival Rate at 85+ years")+
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(3, 'cm'),
        legend.key.height = unit(0.7, 'cm'))

show_N <- 10
RQ3_bar <- RQ3 %>% 
  group_by(country) %>% 
  slice(1) %>%
  ungroup() %>%
  mutate(country = factor(country),
         country = fct_reorder(country, age_diff, .desc = TRUE)) %>%
  filter(!is.na(age_diff)) %>%
  arrange(age_diff) %>%
  slice(1:show_N, (n() - (show_N-1)) : n())

ggplot(RQ3_bar)+
  geom_bar(aes(x = country, y = age_diff, fill = age_diff), stat = "identity")+
  #scale_fill_gradient2(low = "blue", mid = "white", midpoint = 0, high = "red")+
  #coord_flip()+
  geom_vline(xintercept = show_N+0.5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#######
#BONUS#
#######

#This is for the replication of the figure and is just junk code at the moment

library(openxlsx)
xx <- read.xlsx("bonus plot/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", sheet = 1, startRow = 17, colNames = TRUE)

xy <- xx %>%
  clean_names() %>%
  filter(region_subregion_country_or_area == "WORLD") %>%
  select(-index, -variant, -notes, -location_code)

xy <- xy %>%
  pivot_longer(cols = starts_with("x"), names_to = "age") %>%
  mutate(age = gsub("x", "", age),
         age = as.numeric(as.character(age)),
         value = as.numeric(value),
         year = as.numeric(year))


ggplot(xy)+
  geom_path(aes(x = value, y = age, group = year, color = year))+
  scale_x_continuous(limits = c(0, NA))+
  theme(axis.text.x = element_text(angle = 90))+
  theme_classic()

male_est   <- read.xlsx("data/UN_POPULATION_SINGLE_AGE_MALE.xlsx", sheet = 1, startRow = 17, colNames = TRUE)
male_pro   <- read.xlsx("data/UN_POPULATION_SINGLE_AGE_MALE.xlsx", sheet = 2, startRow = 17, colNames = TRUE)
female_est <- read.xlsx("data/UN_POPULATION_SINGLE_AGE_FEMALE.xlsx", sheet = 1, startRow = 17, colNames = TRUE)
female_pro <- read.xlsx("data/UN_POPULATION_SINGLE_AGE_FEMALE.xlsx", sheet = 2, startRow = 17, colNames = TRUE)


male_est <- male_est %>%
  clean_names() %>%
  filter(region_subregion_country_or_area == "WORLD") %>%
  select(-index, -variant, -notes, -location_code, -iso2_alpha_code, -iso3_alpha_code, -sdmx_code, -type, -parent_code) %>%
  mutate(sex = "male",
         variant = "estimate", .after = region_subregion_country_or_area)

male_pro <- male_pro %>%
  clean_names() %>%
  filter(region_subregion_country_or_area == "WORLD") %>%
  select(-index, -variant, -notes, -location_code, -iso2_alpha_code, -iso3_alpha_code, -sdmx_code, -type, -parent_code) %>%
  mutate(sex = "male",
         variant = "pro", .after = region_subregion_country_or_area)

female_est <- female_est %>%
  clean_names() %>%
  filter(region_subregion_country_or_area == "WORLD") %>%
  select(-index, -variant, -notes, -location_code, -iso2_alpha_code, -iso3_alpha_code, -sdmx_code, -type, -parent_code) %>%
  mutate(sex = "female",
         variant = "estimate", .after = region_subregion_country_or_area)

female_pro <- female_pro %>%
  clean_names() %>%
  filter(region_subregion_country_or_area == "WORLD") %>%
  select(-index, -variant, -notes, -location_code, -iso2_alpha_code, -iso3_alpha_code, -sdmx_code, -type, -parent_code) %>%
  mutate(sex = "female",
         variant = "pro", .after = region_subregion_country_or_area)


data <- bind_rows(male_est, male_pro, female_est, female_pro)

data2 <- data %>%
  pivot_longer(cols = starts_with("x"), names_to = "age") %>%
  mutate(age = gsub("x", "", age),
         age = as.numeric(as.character(age)),
         value = as.numeric(value),
         year = as.numeric(year))

data2$value <- ifelse(data2$sex == "male", data2$value*-1, data2$value)

years_highlighted <- seq(1950, 2100, by = 25)
data3 <- data2[data2$year %in% years_highlighted,]
#data3 <- data2[data2$year %in% c(2060),]
helper <- data3 %>%
  group_by(region_subregion_country_or_area , sex, variant, year) %>%
  summarize(value = 0, age = 0)

helper2 <- data3 %>%
  group_by(region_subregion_country_or_area , sex, variant, year) %>%
  summarize(value = 0, age = 100)

helper <- bind_rows(helper2, helper, data3)

helper <- helper %>%
  mutate(year = factor(year, levels = rev(years_highlighted)))

ggplot()+
  geom_polygon(data = helper, aes(x = value, y = age, fill = as.numeric(as.character(year)), group = interaction(year, sex)), show.legend = FALSE)+
  geom_path(data = data3, aes(x = value, y = age, group = interaction(sex, year), linetype = variant), color = "black", lwd = .2)+
  geom_path(data = data2, aes(x = value, y = age, group = interaction(sex, year), linetype = variant), color = "lightgrey", lwd = .2, alpha = .1)+
  scale_fill_viridis_c(begin = .2, option = "plasma")+
  theme_classic()

