library(dplyr)
library(ggplot2)

trend_df <- read("incarceration_trends.csv")

#Summary Information

# Function computing region's  most total prison population
regions_high <- function(area){
  all_regions <- select(trend_df, region, total_prison_pop)
  regions <- filter(all_regions, region == area)
  highest_in_region <- max(regions$total_prison_pop, na.rm=TRUE)
  return(highest_in_region)
}

# South's highest total prison population
south_high <- regions_high("South")

# Northeast's highest total prison population
northeast_high <- regions_high("Northeast")

# Number of unique counties 
unique_counties <- length(unique(trend_df$county_name))

# White median prison population
white_med_jailpop <- median(trend_df$white_prison_pop, na.rm=TRUE)

# Average Asian prison population rate 
aapi_avg_prison_poprate <- mean(trend_df$aapi_prison_pop_rate, na.rm=TRUE)

# Difference between highest black male and black female prison population
male_black_max_prisonpop <- max(trend_df$black_male_prison_pop, na.rm=TRUE)
female_black_max_prisonpop <- max(trend_df$black_female_prison_pop, na.rm=TRUE)
black_sex_diff_prisonpop <- male_black_max_prisonpop - female_black_max_prisonpop

# Average Latinx prison admission rate in Washington state
washington_latin_df <- select(trend_df, state, latinx_prison_adm_rate)
washington_latin_df <- filter(washington_latin_df, state == "WA")
latinx_wash_avg_admrate <- mean(washington_latin_df$latinx_prison_adm_rate, na.rm=TRUE)


#Trend Over Time

race_pris_pop <- function(race){
  race_pris_pop_df <- select(trend_df, year, !!as.name(race))
  race_avg_pop_df <- race_pris_pop_df %>%
    group_by(year) %>% 
    summarise(avg = mean(!!as.name(race), na.rm = TRUE)) %>%
    arrange(avg)
  return(race_avg_pop_df)
}

aapi_pris_data <- race_pris_pop("aapi_prison_pop")
black_pris_data <- race_pris_pop("black_prison_pop")
latin_pris_data <- race_pris_pop("latinx_prison_pop")
native_pris_data <- race_pris_pop("native_prison_pop")
white_pris_data <- race_pris_pop("white_prison_pop")
other_pris_data <- race_pris_pop("other_race_prison_pop")

all_data <- aapi_pris_data %>%
  left_join(black_pris_data, by="year") %>%
  left_join(latin_pris_data, by="year") %>%
  left_join(native_pris_data, by="year") %>%
  left_join(white_pris_data, by="year") %>%
  left_join(other_pris_data, by="year")

trend_time_plt <- ggplot(all_data, aes(x=year, y=population))+
  geom_line(aes(y=avg.x, color="Aapi"))+
  geom_line(aes(y=avg.y, color="Black"))+
  geom_line(aes(y=avg.x.x, color="Latino"))+
  geom_line(aes(y=avg.y.y, color="Native"))+
  geom_line(aes(y=avg.x.x.x, color="White"))+
  geom_line(aes(y=avg.y.y.y, color="Other"))+
  guides(color = guide_legend(title="Race"))+
  labs(title="Average Prison Population by Race")


# Variable Comparison

pop_adm_rate_df <- select(trend_df, aapi_prison_adm_rate, native_prison_adm_rate)

aapi_native_rate_plt <- ggplot(pop_adm_rate_df, 
                               aes(x=aapi_prison_adm_rate, 
                                   y = native_prison_adm_rate))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlim(0, 4000)+
  ylim(0, 20000)+
  ggtitle("Scatter Plot of AAPI and Native",
          "Prison Admission Rates (PAR")+
  xlab("AAPI PAR")+
  ylab("Native American PAR")


# Map

black_pris_pop_df <- select(trend_df, state, black_prison_pop)
black_avg_pop_df <- black_pris_pop_df %>%
  group_by(state) %>% 
  summarise(avg = mean(black_prison_pop, na.rm = TRUE)) %>%
  arrange(avg)

map_plt <- plot_usmap(data = black_avg_pop_df, values = "avg", color = "blue") + 
  scale_fill_continuous(low = "white", 
                        high = "darkorchid1",
                        name = "Average Population", 
                        label = scales::comma) + 
  theme(legend.position = "right")+
  ggtitle("Average Black Prison Population for each \n State in the U.S.")