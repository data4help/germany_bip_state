
# Preliminaries ----

# Packages
library("readxl")
library("gganimate")
library("gifski")
library(magick)
library(dplyr)
library(tidyverse)
library(sp)
library(raster)
library(ggplot2)
library(ggthemes)
library(ineq)


# Paths
main_path = "/Users/paulmora/Documents/projects/germany_bip_state"
raw_path = paste(main_path, "/00 Raw", sep="")
code_path = paste(main_path, "/01 Code", sep="")
data_path = paste(main_path, "/02 Data", sep="")
output_path = paste(main_path, "/03 Output", sep="")

# Importing data
bip_per_state = read_excel(paste(raw_path, "/bip_states.xlsx", sep=""),
                           skip=10)
german_inflation = Quandl("RATEINF/CPI_DEU", api_key="GxoUoLmANtEtV2GmdxhV",
                          collapse="annual")

# Data Cleaning ----

"
We reshape the bip data into a long format and afterwards merge the inflation
number onto that. Afterwards we create the real BIP over time.
"

bip_per_state$id = seq(nrow(bip_per_state))
reshaped_data = gather(bip_per_state, year, gdp, "1991":"2019",
                       factor_key=TRUE)

german_inflation$year = substr(german_inflation$Date, 0, 4)
german_inflation$benchmarked_inflation = (german_inflation$Value
                                          / max(german_inflation$Value))
subset_inflation = german_inflation %>%
  dplyr::select(year, benchmarked_inflation)
gdp_data = merge(reshaped_data,
                 subset_inflation,
                 by=c("year"))
gdp_data$real_gdp_per_capita = gdp_data$gdp / gdp_data$benchmarked_inflation

# Germany map ----

"
In order to show the absolute improvement over time for each state. We plot
a heatmap of Germany over time. For that we need spatial data of a germany
map, which we then have to combine with the real gdp per capita information
we gathered earler. Note that the order of states from the getData command
does not align with the alphabetical order. We therefore have to change
that in the first instance.
"

map_germany = getData(country="Germany", level=1)

from_list = c()
for (i in 1:(nrow(bip_per_state)-1)) {
  id_num = map_germany@polygons[[i]]@ID
  from_list = append(from_list, id_num)
}

to_list = seq(1:(nrow(bip_per_state)-1))
map = setNames(to_list, from_list)

spatial_list = broom::tidy(map_germany)
spatial_list$id = map[spatial_list$id]

overall_min = min(gdp_data$real_gdp_per_capita)
overall_max = max(gdp_data$real_gdp_per_capita)

for (year_num in unique(gdp_data$year)) {

  gdp_per_year = gdp_data %>% filter(year==year_num)
  spatial_data_per_year = merge(spatial_list, gdp_per_year, by="id")

  gdp_plotting = spatial_data_per_year %>%
    ggplot(aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill=real_gdp_per_capita), color="white",
                 data=filter(spatial_data_per_year,
                             !Bundesland %in% c("Berlin", "Bremen"))) +
    geom_polygon(aes(fill=real_gdp_per_capita), color="white",
                 data=filter(spatial_data_per_year,
                             Bundesland %in%  c("Berlin", "Bremen"))) +
    theme_tufte() +
    coord_fixed() +
    scale_fill_gradient(name="Real Euros per Capita",
                        limits=c(overall_min, overall_max),
                        guide=guide_colourbar(barheight=unit(80, units="mm"),
                                              barwidth=unit(5, units="mm"),
                                              draw.ulim=F,
                                              title.hjust=0.5,
                                              label.hjust=0.5,
                                              title.position="top")) +
    coord_map() +
    ggtitle(paste("GDP Per Capita -", year_num)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.title=element_text(size=25),
          legend.text=element_text(size=20),
          legend.position="right",
          plot.title = element_text(lineheight=.8, face="bold", size=45)) +
    ggsave(paste(output_path, "/maps/", year_num, ".png", sep=""))
}

setwd(paste(output_path, "/maps/", sep=""))
system("convert -delay 80 *.png maps_over_time.gif")
file.remove(list.files(pattern=".png"))

# Barplot Catch-Up ----

"
Looking at a dynamic version of the GDP per capita with moving barcharts
"

east_states = c("Berlin", "Mecklenburg-Vorpommern", "Sachsen",
                "Sachsen-Anhalt", "Thüringen")

barplot_gdp_data = gdp_data %>%
  filter(Bundesland != "Deutschland") %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  mutate(position = if_else(Bundesland %in% east_states, "East", "West")) %>%
  group_by(year_num) %>%
  mutate(rank = min_rank(-real_gdp_per_capita) * 1) %>%
  ungroup()

p = ggplot(barplot_gdp_data, aes(rank, group=Bundesland,
                    fill=as.factor(position),
                    color=as.factor(position))) +
  geom_tile(aes(y=real_gdp_per_capita/2,
                height=real_gdp_per_capita,
                width=0.9),
            alpha=0.8, color=NA) +
  geom_text(aes(y=0, label=Bundesland), vjust = 0.2, hjust = 1, size=10) +

  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels=scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme_tufte() +
  ggtitle("GDP per Capita by State in {closest_state}") +
  labs(x="", y="GDP per Capita") +
  theme(plot.title = element_text(hjust = 0, size = 35),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        axis.text=element_text(size=20),
        axis.title=element_text(size=25),
        axis.text.x = element_text(size=25),
        plot.margin = unit(c(1, 1, 1, 12.5), "cm"))+
  transition_states(year_num, transition_length=4, state_length=1) +
  ease_aes("cubic-in-out")

animate(p, fps=25, duration=20, width=1200, height=800,
        renderer=gifski_renderer(paste(output_path,
                                       "/barchart/animation.gif", sep="")))

# Gini-Coefficients ----

"
In order to better see how the inequality disappears over time, we look at the
Lorenz curve and the gini coefficient over time
"

for (year_num in unique(gdp_data$year)) {

  real_gdp_data_year = gdp_data %>%
    filter(year==year_num) %>%
    filter(Bundesland != "Deutschland")

  cum_real_gdp = Lc(real_gdp_data_year$real_gdp_per_capita)
  gini_coef = round(Gini(real_gdp_data_year$real_gdp_per_capita), 2)
  p = cum_real_gdp[1]
  L = cum_real_gdp[2]
  cum_fun_df = data.frame(p,L)

  # plot
  ggplot() +
    geom_point(data=cum_fun_df, aes(x=p, y=L)) +
    geom_line(data=cum_fun_df, aes(x=p, y=L), color="#990000") +

    scale_y_continuous(name="Cumulative GDP per Capita", limits=c(0,1)) +
    scale_x_continuous(name="Cumulative Number of States", limits=c(0,1)) +

    geom_line(data=cum_fun_df, aes(x=p, y=p)) +
    geom_ribbon(data=cum_fun_df, aes(x=p, ymin=L,ymax=p),
                fill="blue", alpha=0.5) +
    theme_tufte() +
    ggtitle(label=paste("Lorenz Curve -", year_num),
            subtitle=paste("Gini Coefficient: ", gini_coef)) +
    theme(plot.title = element_text(lineheight=.8, face="bold", size=35),
          plot.subtitle = element_text(lineheight=.8, size=30),
          axis.text=element_text(size=20),
          axis.title=element_text(size=25,face="bold"),
          axis.text.x = element_text(size = 14)) +
    ggsave(paste(output_path, "/gini/", year_num, ".png", sep=""))

}

setwd(paste(output_path, "/gini/", sep=""))
system("convert -delay 80 *.png gini_over_time.gif")
file.remove(list.files(pattern=".png"))

# West East Average ----

average_gdp_data = gdp_data %>%
  filter(Bundesland != "Deutschland") %>%
  mutate(Area = if_else(Bundesland %in% east_states, "East", "West")) %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  group_by(year_num, Area) %>%
  summarize(avg_gdp = mean(real_gdp_per_capita)) %>%
  ungroup()

avg_gdp_data_wide = spread(average_gdp_data, Area, avg_gdp) %>%
  mutate(east_percent_west = round((East/West)*100))


scaler = max(average_gdp_data$avg_gdp)
ggplot()  +
  geom_bar(data=avg_gdp_data_wide, aes(x=year_num,
                                       y=east_percent_west/100*scaler),
           stat="identity", fill="tan1", colour="sienna3", alpha=0.1) +
  geom_text(data=avg_gdp_data_wide, aes(label=east_percent_west, x=year_num,
                                        y=east_percent_west/100*scaler),
            colour="black") +
  geom_line(data=average_gdp_data, aes(x=year_num, y=avg_gdp, group=Area,
                                       color=Area),
            stat="identity") +
  geom_point(data=average_gdp_data, aes(x=year_num, y=avg_gdp, group=Area,
                                        color=Area),
            stat="identity") +
  scale_y_continuous(sec.axis=sec_axis(trans=~./scaler*100,
     name = "Percentage of the East to West Average GDP")) +
  theme_tufte() +
  ggtitle(label="Average GDP per Capita for East and West Germany",
          subtitle="") +
  labs(x="Year", y="Average GDP per Capita by Area") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=30),
        plot.subtitle = element_text(lineheight=.8, size=20),
        axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        axis.text.x = element_text(size = 14),
        legend.title=element_text(size=25),
        legend.text=element_text(size=20),
        legend.position="bottom",) +

  ggsave(paste(output_path, "/over_time.png", sep=""))

# Berlin's impact in the east ----

east_gdp_data = gdp_data %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  mutate(Area = if_else(Bundesland %in% east_states, "East", "West")) %>%
  filter(Area == "East") %>%
  mutate(Area = if_else(Bundesland == "Berlin", "Berlin", "Rest")) %>%
  group_by(year_num, Area) %>%
  summarize(avg_gdp = mean(real_gdp_per_capita)) %>%
  ungroup()

ggplot()  +
  geom_line(data=east_gdp_data, aes(x=year_num, y=avg_gdp, group=Area,
                                    color=Area)) +
  geom_point(data=east_gdp_data, aes(x=year_num, y=avg_gdp, group=Area,
                                     color=Area)) +
  theme_tufte() +
  ggtitle(label="Average GDP per Capita of all",
          subtitle="") +
  labs(x="Year", y="Average GDP per Capita by Area") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=30),
        plot.subtitle = element_text(lineheight=.8, size=20),
        axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        axis.text.x = element_text(size = 14),
        legend.title=element_text(size=25),
        legend.text=element_text(size=20),
        legend.position="bottom",) +

  ggsave(paste(output_path, "berlin_influence.png", sep=""))
