
# Preliminaries ----

# Packages
library("readxl")
library(Quandl)
library(dplyr)
library(tidyverse)
library(sp)
library(raster)
library(ggplot2)

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
germany = getData(country="Germany", level=1)

from_list = c()
for (i in 1:(nrow(bip_per_state)-1)) {
  id_num = germany@polygons[[i]]@ID
  from_list = append(from_list, id_num)
}

to_list = seq(1:(nrow(bip_per_state)-1))

spatial_list = broom::tidy(germany)

map = setNames(to_list, from_list)
spatial_list$id = map[spatial_list$id]

a = spatial_list %>% filter(id==5)


# TODO Add the information of GDP and go ahead


ggplot() +
  geom_path(data = a, aes(x = long, y = lat, group = group)) +
  coord_fixed()

  geom_polygon(data = germany, aes(fill=real_gdp_per_capita), color = "white") +
  labs(title = "My awesome ggplot map of coastlines",
       subtitle = "my awesome subtitle",
       x = "", y = "") # cause we don't need x and y labels do we?



