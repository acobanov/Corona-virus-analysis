
# clear variables and close windows                                                                  
graphics.off()
rm(list = ls(all=TRUE))

# packages
packages = c("tidyverse","coronavirus","purrr","eurostat","httr","jsonlite","kableExtra","curl",
             "jsonlite")

# install missing packages
lapply(packages, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
# add packages
lapply(packages, library, quietly = TRUE, character.only = TRUE)


# Getting the data from GitHub 
# ariable <data> is a named list (name: country name, list element: data.frame)
# date.frame columns are: date, confirmed, deaths, recovered
data <- fromJSON("https://pomber.github.io/covid19/timeseries.json")

# create vector of EU countries names and vector of Croatian neigbours countries names 
eu_countries <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland",
                  "France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg",
                  "Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden")
croatia_neighbours <-c("Italy","Slovenia","Serbia","Bosnia and Herzegovina","Hungary","Montenegro","Croatia")
countries <- names(data)

# create list of EU countries data frames
dfs_eu <- purrr::map(eu_countries,function(x) data[[x]] %>%  mutate(country=x))
names(dfs_eu) <-eu_countries
#View(dfs_eu[["Croatia"]])

# create list of Croatia's neighbours data frames
dfs_cro_neigh <- purrr::map(croatia_neighbours,function(x) data[[x]] %>%  mutate(country=x))
names(dfs_eu) <-croatia_neighbours
#View(dfs_cro_neigh[["Croatia"]])

# create long data frame
df_eu_long <- bind_rows(dfs_eu)
#View(df_eu)


# create wide data frame for CONFIRMED cases
df_eu_wide_conf <- df_eu_long %>% 
  select(date,country,confirmed) %>% 
  pivot_wider(names_from=country,values_from=confirmed)
#View(df_eu_wide_conf)

# create wide data frame for DEATHS
df_eu_wide_deaths <- df_eu_long %>% 
  select(date,country,deaths) %>% 
  pivot_wider(names_from=country,values_from=deaths)
#View(df_eu_wide_deaths)

# create wide data frame for RECOVERED cases
df_eu_wide_recov <- df_eu_long %>% 
  select(date,country,recovered) %>% 
  pivot_wider(names_from=country,values_from=recovered)
#View(df_eu_wide_recov)


# calculate wide data frame for NEW cases
df_eu_wide_new <- df_eu_wide_conf %>% 
  select(-c(date)) %>% 
  map(.,function(x) x-dplyr::lag(x)) %>% 
  bind_cols()
#View(df_eu_wide_new)

df <- df_eu_long %>% 
  filter(country %in% c('Croatia','Slovenia'))
#View(df)



xx <-1:length(df$date)
cro <- ggplot(data=df, aes(x=xx)) +
  geom_line(aes(y=), colour="#000099") +  # Blue lines
  geom_point(size=2, colour="#CC0000")         # Red dots


