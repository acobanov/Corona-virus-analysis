#***************************************************************************************************************************
# Title:                                      Corona Virus data analysis

# Author: Ante Cobanov
# GitHub: acobanov
# Mail: ante.cobanov@yahoo.com

#***************************************************************************************************************************
# clear variables and close windows                                                                  
graphics.off()
rm(list = ls(all=TRUE))

# packages
packages = c("tidyverse","coronavirus","purrr","eurostat","httr","jsonlite","kableExtra","curl",
             "jsonlite","hablar")

# install missing packages
lapply(packages, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
# add packages
lapply(packages, library, quietly = TRUE, character.only = TRUE)

#***************************************************************************************************************************

# Getting the data from GitHub 
# ariable <data> is a named list (name: country name, list element: data.frame)
# date.frame columns are: date, confirmed, deaths, recovered
data <- fromJSON("https://pomber.github.io/covid19/timeseries.json")
countries <- names(data)

#***************************************************************************************************************************

# create list of EU countries names and list of Croatian neigbours countries names 
# I prefer to use 'named lists' here because of later use of 'map' function (so that output is also NAMED list)
eu_countries <- list("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland",
                     "France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg",
                     "Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden")
names(eu_countries) <-c("aus","bel","bul","cro","cyp","czh","den","est","fin",
                        "fra","ger","gre","hun","ire","ita","lat","lit","lux",
                        "mal","net","pol","por","rom","slk","slo","spa","swe")

cro_neighbours <-list("Italy","Slovenia","Serbia","Bosnia and Herzegovina","Hungary","Montenegro","Croatia")
names(cro_neighbours) <-c("ita","slo","ser","bih","hun","mng","cro")

# perform two analysis
analysis        <- list(eu_countries,cro_neighbours)
names(analysis) <- c("eu","neigh")

#**************************************************************************************************************

# create list of lists of data frames
dfs <- analysis %>% 
  map(.,function(x) map(x,function(y) data[[y]] %>% mutate(country=y)))
# View(dfs[['eu']][['cro']])
# View(dfs[['neigh']][['ser']])
  
# create list of long data frames
dfs_long <- dfs %>% 
  map(.,function(x) bind_rows(x))
# View(dfs_long[['eu']])
# View(dfs_long[['neigh']])


# create list of wide data frames for CONFIRMED cases
dfs_wide_conf <-dfs_long %>% 
  map(.,function(x) x %>% select(date,country,confirmed) %>% pivot_wider(names_from=country,values_from=confirmed))
# View(dfs_wide_conf[['eu']])
# View(dfs_wide_conf[['neigh']])
# save data frame to csv file
readr::write_excel_csv(dfs_wide_conf[['eu']],"eu_confirmed.csv")
readr::write_excel_csv(dfs_wide_conf[['neigh']],"neigh_confirmed.csv")

# create list of wide data frames for DEATHS cases
dfs_wide_deaths <-dfs_long %>% 
  map(.,function(x) x %>% select(date,country,deaths) %>% pivot_wider(names_from=country,values_from=deaths))
# View(dfs_wide_deaths[['eu']])
# View(dfs_wide_deaths[['neigh']])
# save data frame to csv file
readr::write_excel_csv(dfs_wide_deaths[['eu']],"eu_deaths.csv")
readr::write_excel_csv(dfs_wide_deaths[['neigh']],"neigh_deaths.csv")

# create list of wide data frames for RECOVERED cases
dfs_wide_recov <-dfs_long %>% 
  map(.,function(x) x %>% select(date,country,recovered) %>% pivot_wider(names_from=country,values_from=recovered))
# View(dfs_wide_recov[['eu']])
# View(dfs_wide_recov[['neigh']])
# save data frame to csv file
readr::write_excel_csv(dfs_wide_recov[['eu']],"eu_recov.csv")
readr::write_excel_csv(dfs_wide_recov[['neigh']],"neigh_recov.csv")

# calculate wide data frame for NEW cases
dfs_wide_new <- dfs_wide_conf %>% 
  map(.,function(x){ x %>% 
                    select(-c(date)) %>% 
                    map_dfr(.,function(y) y-dplyr::lag(y))})
# View(dfs_wide_new[['eu']])  
# View(dfs_wide_new[['neigh']])  
readr::write_excel_csv(dfs_wide_new[['eu']],"eu_new.csv")
readr::write_excel_csv(dfs_wide_new[['neigh']],"neigh_new.csv")

# calculate wide data frame for LOG2 DIFFERENCE IN CONFIRMED cases
dfs_wide_ld <- dfs_wide_conf %>% 
  map(.,function(x){ x %>% 
      select(-c(date)) %>% 
      map_dfr(.,function(y) (log2(y)-dplyr::lag(log2(y))))*100})
# View(dfs_wide_ld[['eu']])  
# View(dfs_wide_ld[['neigh']])  
readr::write_excel_csv(dfs_wide_ld[['eu']],"eu_ld.csv")
readr::write_excel_csv(dfs_wide_ld[['neigh']],"neigh_ld.csv")

# Remove 'inf'
dfs_wide_ld <- dfs_wide_ld %>% 
  map(.,function(x) hablar:::rationalize(x))
# View(dfs_wide_ld[['eu']])


chosen <- dfs_wide_new[['neigh']] [-(1:60),]
  
data_chosen<-as.matrix(chosen) 
heatmap(t(data_chosen),Colv=NA,Rowv=NA,scale="row")




