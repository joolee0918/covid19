## Download termperature data
#author: "Jooyoung Lee"
#date: "5/11/2020"

library(dplyr)

## temp
countyid <- read.csv("/Users/jooyounglee/Dropbox/2020_covid19/covid19/Data/countyinfo.csv")
countyid <- countyid[,-1]
names(countyid) <- c("FIPS", "County.Name", "State_abbrev")
county_temp = read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/temp_seasonal_county.csv")
names(county_temp)[1] <- "FIPS"
county_temp <- merge(county_temp, countyid, by="FIPS")

state_temp = county_temp %>% 
  group_by(State_abbrev) %>% 
  summarise(mean_winter_temp= mean(winter_tmmx),
            mean_summer_temp= mean(summer_tmmx),
            mean_winter_rm= mean(winter_rmax),
            mean_summer_rm= mean(summer_rmax))
state_temp <- data.frame(state_temp)

write.csv(state_temp, "/Users/jooyounglee/Dropbox/2020_covid19/covid19/Data/state_temp.csv")

