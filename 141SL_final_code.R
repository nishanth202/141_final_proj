library(corrplot)
library(dplyr)
setwd("~/Desktop/CampusClimateProject")
climate = read.csv("campusclimate.csv")

# NA count over 50% NA's
vars_drop = names(which(colMeans(is.na(climate)) > 0.5))

climate_clean = na.omit(climate[,-which(names(climate) %in% vars_drop)])
climate_numeric = climate_clean[, Filter(is.numeric)]

correlation = climate_clean %>% 
		select_if(is.numeric) %>% 
		cor() %>% 
		round(digits=2) %>% 
		as.data.frame()
		
# Threshold correlation change so we can decide desired threshold
THRESHOLD_COR = 0.55
		
correlation[abs(correlation) < THRESHOLD_COR] <- NA
head(correlation)
academics correlation %>% filter_all(any_vars(!is.na(.))) %>% select("academicsp")




