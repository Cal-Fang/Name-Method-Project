# Clear out the history
rm(list=ls())

# Read in the packages needed
library(Matrix)
library(tidyverse)
library(ggplot2)

# Set working directory
setwd("~/Box Sync/Name Identification Project/US Names")    # Please change this to your path

# STEP 1
# Load in the Rdata file of reportable results
load(file="data/ReportResult.Rdata")
# Read in and modify the CENSUS
census100 <- read_csv("data/Names_2010Census.csv") 
census100[, 2:11] <- sapply(census100[, 2:11], as.numeric)
census100_asian <- census100 %>% 
  filter((pctapi >= pctwhite) & 
           (pctapi >= pctblack) & 
           (pctapi >= pctaian) & 
           (pctapi >= pcthispanic)) %>% 
  filter(name != "DELEONGUERRERO")    # Since this is not recorded, over 12 digits and probably a Spanish-origin name [not Asian]

# STEP 2
# Check the inclusion
test_df <- census100_asian %>% 
  mutate(name = ifelse((nchar(name) <= 12), 
                       str_pad(name, 12, side="right", pad=" "), 
                       str_trunc(name, 12, ellipsis=""))) %>% 
  merge(panasian_df, by.x="name", by.y="surname", all.x=TRUE)

sum(!is.na(test_df$asian)) / nrow(test_df)
# 85.2% names covered

# What about the other 14.8% names?
noton <- filter(test_df, is.na(test_df$asian))

# All these names were originally in our dataset
noton1 <- noton[!(noton$name %in% fb_terr_ntv$surname), ]
# 2 names were omitted because the total frequency in our dataset is not larger than 50
noton2 <- noton[(noton$name %in% fb_terr_ntv$surname) & !(noton$name %in% fb_terr_ntv2$surname), ]
# 268 names were omitted because the total 19 Asian countries foreign born frequency is not larger than 10
noton3 <- noton[(noton$name %in% fb_terr_ntv2$surname) & !(noton$name %in% fb_terr_ntv3$surname), ]
# 94 names were omitted because the most foreign born were from outside asia
noton4 <- noton[(noton$name %in% fb_terr_ntv3$surname) & !(noton$name %in% fb_terr_ntv4$surname), ]
# 1 name was omitted because the foreign born rate were smaller than 5%
noton5 <- noton[(noton$name %in% fb_terr_ntv4$surname) & !(noton$name %in% fb_terr_ntv5$surname), ]

# STEP 3
# Check the correlation of the proportion and probability
test_df <- test_df %>% 
  mutate(pctapi = as.numeric(pctapi)) %>% 
  filter(!is.na(asian))

cor.test(test_df$asian, test_df$pctapi, method="pearson")
cor.test(test_df$asian, test_df$pctapi, method="kendall")
# strong and statistically significant correlation

# Check the correlation of the ranks
pctapi_rank <- rank(test_df$pctapi, ties.method="min")
asian_rank <- rank(test_df$asian, ties.method="min")

cor.test(pctapi_rank, asian_rank, method="pearson")
# medium and statistically significant correlation

# STEP 4
test_df <- test_df %>% 
  merge(determine_df, by.x="name", by.y="surname", all.x=TRUE) 

test_df2 <- test_df %>% 
  group_by(maxcntr) %>% 
  filter(n() > 1)

table(test_df2$maxcntr)

ggplot(test_df2, aes(asian, pctapi/100, color=maxcntr, linetype=maxcntr)) +
  geom_point() + 
  geom_smooth(method="glm", 
              method.args=list(family="binomial"), 
              se = FALSE, fullrange = TRUE, alpha = .15)

model1 <- glm(pctapi/100 ~ asian + maxcntr, data=test_df2, family="binomial")
summary(model1)


load(file="data/OriginalData.Rdata")

test_df[test_df$name == "FANG        ",]
fb_terr_ntv[fb_terr_ntv$surname == "PARK        ",]
fb_terr_ntv[fb_terr_ntv$surname == "KIM         ",]

t.test(test_df2$pctapi/100, test_df2$asian)



# Merge us_prop
colnames(fb_terr_ntv5) <- c("name", "freq_fb", "freq_terr", "freq_ntv", "sum_freq", "fb_prop")
test_df <- test_df %>%
  merge(fb_terr_ntv5)

test_df$diff <- test_df$asian - (test_df$pctapi / 100)

