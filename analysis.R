#### REQUIRED LIBRARIES ####
library(dplyr)
library(ggplot2)

#### CUSTOM FUNCTIONS ####


#### LOAD DATA ####
#load site data
nf4_2011 <- read.csv("../data_by_site/NF4-2011.csv")
nf1_2003 <- read.csv("../data_by_site/NF1-2003.csv")
nf2 <- read.csv("../data_by_site/NF2.csv")
mf1 <- read.csv("../data_by_site/MF1.csv")
mf2 <- read.csv("../data_by_site/MF2.csv")
nf6_2011 <- read.csv("../data_by_site/NF6-2011.csv")
p1 <- read.csv("../data_by_site/P1.csv")
p2 <- read.csv("../data_by_site/P2.csv")
nf3nf5_2008 <- read.csv("../data_by_site/NF3NF5-2008.csv")
nf7ab_2008 <- read.csv("../data_by_site/NF7AB-2008.csv")

#combine into single data object
dat <- rbind(nf4_2011, nf1_2003, nf2, mf1, mf2, nf6_2011, p1, p2, nf3nf5_2008,
             nf7ab_2008)

codes <- read.csv("../data_by_site/CODES.csv")

#### PROCESS DATA ####
#convert date to date format
dat$date <- as.Date(dat$date, format = "%d-%b-%y")
#convert year planted to factor
dat$year_planted <- as.factor(dat$year_planted)
#fix 2 NA values for year in LC NF2
dat$year_planted[is.na(dat$year_planted)] <- 2003


#### ANALYZE DATA ####
raw_counts <-dat %>%
  group_by(transect, year_planted) %>%
  count(date) %>%
  summarise(m_count = mean(n))

ggplot(raw_counts) +
  geom_col(aes(x=year_planted, y = m_count))
