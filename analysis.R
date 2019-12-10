#### REQUIRED LIBRARIES ####
library(dplyr)
library(ggplot2)
library(lme4)

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
#nf7ab_2008 <- read.csv("../data_by_site/NF7AB-2008.csv")

#combine into single data object
dat <- rbind(nf4_2011, nf1_2003, nf2, mf1, mf2, nf6_2011, p1, p2, nf3nf5_2008)

codes <- read.csv("../data_by_site/CODES.csv")

#### PROCESS DATA ####
#convert date to date format
dat$date <- as.Date(dat$date, format = "%d-%b-%y")
#convert year planted to factor
dat$year_planted <- as.factor(dat$year_planted) 
#fix 2 NA values for year in LC NF2
dat$year_planted[is.na(dat$year_planted)] <- "2003"
levels(dat$year_planted) <- c("0", "2011", "2008", "2003", "old")


dat_close <- dat[dat$dist == "<25",]

dat_join <- dat_close %>%
  inner_join(., codes, by = "species")

foo <- Vectorize(vectorize.args = "x",
                               FUN = function(x) {
                                 switch(as.character(x), 
                            "altitudinal" = "mig",
                            "long-distance" = "mig",
                            "short-distance" = "mig",
                            "nomadic" = "nomadic",
                            "resident" = "res",
                            "NA" = NA)})

dat_join$mig_code <- unlist(foo(x = dat_join$mig_guild))

mod1_counts <- dat_join %>%
  group_by(transect, year_planted) %>%
  count(date)

f0 <- lmer(n ~ 1 + (1|date), data = mod1_counts)
f1 <- lmer(n ~ year_planted + (1|date), data = mod1_counts)
summary(f0)
summary(f1)

AIC(f0)
AIC(f1)

#### ANALYZE DATA ####
#break down mean counts per mig guild
mig_counts <-dat_join %>%
  group_by(transect, year_planted, mig_code) %>%
  count(date) %>%
  summarise(m_count = mean(n), se = sd(n)/length(n))

#break down mean counts per mig guild
guild_counts <-dat_join %>%
  group_by(transect, year_planted, feed_guild.y) %>%
  count(date) %>%
  summarise(m_count = mean(n), se = sd(n)/length(n))

#break down mean counts per mig guild
iucn_counts <-dat_join %>%
  group_by(transect, year_planted, IUCN) %>%
  count(date) %>%
  summarise(m_count = mean(n), se = sd(n)/length(n))

#mean counts per transect
tot_counts <-dat_join %>%
  group_by(transect, year_planted) %>%
  count(date) %>%
  summarise(m_count = mean(n), se = sd(n)/length(n))

ggplot(mig_counts) +
  geom_bar(aes(x=year_planted, fill = mig_code), position = "fill")

ggplot(guild_counts) +
  geom_bar(aes(x=year_planted, fill = feed_guild.y), position = "fill")

ggplot(iucn_counts) +
  geom_bar(aes(x=year_planted, fill = IUCN), position = "fill")

ggplot(tot_counts) +
  geom_pointrange(aes(x = year_planted, y = m_count, 
                      ymin = m_count-se, ymax = m_count+se))
