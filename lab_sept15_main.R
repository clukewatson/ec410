# clear workspace
rm(list=ls())

### Students Read Below

## install necessary packages
# You have probably already installed this one, so do not need to redo it
# install.packages("tidyverse", dependencies = T)

# New Packages to Install
install.packages("devtools", dependencies = T)
library(devtools)
install_github("WIDworld/wid-r-tool")

install.packages("wbstats", dependencies = T)
install.packages("fixest", dependencies = T)
install.packages("countrycode", dependencies = T)

# load libraries

library(tidyverse)
library(wbstats)
library(wid)
library(countrycode)
library(fixest)

############################################################################################################

# Set working directory
setwd("/Users/lukewatson/Dropbox/03_TEACHING/TEACHING/Development/class/ec410/labs/sept15")

############################################################################################################

# Full list of countries by year
country.df <- codelist_panel
country.df <- country.df[ country.df$year >=1950 ,c("country.name.en","iso3c","iso2c","year")] %>%
  rename(iso3=iso3c,iso2=iso2c)

# Download World Bank Data
# wb.vars <- wb_search(pattern = "poverty")

wb.df <- wbstats::wb(indicator = c("SP.DYN.LE00.IN","SE.PRM.UNER.ZS","SE.ADT.LITR.FE.ZS",
                                   "SP.DYN.TFRT.IN","SP.POP.TOTL","BAR.NOED.15UP.ZS",
                                   "BAR.SCHL.15UP.FE","NY.GNP.PCAP.KN","NY.GNP.PCAP.PP.KD","NY.GNP.MKTP.KN",
                                   "PA.NUS.PPP","PA.NUS.PRVT.PP"), 
                     return_wide=T) 

write_csv(wb.df, "wb_data.csv")
# wb.df <- read_csv("wb_data.csv")

wb.df <- wb.df %>%
  rename(iso3 =iso3c,
         pct.life.expect    =SP.DYN.LE00.IN,
         pct.kid.not.in.sch =SE.PRM.UNER.ZS,
         pct.litr.women     =SE.ADT.LITR.FE.ZS,
         rate.fertility     =SP.DYN.TFRT.IN,
         tot.pop            =SP.POP.TOTL,
         pct.no.edu.all     =BAR.NOED.15UP.ZS,
         avg.edu.women      =BAR.SCHL.15UP.FE,
         gni.pc.lc          =NY.GNP.PCAP.KN,
         gni.pc.ppp17       =NY.GNP.PCAP.PP.KD,
         gni.lvl.lc         =NY.GNP.MKTP.KN,
         ppp.factor.1         =PA.NUS.PPP,
         ppp.factor.2         =PA.NUS.PRVT.PP) %>%
  group_by(iso3) %>%
  arrange(date) %>%
  ungroup() %>%
  mutate(year = as.numeric(date)) %>%
  right_join(country.df, by=c("year","iso3"))

wb.ppp.17 <- wb.df[wb.df$year==2017,c("iso3","ppp.factor.1","ppp.factor.2")] %>%
  rename(ppp.f1.17 = ppp.factor.1,
         ppp.f2.17 = ppp.factor.2,)

wb.df <- left_join(wb.df,wb.ppp.17,by="iso3")


# Let's Try to Replicate the GNI Per Capita PPP data
wb.df <- wb.df %>%
  mutate(get.pop = (gni.lvl.lc/gni.pc.lc),
         get.ppp =  (gni.pc.lc/gni.pc.ppp17),
         rep1.gni.pc.ppp = (gni.lvl.lc/tot.pop)/ppp.f1.17,
         rep2.gni.pc.ppp = (gni.lvl.lc/tot.pop)/ppp.f2.17,
         rep3.gni.pc.ppp = (gni.lvl.lc/tot.pop)/ppp.factor.1,
         rep4.gni.pc.ppp = (gni.lvl.lc/tot.pop)/ppp.factor.2
  ) 

wb.chk.df <- wb.df[wb.df$year>2010,
                   c("iso3","year",
                     "gni.pc.ppp17",
                     "rep1.gni.pc.ppp","rep2.gni.pc.ppp","rep3.gni.pc.ppp","rep4.gni.pc.ppp",
                     "get.pop","tot.pop","get.ppp","ppp.factor.1","ppp.factor.2","ppp.f1.17","ppp.f2.17")]

qplot(gni.pc.ppp17,
      rep1.gni.pc.ppp,
      data=wb.chk.df[wb.chk.df$year==2017,]) +
  geom_smooth() +
  geom_smooth(method='lm',color = "red") +
  geom_abline(color = "yellow")  # adds x = y (45-degree) line

qplot(gni.pc.ppp17,
      rep2.gni.pc.ppp,
      data=wb.chk.df[wb.chk.df$year==2017,]) +
  geom_smooth() +
  geom_smooth(method='lm',color = "red") +
  geom_abline(color = "yellow")  # adds x = y (45-degree) line

qplot(gni.pc.ppp17,
      rep3.gni.pc.ppp,
      data=wb.chk.df[wb.chk.df$year==2017,]) +
  geom_smooth() +
  geom_smooth(method='lm',color = "red") +
  geom_abline(color = "yellow")  # adds x = y (45-degree) line

qplot(gni.pc.ppp17,
      rep4.gni.pc.ppp,
      data=wb.chk.df[wb.chk.df$year==2017,]) +
  geom_smooth() +
  geom_smooth(method='lm',color = "red") +
  geom_abline(color = "yellow")  # adds x = y (45-degree) line

est1 <- feols(gni.pc.ppp17 ~   rep1.gni.pc.ppp,
              wb.chk.df[wb.chk.df$year==2017,],  verbose=1)
est2 <- feols(gni.pc.ppp17 ~    rep2.gni.pc.ppp,
              wb.chk.df[wb.chk.df$year==2017,],  verbose=1)
est3 <- feols(gni.pc.ppp17 ~    rep3.gni.pc.ppp,
              wb.chk.df[wb.chk.df$year==2017,], verbose=1)
est4 <- feols(gni.pc.ppp17 ~    rep4.gni.pc.ppp,
              wb.chk.df[wb.chk.df$year==2017,], verbose=1)
etable(est1, est2,est3,est4, drop="Int", se="white")

est1 <- feols(gni.pc.ppp17 ~   rep1.gni.pc.ppp | year + iso3,
              wb.chk.df,  verbose=1)
est2 <- feols(gni.pc.ppp17 ~    rep2.gni.pc.ppp | year + iso3,
              wb.chk.df,  verbose=1)
est3 <- feols(gni.pc.ppp17 ~    rep3.gni.pc.ppp | year + iso3,
              wb.chk.df,  verbose=1)
est4 <- feols(gni.pc.ppp17 ~    rep4.gni.pc.ppp | year + iso3,
              wb.chk.df,  verbose=1)
etable(est1, est2,est3,est4, drop="Int", se="white")

rm(est1,est2,est3,est4)
############################################################################################################

# Make Data
tmp.df.1 <- wb.df[wb.df$year==1970,c("iso3","rep1.gni.pc.ppp","pct.life.expect","tot.pop")]
colnames(tmp.df.1) <- c("iso3","gni.70","pct.le.70","pop.70")
tmp.df.2 <- wb.df[wb.df$year==2017,c("iso3","rep1.gni.pc.ppp","pct.life.expect")]
colnames(tmp.df.2) <- c("iso3","gni.17","pct.le.17")
tmp.df.3 <- wb.df[wb.df$year==2010,c("iso3","rep1.gni.pc.ppp","avg.edu.women","pct.life.expect","tot.pop")]
colnames(tmp.df.3) <- c("iso3","gni.10","edu.wo.10","pct.le.10","pop.10")
tmp.df.4 <- wb.df[wb.df$year==1980,c("iso3","rep1.gni.pc.ppp","tot.pop","pct.life.expect")]
colnames(tmp.df.4) <- c("iso3","gni.80","pop.80","pct.le.80")
tmp.df.5 <- wb.df[wb.df$year==1990,c("iso3","rep1.gni.pc.ppp","tot.pop","pct.life.expect")]
colnames(tmp.df.5) <- c("iso3","gni.90","pop.90","pct.le.90")

reg.df <- full_join(tmp.df.1,tmp.df.2,by="iso3")
reg.df <- full_join(reg.df,tmp.df.3,by="iso3")
reg.df <- full_join(reg.df,tmp.df.4,by="iso3")
reg.df <- full_join(reg.df,tmp.df.5,by="iso3")
reg.df <- reg.df[is.na(reg.df$iso3)==0,]

reg.df$g.pop.80 <- log(reg.df$pop.80)-log(reg.df$pop.70)
reg.df$g.pop.90 <- log(reg.df$pop.90)-log(reg.df$pop.70)
reg.df$g.pop.10 <- log(reg.df$pop.10)-log(reg.df$pop.90)

reg.df$g.gni.80 <- log(reg.df$gni.80)-log(reg.df$gni.70)
reg.df$g.gni.90 <- log(reg.df$gni.90)-log(reg.df$gni.70)
reg.df$g.gni.10 <- log(reg.df$gni.10)-log(reg.df$gni.90)

# Outliers
reg.df$very.low <- reg.df$iso3 %in% c("SOM","SDN","HTI")

# 
qplot(gni.70,pct.le.17,data = reg.df) +
  geom_smooth() +
  geom_smooth(method='lm',color = "red")

qplot(log(gni.70),pct.le.17,data = reg.df) +
  geom_smooth() +
  geom_smooth(method='lm',color = "red")

qplot(log(gni.70),pct.le.17,data = reg.df[reg.df$very.low==F,]) +
  geom_smooth() +
  geom_smooth(method='lm',color = "red")

qplot(gni.90,pct.le.17,data = reg.df) +
  geom_smooth() +
  geom_smooth(method='lm',color = "red")

qplot(log(gni.90),pct.le.17,data = reg.df) +
  geom_smooth() +
  geom_smooth(method='lm',color = "red")


# Regression
est1 <- feols(pct.le.17 ~   log(gni.90),
              reg.df,  verbose=1)
est2 <- feols(pct.le.17 ~   log(gni.90) + pct.le.90,
              reg.df,  verbose=1)
etable(est1, est2, drop="Int", se="white")

est3 <- feols(pct.le.17 ~   log(gni.90),
              reg.df, weights = ~pop.90, verbose=1)
est4 <- feols(pct.le.17 ~   log(gni.90) + pct.le.90,
              reg.df, weights = ~pop.90, verbose=1)
etable(est1, est2,est3,est4, drop="Int", se="white")

est5 <- feols(pct.le.17 ~   log(gni.90),
              reg.df[reg.df$very.low==F,], weights = ~pop.90, verbose=1)
est6 <- feols(pct.le.17 ~   log(gni.90) + pct.le.90,
              reg.df[reg.df$very.low==F,], weights = ~pop.90, verbose=1)
etable(est3,est4,est5,est6, drop="Int", se="white")

# Regression 
est1 <- feols(edu.wo.10 ~   log(gni.90),
              reg.df,  verbose=1)
est2 <- feols(edu.wo.10 ~   log(gni.90) + pct.le.90,
              reg.df,  verbose=1)
etable(est1, est2, drop="Int", se="white")

est3 <- feols(edu.wo.10 ~   log(gni.90),
              reg.df, weights = ~pop.90, verbose=1)
est4 <- feols(edu.wo.10 ~   log(gni.90) + pct.le.90,
              reg.df, weights = ~pop.90, verbose=1)
etable(est1, est2,est3,est4, drop="Int", se="white")

est5 <- feols(edu.wo.10 ~   log(gni.90),
              reg.df[reg.df$very.low==F,], weights = ~pop.90, verbose=1)
est6 <- feols(edu.wo.10 ~   log(gni.90) + pct.le.90,
              reg.df[reg.df$very.low==F,], weights = ~pop.90, verbose=1)
etable(est3,est4,est5,est6, drop="Int", se="white")

# Growth
est1 <- feols(g.pop.10 ~   g.gni.80,
              reg.df,  verbose=1)
est2 <- feols(g.pop.10 ~  g.gni.90,
              reg.df,  verbose=1)
est3 <- feols(g.pop.10 ~  g.gni.80 + g.gni.90,
              reg.df,  verbose=1)
etable(est1, est2,est3, drop="Int", se="white")

#
est1 <- feols(pct.le.17 ~   g.gni.90,
              reg.df,  verbose=1)
est2 <- feols(pct.le.17 ~  g.gni.10,
              reg.df,  verbose=1)
est3 <- feols(pct.le.17 ~  g.gni.10 + g.pop.10,
              reg.df,  verbose=1)
etable(est1, est2,est3, drop="Int", se="white")

#
est1 <- feols(edu.wo.10 ~   g.gni.90,
              reg.df,  verbose=1)
est2 <- feols(edu.wo.10 ~  g.gni.10,
              reg.df,  verbose=1)
est3 <- feols(edu.wo.10 ~  g.gni.10 + g.pop.10,
              reg.df,  verbose=1)
etable(est1, est2,est3, drop="Int", se="white")

