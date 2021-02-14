## Replication 1 Draft 

## Getting Data and Library'2
library(tidyverse)
library(ggplot2)
library(readr)
library(stargazer)
library(dplyr)
library(tidyr)
library(rdd)
library(cobalt)
library(haven)
library(estimatr)
library(rdrobust)


setwd("C:/Users/megan/Documents/GitHub/RDD")
raw_hansen_dwi <- read_csv("Data/hansen_dwi.csv")
head(raw_elections_data)
## Adding dummy
RDDdata <- mutate(raw_hansen_dwi, cutoff = car::Recode(bac1, "lo: 0.08= 0; else = 1"))
## McCrary Density Test (outputted p-value is less than 0.05 then we say 95% sure there is 
## data manipulation at the cutoff)
DCdensity(RDDdata$bac1,0.08,plot=TRUE)
print(DCdensity(RDDdata$bac1, 0.08, plot = FALSE))

library(tidyverse)
library(rdrobust)

rdr <- rdrobust(y = RDDdata$recidivism,
                x = RDDdata$bac1, c = 0.08)
summary(rdr)

## Density Figure

figure1<-ggplot(RDDdata, aes(x=bac1))+
  geom_histogram(aes(y=..density..), colour = "grey", fill = "white", binwidth = 0.001)
figure1 + geom_vline(aes(xintercept = 0.08), color = "black", size = 1) +
  geom_vline(aes(xintercept = 0.15), color = "black", size = 1) +
  labs(title = "BAC Histogram", x = "bac1", y = "Frequency")

## Table 3. wont due clustered id but will work without. 
## Panel A (0.03 to 0.13: 
RDDdata_subset <- RDDdata %>% 
  filter(bac1>0.03 & bac1 < 0.13)

lm_1 <- lm_robust(recidivism ~ bac1, data = RDDdata_subset)
lm_2 <- lm_robust(recidivism ~ bac1*cutoff, data = RDDdata_subset, cluster = id)
lm_3 <- lm_robust(recidivism ~ bac1^2*cutoff, data = RDDdata_subset, cluster = id)

summary(lm_1)
summary(lm_2)
summary(lm_3)

## Panel B 
RDDdata_subset2 <- RDDdata %>% 
  filter(bac1>0.055 & bac1 < 0.105)

lm_4 <- lm_robust(recidivism ~ bac1, data = RDDdata_subset2, clusters = id)
lm_5 <- lm_robust(recidivism ~ bac1*cutoff, data = RDDdata_subset2, cluster = id)
lm_6 <- lm_robust(recidivism ~ bac1^2*cutoff, data = RDDdata_subset2, cluster = id)

summary(lm_4)
summary(lm_5)
summary(lm_6)