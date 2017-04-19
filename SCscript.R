## Luke A. Miller
## South Carolina: Regional Analysis

library(readxl)
sc <- read_excel("~/Desktop/SCdata/SCdata01.xlsx", 
                       sheet = "postpre data")
View(sc)
library(ggplot2)


##################  HYPOTHESIS I
### BAR PLOTS
ggplot(sc, aes(x=County, y=CH_LABOR)) +
  geom_bar(stat = "identity", fill = 'orange' , alpha = .8) +
  theme(axis.text.x = element_text(angle=60, hjust=1))

ggplot(sc, aes(x=County, y=CH_MANEM)) +
  geom_bar(stat = "identity", fill = 'orange' , alpha = .8) +
  theme(axis.text.x = element_text(angle=60, hjust=1))

### EXPLANATORY VARIABLE CORRELATION
cor.test(sc$CH_FARM, sc$CH_ACRE, use = "complete.obs", method = "pearson")
cor.test(sc$CH_TRACT, sc$CH_ACRE, use = "complete.obs", method = "pearson")
cor.test(sc$CH_FARM, sc$CH_TRACT, use = "complete.obs", method = "pearson")

### CH_LABOR = Man. Employment + Number of Farms + Total Ag. Acreage + Total Tractors (not self-propelled) + u
fit1 <- lm(CH_LABOR~ CH_MANEM + CH_FARM + CH_ACRE + CH_TRACT, sc)
summary(fit1)



##################  HYPOTHESIS II
### BAR PLOTS
ggplot(sc, aes(x=County, y=CH_FARMOP)) +
  geom_bar(stat = "identity", fill = 'springgreen3' , alpha = .8) +
  theme(axis.text.x = element_text(angle=60, hjust=1))

ggplot(sc, aes(x=County, y=CH_GOV)) +
  geom_bar(stat = "identity", fill = 'springgreen3' , alpha = .8) +
  theme(axis.text.x = element_text(angle=60, hjust=1))

### EXPLANATORY VARIABLE CORRELATION
cor.test(sc$CH_FARM, sc$CH_ACRE, use = "complete.obs", method = "pearson")
cor.test(sc$CH_ACRE, sc$CH_GOV, use = "complete.obs", method = "pearson")
cor.test(sc$CH_FARM, sc$CH_GOV, use = "complete.obs", method = "pearson")
cor.test(sc$CH_FARM, sc$CH_FARMOP, use = "complete.obs", method = "pearson")


### REGRESSION: CH_FARMOP = Man. Employment + Number of Farms + Total Ag. Acreage + Gov Ag. Spending + u
fit2 <- lm(CH_FARMOP~ CH_MANEM + CH_FARM + CH_ACRE + CH_GOV, sc)
summary(fit2)







