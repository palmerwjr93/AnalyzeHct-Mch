##BTEC330 Warren Project2

## Install necessary packages

install.packages("ggplot2")
library(ggplot2)


## Read data
IBS1 <- read.csv("data/RobinsonEtAl_Sup1.csv", header = TRUE)
head(IBS1)
write.csv(IBS1, "data_output/output.csv")

##  Single Regressions for BMI vs. Hct/Mch
##  Data was obtained from Robinson, et al. 2019 (doi: https://doi.org/10.1101/608208)
##  https://statquest.org/2017/10/30/statquest-multiple-regression-in-r/
##  http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/
##  http://r-statistics.co/Linear-Regression.html

##Single Regression Test
single.regression <- lm(BMI ~ HCT, data=IBS1)
summary(single.regression)

single.regression <- lm(BMI ~ MCH, data=IBS1)
summary(single.regression)


## Scatterplots
## https://www.statmethods.net/graphs/scatterplot.html


ggplot(IBS1, aes(x=BMI, y=HCT)) +
  geom_point() +    
  geom_smooth(method=lm) 

ggplot(IBS1, aes(x=BMI, y=MCH)) +
  geom_point() +    
  geom_smooth(method=lm)  


## 3D scatterplot for the most significant 3-variable multiple regression model
## http://www.sthda.com/english/wiki/scatterplot3d-3d-graphics-r-software-and-data-visualization

s3d <- scatterplot3d(IBS$BMI, IBS$SerumCortisol, IBS$CRP,  pch=16, color="steelblue", box="TRUE", highlight.3d=FALSE, type="h", main="BMI x Cortisol x CRP")
fit <- lm(SerumCortisol ~ BMI + CRP, data=IBS)
