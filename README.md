## AnalyzeHct-Mch
This standard, easily readable format of R code is capable of performing ANOVA, sinlge linear regression, producing a box-and-whisker plot along with a scatter plot with regression line. This code also identifies subject IDs with abnormal lab values according to values provided by cited reference. 

Chernecky CC, Berger BJ. Blood indices – blood. In: Chernecky CC, Berger BJ, eds. Laboratory Tests and Diagnostic Procedures. 6th ed. Philadelphia, PA: Elsevier; 2013:217-219.

Data (RobinsonEtAl_Sup1.csv) was downloaded from: 
Robinson, JM. et al. 2019. Complete blood count with differential: An effective diagnostic for IBS subtype in the context of BMI? BioRxiv. doi: https://doi.org/10.1101/608208.


##  Single Regressions 
##  Data obtained from Robinson, et al. 2019 (doi: https://doi.org/10.1101/608208)
##  https://statquest.org/2017/10/30/statquest-multiple-regression-in-r/
##  http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/
##  http://r-statistics.co/Linear-Regression.html

## Single Regression Test, BMI vs. Bloodwork parameter
HCT.regression <- lm(BMI ~ HCT, data = IBS)
summary(HCT.regression)

MCH.regression <- lm(BMI ~ MCH, data = IBS)
summary(MCH.regression)

## Output the results to a file
## http://www.cookbook-r.com/Data_input_and_output/Writing_text_and_output_from_analyses_to_a_file/
sink('data_output/MCH_regression.txt', append = TRUE)
print(MCH.regression)
sink()

sink('data_output/HCT_regression.txt', append = TRUE)
print(HCT.regression)
sink()

## ANOVA: IBS-subtype vs. Bloodwork parameter
## http://www.sthda.com/english/wiki/one-way-anova-test-in-r
MCH.aov <- aov(MCH ~ IBS.subtype, data = IBS)
summary(MCH.aov)
sink('data_output/MCH_anova.txt', append = TRUE)
print(MCH.aov)
sink()

HCT.aov <- aov(HCT ~ IBS.subtype, data = IBS)
summary(HCT.aov)
sink('data_output/HCT_anova.txt', append = TRUE)
print(HCT.aov)
sink()


## Print scatterplot and box plots as .png files into "fig_output" project directory.
## http://www.sthda.com/english/wiki/ggsave-save-a-ggplot-r-software-and-data-visualization

## Scatterplots
## https://www.statmethods.net/graphs/scatterplot.html

ggplot(IBS, aes(x = BMI, y = MCH)) +
  geom_point() +    
  geom_smooth(method = lm) 

ggplot(IBS, aes(x = BMI, y = HCT)) +
  geom_point() +    
  geom_smooth(method = lm) 

png("fig_output/MCH_scatterplot.png")
MCH_scatterplot <- ggplot(IBS, aes(x = BMI, y = MCH)) +
  geom_point() +    
  geom_smooth(method = lm)
print(MCH_scatterplot)
dev.off()


![BMI_MCH](../AnalyzeHct-Mch/Images/MCH_scatterplot.png?sanitize=true)

png("fig_output/HCT_scatterplot.png")
HCT_scatterplot <- ggplot(IBS, aes(x = BMI, y = HCT)) +
  geom_point() +    
  geom_smooth(method = lm)
print(HCT_scatterplot)
dev.off()


![](Images/HCT_scatterplot.png?sanitize=true)

## Box plots
## https://www.statmethods.net/graphs/boxplot.html

boxplot(HCT ~ IBS.subtype, data = IBS, main="HCT by IBS subtype", 
                       xlab = "IBS.subtype", ylab = "HCT"
)

png("fig_output/HCT_boxplot.png")
HCT_boxplot <- boxplot(HCT ~ IBS.subtype, data = IBS, main="HCT by IBS subtype", 
                       xlab = "IBS.subtype", ylab = "HCT"
)
print(HCT_boxplot)
dev.off()

![HCT Boxplot](../AnalyzeHct-Mch/images/HCT_boxplot.png?sanitize=true)

boxplot(MCH ~ IBS.subtype, data = IBS, main="MCH by IBS subtype", 
        xlab = "IBS.subtype", ylab = "MCH"
)

boxplot(HCT ~ IBS.subtype, data = IBS, main="HCT by IBS subtype", 
                       xlab = "IBS.subtype", ylab = "HCT"
)

png("fig_output/MCH_boxplot.png")
MCH_boxplot <- 
print(MCH_boxplot)
dev.off()

![MCH Boxplot](../AnalyzeHct-Mch/images/Rplot.png?sanitize=true)

boxplot(MCH ~ IBS.subtype, data = IBS, main="MCH by IBS subtype", xlab = "IBS.subtype", ylab = "MCH"
)

## Identification of Values outside of range

library(dplyr
      )
labels <- c("low", "in range", "high")
IBS %>% mutate(
  MCH_bucket = cut(MCH, c(-Inf, 27 - .Machine$double.eps, 33, Inf), labels),
  HCT_bucket = cut(HCT, c(-Inf, 37 - .Machine$double.eps, 42, Inf), labels)
)

IBS %>% mutate(
  MCH_bucket = if_else(MCH < 27, "low", if_else(MCH > 33, "high", "in range")),
  HCT_bucket = if_else(HCT < 37, "low", if_else(HCT > 42, "high", "in range"))
)

dput(head(IBS)) 
labels <- c("low", "in range", "high")
IBS %>% mutate (
  MCH_bucket = cut(MCH), c(-Inf, 27 - .Machine$double.eps, 33, Inf), (labels),
  HCT_bucket = cut(HCT), c(-Inf, 37 - .Machine$double.eps, 42, Inf), (labels)
)

IBS %>% mutate (
  MCH_bucket = if_else(MCH < 27, "low", if_else(MCH > 33, "high", "in range")),
  HCT_bucket = if_else(HCT < 37, "low", if_else(HCT > 42, "high", "in range"))
)

##Linking abnormal values to Pt ID

IBS %>% mutate(
  MCH_bucket = if_else(MCH < 27, "low", if_else(MCH > 33, "high", "in range")),
) %>%
  group_by(MCH_bucket) %>% 
  summarise(IDS=toString(ID))

IBS %>% mutate(
  MCH_bucket = if_else(MCH < 27, "low", if_else(MCH > 33, "high", "in range"))
) %>%
  group_by(MCH_bucket) %>% 
  summarise(IDS=toString(ID))

IBS %>% mutate(
  HCT_bucket = if_else(HCT < 37, "low", if_else(HCT > 42, "high", "in range"))
) %>%
  group_by(HCT_bucket) %>% 
  summarise(IDS=toString(ID))

summarise(IDS=toString(ID))
IBS %>% head()
