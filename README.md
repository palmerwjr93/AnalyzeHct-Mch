## AnalyzeHct-Mch
   This standard, easily readable format of R code is capable of performing ANOVA, sinlge linear regression, producing a box and-whisker plot along with a scatter plot with regression line. This code also identifies subject IDs with abnormal lab values according to values provided by Chernecky CC, Berger BJ. Blood indices – blood. In: Chernecky CC, Berger BJ, eds. Laboratory Tests and Diagnostic Procedures. 6th ed. Philadelphia, PA: Elsevier; 2013:217-219.(RobinsonEtAl_Sup1.csv) was downloaded from: Robinson, JM. et al. 2019. Complete blood count with differential: An effective diagnostic for IBS subtype in the context of BMI? BioRxiv. doi: https://doi.org/10.1101/608208.

## Single Regression Test, BMI vs. Bloodwork parameter (Hct-Mch)

          HCT.regression <- lm(BMI ~ HCT, data = IBS)
           summary(HCT.regression)
          MCH.regression <- lm(BMI ~ MCH, data = IBS)
          summary(MCH.regression)

## ANOVA: IBS-subtype vs. Hct-Mch
## (http://www.sthda.com/english/wiki/one-way-anova-test-in-r)

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

## Scatterplot of Hct-Mch
## (https://www.statmethods.net/graphs/scatterplot.html)

          ggplot(IBS, aes(x = BMI, y = MCH)) +
            geom_point() +    
            geom_smooth(method = lm) 
          png("fig_output/MCH_scatterplot.png")
          MCH_scatterplot <- ggplot(IBS, aes(x = BMI, y = MCH)) +
            geom_point() +    
            geom_smooth(method = lm)
          print(MCH_scatterplot)
          dev.off()
![](Images/MCH_scatterplot.png?sanitize=true)

          ggplot(IBS, aes(x = BMI, y = HCT)) +
            geom_point() +    
            geom_smooth(method = lm) 
          png("fig_output/HCT_scatterplot.png")
          HCT_scatterplot <- ggplot(IBS, aes(x = BMI, y = HCT)) +
            geom_point() +    
            geom_smooth(method = lm)
          print(HCT_scatterplot)
          dev.off()
![](Images/HCT_scatterplot.png?sanitize=true)


## Boxplot of Hct-Mch
## (https://www.statmethods.net/graphs/boxplot.html)

          boxplot(HCT ~ IBS.subtype, data = IBS, main="HCT by IBS subtype", 
                                 xlab = "IBS.subtype", ylab = "HCT"
          )
          png("fig_output/HCT_boxplot.png")
          HCT_boxplot <- boxplot(HCT ~ IBS.subtype, data = IBS, main="HCT by IBS subtype", 
                                 xlab = "IBS.subtype", ylab = "HCT"
          )
          print(HCT_boxplot)
          dev.off()
![HCT](Images/HCT_boxplot.png?sanitize=true)

          boxplot(MCH ~ IBS.subtype, data = IBS, main="MCH by IBS subtype", 
                  xlab = "IBS.subtype", ylab = "MCH"
          )
          png("fig_output/MCH_boxplot.png")
          MCH_boxplot <- boxplot(MCH ~ IBS.subtype, data = IBS, main="MCH by IBS subtype", 
                  xlab = "IBS.subtype", ylab = "MCH"
          )
          print(MCH_boxplot)
          dev.off()
![MCH](Images/Rplot.png?sanitize=true)

## Identification of Values outside of range (Run for results)

          library(dplyr)
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

## Linking of Abnormal Values to Pt ID (Run for results)

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

