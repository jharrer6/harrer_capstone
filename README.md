---
title: "Capstone"
author: "Julia Harrer"
date: "4/28/2020"
output: html_document
---

**Step 1: Lay out the big picture of the problem in a way that leads to a “What is not yet known” assertion.**

Bone defects due to traumatic injury present a significant clinical burden. In the United States alone, the annual rate of bone fracture ranges from 12-15 million, with up to 10% of those cases resulting in non-union, depending on the anatomical site (Pollack & Watkins-Castillo, 2014, Tzioupis & Giannoudis, 2007). Effects of non-union include multiple revision surgeries, prolonged disability, and chronic pain. These debilitating effects underscore the need to develop other methods of treatment for critically-sized, segmental bone defects. Bone morphogenic proteins (BMPs) are used in clinics and are known to be effective, but are very expensive. FK506 is an FDA approved immunomodulatory compound that has been shown to have neurotrophic and osteogenic effects, and is not as expensive. However, it is not known whether the osteogenic effects are enough to aid in fracture healing. 


**Step 2: Transform the “What is not known” statement into a bold and simple scientific prediction, as if “what is not known” were answered:**

Localized delivery of FK506 following an injury resulting in a bone defect will aid in the bone regeneration process. 


**Step 3: Now frame the experimental plan in terms of the independent and dependent variables, written as an if/then statement. In narrative format, if you manipulate what predictor variables, then what outcome do you expect to observe?**

If FK506 is delivered locally to the bone defect region, then the strength of the bone, measured by bone stiffness, at end of healing will not differ from mice treated with BMP-2.  


**Step 4: Define the dependent and the independent variables of the experiment. What type of variables are these? What are the experimental units? Are the measurements intrinsically-linked, or not?**

The dependent variable is bone stiffness (measured), calculated as the slope of the linear portion of the torque vs displacement (in degrees) curve.  The independent variable is the treatment of the limb (sorted). Each rodent with a critically-sized, 8mm bone defect is the experimental unit. Treatment is a discrete, factoral variable that will be at three levels, BMP-2, FK506, and no treatment (negative control, note below).

Note: Bone defects  in this study are critically sized, meaning if there is no treatment, bone will not heal and will result in a nonunion. This means we cannot test the strength of the bone as there is no bone to test, therefore stiffness is 0.


**Step 5: Write the null and alternate hypothesis on the basis of the statistical parameters to be tested. Note here that greek notation is used to symbolize that the hypothesis is about the sampled population parameters, rather than the sample.**

Where μ represents bone strength of the populations corresponding to the sampled groups, the null and alternate hypotheses are:
$H_0: \mu_{FK506} \neq \mu_{BMP-2}$
$H_A: \mu_{FK506} = \mu_{BMP-2}$


**Step 6: What statistical test will be used to test the null hypothesis? What are the decision rules?**

A one-way analysis of variance (ANOVA) will be used to compare group means since there is one dependent variable (stiffness). Sample size will be based upon a power of 90%, which means that the tolerance level for type2 error will be 10%. The decision threshold for type1 error will be 5%. Thus, the null hypothesis will be rejected at a p-value of less than 0.05. Post-hoc testing will done using a pairwise t-test with a Tukey adjustment method since it assumes equal variance and is less conservative and won't reduce power.
Bonferroni adjustment method since it assumes equal variance and is meant for smaller sample sizes.


**Step 7: List the procedures and decision rules for executing and interpreting the experiment.**

Each rodent with a critically-sized, 8mm bone defect is the experimental unit. Randomization was based on a block randomization method so all groups had 6 samples per group, but rodents were randomly assigned to each group. The primary endpoint in this study is the stiffness of the bone. Threshold value is within 5%, so to claim that treatment with FK506 is equivalent to BMP the values need to be within 5%. Each rodent in a group is an independent replicate since they all recieve treatments and are measured independently of each other.  Therefore each group has 6 independent replicates.


**Step 8: Graph a simulation of the expected results.**

```{r message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE}
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)
library(cowplot)
library(broom)
library(ncar)
library(ez)

bonetorsion <- read_excel("Capstone/bonetorsion.xlsx", sheet=1)   #load the dataset
#change columns into numeric and create a new dataframe
r <- as.numeric(bonetorsion$Rotation)
B1 <- as.numeric(bonetorsion$BMP_1)
B2 <- as.numeric(bonetorsion$BMP_2)
B3 <- as.numeric(bonetorsion$BMP_3)
B4 <- as.numeric(bonetorsion$BMP_4)
B5 <- as.numeric(bonetorsion$BMP_5)
B6 <- as.numeric(bonetorsion$BMP_6)
F1 <- as.numeric(bonetorsion$FK506_1)
F2 <- as.numeric(bonetorsion$FK506_2)
F3 <- as.numeric(bonetorsion$FK506_3)
F4 <- as.numeric(bonetorsion$FK506_4)
F5 <- as.numeric(bonetorsion$FK506_5)
F6 <- as.numeric(bonetorsion$FK506_6)
E1 <- as.numeric(bonetorsion$Empty_1)
E2 <- as.numeric(bonetorsion$Empty_2)
E3 <- as.numeric(bonetorsion$Empty_3)
E4 <- as.numeric(bonetorsion$Empty_4)
E5 <- as.numeric(bonetorsion$Empty_5)
E6 <- as.numeric(bonetorsion$Empty_6)
df <- data.frame(r,B1, B2, B3, B4, B5, B6, F1, F2, F3, F4, F5, F6, E1, E2, E3, E4, E5, E6) 

#plot Torque vs Rotation
p1 <- ggplot(df, aes(r, B1)) + geom_point()
p2 <- ggplot(df, aes(r, B2)) + geom_point()
p3 <- ggplot(df, aes(r, B3)) + geom_point()
p4 <- ggplot(df, aes(r, B4)) + geom_point()
p5 <- ggplot(df, aes(r, B5)) + geom_point()
p6 <- ggplot(df, aes(r, B6)) + geom_point()

#BMP plots
(plot_grid(p1, p2, p3, p4, p5, p6))

p7 <- ggplot(df, aes(r, F1)) + geom_point()
p8 <- ggplot(df, aes(r, F2)) + geom_point()
p9 <- ggplot(df, aes(r, F3)) + geom_point()
p10 <- ggplot(df, aes(r, F4)) + geom_point()
p11 <- ggplot(df, aes(r, F5)) + geom_point()
p12 <- ggplot(df, aes(r, F6)) + geom_point()

#FK506 plots
(plot_grid(p7, p8, p9, p10, p11, p12))

p13 <- ggplot(df, aes(r, E1)) + geom_point()
p14 <- ggplot(df, aes(r, E2)) + geom_point()
p15 <- ggplot(df, aes(r, E3)) + geom_point()
p16 <- ggplot(df, aes(r, E4)) + geom_point()
p17 <- ggplot(df, aes(r, E5)) + geom_point()
p18 <- ggplot(df, aes(r, E6)) + geom_point()

#No Treatment plots
(plot_grid(p13, p14, p15, p16, p17, p18))

#view rotation values
#view(df$r) 
```
![image](https://user-images.githubusercontent.com/64428723/80420738-419ed400-88a9-11ea-8561-a507781184b1.png)
![image](https://user-images.githubusercontent.com/64428723/80420745-45caf180-88a9-11ea-8702-822417f2b8d1.png)
![image](https://user-images.githubusercontent.com/64428723/80420758-4bc0d280-88a9-11ea-8733-c0c50b406d6d.png)

**All plots are linear between -80 degrees and reach max torque by -75 degrees. The drop indicates when the bone breaks. Bone stiffness is calculated by the slope of the linear region of the force vs displacement curve before the bone breaks, which is between -80 degrees and -75 degrees for all plots. Therefore, to see if there is a difference in bone stiffness we must compare the slopes of the plots between -80 and -75 degrees. Using the view command, we see -80 is at point 110 and -75 is at point 160.**

```{r}
#calculate stiffness of regenerated bone by calculating the slopes
BMP_1	<- (B1[160]-B1[110])/(r[160]-r[110])
BMP_2	<- (B2[160]-B2[110])/(r[160]-r[110])
BMP_3	<- (B3[160]-B3[110])/(r[160]-r[110])
BMP_4	<- (B4[160]-B4[110])/(r[160]-r[110])
BMP_5	<- (B5[160]-B5[110])/(r[160]-r[110])
BMP_6	<- (B6[160]-B6[110])/(r[160]-r[110])
FK506_1	<- (F1[160]-F1[110])/(r[160]-r[110])
FK506_2	<- (F2[160]-F2[110])/(r[160]-r[110])
FK506_3	<- (F3[160]-F3[110])/(r[160]-r[110])
FK506_4	<- (F4[160]-F4[110])/(r[160]-r[110])
FK506_5	<- (F5[160]-F5[110])/(r[160]-r[110])
FK506_6	<- (F6[160]-F6[110])/(r[160]-r[110])
Empty_1	<- (E1[160]-E1[110])/(r[160]-r[110])
Empty_2	<- (E2[160]-E2[110])/(r[160]-r[110])
Empty_3	<- (E3[160]-E3[110])/(r[160]-r[110])
Empty_4	<- (E4[160]-E4[110])/(r[160]-r[110])
Empty_5	<- (E5[160]-E5[110])/(r[160]-r[110])
Empty_6 <- (E6[160]-E6[110])/(r[160]-r[110])

#Organize the data
FK506 <- c(BMP_1,	BMP_2,	BMP_3,	BMP_4,	BMP_5,	BMP_6)
BMP <- c(FK506_1,	FK506_2,	FK506_3,	FK506_4,	FK506_5,	FK506_6)
Empty <- c(Empty_1,	Empty_2,	Empty_3,	Empty_4,	Empty_5,	Empty_6)
Stiffness <- data.frame(BMP, FK506, Empty)

Stiffnes <- tibble(
  id=c(1:6),
  BMP = c(Stiffness$BMP),
  FK506 = c(Stiffness$FK506),
  None = c(Stiffness$Empty)
)
Stiff<-pivot_longer(Stiffnes, -id, names_to="Treatment", values_to="Stiffness")

min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

p <- ggplot(Stiff, aes(x=Treatment, y=Stiffness), group=id)+   
  geom_point(size=2)+
  ylab("Stiffness (Nm/rad)")+
  ggtitle("Bone Stiffness")+
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Treatment")

p <- p + stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") + 
  geom_jitter(position=position_jitter(width=.2), size=3)
p
```
![image](https://user-images.githubusercontent.com/64428723/80419133-8ecd7680-88a6-11ea-8dd5-6a198b7cb038.png)

**Step 9: Write and perform a Monte Carlo analysis to calculate sample size necessary to test the hypothesis.**

```{r}
sims <- 100 #number of Monte Carlo simulations to run
pval <- replicate(
  sims, {
    sim.ezaov <- ezANOVA(
            data = Stiff, 
            wid = id,
            dv = Stiffness,
            between = Treatment,
            type = 2
            )
  pval <- sim.ezaov$ANOVA[1,5]
    }
  )
pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power.")

ggplot(data.frame(pval))+
  geom_histogram(aes(pval), color="#d28e00")+
  labs(x="p-value")
![image](https://user-images.githubusercontent.com/64428723/80419415-1f0bbb80-88a7-11ea-9e08-8ed649a76239.png)

nSims <- 100 #number of simulated experiments
p <- c()
alpha <- 0.05

# the monte carlo function
for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = 6, mean = mean(BMP), sd = sd(BMP)) 
  y<-rnorm(n = 6, mean = mean(FK506), sd = sd(FK506)) 
  z<-t.test(x,y, 
            alternative = "two.sided", 
            paired = FALSE, 
            var.equal = TRUE, 
            conf.level = 1-alpha, ) #perform the t-test
  
  p[i]<-z$p.value #get the p-value and store it
}

# the output
hits <- length(which(p < alpha)); paste("hits=", hits)
power <- hits/nSims; paste("power=", power)

```

**Step 10: Write in RMarkdown, knit, and submit**


