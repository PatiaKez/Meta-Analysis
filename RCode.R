######### My Actual MetaAnalysis Code #####################

## First I will install and load the required packages:

## "metafor"
install.packages("metafor",dependencies=TRUE,repos="https://cloud.r-project.org")

install.packages("metafor",dependencies=FALSE,repos="https://cloud.r-project.org")

install.packages("metafor",dependencies=TRUE,INSTALL_opts = '--no-lock')

library(metafor)
library(tidyr)
library(dplyr)


### Load Dataset for Approach 1:

AvWData <- read.csv("C:/Users/User/Documents/Edi Masters/Professional Skills for Ecologists/EcIA/Meta-Analysis/data/AvWData.csv")

### Load Dataset for Approach 2:

BvAData <- read.csv("C:/Users/User/Documents/Edi Masters/Professional Skills for Ecologists/EcIA/Meta-Analysis/data/SvEData.csv")


##### For Approach 1: 

#removing rows 10 and 11 as they're not numeric

AvWData <- AvWData[-c(10,11), ]

#and change the column to numeric
AvWData$lnRR <- as.numeric(AvWData$lnRR)

#Remove rows with extremely high variance

AvWData1 <- AvWData[AvWData$var <= 4.0, ]


#### Model 1

Model1 <- rma(yi=lnRR, vi=var, sd1i=SDa, sd2i=SDw, data=AvWData1)
Model1
summary(Model1)


## This model is used to create figure 2:

names(AvWData1)[1] <- "study_ref"

funnel(Model1, yaxis="vi")
#Oh wow, very different from before! Much clearer effect I think

forest(Model1,mlab = "Response Ratio", header="Study", slab=AvWData1$study_ref, cex=0.8, xlab="Response Ratio", cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra")
#this one as well



#### Model 2

#Remove rows that do not contain values for phytoplankton subgroup
AvWData1.2 <- AvWData1[AvWData1$subgroup != "Na", ]

Model2 <- rma(yi=lnRR, vi=var, sd1i=SDa, sd2i=SDw, mods = ~subgroup + study_ref, data=AvWData1.2)
Model2
summary(Model2)



#### Model 3

Model3 <- rma.mv(yi=lnRR, V=var, mods = ~subgroup, random= ~1|study_ref, data=AvWData1.2)
Model3
summary(Model3)



#### Model 4

Model4 <- rma.mv(yi=lnRR, V=var, mods = ~region, random= ~1|study_ref, data=AvWData1)
Model4
summary(Model4)



#### Code to create Figure 3

##Figure 3a:

Model2.1 <- rma(yi=lnRR, vi=var, sd1i=SDa, sd2i=SDw, data=AvWData1.2)


AvWData2 <- escalc(measure="ROM", yi = lnRR, vi=var, sd1i=SDa, sd2i=SDw, data = AvWData1.2)

MAgg <- aggregate(AvWData2, cluster = subgroup, V=vcov(Model2.1, type = "obs"), random= ~1|study_ref, addk = TRUE)

metaAGG <- rma(yi, vi, method = "EE", data=MAgg, digits = 3)
summary(metaAGG)

forest(metaAGG, mlab = "Pooled Response Ratio", header="Subgroup", ilab=ki, slab=MAgg$subgroup, cex=0.8, xlab="Response Ratio")


##Figure 3b: 

AvWData3 <- escalc(measure="ROM", yi = lnRR, vi=var, sd1i=SDa, sd2i=SDw, data = AvWData1)

MAgg3 <- aggregate(AvWData3, cluster = region, V=vcov(Model1, type = "obs"), addk = TRUE)

metaAGG3 <- rma(yi, vi, method = "EE", random= ~1|study_ref, data=MAgg3, digits = 3)
metaAGG3

forest(metaAGG3, mlab = "Pooled Response Ratio", header="Region", ilab=ki, slab=MAgg3$region, cex=0.8, xlab="Response Ratio")



##### For Approach 2:

#Correct column name:
names(BvAData)[1] <- "study_ref"

#Remove rows with extreely large variance
BvAData2 <- BvAData[BvAData$var <= 4.0, ]



#### Model 5

Model5 <- rma(yi=lnRR, vi=var, sd1i=SDc, sd2i=SDe, data=BvAData2)
summary(Model5)

## This model is used to create figure 4:

funnel(Model5, yaxis="vi")

forest(Model5, mlab = "Response Ratio", header="Study", slab=BvAData2$study_ref, cex=0.8, xlab="Response Ratio", cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra")



#### Model 6

Model6 <- rma(yi=lnRR, vi=var, sd1i=SDc, sd2i=SDe,mods = ~study_ref, data=BvAData2)
summary(Model6)



#################################################################################################################################################################################



