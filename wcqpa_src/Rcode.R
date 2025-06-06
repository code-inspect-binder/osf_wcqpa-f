
####################################################################################
####################################################################################
############################### R CODES ############################################
####################################################################################
####################################################################################

# R Codes: López Steinmetz, L. C.

# R Codes for: López Steinmetz, L. C., Fong, S. B., & Godoy, J. C. "Suicidal risk and 
# impulsivity-related traits among young Argentinean college students during a quarantine 
# of up to 103-days duration: Longitudinal evidence from the COVID-19 pandemic"

####################################################################################
############################ DATASET CODES #########################################
####################################################################################

# SUICIDAL BEAHAVIOR HISTORY: No (absence) = 0, Ideation (suicidal ideation) = 1, Yes (suicide attempt = 2)
# SEX: Man = 0, Woman = 1
# AGE: 18-21 years old = 0, 22-25 years old = 1
# MENTAL DISORDER HISTORY: No (absence) = 0, Yes (presence) = 1
# LONELINESS: Living alone = 0, Living with somebody (accompanied) = 1
# QUARANTINE DURATION: 0-days duration (baseline)) = 0, 10-days duration = 1, 50-days duration = 2, 103-days duration = 3

####################################################################################
############################ PREPARE DATA ##########################################
####################################################################################

# Load the dataset

dataset<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t")
dataset<-subset(dataset,age_number<26)

dataset$participant<-as.factor(dataset$participant)
is.factor(dataset$participant)

dataset$quar.duration_text<- factor(dataset$quar.duration_text, levels=c("0.days", "10.days", "50.days", "103.days")) # to order
dataset$suic.att.hist_text<- factor(dataset$suic.att.hist_text, levels=c("no", "ideation", "yes")) # to order

# Dummy code categories of age:
vector0<-dataset$age_number # To create a vector with the values of the column age_number
dataset<-cbind(dataset, age=vector0) # To add a column with the values of age_number
dataset$age[dataset$age <= 21] <- 0
dataset$age[dataset$age > 21] <- 1
table(dataset$age)
dataset$age<-as.integer(dataset$age)
is.integer(dataset$age)

young<-subset(dataset,age_number<26,select=c(participant, quar.duration, sex, age_number, age, ment.dis.hist, suic.att.hist, alone.or.accomp, ASUICRISK, BSUICRISK, ANEG.URG, APOS.URG, APERSEV.LACK, APREMED.LACK, ASENSATION.SEEKING))

youngIMPT2<-subset(dataset,age_number<26,select=c(participant, quar.duration, sex, age_number, age, ment.dis.hist, suic.att.hist, alone.or.accomp, ASUICRISK, BSUICRISK, BNEG.URG, BPOS.URG, BPERSEV.LACK, BPREMED.LACK, BSENSATION.SEEKING))

####################################################################################
############################ METHOD SECTION ########################################
####################################################################################

######  Section: METHOD. Sub-tile > Sample and procedure

# Distribution by mental disorder history 
table(dataset$suic.att.hist_text)
#  no ideation      yes 
# 586      518       98 

prop.table(table(dataset$suic.att.hist_text))*100
#        no  ideation       yes 
# 48.752080 43.094842  8.153078

# Distribution by sex
table(dataset$sex_text)
#  man woman 
#  173  1029
prop.table(table(dataset$sex_text))*100
#      man    woman 
# 14.39268 85.60732 

# Age: mean and standard deviation
mean(dataset$age_number)
# mean age = 21.46922
sd(dataset$age_number)
# sd age = 2.076666

# Distribution by age categories
table(dataset$age)
#   0   1 
# 610 592
prop.table(table(dataset$age))*100
#        0        1 
# 50.74875 49.25125

# Distribution by quarantine duration
table(dataset$quar.duration_text)
#   0.days  10.days  50.days 103.days 
#      131      611      213      247

prop.table(table(dataset$quar.duration_text))*100
#   0.days  10.days  50.days 103.days 
# 10.89850 50.83195 17.72047 20.54908

# Distribution by mental disorder history
table(dataset$ment.dis.hist_text)
#  no yes 
# 900 302 
prop.table(table(dataset$ment.dis.hist_text))*100
#       no      yes 
# 74.87521 25.12479

# Distribution by loneliness (living alone or accompanied)
table(dataset$alone.or.accomp_text)
# accompanied       alone 
#        1101         101
prop.table(table(dataset$alone.or.accomp_text))*100
# accompanied       alone 
#   91.597338    8.402662


######  Section: METHOD. Sub-tile > Data analysis

# Testing Skewness and Kurtosis. 
# Criteria: range of acceptable values -1 to +1 for Skewness and -3 to +3 for Kurtosis (Brown, 2006) or near to.
# Reference: Brown, T. A. (2006). Confirmatory factor analysis for applied research. New York: Guilford Press.

library(moments)

### SUICIDAL RISK
# Time 1:
skewness(dataset$ASUICRISK)
# 0.3997372
kurtosis(dataset$ASUICRISK)
# 2.449811

# Time 2
skewness(dataset$BSUICRISK)
# 0.4580211
kurtosis(dataset$BSUICRISK)
# 2.579951

# Histograms
hist(dataset$ASUICRISK)
hist(dataset$BSUICRISK)


### NEGATIVE URGENCY
# Time 1:
skewness(dataset$ANEG.URG)
# 0.07640885
kurtosis(dataset$ANEG.URG)
# 2.400869

# Histogram
hist(dataset$ANEG.URG)


### POSITIVE URGENCY
# Time 1:
skewness(dataset$APOS.URG)
# 0.7882336
kurtosis(dataset$APOS.URG)
# 2.987904

# Histogram
hist(dataset$APOS.URG)


### LACK OF PERSEVERANCE
# Time 1:
skewness(dataset$APERSEV.LACK)
# 0.4834759
kurtosis(dataset$APERSEV.LACK)
# 2.945617

# Histogram
hist(dataset$APERSEV.LACK)


### LACK OF PREMEDITATION
# Time 1:
skewness(dataset$APREMED.LACK)
# 0.5313196
kurtosis(dataset$APREMED.LACK)
# 3.005363

# Histogram
hist(dataset$APREMED.LACK)


### SENSATION SEEKING
# Time 1:
skewness(dataset$ASENSATION.SEEKING)
# 0.1360428
kurtosis(dataset$ASENSATION.SEEKING)
# 2.169693

# Histogram
hist(dataset$ASENSATION.SEEKING)


### NEGATIVE URGENCY
# Time 2:
skewness(dataset$BNEG.URG)
# 0.1287696
kurtosis(dataset$BNEG.URG)
# 2.347588

# Histogram
hist(dataset$BNEG.URG)


### POSITIVE URGENCY
# Time 2:
skewness(dataset$BPOS.URG)
# 0.8315126
kurtosis(dataset$BPOS.URG)
# 3.220162

# Histogram
hist(dataset$BPOS.URG)


### LACK OF PERSEVERANCE
# Time 2:
skewness(dataset$BPERSEV.LACK)
# 0.4707218
kurtosis(dataset$BPERSEV.LACK)
# 2.894581

# Histogram
hist(dataset$BPERSEV.LACK)


### LACK OF PREMEDITATION
# Time 2:
skewness(dataset$BPREMED.LACK)
# 0.5171418
kurtosis(dataset$BPREMED.LACK)
# 3.15163

# Histogram
hist(dataset$BPREMED.LACK)


### SENSATION SEEKING
# Time 2:
skewness(dataset$BSENSATION.SEEKING)
# 0.2123838
kurtosis(dataset$BSENSATION.SEEKING)
# 2.148886

# Histogram
hist(dataset$BSENSATION.SEEKING)


################################################
######## To Assess for multicollinearity #######
# To check for multicollinearity, we used the VIF (Variance Inflation Factor) values. We adopted the following criteria (Field et al., 2012):
# If VIF values are less than 10 then that indicates there probably isn't cause for concern.
# If the average of VIF values is not substantially greater than 1, then that also indicates that there's no cause for concern.
# Tolerance below 0.1 and below 0.2 indicates a serious and a potential problem
# Reference: Field, A., Miles, J., & Field, Z. (2012). Discovering statistics using R. London: SAGE.
# For assessing the assumption of no multicollinearity we used the vif() function, for the initial model:

library(car)

######### ISO-30: Suicidal risk
# First measurement of impulsivity-related traits:
fityoungA<-lm(ASUICRISK ~ sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG + APOS.URG + APERSEV.LACK + APREMED.LACK + ASENSATION.SEEKING, data = dataset)

# The VIF
vif(fityoungA)
#      sex                age      quar.duration      ment.dis.hist    alone.or.accomp           ANEG.URG           APOS.URG       APERSEV.LACK       APREMED.LACK ASENSATION.SEEKING 
# 1.072070           1.030456           1.010318           1.055392           1.021542           1.372641           1.409055           1.231530           1.396742           1.165350 

# The mean VIF
mean(vif(fityoungA))
# 1.176509

# The tolerance
1/vif(fityoungA)
#       sex                age      quar.duration      ment.dis.hist    alone.or.accomp           ANEG.URG           APOS.URG       APERSEV.LACK       APREMED.LACK ASENSATION.SEEKING 
# 0.9327748          0.9704446          0.9897877          0.9475156          0.9789126          0.7285225          0.7096955          0.8119981          0.7159520          0.8581111


######### ISO-30: Suicidal risk
# Second measurement of impulsivity-related traits:
fityoungB<-lm(ASUICRISK ~ sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG + BPERSEV.LACK + BPREMED.LACK + BSENSATION.SEEKING, data = dataset)

# The VIF
vif(fityoungB)
#      sex                age      quar.duration      ment.dis.hist    alone.or.accomp           BNEG.URG           BPOS.URG       BPERSEV.LACK       BPREMED.LACK BSENSATION.SEEKING 
# 1.070050           1.036601           1.008843           1.038662           1.022710           1.412043           1.506240           1.234495           1.413340           1.175687  

# The mean VIF
mean(vif(fityoungB))
# 1.191867

# The tolerance
1/vif(fityoungB)
#       sex                age      quar.duration      ment.dis.hist    alone.or.accomp           BNEG.URG           BPOS.URG       BPERSEV.LACK       BPREMED.LACK BSENSATION.SEEKING 
# 0.9345353          0.9646915          0.9912349          0.9627769          0.9777940          0.7081939          0.6639048          0.8100482          0.7075436          0.8505663


####################################################################################
############################ RESULTS SECTION #######################################
####################################################################################

####################################################################################
########################## DESCRIPTIVE RESULTS #####################################


vector1<-dataset$ASUICRISK # To create a vector with the values of the column ASUICRISK
dataset<-cbind(dataset, DXASUICRISK=vector1) # To add a column with the values of ASUICRISK
dataset$DXASUICRISK[dataset$DXASUICRISK < 30] <- "low risk" 
dataset$DXASUICRISK[dataset$DXASUICRISK >= 45 & dataset$DXASUICRISK != "low risk"] <- "high risk"
dataset$DXASUICRISK[dataset$DXASUICRISK >= 30 & dataset$DXASUICRISK < 45 & dataset$DXASUICRISK != "low rsik" & dataset$DXASUICRISK != "high rsik"] <- "moderate risk"

dataset$DXASUICRISK<- factor(dataset$DXASUICRISK, levels=c("low risk", "moderate risk", "high risk")) # to order

table(dataset$DXASUICRISK)
# low risk moderate risk     high risk 
#      454           378           370

prop.table(table(dataset$DXASUICRISK))*100
# low risk moderate risk     high risk 
# 37.77038      31.44759      30.78203 

prop.table(table(dataset$DXASUICRISK[dataset$suic.att.hist_text =="no"],dataset$suic.att.hist_text[dataset$suic.att.hist_text=="no"]))*100
#                     no ideation      yes
# low risk      58.36177  0.00000  0.00000
# moderate risk 30.37543  0.00000  0.00000
# high risk     11.26280  0.00000  0.00000

prop.table(table(dataset$DXASUICRISK[dataset$suic.att.hist_text =="ideation"],dataset$suic.att.hist_text[dataset$suic.att.hist_text=="ideation"]))*100
#                     no ideation      yes
# low risk       0.00000 19.30502  0.00000
# moderate risk  0.00000 34.55598  0.00000
# high risk      0.00000 46.13900  0.00000

prop.table(table(dataset$DXASUICRISK[dataset$suic.att.hist_text =="yes"],dataset$suic.att.hist_text[dataset$suic.att.hist_text=="yes"]))*100
#                    no ideation      yes
# low risk       0.00000  0.00000 12.24490
# moderate risk  0.00000  0.00000 21.42857
# high risk      0.00000  0.00000 66.32653


vector2<-dataset$BSUICRISK # To create a vector with the values of the column BSUICRISK
dataset<-cbind(dataset, DXBSUICRISK=vector2) # To add a column with the values of BSUICRISK
dataset$DXBSUICRISK[dataset$DXBSUICRISK < 30] <- "low risk" 
dataset$DXBSUICRISK[dataset$DXBSUICRISK >= 45 & dataset$DXBSUICRISK != "low risk"] <- "high risk"
dataset$DXBSUICRISK[dataset$DXBSUICRISK >= 30 & dataset$DXBSUICRISK < 45 & dataset$DXBSUICRISK != "low rsik" & dataset$DXBSUICRISK != "high rsik"] <- "moderate risk"

dataset$DXBSUICRISK<- factor(dataset$DXBSUICRISK, levels=c("low risk", "moderate risk", "high risk")) # to order

table(dataset$DXBSUICRISK)
# low risk moderate risk     high risk 
#      509           347           346

prop.table(table(dataset$DXBSUICRISK))*100
# low risk moderate risk     high risk 
# 42.34609      28.86855      28.78536

prop.table(table(dataset$DXBSUICRISK[dataset$suic.att.hist_text =="no"],dataset$suic.att.hist_text[dataset$suic.att.hist_text=="no"]))*100
#                     no ideation      yes
# low risk      61.26280  0.00000  0.00000
# moderate risk 26.79181  0.00000  0.00000
# high risk     11.94539  0.00000  0.00000

prop.table(table(dataset$DXBSUICRISK[dataset$suic.att.hist_text =="ideation"],dataset$suic.att.hist_text[dataset$suic.att.hist_text=="ideation"]))*100
#                     no ideation      yes
# low risk       0.00000 25.86873  0.00000
# moderate risk  0.00000 31.46718  0.00000
# high risk      0.00000 42.66409  0.00000

prop.table(table(dataset$DXBSUICRISK[dataset$suic.att.hist_text =="yes"],dataset$suic.att.hist_text[dataset$suic.att.hist_text=="yes"]))*100
#                     no ideation      yes
# low risk       0.00000  0.00000 16.32653
# moderate risk  0.00000  0.00000 27.55102
# high risk      0.00000  0.00000 56.12245 



# ENTIRE SAMPLE. DIFFERENCES IN SUICIDAL RISK IN TIME 1 AND TIME 2:

# Negative urgency
t.test(x = dataset$ASUICRISK, y = dataset$BSUICRISK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# P	Paired t-test
# data:  dataset$ASUICRISK and dataset$BSUICRISK
# t = 5.2941, df = 1201, p-value = 1.421e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  0.8430578 1.8358107
# sample estimates:
# mean of the differences 
# 1.339434

mean(dataset$ASUICRISK)
# 36.60483
sd(dataset$ASUICRISK)
# 16.75576

mean(dataset$BSUICRISK)
# 35.26539
sd(dataset$BSUICRISK)
# 17.39008

# ENTIRE SAMPLE. DIFFERENCES IN IMPULSIVITY-RELATED TRAITS IN TIME 1 AND TIME 2:

# Negative urgency
t.test(x = dataset$ANEG.URG, y = dataset$BNEG.URG, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  dataset$ANEG.URG and dataset$BNEG.URG
# t = 4.7772, df = 1201, p-value = 1.997e-06
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1764984 0.4225033
# sample estimates:
# mean of the differences 
# 0.2995008 

mean(dataset$ANEG.URG)
# 9.424293
sd(dataset$ANEG.URG)
# 2.781767
mean(dataset$BNEG.URG)
# 9.124792
sd(dataset$BNEG.URG)
# 2.793665

# Positive urgency
t.test(x = dataset$APOS.URG, y = dataset$BPOS.URG, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  dataset$APOS.URG and dataset$BPOS.URG
# t = 0.80643, df = 1201, p-value = 0.4202
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.0715239  0.1713575
# sample estimates:
# mean of the differences 
# 0.04991681 

mean(dataset$APOS.URG)
# 6.869384
sd(dataset$APOS.URG)
# 2.513658

mean(dataset$BPOS.URG)
# 6.819468
sd(dataset$BPOS.URG)
# 2.566327

# Lack of perseverance
t.test(x = dataset$APERSEV.LACK, y = dataset$BPERSEV.LACK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  dataset$APERSEV.LACK and dataset$BPERSEV.LACK
# t = -1.0094, df = 1201, p-value = 0.313
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.15918680  0.05103373
# sample estimates:
# mean of the differences 
# -0.05407654 

mean(dataset$APERSEV.LACK)
# 7.848586
sd(dataset$APERSEV.LACK)
# 2.366932

mean(dataset$BPERSEV.LACK)
# 7.902662
sd(dataset$BPERSEV.LACK)
# 2.412086

# Lack of premeditation
t.test(x = dataset$APREMED.LACK, y = dataset$BPREMED.LACK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  dataset$APREMED.LACK and dataset$BPREMED.LACK
# t = 0.94183, df = 1201, p-value = 0.3465
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.05406514  0.15389875
# sample estimates:
# mean of the differences 
# 0.04991681

mean(dataset$APREMED.LACK)
# 7.503328
sd(dataset$APREMED.LACK)
# 2.334352

mean(dataset$BPREMED.LACK)
# 7.453411
sd(dataset$BPREMED.LACK)
# 2.304449

# Sensation seeking
t.test(x = dataset$ASENSATION.SEEKING, y = dataset$BSENSATION.SEEKING, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  dataset$ASENSATION.SEEKING and dataset$BSENSATION.SEEKING
# t = 4.1601, df = 1201, p-value = 3.407e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1252842 0.3489255
# sample estimates:
# mean of the differences 
# 0.2371048

mean(dataset$ASENSATION.SEEKING)
# 9.348586
sd(dataset$ASENSATION.SEEKING)
# 3.081591

mean(dataset$BSENSATION.SEEKING)
# 9.111481
sd(dataset$BSENSATION.SEEKING)
# 3.200627


#### Group without suicidal behavior history
withoutdiff<-subset(dataset, dataset$suic.att.hist==0)

# WITHOUT SUICIDAL BEHAVIOR HISTORY. DIFFERENCES IN SUICIDAL RISK IN TIME 1 AND TIME 2:

# Negative urgency
t.test(x = withoutdiff$ASUICRISK, y = withoutdiff$BSUICRISK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# P	Paired t-test
# data:  withoutdiff$ASUICRISK and withoutdiff$BSUICRISK
# t = 2.5266, df = 585, p-value = 0.01178
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1770617 1.4133820
# sample estimates:
#   mean of the differences 
# 0.7952218 

mean(withoutdiff$ASUICRISK)
# 27.87543
sd(withoutdiff$ASUICRISK)
# 12.59683

mean(withoutdiff$BSUICRISK)
# 27.0802
sd(withoutdiff$BSUICRISK)
# 13.56461

# WITHOUT SUICIDAL BEHAVIOR HISTORY. DIFFERENCES IN IMPULSIVITY-RELATED TRAITS IN TIME 1 AND TIME 2:

# Negative urgency
t.test(x = withoutdiff$ANEG.URG, y = withoutdiff$BNEG.URG, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  withoutdiff$ANEG.URG and withoutdiff$BNEG.URG
# t = 2.3894, df = 585, p-value = 0.01719
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.03706175 0.37932050
# sample estimates:
#   mean of the differences 
# 0.2081911  

mean(withoutdiff$ANEG.URG)
# 8.561433
sd(withoutdiff$ANEG.URG)
# 2.58076

mean(withoutdiff$BNEG.URG)
# 8.353242
sd(withoutdiff$BNEG.URG)
# 2.695954

# Positive urgency
t.test(x = withoutdiff$APOS.URG, y = withoutdiff$BPOS.URG, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  withoutdiff$APOS.URG and withoutdiff$BPOS.URG
# t = -1.1428, df = 585, p-value = 0.2536
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.25052208  0.06622174
# sample estimates:
#   mean of the differences 
# -0.09215017

mean(withoutdiff$APOS.URG)
# 6.375427
sd(withoutdiff$APOS.URG)
# 2.2906

mean(withoutdiff$BPOS.URG)
# 6.467577
sd(withoutdiff$BPOS.URG)
# 2.373603

# Lack of perseverance
t.test(x = withoutdiff$APERSEV.LACK, y = withoutdiff$BPERSEV.LACK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  withoutdiff$APERSEV.LACK and withoutdiff$BPERSEV.LACK
# t = -1.5215, df = 585, p-value = 0.1287
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.23846825  0.03027712
# sample estimates:
#   mean of the differences 
# -0.1040956 

mean(withoutdiff$APERSEV.LACK)
# 7.511945
sd(withoutdiff$APERSEV.LACK)
# 2.232785

mean(withoutdiff$BPERSEV.LACK)
# 7.616041
sd(withoutdiff$BPERSEV.LACK)
# 2.30444

# Lack of premeditation
t.test(x = withoutdiff$APREMED.LACK, y = withoutdiff$BPREMED.LACK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  withoutdiff$APREMED.LACK and withoutdiff$BPREMED.LACK
# t = 0.842, df = 585, p-value = 0.4001
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.07958999  0.19904392
# sample estimates:
#   mean of the differences 
# 0.05972696

mean(withoutdiff$APREMED.LACK)
# 7.078498
sd(withoutdiff$APREMED.LACK)
# 2.22971

mean(withoutdiff$BPREMED.LACK)
# 7.018771
sd(withoutdiff$BPREMED.LACK)
# 2.14866

# Sensation seeking
t.test(x = withoutdiff$ASENSATION.SEEKING, y = withoutdiff$BSENSATION.SEEKING, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  withoutdiff$ASENSATION.SEEKING and withoutdiff$BSENSATION.SEEKING
# t = 4.1133, df = 585, p-value = 4.459e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1712008 0.4840893
# sample estimates:
#   mean of the differences 
# 0.3276451 

mean(withoutdiff$ASENSATION.SEEKING)
# 9.153584
sd(withoutdiff$ASENSATION.SEEKING)
# 3.043889

mean(withoutdiff$BSENSATION.SEEKING)
# 8.825939
sd(withoutdiff$BSENSATION.SEEKING)
# 3.1188



#### Group with suicidal ideation history
ideationdiff<-subset(dataset, dataset$suic.att.hist==1)

# SUICIDAL IDEATION HISTORY. DIFFERENCES IN SUICIDAL RISK IN TIME 1 AND TIME 2:

# Negative urgency
t.test(x = ideationdiff$ASUICRISK, y = ideationdiff$BSUICRISK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# P	Paired t-test
# data:  ideationdiff$ASUICRISK and ideationdiff$BSUICRISK
# t = 3.907, df = 517, p-value = 0.0001059
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.8023807 2.4254185
# sample estimates:
#   mean of the differences 
# 1.6139

mean(ideationdiff$ASUICRISK)
# 43.57529
sd(ideationdiff$ASUICRISK)
# 15.40156

mean(ideationdiff$BSUICRISK)
# 41.96139
sd(ideationdiff$BSUICRISK)
# 16.5171

# SUICIDAL IDEATION HISTORY. DIFFERENCES IN IMPULSIVITY-RELATED TRAITS IN TIME 1 AND TIME 2:

# Negative urgency
t.test(x = ideationdiff$ANEG.URG, y = ideationdiff$BNEG.URG, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  ideationdiff$ANEG.URG and ideationdiff$BNEG.URG
# t = 3.6775, df = 517, p-value = 0.0002602
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1672510 0.5508957
# sample estimates:
#   mean of the differences 
# 0.3590734 

mean(ideationdiff$ANEG.URG)
# 10.14672
sd(ideationdiff$ANEG.URG)
# 2.659551

mean(ideationdiff$BNEG.URG)
# 9.787645
sd(ideationdiff$BNEG.URG)
# 2.641962

# Positive urgency
t.test(x = ideationdiff$APOS.URG, y = ideationdiff$BPOS.URG, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  ideationdiff$APOS.URG and ideationdiff$BPOS.URG
# t = 0.97586, df = 517, p-value = 0.3296
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.09975052  0.29666172
# sample estimates:
#   mean of the differences 
# 0.0984556 

mean(ideationdiff$APOS.URG)
# 7.241313
sd(ideationdiff$APOS.URG)
# 2.579804

mean(ideationdiff$BPOS.URG)
# 7.142857
sd(ideationdiff$BPOS.URG)
# 2.684373

# Lack of perseverance
t.test(x = ideationdiff$APERSEV.LACK, y = ideationdiff$BPERSEV.LACK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  ideationdiff$APERSEV.LACK and ideationdiff$BPERSEV.LACK
# t = -0.15705, df = 517, p-value = 0.8753
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.1825603  0.1555332
# sample estimates:
#   mean of the differences 
# -0.01351351 

mean(ideationdiff$APERSEV.LACK)
# 8.169884
sd(ideationdiff$APERSEV.LACK)
# 2.407696

mean(ideationdiff$BPERSEV.LACK)
# 8.183398
sd(ideationdiff$BPERSEV.LACK)
# 2.3723

# Lack of premeditation
t.test(x = ideationdiff$APREMED.LACK, y = ideationdiff$BPREMED.LACK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  ideationdiff$APREMED.LACK and ideationdiff$BPREMED.LACK
# t = 0.81877, df = 517, p-value = 0.4133
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.0972566  0.2362527
# sample estimates:
#   mean of the differences 
# 0.06949807 

mean(ideationdiff$APREMED.LACK)
# 7.872587
sd(ideationdiff$APREMED.LACK)
# 2.274912

mean(ideationdiff$BPREMED.LACK)
# 7.803089
sd(ideationdiff$BPREMED.LACK)
# 2.308807

# Sensation seeking
t.test(x = ideationdiff$ASENSATION.SEEKING, y = ideationdiff$BSENSATION.SEEKING, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  ideationdiff$ASENSATION.SEEKING and ideationdiff$BSENSATION.SEEKING
# t = 2.2933, df = 517, p-value = 0.02223
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.02933362 0.37993279
# sample estimates:
#   mean of the differences 
# 0.2046332 

mean(ideationdiff$ASENSATION.SEEKING)
# 9.552124
sd(ideationdiff$ASENSATION.SEEKING)
# 3.033052

mean(ideationdiff$BSENSATION.SEEKING)
# 9.34749
sd(ideationdiff$BSENSATION.SEEKING)
# 3.198601



#### Group with suicide attempt history
attemptdiff<-subset(dataset, dataset$suic.att.hist==2)

# SUICIDE ATTEMPT HISTORY. DIFFERENCES IN SUICIDAL RISK IN TIME 1 AND TIME 2:

# Negative urgency
t.test(x = attemptdiff$ASUICRISK, y = attemptdiff$BSUICRISK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# P	Paired t-test
# data:  attemptdiff$ASUICRISK and attemptdiff$BSUICRISK
# t = 2.7722, df = 97, p-value = 0.006676
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.8927609 5.3929534
# sample estimates:
#   mean of the differences 
# 3.142857

mean(attemptdiff$ASUICRISK)
# 51.95918
sd(attemptdiff$ASUICRISK)
# 17.22975

mean(attemptdiff$BSUICRISK)
# 48.81633
sd(attemptdiff$BSUICRISK)
# 18.73161

# SUICIDE ATTEMPT HISTORY. DIFFERENCES IN IMPULSIVITY-RELATED TRAITS IN TIME 1 AND TIME 2:

# Negative urgency
t.test(x = attemptdiff$ANEG.URG, y = attemptdiff$BNEG.URG, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  attemptdiff$ANEG.URG and attemptdiff$BNEG.URG
# t = 2.2989, df = 97, p-value = 0.02365
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.07252164 0.98870285
# sample estimates:
#   mean of the differences 
# 0.5306122 

mean(attemptdiff$ANEG.URG)
# 10.76531
sd(attemptdiff$ANEG.URG)
# 2.973425

mean(attemptdiff$BNEG.URG)
# 10.23469
sd(attemptdiff$BNEG.URG)
# 2.899701

# Positive urgency
t.test(x = attemptdiff$APOS.URG, y = attemptdiff$BPOS.URG, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  attemptdiff$APOS.URG and attemptdiff$BPOS.URG
# t = 2.7264, df = 97, p-value = 0.007599
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1748812 1.1108331
# sample estimates:
#   mean of the differences 
# 0.6428571 

mean(attemptdiff$APOS.URG)
# 7.857143
sd(attemptdiff$APOS.URG)
# 2.81381

mean(attemptdiff$BPOS.URG)
# 7.214286
sd(attemptdiff$BPOS.URG)
# 2.774144

# Lack of perseverance
t.test(x = attemptdiff$APERSEV.LACK, y = attemptdiff$BPERSEV.LACK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  attemptdiff$APERSEV.LACK and attemptdiff$BPERSEV.LACK
# t = 0.12693, df = 97, p-value = 0.8993
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4480482  0.5092727
# sample estimates:
#   mean of the differences 
# 0.03061224  

mean(attemptdiff$APERSEV.LACK)
# 8.163265
sd(attemptdiff$APERSEV.LACK)
# 2.658167

mean(attemptdiff$BPERSEV.LACK)
# 8.132653
sd(attemptdiff$BPERSEV.LACK)
# 3.007337

# Lack of premeditation
t.test(x = attemptdiff$APREMED.LACK, y = attemptdiff$BPREMED.LACK, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  attemptdiff$APREMED.LACK and attemptdiff$BPREMED.LACK
# t = -0.54872, df = 97, p-value = 0.5845
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.5182341  0.2937443
# sample estimates:
#   mean of the differences 
# -0.1122449 

mean(attemptdiff$APREMED.LACK)
# 8.091837
sd(attemptdiff$APREMED.LACK)
# 2.780962

mean(attemptdiff$BPREMED.LACK)
# 8.204082
sd(attemptdiff$BPREMED.LACK)
# 2.670801

# Sensation seeking
t.test(x = attemptdiff$ASENSATION.SEEKING, y = attemptdiff$BSENSATION.SEEKING, paired = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Paired t-test
# data:  attemptdiff$ASENSATION.SEEKING and attemptdiff$BSENSATION.SEEKING
# t = -0.67858, df = 97, p-value = 0.499
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.5206418  0.2553357
# sample estimates:
#   mean of the differences 
# -0.1326531 

mean(attemptdiff$ASENSATION.SEEKING)
# 9.438776
sd(attemptdiff$ASENSATION.SEEKING)
# 3.493931

mean(attemptdiff$BSENSATION.SEEKING)
# 9.571429
sd(attemptdiff$BSENSATION.SEEKING)
# 3.555162



# Figure 1: color version
library(ggplot2)
library(ggpubr)

# ASUICRISK
p1 <- ggplot(data = dataset, aes(x = ASUICRISK, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Suicidal risk") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = ASUICRISK, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Suicidal risk") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   a)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot

# BSUICRISK
p1 <- ggplot(data = dataset, aes(x = BSUICRISK, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Suicidal risk") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = BSUICRISK, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Suicidal risk") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   b)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot


# Impulsivity-related traits:
# ANEG.URG
p1 <- ggplot(data = dataset, aes(x = ANEG.URG, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Negative urgency") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = ANEG.URG, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Negative urgency") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   c)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot

# BNEG.URG
p1 <- ggplot(data = dataset, aes(x = BNEG.URG, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Negative urgency") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = BNEG.URG, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Negative urgency") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   d)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot


# APOS.URG
p1 <- ggplot(data = dataset, aes(x = APOS.URG, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Positive urgency") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = APOS.URG, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Positive urgency") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   e)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot

# BPOS.URG
p1 <- ggplot(data = dataset, aes(x = BPOS.URG, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Positive urgency") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = BPOS.URG, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Positive urgency") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   f)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot


# APERSEV.LACK
p1 <- ggplot(data = dataset, aes(x = APERSEV.LACK, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Lack of Perseverance") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = APERSEV.LACK, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Lack of Perseverance") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   g)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot

# BPERSEV.LACK
p1 <- ggplot(data = dataset, aes(x = BPERSEV.LACK, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Lack of Perseverance") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = BPERSEV.LACK, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Lack of Perseverance") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   h)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot


# APREMED.LACK
p1 <- ggplot(data = dataset, aes(x = APREMED.LACK, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Lack of Premeditation") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = APREMED.LACK, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Lack of Premeditation") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   i)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot

# BPREMED.LACK
p1 <- ggplot(data = dataset, aes(x = BPREMED.LACK, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Lack of Premeditation") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = BPREMED.LACK, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Lack of Premeditation") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   j)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot


# ASENSATION.SEEKING
p1 <- ggplot(data = dataset, aes(x = ASENSATION.SEEKING, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Sensation seeking") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = ASENSATION.SEEKING, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Sensation seeking") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   k)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot

# BSENSATION.SEEKING
p1 <- ggplot(data = dataset, aes(x = BSENSATION.SEEKING, fill = suic.att.hist_text)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2", "blue")) +
  geom_rug(aes(color = suic.att.hist_text), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Density") + xlab("Sensation seeking") +
  theme_bw()
p2 <- ggplot(data = dataset, aes(x = suic.att.hist_text, y = BSENSATION.SEEKING, color = suic.att.hist_text)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2", "blue")) + ylab("Sensation seeking") + xlab("Suicidal behavior history") +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "none")
final_plot <- annotate_figure(final_plot, fig.lab.pos = "top.left", fig.lab = "   l)", fig.lab.size = 15, fig.lab.face = "bold")
final_plot


####################################################################################
######################## MULTILEVEL MODELLING ######################################

# We have followed the steps indicated in Field et al. (2012) for carrying out these analyses. 
# Reference: Field, A., Miles, J., & Field, Z. (2012). Discovering statistics using R. SAGE: London.

################################# SUICIDAL RISK ###########################################
################# IMPULSIVITY-RELATED TRAITS MEASURED IN TIME 1 ###########################

####### TO ASSESS THE NEED FOR A MULTILEVEL MODEL WITH RANDOM EFFECTS BY SUICIDE ATTEMPT HISTORY

####### Preparing the data

# To select the variables from the "young" dataset: IMPULSIVITY HERE I USE THE SCORES FROM TIME 1
library(dplyr)
youngsuicriskIMP1<-select(young, participant, sex, age, quar.duration, ment.dis.hist, suic.att.hist, alone.or.accomp, ANEG.URG, APOS.URG, APERSEV.LACK, APREMED.LACK, ASENSATION.SEEKING, ASUICRISK, BSUICRISK)

# To convert the format of the dataframe into the long format:
library(reshape)
myDataISO<-melt(youngsuicriskIMP1, id = c("participant","sex", "age", "quar.duration","ment.dis.hist", "suic.att.hist", "alone.or.accomp", "ANEG.URG", "APOS.URG", "APERSEV.LACK", "APREMED.LACK", "ASENSATION.SEEKING"), measured = c("ASUICRISK","BSUICRISK"))

# To rename variables:
names(myDataISO)<-c("participant","sex", "age", "quar.duration","ment.dis.hist", "suic.att.hist", "alone.or.accomp", "ANEG.URG", "APOS.URG", "APERSEV.LACK", "APREMED.LACK", "ASENSATION.SEEKING", "variable", "scores") # "variable" refers to the repeated-measures variable

# This creates a variable "period" in the dataframe myDataISO:
myDataISO$period<-gl(2, 1202, labels = c("first", "second")) # We created 2 sets of 1202 scores, the labels option then specifies the names to attach to these 2 sets, which correspond to the levels of "period" (first measurement and follow-up).

# To make it clearer that there are two observations for each participant we have sorted the data by participant:
myDataISO<-myDataISO[order(myDataISO$participant),]

####### MIXED DESIGNS AS A GLM

# Setting contrasts for quarantine sub-period:
myDataISO$quar.duration<-as.factor(myDataISO$quar.duration)
is.factor(myDataISO$quar.duration)

basvs10days<-c(0,1,0,0) # this compares the baseline (prior to quarantine) to a quarantine sub.period of up to 10-days duration
basvs50days<-c(0,0,1,0) # this compares the baseline (prior to quarantine) to a quarantine sub.period of up to 50-days duration
basvs103days<-c(0,0,0,1) # this compares the baseline (prior to quarantine) to a quarantine sub.period of up to 103-days duration
contrasts(myDataISO$quar.duration)<-cbind(basvs10days, basvs50days, basvs103days)
myDataISO$quar.duration # To check we setted the contrasts correctly 


# Building the model
library(nlme)

interceptOnlySUICImpT1 <-gls(scores ~ 1, data = myDataISO, method = "ML") # only the intercept

randomInterceptOnlySUICImpT1 <-lme(scores ~ 1, random = ~1|suic.att.hist, data = myDataISO, method = "ML") # Suicide attemtp history as a random effect

model1<-lme(scores ~ 1, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim")) # the nested structure indicates repeated measures (within variable) # repeated measures nested within participants and participants nested within mental disorder history
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1)  # to compare models

# To see the overall effect of each main effect (additive and interaction) we added them to the model one at a time:

model2<-lme(scores ~ period, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim"))
summary(model2) # period is negatively related to suicidal risk
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1, model2) #### Adding period significantly improved the fit of the model

model3<-lme(scores ~ period + sex, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim"))
summary(model3) # sex is not significantly related to suicidal risk
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1, model2, model3) #### Adding sex does not significantly improved the fit of the model

model4<-lme(scores ~ period + sex + age, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim"))
summary(model4) # age is negatively related to suicidal risk
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1, model2, model3, model4) #### Adding age significantly improved the fit of the model

model5<-lme(scores ~ period + sex + age + quar.duration, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim"))
summary(model5) # quarantine duration is positively related to suicidal risk # 50.days p = 0.0326
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1, model2, model3, model4, model5) #### Adding quarantine duration does not significantly improved the fit of the model

model6<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim"))
summary(model6) # mental disorder history is positively related to suicidal risk
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1, model2, model3, model4, model5, model6) #### Adding ment dis hist significantly improved the fit of the model

model7<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim"))
summary(model7) # alone or accompanied is not significantly related to suicidal risk
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1, model2, model3, model4, model5, model6, model7) #### Adding alone or accompanied does not improved the fit of the model

model8<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim"))
summary(model8) # negative urgency is positively related to suicidal risk
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1, model2, model3, model4, model5, model6, model7, model8) #### Adding negative urgency significantly improved the fit of the model

model9<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG + APOS.URG, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim"))
summary(model9) # positive urgency is not significantly related to suicidal risk
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1, model2, model3, model4, model5, model6, model7, model8, model9) #### Adding positive urgency does not significantly improved the fit of the model

model10<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG + APOS.URG + APERSEV.LACK, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim"))
summary(model10) # lack of perseverance is positively related to suicidal risk
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1, model2, model3, model4, model5, model6, model7, model8, model9, model10) #### Adding lack of perseverance significantly improved the fit of the model

model11<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG + APOS.URG + APERSEV.LACK + APREMED.LACK, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim"))
summary(model11) # lack of premeditation is positively related to suicidal risk
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11) #### Adding lack of premeditation significantly improved the fit of the model

model12<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG + APOS.URG + APERSEV.LACK + APREMED.LACK + ASENSATION.SEEKING, random = ~1|suic.att.hist/participant, data = myDataISO, method = "ML", control = lmeControl(opt = "optim"))
summary(model12) # sensation seeking is negatively related to suicidal risk
anova(interceptOnlySUICImpT1, randomInterceptOnlySUICImpT1, model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12) #### Adding sensation seeking significantly improved the fit of the model
#                              Model df      AIC      BIC     logLik     Test   L.Ratio p-value
# interceptOnlySUICImpT1           1  2 20471.45 20483.02 -10233.726                           
# randomInterceptOnlySUICImpT1     2  3 19811.64 19828.99  -9902.818   1 vs 2  661.8157  <.0001
# model1                           3  4 18464.32 18487.46  -9228.161   2 vs 3 1349.3137  <.0001
# model2                           4  5 18438.59 18467.52  -9214.297   3 vs 4   27.7290  <.0001
# model3                           5  6 18437.52 18472.23  -9212.761   4 vs 5    3.0727  0.0796
# model4                           6  7 18434.40 18474.89  -9210.200   5 vs 6    5.1215  0.0236
# model5                           7 10 18435.57 18493.42  -9207.784   6 vs 7    4.8322  0.1845
# model6                           8 11 18403.64 18467.27  -9190.820   7 vs 8   33.9274  <.0001
# model7                           9 12 18405.49 18474.90  -9190.742   8 vs 9    0.1551  0.6937
# model8                          10 13 18112.94 18188.15  -9043.472  9 vs 10  294.5403  <.0001
# model9                          11 14 18112.85 18193.84  -9042.424 10 vs 11    2.0973  0.1476
# model10                         12 15 18085.26 18172.03  -9027.630 11 vs 12   29.5875  <.0001
# model11                         13 16 18071.40 18163.96  -9019.700 12 vs 13   15.8593  0.0001
# model12                         14 17 18061.78 18160.13  -9013.891 13 vs 14   11.6178  0.000

summary(model12)
# Linear mixed-effects model fit by maximum likelihood
# Data: myDataISO 
#      AIC      BIC    logLik
# 18061.78 18160.13 -9013.891
# 
# Random effects:
#   Formula: ~1 | suic.att.hist
#         (Intercept)
# StdDev:     5.87498
# 
# Formula: ~1 | participant %in% suic.att.hist
#         (Intercept) Residual
# StdDev:    11.16789 6.199857
# 
# Fixed effects: scores ~ period + sex + age + quar.duration + ment.dis.hist +      alone.or.accomp + ANEG.URG + APOS.URG + APERSEV.LACK + APREMED.LACK +      ASENSATION.SEEKING 
#                               Value Std.Error   DF   t-value p-value
# (Intercept)                9.503040  4.421587 1201  2.149237  0.0318
# periodsecond              -1.339434  0.253637 1201 -5.280909  0.0000 #
# sex                       -0.845761  1.032444 1187 -0.819183  0.4128
# age                       -1.196026  0.705315 1187 -1.695734  0.0902
# quar.durationbasvs10days   1.122351  1.169605 1187  0.959598  0.3375
# quar.durationbasvs50days   2.143296  1.350019 1187  1.587605  0.1126
# quar.durationbasvs103days  0.589621  1.311600 1187  0.449543  0.6531
# ment.dis.hist              4.129575  0.866394 1187  4.766391  0.0000 #
# alone.or.accomp           -0.143988  1.265819 1187 -0.113750  0.9095
# ANEG.URG                   2.269814  0.149287 1187 15.204356  0.0000 #
# APOS.URG                   0.303023  0.164590 1187  1.841079  0.0659
# APERSEV.LACK               0.481787  0.163921 1187  2.939136  0.0034 #
# APREMED.LACK               0.744342  0.175926 1187  4.230993  0.0000 #
# ASENSATION.SEEKING        -0.414896  0.121784 1187 -3.406807  0.0007 #
# 
# Standardized Within-Group Residuals:
#        Min         Q1        Med         Q3        Max 
# -3.8505205 -0.4465672 -0.0103038  0.4380075  3.0303747 
# 
# Number of Observations: 2404
# Number of Groups: 
#   suic.att.hist participant %in% suic.att.hist 
#               3                           1202  

intervals(model12)
# Approximate 95% confidence intervals
# 
# Fixed effects:
#                                 lower       est.      upper
# (Intercept)                0.85344305  9.5030398 18.1526366
# periodsecond              -1.83560417 -1.3394343 -0.8432644
# sex                       -2.86547264 -0.8457610  1.1739507
# age                       -2.57579308 -1.1960261  0.1837410
# quar.durationbasvs10days  -1.16568097  1.1223506  3.4103823
# quar.durationbasvs50days  -0.49766901  2.1432958  4.7842606
# quar.durationbasvs103days -1.97618833  0.5896205  3.1554294
# ment.dis.hist              2.43469609  4.1295745  5.8244530
# alone.or.accomp           -2.62023810 -0.1439876  2.3322630
# ANEG.URG                   1.97777230  2.2698142  2.5618561
# APOS.URG                  -0.01895482  0.3030231  0.6250009
# APERSEV.LACK               0.16111701  0.4817868  0.8024567
# APREMED.LACK               0.40018783  0.7443421  1.0884964
# ASENSATION.SEEKING        -0.65313600 -0.4148960 -0.1766561
# attr(,"label")
# [1] "Fixed effects:"
# 
# Random Effects:
#   Level: suic.att.hist 
# lower    est.    upper
# sd((Intercept)) 2.587241 5.87498 13.34061
# Level: participant 
# lower     est.    upper
# sd((Intercept)) 10.65936 11.16789 11.70067
# 
# Within-group standard error:
#   lower     est.    upper 
# 5.956896 6.199857 6.452727 


library(pastecs)
by(myDataISO$scores, list(myDataISO$period), stat.desc, basic = FALSE)
by(myDataISO$scores, list(myDataISO$period, myDataISO$ment.dis.hist), stat.desc, basic = FALSE)
by(myDataISO$scores, list(myDataISO$period, myDataISO$quar.duration), stat.desc, basic = FALSE)
by(myDataISO$scores, list(myDataISO$period, myDataISO$age), stat.desc, basic = FALSE)


# Calculating effect sizes
library(DSUR.noof)

# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# periodsecond
rcontrast(-5.280909, 1201)
# [1] "r = 0.150644240496263"

# ment.dis.hist
rcontrast(4.766391, 1187)
# [1] "r = 0.137040049777564"

# ANEG.URG
rcontrast(15.204356, 1187)
# [1] "r = 0.403741608509188"

# APERSEV.LACK
rcontrast(2.939136, 1187)
# [1] "r = 0.0850001580075473"

# APREMED.LACK
rcontrast(4.230993, 1187)
# [1] "r = 0.121889577034475"

# ASENSATION.SEEKING
rcontrast(-3.406807, 1187)
# [1] "r = 0.0984032056972442"


############# TO RUN THE FINAL MODEL IN EACH GROUP, SEPARATELY: ###################

#### Group without suicide attempt history
without<-subset(myDataISO, myDataISO$suic.att.hist==0)
withoutModel12NoInt<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG + APOS.URG + APERSEV.LACK + APREMED.LACK + ASENSATION.SEEKING, random = ~1|participant, data = without, method = "ML", control = lmeControl(opt = "optim"))
summary(withoutModel12NoInt)
# Linear mixed-effects model fit by maximum likelihood
# Data: without 
#      AIC      BIC    logLik
# 8458.339 8539.402 -4213.169
# 
# Random effects:
#   Formula: ~1 | participant
#         (Intercept) Residual
# StdDev:     9.45928   5.3829
# 
# Fixed effects: scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG + APOS.URG + APERSEV.LACK + APREMED.LACK + ASENSATION.SEEKING 
#                               Value Std.Error  DF   t-value p-value
# (Intercept)               -0.162709  3.453949 585 -0.047108  0.9624
# periodsecond              -0.795222  0.316368 585 -2.513600  0.0122 #
# sex                       -1.100589  1.213225 573 -0.907160  0.3647
# age                       -0.934679  0.875648 573 -1.067414  0.2862
# quar.durationbasvs10days   1.324962  1.475330 573  0.898078  0.3695
# quar.durationbasvs50days   0.943192  1.712891 573  0.550643  0.5821
# quar.durationbasvs103days  0.732588  1.609138 573  0.455268  0.6491
# ment.dis.hist              4.435565  1.278720 573  3.468754  0.0006 #
# alone.or.accomp            0.164508  1.669551 573  0.098534  0.9215
# ANEG.URG                   2.098599  0.192877 573 10.880489  0.0000 #
# APOS.URG                   0.762553  0.226095 573  3.372713  0.0008 #
# APERSEV.LACK               0.741756  0.213999 573  3.466166  0.0006 #
# APREMED.LACK               0.486834  0.225240 573  2.161399  0.0311 #
# ASENSATION.SEEKING        -0.452599  0.152141 573 -2.974868  0.0031 #
# 
# Standardized Within-Group Residuals:
#   Min           Q1          Med           Q3          Max 
# -3.244330867 -0.472570566  0.000580543  0.428178315  3.450602362 
# 
# Number of Observations: 1172
# Number of Groups: 586 

intervals(withoutModel12NoInt)
# Approximate 95% confidence intervals
# 
# Fixed effects:
#                                 lower       est.      upper
# (Intercept)               -6.90572173 -0.1627088  6.5803042
# periodsecond              -1.41285427 -0.7952218 -0.1775894
# sex                       -3.46922537 -1.1005892  1.2680469
# age                       -2.64424816 -0.9346792  0.7748898
# quar.durationbasvs10days  -1.55539334  1.3249617  4.2053167
# quar.durationbasvs50days  -2.40096498  0.9431918  4.2873485
# quar.durationbasvs103days -2.40900582  0.7325885  3.8741828
# ment.dis.hist              1.93906068  4.4355652  6.9320698
# alone.or.accomp           -3.09503396  0.1645077  3.4240494
# ANEG.URG                   1.72203542  2.0985986  2.4751617
# APOS.URG                   0.32113746  0.7625527  1.2039679
# APERSEV.LACK               0.32395583  0.7417559  1.1595559
# APREMED.LACK               0.04708714  0.4868340  0.9265808
# ASENSATION.SEEKING        -0.74963081 -0.4525991 -0.1555674
# attr(,"label")
# [1] "Fixed effects:"
# 
# Random Effects:
#   Level: participant 
#                    lower    est.    upper
# sd((Intercept)) 8.844832 9.45928 10.11641
# 
# Within-group standard error:
#    lower     est.    upper 
# 5.083373 5.382900 5.700077 



# Calculating effect sizes
# library(DSUR.noof)
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# periodsecond
rcontrast(-2.513600, 585)
# [1] "r = 0.103367865911447"

# ment.dis.hist
rcontrast(3.468754, 573)
# [1] "r = 0.143411373768495"

# ANEG.URG
rcontrast(10.880489, 573)
# [1] "r = 0.41379803582344"

# APOS.URG
rcontrast(3.372713, 573)
# [1] "r = 0.139519046897944"

# APERSEV.LACK
rcontrast(3.466166, 573)
# [1] "r = 0.143306574277807"

# APREMED.LACK
rcontrast(2.161399, 573)
# [1] "r = 0.0899278935242262"

# ASENSATION.SEEKING
rcontrast(-2.974868, 573)
# [1] "r = 0.12332815674996"


## Central tendencies measures: Mean and standard deviation:

# Suicidal risk - time 1
mean(without$scores[without$variable=="ASUICRISK"])
# 27.87543
sd(without$scores[without$variable=="ASUICRISK"])
# 12.59683

# Suicidal risk - time 2
mean(without$scores[without$variable=="BSUICRISK"])
# 27.0802
sd(without$scores[without$variable=="BSUICRISK"])
# 13.56461

# Negative urgency
mean(without$ANEG.URG)
# 8.561433
sd(without$ANEG.URG)
# 2.579658

# Positive urgency
mean(without$APOS.URG)
# 6.375427
sd(without$APOS.URG)
# 2.289622

# (Lack of) Persverance
mean(without$APERSEV.LACK)
# 7.511945
sd(without$APERSEV.LACK)
# 2.231831

# (Lack of) Premeditation
mean(without$APREMED.LACK)
# 7.078498
sd(without$APREMED.LACK)
# 2.228757

# Sensation seeking
mean(without$ASENSATION.SEEKING)
# 9.153584
sd(without$ASENSATION.SEEKING)
# 3.042589

    

#### Group with suicidal ideation history
ideation<-subset(myDataISO, myDataISO$suic.att.hist==1)
ideationModel12NoInt<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG + APOS.URG + APERSEV.LACK + APREMED.LACK + ASENSATION.SEEKING, random = ~1|participant, data = ideation, method = "ML", control = lmeControl(opt = "optim"))
summary(ideationModel12NoInt)
# Linear mixed-effects model fit by maximum likelihood
# Data: ideation 
#      AIC      BIC    logLik
# 7981.772 8060.862 -3974.886
# 
# Random effects:
#   Formula: ~1 | participant
#         (Intercept) Residual
# StdDev:    12.55713  6.64145
# 
# Fixed effects: scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG + APOS.URG + APERSEV.LACK + APREMED.LACK + ASENSATION.SEEKING 
#                               Value Std.Error  DF   t-value p-value
# (Intercept)               11.384727  4.897462 517  2.324618  0.0205
# periodsecond              -1.613900  0.415497 517 -3.884267  0.0001 #
# sex                        0.187110  1.843490 505  0.101498  0.9192
# age                       -1.410401  1.209614 505 -1.165992  0.2442
# quar.durationbasvs10days   2.371735  1.983171 505  1.195931  0.2323
# quar.durationbasvs50days   4.341157  2.306229 505  1.882362  0.0604
# quar.durationbasvs103days  2.800660  2.277380 505  1.229772  0.2194
# ment.dis.hist              2.679503  1.309633 505  2.045995  0.0413 #
# alone.or.accomp           -0.310357  2.112944 505 -0.146884  0.8833
# ANEG.URG                   2.166837  0.247129 505  8.768031  0.0000 #
# APOS.URG                   0.057567  0.257584 505  0.223489  0.8232
# APERSEV.LACK               0.458841  0.275674 505  1.664435  0.0966
# APREMED.LACK               0.924391  0.303472 505  3.046047  0.0024 #
# ASENSATION.SEEKING        -0.397702  0.212179 505 -1.874375  0.0615
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -2.52571390 -0.45528246 -0.01906776  0.47838195  2.42744983 
# 
# Number of Observations: 1036
# Number of Groups: 518


intervals(ideationModel12NoInt)
# Approximate 95% confidence intervals
# 
# Fixed effects:
#                                lower        est.       upper
# (Intercept)                1.8285834 11.38472661 20.94086986
# periodsecond              -2.4246348 -1.61389961 -0.80316442
# sex                       -3.4101887  0.18710990  3.78440848
# age                       -3.7707844 -1.41040075  0.94998294
# quar.durationbasvs10days  -1.4981310  2.37173500  6.24160101
# quar.durationbasvs50days  -0.1591092  4.34115676  8.84142275
# quar.durationbasvs103days -1.6433126  2.80065973  7.24463208
# ment.dis.hist              0.1239464  2.67950281  5.23505922
# alone.or.accomp           -4.4334555 -0.31035707  3.81274136
# ANEG.URG                   1.6846006  2.16683698  2.64907332
# APOS.URG                  -0.4450692  0.05756716  0.56020349
# APERSEV.LACK              -0.0790957  0.45884146  0.99677862
# APREMED.LACK               0.3322096  0.92439137  1.51657314
# ASENSATION.SEEKING        -0.8117375 -0.39770227  0.01633298
# attr(,"label")
# [1] "Fixed effects:"
# 
# Random Effects:
#   Level: participant 
#                    lower     est.    upper
# sd((Intercept)) 11.70902 12.55713 13.46668
# 
# Within-group standard error:
#    lower     est.    upper 
# 6.249113 6.641450 7.058420 



# Calculating effect sizes
# library(DSUR.noof)
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# periodsecond
rcontrast(-3.884267, 517)
# [1] "r = 0.168390479900122"

# ment.dis.hist
rcontrast(2.045995, 505)
# [1] "r = 0.0906705604459675"

# ANEG.URG
rcontrast(8.768031, 505)
# [1] "r = 0.363484562490593"

# APREMED.LACK
rcontrast(3.046047, 505)
# [1] "r = 0.134319002253875"


## Central tendencies measures: Mean and standard deviation:

# Suicidal risk - time 1
mean(ideation$scores[ideation$variable=="ASUICRISK"])
# 43.57529
sd(ideation$scores[ideation$variable=="ASUICRISK"])
# 15.40156

# Suicidal risk - time 2
mean(ideation$scores[ideation$variable=="BSUICRISK"])
# 41.96139
sd(ideation$scores[ideation$variable=="BSUICRISK"])
# 16.5171

# Negative urgency
mean(ideation$ANEG.URG)
# 10.14672
sd(ideation$ANEG.URG)
# 2.658266

# Positive urgency
mean(ideation$APOS.URG)
# 7.241313
sd(ideation$APOS.URG)
# 2.578557

# (Lack of) Persverance
mean(ideation$APERSEV.LACK)
# 8.169884
sd(ideation$APERSEV.LACK)
# 2.406533

# (Lack of) Premeditation
mean(ideation$APREMED.LACK)
# 7.872587
sd(ideation$APREMED.LACK)
# 2.273813

# Sensation seeking
mean(ideation$ASENSATION.SEEKING)
# 9.552124
sd(ideation$ASENSATION.SEEKING)
# 3.031587



#### Group with suicide attempt history
attempt<-subset(myDataISO, myDataISO$suic.att.hist==2)
attemptModel12NoInt<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG + APOS.URG + APERSEV.LACK + APREMED.LACK + ASENSATION.SEEKING, random = ~1|participant, data = attempt, method = "ML", control = lmeControl(opt = "optim"))
summary(attemptModel12NoInt)
# Linear mixed-effects model fit by maximum likelihood
# Data: attempt 
#      AIC      BIC    logLik
# 1547.947 1600.397 -757.9734
# 
# Random effects:
#   Formula: ~1 | participant
#         (Intercept) Residual
# StdDev:    10.60591 7.895324
# 
# Fixed effects: scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + ANEG.URG + APOS.URG + APERSEV.LACK + APREMED.LACK + ASENSATION.SEEKING 
#                                Value Std.Error DF   t-value p-value
# (Intercept)                25.100814  9.146693 97  2.744250  0.0072
# periodsecond               -3.142857  1.170481 97 -2.685100  0.0085 #
# sex                        -3.953367  4.908446 85 -0.805421  0.4228
# age                        -1.492830  2.708194 85 -0.551227  0.5829
# quar.durationbasvs10days   -5.480536  4.014854 85 -1.365065  0.1758
# quar.durationbasvs50days   -2.508537  4.475328 85 -0.560526  0.5766
# quar.durationbasvs103days -11.240453  4.936639 85 -2.276944  0.0253 #
# ment.dis.hist              10.208703  3.053888 85  3.342855  0.0012 #
# alone.or.accomp            -2.696186  4.084254 85 -0.660142  0.5109
# ANEG.URG                    3.053348  0.614174 85  4.971473  0.0000 #
# APOS.URG                   -0.128955  0.596697 85 -0.216115  0.8294
# APERSEV.LACK               -0.361523  0.533432 85 -0.677731  0.4998
# APREMED.LACK                0.888883  0.578726 85  1.535930  0.1283
# ASENSATION.SEEKING         -0.508802  0.448064 85 -1.135557  0.2593
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -2.93637652 -0.50033407 -0.05769037  0.42430504  2.23847683 
# 
# Number of Observations: 196
# Number of Groups: 98 


intervals(attemptModel12NoInt)
# Approximate 95% confidence intervals
# 
# Fixed effects:
#                                 lower        est.      upper
# (Intercept)                 7.6075148  25.1008136 42.5941124
# periodsecond               -5.3814332  -3.1428571 -0.9042811
# sex                       -13.3576701  -3.9533670  5.4509361
# age                        -6.6815746  -1.4928301  3.6959143
# quar.durationbasvs10days  -13.1727680  -5.4805364  2.2116951
# quar.durationbasvs50days  -11.0830092  -2.5085365  6.0659362
# quar.durationbasvs103days -20.6987718 -11.2404527 -1.7821335
# ment.dis.hist               4.3576288  10.2087030 16.0597773
# alone.or.accomp           -10.5213839  -2.6961861  5.1290118
# ANEG.URG                    1.8766264   3.0533484  4.2300704
# APOS.URG                   -1.2721918  -0.1289551  1.0142815
# APERSEV.LACK               -1.3835483  -0.3615234  0.6605016
# APREMED.LACK               -0.2199233   0.8888832  1.9976898
# ASENSATION.SEEKING         -1.3672668  -0.5088021  0.3496626
# attr(,"label")
# [1] "Fixed effects:"
# 
# Random Effects:
#   Level: participant 
#                    lower     est.    upper
# sd((Intercept)) 8.832742 10.60591 12.73504
# 
# Within-group standard error:
#    lower     est.    upper 
# 6.863902 7.895324 9.081735 


# Descriptive statistics
by(attempt$scores, list(attempt$quar.duration), stat.desc, basic = FALSE)


# Calculating effect sizes
# library(DSUR.noof)
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# periodsecond
rcontrast(-2.685100, 97)
# [1] "r = 0.263030592070644"

# quar.durationbasvs103days
rcontrast(-2.276944, 85)
# [1] "r = 0.23976537242965"

# ment.dis.hist
rcontrast(3.342855, 85)
# [1] "r = 0.340868731963126"

# ANEG.URG
rcontrast(4.971473, 85)
# [1] "r = 0.474625431655934"


## Central tendencies measures: Mean and standard deviation:

# Suicidal risk - time 1
mean(attempt$scores[attempt$variable=="ASUICRISK"])
# 51.95918
sd(attempt$scores[attempt$variable=="ASUICRISK"])
# 17.22975

# Suicidal risk - time 2
mean(attempt$scores[attempt$variable=="BSUICRISK"])
# 48.81633
sd(attempt$scores[attempt$variable=="BSUICRISK"])
# 18.73161

# Negative urgency
mean(attempt$ANEG.URG)
# 10.76531
sd(attempt$ANEG.URG)
# 2.965791

# Positive urgency
mean(attempt$APOS.URG)
# 7.857143
sd(attempt$APOS.URG)
# 2.806586

# (Lack of) Persverance
mean(attempt$APERSEV.LACK)
# 8.163265
sd(attempt$APERSEV.LACK)
# 2.651343

# (Lack of) Premeditation
mean(attempt$APREMED.LACK)
# 8.091837
sd(attempt$APREMED.LACK)
# 2.773822

# Sensation seeking
mean(attempt$ASENSATION.SEEKING)
# 9.438776
sd(attempt$ASENSATION.SEEKING)
# 3.484961


##################################### SUICIDAL RISK #######################################
################# IMPULSIVITY-RELATED TRAITS MEASURED IN TIME 2 ###########################

####### TO ASSESS THE NEED FOR A MULTILEVEL MODEL WITH RANDOM EFFECTS BY SUICIDE ATTEMPT HISTORY

####### Preparing the data

# To select the variables from the "youngIMPT2" dataset: IMPULSIVITY HERE I USE THE SCORES FROM TIME 2
# library(dplyr)
youngsuicriskIMP2<-select(youngIMPT2, participant, sex, age, quar.duration, ment.dis.hist, suic.att.hist, alone.or.accomp, BNEG.URG, BPOS.URG, BPERSEV.LACK, BPREMED.LACK, BSENSATION.SEEKING, ASUICRISK, BSUICRISK)

# To convert the format of the dataframe into the long format:
# library(reshape)
myDataISOIMP2<-melt(youngsuicriskIMP2, id = c("participant","sex", "age", "quar.duration","ment.dis.hist", "suic.att.hist", "alone.or.accomp", "BNEG.URG", "BPOS.URG", "BPERSEV.LACK", "BPREMED.LACK", "BSENSATION.SEEKING"), measured = c("ASUICRISK","BSUICRISK"))

# To rename variables:
names(myDataISOIMP2)<-c("participant","sex", "age", "quar.duration","ment.dis.hist", "suic.att.hist", "alone.or.accomp", "BNEG.URG", "BPOS.URG", "BPERSEV.LACK", "BPREMED.LACK", "BSENSATION.SEEKING", "variable", "scores") # "variable" refers to the repeated-measures variable

# This creates a variable "period" in the dataframe myDataISOIMP2:
myDataISOIMP2$period<-gl(2, 1202, labels = c("first", "second")) # We created 2 sets of 1202 scores, the labels option then specifies the names to attach to these 2 sets, which correspond to the levels of "period" (first measurement and follow-up).

# To make it clearer that there are two observations for each participant we have sorted the data by participant:
myDataISOIMP2<-myDataISOIMP2[order(myDataISOIMP2$participant),]

####### MIXED DESIGNS AS A GLM

# Setting contrasts for quarantine sub-period:
myDataISOIMP2$quar.duration<-as.factor(myDataISOIMP2$quar.duration)
is.factor(myDataISOIMP2$quar.duration)

basvs10days<-c(0,1,0,0) # this compares the baseline (prior to quarantine) to a quarantine sub.period of up to 10-days duration
basvs50days<-c(0,0,1,0) # this compares the baseline (prior to quarantine) to a quarantine sub.period of up to 50-days duration
basvs103days<-c(0,0,0,1) # this compares the baseline (prior to quarantine) to a quarantine sub.period of up to 103-days duration
contrasts(myDataISOIMP2$quar.duration)<-cbind(basvs10days, basvs50days, basvs103days)
myDataISOIMP2$quar.duration # To check we setted the contrasts correctly 

# Building the model
# library(nlme)

interceptOnlySUICImpT2 <-gls(scores ~ 1, data = myDataISOIMP2, method = "ML") # only the intercept

randominterceptOnlySUICImpT2 <-lme(scores ~ 1, random = ~1|suic.att.hist, data = myDataISOIMP2, method = "ML") # Suicide attemtp history as a random effect

model1ImpT2<-lme(scores ~ 1, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim")) # the nested structure indicates repeated measures (within variable) # repeated measures nested within participants and participants nested within mental disorder history
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2)  # to compare models

# To see the overall effect of each main effect (additive and interaction) we added them to the model one at a time:

model2ImpT2<-lme(scores ~ period, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim"))
summary(model2ImpT2) # period is negatively related to suicidal risk
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2, model2ImpT2) #### Adding period significantly improved the fit of the model

model3ImpT2<-lme(scores ~ period + sex, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim"))
summary(model3ImpT2) # sex is not significantly related to suicidal risk
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2, model2ImpT2, model3ImpT2) #### Adding sex does not significantly improved the fit of the model

model4ImpT2<-lme(scores ~ period + sex + age, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim"))
summary(model4ImpT2) # age is negatively related to suicidal risk
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2, model2ImpT2, model3ImpT2, model4ImpT2) #### Adding age significantly improved the fit of the model

model5ImpT2<-lme(scores ~ period + sex + age + quar.duration, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim"))
summary(model5ImpT2) # quarantine duration is positively related to suicidal risk # 50.days p = 0.0326
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2, model2ImpT2, model3ImpT2, model4ImpT2, model5ImpT2) #### Adding quarantine duration does not significantly improved the fit of the model

model6ImpT2<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim"))
summary(model6ImpT2) # mental disorder history is positively related to suicidal risk
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2, model2ImpT2, model3ImpT2, model4ImpT2, model5ImpT2, model6ImpT2) #### Adding ment dis hist significantly improved the fit of the model

model7ImpT2<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim"))
summary(model7ImpT2) # alone or accompanied is not significantly related to suicidal risk
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2, model2ImpT2, model3ImpT2, model4ImpT2, model5ImpT2, model6ImpT2, model7ImpT2) #### Adding alone or accompanied does not improved the fit of the model

model8ImpT2<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim"))
summary(model8ImpT2) # negative urgency is positively related to suicidal risk
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2, model2ImpT2, model3ImpT2, model4ImpT2, model5ImpT2, model6ImpT2, model7ImpT2, model8ImpT2) #### Adding negative urgency significantly improved the fit of the model

model9ImpT2<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim"))
summary(model9ImpT2) # positive urgency is not significantly related to suicidal risk
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2, model2ImpT2, model3ImpT2, model4ImpT2, model5ImpT2, model6ImpT2, model7ImpT2, model8ImpT2, model9ImpT2) #### Adding positive urgency does not significantly improved the fit of the model

model10ImpT2<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG + BPERSEV.LACK, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim"))
summary(model10ImpT2) # lack of perseverance is positively related to suicidal risk
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2, model2ImpT2, model3ImpT2, model4ImpT2, model5ImpT2, model6ImpT2, model7ImpT2, model8ImpT2, model9ImpT2, model10ImpT2) #### Adding lack of perseverance significantly improved the fit of the model

model11ImpT2<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG + BPERSEV.LACK + BPREMED.LACK, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim"))
summary(model11ImpT2) # lack of premeditation is positively related to suicidal risk
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2, model2ImpT2, model3ImpT2, model4ImpT2, model5ImpT2, model6ImpT2, model7ImpT2, model8ImpT2, model9ImpT2, model10ImpT2, model11ImpT2) #### Adding lack of premeditation significantly improved the fit of the model

model12ImpT2<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG + BPERSEV.LACK + BPREMED.LACK + BSENSATION.SEEKING, random = ~1|suic.att.hist/participant, data = myDataISOIMP2, method = "ML", control = lmeControl(opt = "optim"))
summary(model12ImpT2) # sensation seeking is negatively related to suicidal risk
anova(interceptOnlySUICImpT2, randominterceptOnlySUICImpT2, model1ImpT2, model2ImpT2, model3ImpT2, model4ImpT2, model5ImpT2, model6ImpT2, model7ImpT2, model8ImpT2, model9ImpT2, model10ImpT2, model11ImpT2, model12ImpT2) #### Adding sensation seeking significantly improved the fit of the model
#                              Model df      AIC      BIC     logLik     Test   L.Ratio p-value
# interceptOnlySUICImpT2           1  2 20471.45 20483.02 -10233.726                           
# randominterceptOnlySUICImpT2     2  3 19811.64 19828.99  -9902.818   1 vs 2  661.8157  <.0001
# model1ImpT2                      3  4 18464.32 18487.46  -9228.161   2 vs 3 1349.3137  <.0001
# model2ImpT2                      4  5 18438.59 18467.52  -9214.297   3 vs 4   27.7290  <.0001
# model3ImpT2                      5  6 18437.52 18472.23  -9212.761   4 vs 5    3.0727  0.0796
# model4ImpT2                      6  7 18434.40 18474.89  -9210.200   5 vs 6    5.1215  0.0236
# model5ImpT2                      7 10 18435.57 18493.42  -9207.784   6 vs 7    4.8322  0.1845
# model6ImpT2                      8 11 18403.64 18467.27  -9190.820   7 vs 8   33.9274  <.0001
# model7ImpT2                      9 12 18405.49 18474.90  -9190.742   8 vs 9    0.1551  0.6937
# model8ImpT2                     10 13 18069.10 18144.30  -9021.549  9 vs 10  338.3866  <.0001
# model9ImpT2                     11 14 18070.32 18151.31  -9021.161 10 vs 11    0.7765  0.3782
# model10ImpT2                    12 15 18032.34 18119.11  -9001.168 11 vs 12   39.9849  <.0001
# model11ImpT2                    13 16 18017.77 18110.33  -8992.884 12 vs 13   16.5681  <.0001
# model12ImpT2                    14 17 18008.21 18106.55  -8987.105 13 vs 14   11.5587  0.0007

summary(model12ImpT2)
# Linear mixed-effects model fit by maximum likelihood
# Data: myDataISOIMP2 
#      AIC      BIC    logLik
# 18008.21 18106.55 -8987.105
# 
# Random effects:
# Formula: ~1 | suic.att.hist
#         (Intercept)
# StdDev:    6.147008
# 
# Formula: ~1 | participant %in% suic.att.hist
#         (Intercept) Residual
# StdDev:    10.88124 6.199859
# 
# Fixed effects: scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG + BPERSEV.LACK + BPREMED.LACK + BSENSATION.SEEKING 
#                               Value Std.Error   DF   t-value p-value
# (Intercept)                6.784422  4.462717 1201  1.520245  0.1287
# periodsecond              -1.339434  0.253637 1201 -5.280908  0.0000 #
# sex                       -0.957762  1.008201 1187 -0.949972  0.3423
# age                       -0.633270  0.691532 1187 -0.915750  0.3600
# quar.durationbasvs10days   1.354066  1.143372 1187  1.184274  0.2365
# quar.durationbasvs50days   2.059487  1.320812 1187  1.559259  0.1192
# quar.durationbasvs103days  1.545805  1.280814 1187  1.206892  0.2277
# ment.dis.hist              4.803159  0.844277 1187  5.689082  0.0000 #
# alone.or.accomp            1.347874  1.238091 1187  1.088671  0.2765
# BNEG.URG                   2.432795  0.147294 1187 16.516570  0.0000 #
# BPOS.URG                   0.153858  0.162475 1187  0.946969  0.3438
# BPERSEV.LACK               0.605291  0.156881 1187  3.858289  0.0001 #
# BPREMED.LACK               0.718372  0.175825 1187  4.085726  0.0000 #
# BSENSATION.SEEKING        -0.391965  0.115334 1187 -3.398525  0.0007 #
# 
# Standardized Within-Group Residuals:
#   Min          Q1         Med          Q3         Max 
# -3.41353344 -0.46205641 -0.01271642  0.44523899  3.46735976 
# 
# Number of Observations: 2404
# Number of Groups: 
# suic.att.hist participant %in% suic.att.hist 
#             3                           1202 

intervals(model12ImpT2)
# Approximate 95% confidence intervals
# 
# Fixed effects:
#                                lower       est.      upper
# (Intercept)               -1.9456336  6.7844225 15.5144786
# periodsecond              -1.8356043 -1.3394343 -0.8432642
# sex                       -2.9300475 -0.9577620  1.0145235
# age                       -1.9860742 -0.6332700  0.7195343
# quar.durationbasvs10days  -0.8826476  1.3540663  3.5907803
# quar.durationbasvs50days  -0.5243414  2.0594873  4.6433160
# quar.durationbasvs103days -0.9597798  1.5458045  4.0513889
# ment.dis.hist              3.1515487  4.8031594  6.4547700
# alone.or.accomp           -1.0741340  1.3478737  3.7698814
# BNEG.URG                   2.1446514  2.4327947  2.7209380
# BPOS.URG                  -0.1639814  0.1538583  0.4716980
# BPERSEV.LACK               0.2983942  0.6052911  0.9121879
# BPREMED.LACK               0.3744158  0.7183719  1.0623279
# BSENSATION.SEEKING        -0.6175858 -0.3919648 -0.1663439
# attr(,"label")
# [1] "Fixed effects:"
# 
# Random Effects:
# Level: suic.att.hist 
#                 lower     est.   upper
# sd((Intercept)) 2.721 6.147008 13.8867
# Level: participant 
#                    lower     est.    upper
# sd((Intercept)) 10.38194 10.88124 11.40455
# 
# Within-group standard error:
#    lower     est.    upper 
# 5.956912 6.199859 6.452714


# library(pastecs)
by(myDataISOIMP2$scores, list(myDataISOIMP2$period), stat.desc, basic = FALSE)
by(myDataISOIMP2$scores, list(myDataISOIMP2$period, myDataISOIMP2$ment.dis.hist), stat.desc, basic = FALSE)
by(myDataISOIMP2$scores, list(myDataISOIMP2$period, myDataISOIMP2$quar.duration), stat.desc, basic = FALSE)
by(myDataISOIMP2$scores, list(myDataISOIMP2$period, myDataISOIMP2$age), stat.desc, basic = FALSE)


# Calculating effect sizes
# library(DSUR.noof)

# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# periodsecond
rcontrast(-5.280908, 1201)
# [1] "r = 0.150644212617432"

# ment.dis.hist
rcontrast(5.689082, 1187)
# [1] "r = 0.162920305029464"

# BNEG.URG
rcontrast(16.516570, 1187)
# [1] "r = 0.432288434310142"

# BPERSEV.LACK
rcontrast(3.858289, 1187)
# [1] "r = 0.111291768936158"

# BPREMED.LACK
rcontrast(4.085726, 1187)
# [1] "r = 0.117763672959143"

# BSENSATION.SEEKING
rcontrast(-3.398525, 1187)
# [1] "r = 0.0981662940483493"



##### TO RUN THE FINAL MODEL IN EACH GROUP, SEPARATELY:

# Group without suicide attempt history
withoutIMPTT2<-subset(myDataISOIMP2, myDataISOIMP2$suic.att.hist==0)
withoutmodel11ImpT2NoInt<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG + BPERSEV.LACK + BPREMED.LACK + BSENSATION.SEEKING, random = ~1|participant, data = withoutIMPTT2, method = "ML", control = lmeControl(opt = "optim"))
summary(withoutmodel11ImpT2NoInt)
# Linear mixed-effects model fit by maximum likelihood
# Data: withoutIMPTT2 
# AIC      BIC    logLik
# 8429.929 8510.993 -4198.965
# 
# Random effects:
# Formula: ~1 | participant
#         (Intercept) Residual
# StdDev:    9.195536   5.3829
# 
# Fixed effects: scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG + BPERSEV.LACK + BPREMED.LACK + BSENSATION.SEEKING 
#                               Value Std.Error  DF   t-value p-value
# (Intercept)               -2.434449  3.369590 585 -0.722476  0.4703
# periodsecond              -0.795222  0.316368 585 -2.513601  0.0122 #
# sex                       -1.314826  1.188763 573 -1.106046  0.2692
# age                       -0.081865  0.851893 573 -0.096098  0.9235
# quar.durationbasvs10days   1.923999  1.442815 573  1.333504  0.1829
# quar.durationbasvs50days   1.316389  1.678887 573  0.784084  0.4333
# quar.durationbasvs103days  1.832859  1.565572 573  1.170728  0.2422
# ment.dis.hist              4.478664  1.239392 573  3.613598  0.0003 #
# alone.or.accomp            2.173001  1.636364 573  1.327945  0.1847
# BNEG.URG                   2.327085  0.181938 573 12.790533  0.0000 #
# BPOS.URG                   0.486569  0.217659 573  2.235466  0.0258 #
# BPERSEV.LACK               0.748061  0.198799 573  3.762907  0.0002 #
# BPREMED.LACK               0.352153  0.225691 573  1.560330  0.1192
# BSENSATION.SEEKING        -0.399439  0.144758 573 -2.759349  0.0060 #
# 
# Standardized Within-Group Residuals:
#          Min           Q1          Med           Q3          Max 
# -3.537721882 -0.470316212 -0.003275543  0.440424873  3.105977402 
# 
# Number of Observations: 1172
# Number of Groups: 586 


intervals(withoutmodel11ImpT2NoInt)
# Approximate 95% confidence intervals
# 
# Fixed effects:
#                                 lower        est.      upper
# (Intercept)               -9.01277003 -2.43444936  4.1438713
# periodsecond              -1.41285423 -0.79522184 -0.1775895
# sex                       -3.63570369 -1.31482622  1.0060513
# age                       -1.74505491 -0.08186533  1.5813242
# quar.durationbasvs10days  -0.89287571  1.92399896  4.7408736
# quar.durationbasvs50days  -1.96137931  1.31638885  4.5941570
# quar.durationbasvs103days -1.22367913  1.83285896  4.8893970
# ment.dis.hist              2.05894177  4.47866354  6.8983853
# alone.or.accomp           -1.02174898  2.17300128  5.3677515
# BNEG.URG                   1.97187881  2.32708496  2.6822911
# BPOS.URG                   0.06162361  0.48656919  0.9115148
# BPERSEV.LACK               0.35993698  0.74806069  1.1361844
# BPREMED.LACK              -0.08847475  0.35215263  0.7927800
# BSENSATION.SEEKING        -0.68205769 -0.39943904 -0.1168204
# attr(,"label")
# [1] "Fixed effects:"
# 
# Random Effects:
#   Level: participant 
#                    lower     est.    upper
# sd((Intercept)) 8.592964 9.195536 9.840362
# 
# Within-group standard error:
#    lower     est.    upper 
# 5.083370 5.382900 5.700079 



# Calculating effect sizes
# library(DSUR.noof)
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# periodsecond
rcontrast(-2.513601, 585)
# [1] "r = 0.103367906595482"

# ment.dis.hist
rcontrast(3.613598, 573)
# [1] "r = 0.149268959087291"

# BNEG.URG
rcontrast(12.790533, 573)
# [1] "r = 0.47127397026103"

# BPOS.URG
rcontrast(2.235466, 573)
# [1] "r = 0.0929833440257921"

# BPERSEV.LACK
rcontrast(3.762907, 573)
# [1] "r = 0.155290700971066"

# BSENSATION.SEEKING
rcontrast(-2.759349, 573)
# [1] "r = 0.114515132202507"


## Central tendencies measures: Mean and standard deviation:

# Negative urgency
mean(withoutIMPTT2$BNEG.URG)
# 8.353242
sd(withoutIMPTT2$BNEG.URG)
# 2.694803

# Positive urgency
mean(withoutIMPTT2$BPOS.URG)
# 6.467577
sd(withoutIMPTT2$BPOS.URG)
# 2.372589

# (Lack of) Persverance
mean(withoutIMPTT2$BPERSEV.LACK)
# 7.616041
sd(withoutIMPTT2$BPERSEV.LACK)
# 2.303456

# (Lack of) Premeditation
mean(withoutIMPTT2$BPREMED.LACK)
# 7.018771
sd(withoutIMPTT2$BPREMED.LACK)
# 2.147743

# Sensation seeking
mean(withoutIMPTT2$BSENSATION.SEEKING)
# 8.825939
sd(withoutIMPTT2$BSENSATION.SEEKING)
# 3.117468




# Group with suicidal ideation history
ideationIMPT2<-subset(myDataISOIMP2, myDataISOIMP2$suic.att.hist==1)
ideationmodel11ImpT2NoInt<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG + BPERSEV.LACK + BPREMED.LACK + BSENSATION.SEEKING, random = ~1|participant, data = ideationIMPT2, method = "ML", control = lmeControl(opt = "optim"))
summary(ideationmodel11ImpT2NoInt)
# Linear mixed-effects model fit by maximum likelihood
# Data: ideationIMPT2 
# AIC      BIC    logLik
# 7963.447 8042.537 -3965.724
# 
# Random effects:
#   Formula: ~1 | participant
#         (Intercept) Residual
# StdDev:    12.30586  6.64145
# 
# Fixed effects: scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG + BPERSEV.LACK + BPREMED.LACK + BSENSATION.SEEKING 
#                               Value Std.Error  DF   t-value p-value
# (Intercept)                9.323584  4.598578 517  2.027493  0.0431
# periodsecond              -1.613900  0.415497 517 -3.884267  0.0001 #
# sex                        0.251267  1.807152 505  0.139041  0.8895
# age                       -1.289226  1.191456 505 -1.082060  0.2797
# quar.durationbasvs10days   1.327339  1.949871 505  0.680732  0.4964
# quar.durationbasvs50days   2.886792  2.271667 505  1.270781  0.2044
# quar.durationbasvs103days  2.436290  2.238441 505  1.088387  0.2769
# ment.dis.hist              3.548314  1.292809 505  2.744656  0.0063 #
# alone.or.accomp            0.996888  2.067510 505  0.482169  0.6299
# BNEG.URG                   2.455840  0.259680 505  9.457190  0.0000 #
# BPOS.URG                   0.049542  0.266623 505  0.185812  0.8527
# BPERSEV.LACK               0.683081  0.279832 505  2.441038  0.0150 #
# BPREMED.LACK               0.589860  0.304131 505  1.939493  0.0530
# BSENSATION.SEEKING        -0.383627  0.200432 505 -1.914004  0.0562
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -2.62859450 -0.48152423 -0.01541533  0.47942328  2.39236141 
# 
# Number of Observations: 1036
# Number of Groups: 518 


intervals(ideationmodel11ImpT2NoInt)
# Approximate 95% confidence intervals
# 
# Fixed effects:
#                                  lower        est.        upper
# (Intercept)                0.350636923  9.32358362 18.296530310
# periodsecond              -2.424634760 -1.61389961 -0.803164468
# sex                       -3.275123965  0.25126740  3.777658769
# age                       -3.614177319 -1.28922644  1.035724442
# quar.durationbasvs10days  -2.477546089  1.32733936  5.132224811
# quar.durationbasvs50days  -1.546030906  2.88679163  7.319614171
# quar.durationbasvs103days -1.931697046  2.43629004  6.804277125
# ment.dis.hist              1.025588881  3.54831440  6.071039912
# alone.or.accomp           -3.037552571  0.99688831  5.031329194
# BNEG.URG                   1.949113768  2.45584046  2.962567151
# BPOS.URG                  -0.470734408  0.04954178  0.569817962
# BPERSEV.LACK               0.137029773  0.68308075  1.229131727
# BPREMED.LACK              -0.003606765  0.58986029  1.183327339
# BSENSATION.SEEKING        -0.774739783 -0.38362695  0.007485873
# attr(,"label")
# [1] "Fixed effects:"
# 
# Random Effects:
#   Level: participant 
#                    lower     est.   upper
# sd((Intercept)) 11.47021 12.30586 13.2024
# 
# Within-group standard error:
#    lower     est.    upper 
# 6.249099 6.641450 7.058435 



# Calculating effect sizes
# library(DSUR.noof)
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# periodsecond
rcontrast(-3.884267, 517)
# [1] "r = 0.168390479900122"

# ment.dis.hist
rcontrast(2.744656, 505)
# [1] "r = 0.121234700997747"

# BNEG.URG
rcontrast(9.457190, 505)
# [1] "r = 0.387890077743358"

# BPERSEV.LACK
rcontrast(2.441038, 505)
# [1] "r = 0.107989531073035"


## Central tendencies measures: Mean and standard deviation:

# Negative urgency
mean(ideationIMPT2$BNEG.URG)
# 9.787645
sd(ideationIMPT2$BNEG.URG)
# 2.640685

# Positive urgency
mean(ideationIMPT2$BPOS.URG)
# 7.142857
sd(ideationIMPT2$BPOS.URG)
# 2.683076

# (Lack of) Persverance
mean(ideationIMPT2$BPERSEV.LACK)
# 8.183398
sd(ideationIMPT2$BPERSEV.LACK)
# 2.371154

# (Lack of) Premeditation
mean(ideationIMPT2$BPREMED.LACK)
# 7.803089
sd(ideationIMPT2$BPREMED.LACK)
# 2.307691

# Sensation seeking
mean(ideationIMPT2$BSENSATION.SEEKING)
# 9.34749
sd(ideationIMPT2$BSENSATION.SEEKING)
# 3.197055



# Group with suicide attempt history
attemptIMPT2<-subset(myDataISOIMP2, myDataISOIMP2$suic.att.hist==2)
attemptmodel11ImpT2NoInt<-lme(scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG + BPERSEV.LACK + BPREMED.LACK + BSENSATION.SEEKING, random = ~1|participant, data = attemptIMPT2, method = "ML", control = lmeControl(opt = "optim"))
summary(attemptmodel11ImpT2NoInt)
# Linear mixed-effects model fit by maximum likelihood
# Data: attemptIMPT2 
# AIC      BIC   logLik
# 1536.972 1589.422 -752.486
# 
# Random effects:
# Formula: ~1 | participant
#         (Intercept) Residual
# StdDev:    9.862447 7.895274
# 
# Fixed effects: scores ~ period + sex + age + quar.duration + ment.dis.hist + alone.or.accomp + BNEG.URG + BPOS.URG + BPERSEV.LACK + BPREMED.LACK + BSENSATION.SEEKING 
#                               Value Std.Error DF   t-value p-value
# (Intercept)               13.317626  8.437448 97  1.578395  0.1177
# periodsecond              -3.142857  1.170473 97 -2.685116  0.0085 #
# sex                       -0.672398  4.493312 85 -0.149644  0.8814
# age                       -1.122365  2.570832 85 -0.436577  0.6635
# quar.durationbasvs10days  -3.633458  3.831192 85 -0.948388  0.3456
# quar.durationbasvs50days   0.128344  4.218910 85  0.030421  0.9758
# quar.durationbasvs103days -7.089943  4.668947 85 -1.518532  0.1326
# ment.dis.hist             12.297514  2.771887 85  4.436513  0.0000 #
# alone.or.accomp           -2.973480  4.005928 85 -0.742270  0.4600
# BNEG.URG                   2.382979  0.521712 85  4.567612  0.0000 #
# BPOS.URG                   0.259480  0.542510 85  0.478296  0.6337
# BPERSEV.LACK              -0.175345  0.461261 85 -0.380141  0.7048
# BPREMED.LACK               2.287409  0.532745 85  4.293629  0.0000 #
# BSENSATION.SEEKING        -0.733648  0.399884 85 -1.834654  0.0701
# 
# Standardized Within-Group Residuals:
#        Min         Q1        Med         Q3        Max 
# -2.7991034 -0.4013783 -0.1099828  0.4516117  2.8204681 
# 
# Number of Observations: 196
# Number of Groups: 98 


intervals(attemptmodel11ImpT2NoInt)
# Approximate 95% confidence intervals
# 
# Fixed effects:
#                                 lower       est.       upper
# (Intercept)                -2.8192214 13.3176260 29.45447331
# periodsecond               -5.3814191 -3.1428571 -0.90429517
# sex                        -9.2813275 -0.6723976  7.93653227
# age                        -6.0479313 -1.1223648  3.80320160
# quar.durationbasvs10days  -10.9738035 -3.6334580  3.70688743
# quar.durationbasvs50days   -7.9548461  0.1283442  8.21153453
# quar.durationbasvs103days -16.0353788 -7.0899435  1.85549181
# ment.dis.hist               6.9867356 12.2975136 17.60829153
# alone.or.accomp           -10.6486084 -2.9734799  4.70164849
# BNEG.URG                    1.3834083  2.3829791  3.38254991
# BPOS.URG                   -0.7799373  0.2594802  1.29889776
# BPERSEV.LACK               -1.0590949 -0.1753445  0.70840593
# BPREMED.LACK                1.2667004  2.2874093  3.30811816
# BSENSATION.SEEKING         -1.4998018 -0.7336478  0.03250618
# attr(,"label")
# [1] "Fixed effects:"
# 
# Random Effects:
# Level: participant 
#                    lower     est.    upper
# sd((Intercept)) 8.154044 9.862447 11.92879
# 
# Within-group standard error:
#    lower     est.    upper 
# 6.863866 7.895274 9.081668



# Calculating effect sizes
# library(DSUR.noof)
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# periodsecond
rcontrast(-2.685116, 97)
# [1] "r = 0.26303205098179"

# ment.dis.hist
rcontrast(4.436513, 85)
# [1] "r = 0.433615111898302"

# BNEG.URG
rcontrast(4.567612, 85)
# [1] "r = 0.443932503362264"

# BPREMED.LACK
rcontrast(4.293629, 85)
# [1] "r = 0.422172782985929"


## Central tendencies measures: Mean and standard deviation:

# Negative urgency
mean(attemptIMPT2$BNEG.URG)
# 10.23469
sd(attemptIMPT2$BNEG.URG)
# 2.892257

# Positive urgency
mean(attemptIMPT2$BPOS.URG)
# 7.214286
sd(attemptIMPT2$BPOS.URG)
# 2.767022

# (Lack of) Persverance
mean(attemptIMPT2$BPERSEV.LACK)
# 8.132653
sd(attemptIMPT2$BPERSEV.LACK)
# 2.999616

# (Lack of) Premeditation
mean(attemptIMPT2$BPREMED.LACK)
# 8.204082
sd(attemptIMPT2$BPREMED.LACK)
# 2.663944

# Sensation seeking
mean(attemptIMPT2$BSENSATION.SEEKING)
# 9.571429
sd(attemptIMPT2$BSENSATION.SEEKING)
# 3.546034

####################################################################################
############################### THE END ############################################
####################################################################################