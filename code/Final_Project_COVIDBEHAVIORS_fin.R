##Title: Associations of Urbanicity and Sociodemographic Characteristics with Protective Health Behaviors and Reasons for Leaving the Home during COVID-19
# Purpose: Clean data and run analysis 

### Code from original analysis unless otherwise noted ###
### Adeola code: starts on line 407
### Joseph code: starts on line 471
### Andrew: combined individual team member code, updated relative directories, etc.

#Install packages 
library("dplyr")
library("here") # not in original code
library("kableExtra") # not in original code
library("tidyr")
library("lubridate")
library("tidyverse")
library("janitor")
library("oddsratio")
library("car")


### UPDATED TO REFLECT RELATIVE DIRECTORIES USING GITHUB DIRECTORY STRUCTURE ###

COVID_raw_2 <- read.csv(here::here("data", "COVIDraw2.0.csv"))

################################ ORIGINAL CODE ################################
### NOTE: Certain lines were removed due to reproduciblity issues, e.g. missing columns

# -------------------------------FILTER OUT OUTLIERS ------------------------------------

## select variables to analyze
COVID_clean <- COVID_raw_2

str(COVID_clean)

# outlier variables to NA
table(COVID_clean$localsiphours)
COVID_clean$localsiphours[COVID_clean$localsiphours == 528] <-NA 
COVID_clean$localsiphours[COVID_clean$localsiphours == 45] <-NA 

COVID_data<-COVID_clean

###------------------------------CREATE NEW VARIABLES-------------------------------------------------###

attach(COVID_data)

##factor sex and create labels
(sexf<- factor(sex))
(sexf <-factor(sex, labels= c("Male", "Female")))
summary(sexf)


## create age categories and turn to categorical variable
agegroup = cut(COVID_data$age, breaks= c(18,34,49,150)) 
levels(agegroup) = c("18-34", "35-49", ">=50")


## create household income (hhiincome) categories and turn to categorical variable
# 1,2,3,4,5 (<50,000k) 
# 6,7,8,9,10 (50k to <100K)  
# 11 (100k to 150k) 
# 12 (>150k) 
(hhincomef <- factor(hhincome))
(hhincomef <-factor(hhincome, labels =c ("<50k", "<50k", "<50k", "<50k", "<50k","50-<100k","50-<100k","50-<100k", "50-<100k", "50-<100k", "100-150k", ">150k")))
summary(hhincomef)
table(hhincomef)


## create new children binary variable (code as yes/no)
#0=no children
#1-6 = yes children
##(childrenf<- factor(hhchildren))
##(childrenf <-factor(hhchildren, labels= c("No", "Yes", "Yes","Yes","Yes","Yes", "Yes")))
##summary(childrenf)


## create new education binary variable
#1,3,4,5,7 = not a college graduate
#6 = college or more
table(educ)
(educf<-factor(educ))
(educf<-factor(educ, labels = c("Notcollegegraduate", "Notcollegegraduate", "Notcollegegraduate", "College","Notcollegegraduate")))
summary(educf)


## create new depression score binary variable
# none or mild = 0
# moderate or severe = 1
COVID_data <- COVID_data %>%
  mutate(depression_dichot= case_when(phq_sum <=9 ~ 0,
                                      phq_sum >=10 ~ 1))


## create new employment binary variable
# employed or student (1,2,6)
# unemployed or other (3,4,5,7,8)
(employf<-factor(employ1))
(employf<-factor(employ1, labels = c("employed", "employed", "unemployed", "unemployed","unemployed", "employed","unemployed","unemployed")))
summary(educf)


## create essential and nonessential services binary variable
#1= essential 
#2 =nonessential

#mutate to create a new essential services variable with employment or unemployment status 
COVID_data <- COVID_data %>%
  mutate(essentialnew= case_when(employ1 ==1 & essntlsrvcs== 1 ~ 1,
                                 employ1 ==1 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==1 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==2 & essntlsrvcs== 1 ~ 1,
                                 employ1 ==2 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==2 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==3 & essntlsrvcs== 1 ~ 2,
                                 employ1 ==3 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==3 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==4 & essntlsrvcs== 1 ~ 2,
                                 employ1 ==4 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==4 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==5 & essntlsrvcs== 1 ~ 2,
                                 employ1 ==5 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==5 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==6 & essntlsrvcs== 1 ~ 1,
                                 employ1 ==6 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==6 & essntlsrvcs== 7 ~ 2,
                                 
                                
                                 employ1 ==7 & essntlsrvcs== 1 ~ 2,
                                 employ1 ==7 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==7 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==8 & essntlsrvcs== 1 ~ 2,
                                 employ1 ==8 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==8 & essntlsrvcs== 7 ~ 2,))
                                
  
(essentialnewf<-factor(COVID_data$essentialnew))
(essentialnewf<-factor(COVID_data$essentialnew, labels = c("essential", "nonessential")))

## create new race/ethnicity binary variable
#nonwhite=1
#nonhispanicwhite=2
COVID_data <- COVID_data %>%
  mutate(racenew= case_when(ethnicity ==1 & race== 0 ~ 1,
                            ethnicity ==1 & race== 1 ~ 1,
                            ethnicity ==1 & race== 2 ~ 1,
                            ethnicity ==1 & race== 3 ~ 1,
                            ethnicity ==1 & race== 4 ~ 1,
                            ethnicity ==1 & race== 5 ~ 1,
                            ethnicity ==1 & race== 6 ~ 1,
                            ethnicity ==2 & race== 0 ~ 1,
                            ethnicity ==2 & race== 1 ~ 1,
                            ethnicity ==2 & race== 2 ~ 1,
                            ethnicity ==2 & race== 3 ~ 1,
                            ethnicity ==2 & race== 4 ~ 2,
                            ethnicity ==2 & race== 5 ~ 1,
                            ethnicity ==2 & race== 6 ~ 1,))


(racenewf<-factor(COVID_data$racenew))
(racenewf<-factor(COVID_data$racenew, labels = c("nonwhite", "nonhispanicwhite")))                         
levels(racenewf)


## create new depression severity binary variable
(depressionnewf<-factor(COVID_data$depression_dichot))
(depressionnewf<- factor(COVID_data$depression_dichot, labels = c("noneormild", "moderateorsevere")))
levels(depressionnewf)


## create new variable for localsiphours (>= 23 hours at home)
#compliant = 1
#notcompliant = 0

COVID_data<-COVID_data %>%
  mutate(localsiphours_1 = case_when(localsiphours >=23 ~1,
                                     localsiphours <=22 ~ 0))


## create a new variable that is a sum of leavehomeact variables and spending time at home#
COVID_data <- COVID_data %>%
  mutate(leavehomeacttotnew = leavehomeact___1+leavehomeact___2+leavehomeact___3+leavehomeact___4+leavehomeact___5+leavehomeact___6+localsiphours_1)


## create a new variable that is a sum of leavehome variables#
COVID_data <- COVID_data %>%
  mutate(leavehometotnew = leavehomereason___1+leavehomereason___3+leavehomereason___4+leavehomereason___5+leavehomereason___6)

COVID_data <- data.frame(COVID_data, sexf, agegroup, hhincomef, educf, racenewf, depressionnewf)



###------------------------------ANALYSIS-------------------------------------------------###
# filter out covid sick outcome if still sick and limited functioning 
COVID_data_sample <- COVID_data %>%
  filter(localsip==1) %>%
  filter(is.na(covidsickoutcome) | covidsickoutcome !=2) %>%
  filter(is.na(dis_alone) | dis_alone !=1) 


### filter out not essential workers
COVID_data_notessential <- COVID_data_sample %>%
  filter(is.na(essntlsrvcs) | essntlsrvcs !=1)

## descriptive analyses for each characteristic
summary(COVID_data_notessential$age, na.rm=TRUE)
sd(COVID_data_notessential$age, na.rm=TRUE)
      
tabyl(COVID_data_notessential$state, sort = true)

tabyl(COVID_data_notessential$Classification, sort = true)

tabyl(COVID_data_notessential$sexf, sort = true)

tabyl(COVID_data_notessential$racenewf, sort = true)

tabyl(COVID_data_notessential$agegroup, sort = true)

tabyl(COVID_data_notessential$educf, sort = true)

tabyl(COVID_data_notessential$hhincomef, sort = true)

tabyl(COVID_data_notessential$depression_dichot, sort = true)

tabyl(COVID_data_notessential$comorbid, sort = true)

summary(COVID_data_notessential$leavehometotnew)
sd(COVID_data_notessential$leavehometotnew)

summary(COVID_data_notessential$leavehomeacttotnew, na.rm=TRUE)
sd(COVID_data_notessential$leavehomeacttotnew, na.rm=TRUE)


### frequencies for reasons for staying home and public health practices###
tabyl(COVID_data_notessential$leavehomeact___1, sort = true)

tabyl(COVID_data_notessential$leavehomeact___2, sort = true)

tabyl(COVID_data_notessential$leavehomeact___3, sort = true)

tabyl(COVID_data_notessential$leavehomeact___4, sort = true)

tabyl(COVID_data_notessential$leavehomeact___5, sort = true)

tabyl(COVID_data_notessential$leavehomeact___6, sort = true)

tabyl(COVID_data_notessential$localsiphours_1, sort = true)


####### logisitic regression #######

## check reference and levels for each categorical variable
contrasts(as.factor(Classification))
contrasts(agegroup)
contrasts(hhincomef)
##contrasts(childrenf)
contrasts(depressionnewf)
contrasts(educf)

##There's an unexplained length difference in comorbid and comorbidf
## create new comorbid binary variable 
comorbidf <- factor(COVID_data_notessential$comorbid)
levels(comorbidf) = c("1 or more", "None")
table(comorbid, comorbidf)
contrasts(comorbidf)


## change reference for comorbid and depression severity 
comorbidf_relevel <- relevel(comorbidf, ref = "None")
COVID_data_notessential$depression_dichotf <- factor(COVID_data_notessential$depression_dichot)
levels(COVID_data_notessential$depression_dichotf) = c("Mildnone", "Modsevere")


## change reference categories for cateogrical variables
Classification1 <-relevel(as.factor(COVID_data_notessential$Classification), ref="Urban")
levels(Classification1)

edu1 <-relevel(COVID_data_notessential$educf, ref="College")
levels(edu1)
class(leavehomereason___1)

### Adjusted logistic regression For Leaving Home and association with each characteristic ###
# work
fullmod_work <- glm(leavehomereason___1 ~ Classification1 + sexf + agegroup + edu1 + hhincomef + depression_dichotf + comorbidf_relevel, 
                          data = COVID_data_notessential,
                          family = "binomial")
summary(fullmod_work)
##multicollinearity##
vif(fullmod_work) 
or_glm(data=COVID_data_notessential,model=fullmod_work)


# grocery shopping
fullmod_grocery <- glm(leavehomereason___3 ~ Classification1 + sexf + agegroup + edu1 + hhincomef + depression_dichotf + comorbidf_relevel, 
                    data = COVID_data_notessential,
                    family = "binomial")
summary(fullmod_grocery)
##multicollinearity##
vif(fullmod_grocery) 
or_glm(data=COVID_data_notessential,model=fullmod_grocery)


# other essential shopping
fullmod_othershop <- glm(leavehomereason___4 ~ Classification1 + sexf + agegroup + edu1 + hhincomef + depression_dichotf + comorbidf_relevel, 
                       data = COVID_data_notessential,
                       family = "binomial")
summary(fullmod_othershop)
##multicollinearity##
vif(fullmod_othershop)
or_glm(data=COVID_data_notessential,model=fullmod_othershop)


# exercise
fullmod_exercise <- glm(leavehomereason___5 ~ Classification1 + sexf + agegroup + edu1 + hhincomef + depression_dichotf + comorbidf_relevel, 
                         data = COVID_data_notessential,
                         family = "binomial")
summary(fullmod_exercise)
##multicollinearity##
vif(fullmod_exercise)
or_glm(data=COVID_data_notessential,model=fullmod_exercise)


# walking dog
fullmod_dog <- glm(leavehomereason___6 ~ Classification1 + sexf + agegroup + edu1 + hhincomef + depression_dichotf + comorbidf_relevel, 
                        data = COVID_data_notessential,
                        family = "binomial")
summary(fullmod_dog)
##multicollinearity##
vif(fullmod_dog) 
or_glm(data=COVID_data_notessential,model=fullmod_dog)


##adjusted logistic regression model Protective Health Behaviors and association with each characteristic ##
# spending less than one hour outside home
fullmod_leavehome <- glm(localsiphours_1 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + depression_dichotf + comorbidf_relevel, 
                         data = COVID_data_notessential,
                         family = "binomial")
summary(fullmod_leavehome)
#multicollinearity
vif(fullmod_leavehome) 
or_glm(data=COVID_data_notessential,model=fullmod_leavehome)


# physical/social distancing
fullmod_socialdist <- glm(leavehomeact___1 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + depression_dichotf + comorbidf_relevel, 
                          data = COVID_data_notessential,
                          family = "binomial")
summary(fullmod_socialdist)
##multicollinearity##
vif(fullmod_socialdist)
or_glm(data=COVID_data_notessential,model=fullmod_socialdist)


# protective mask
fullmod_mask <- glm(leavehomeact___2 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + depression_dichotf + comorbidf_relevel, 
                    data = COVID_data_notessential,
                    family = "binomial")
summary(fullmod_mask)
##multicollinearity##
vif(fullmod_mask)
or_glm(data=COVID_data_notessential,model=fullmod_mask)


# wearing gloves
fullmod_gloves <- glm(leavehomeact___3 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + depression_dichotf + comorbidf_relevel, 
                      data = COVID_data_notessential,
                      family = "binomial")
summary(fullmod_gloves)
##multicollinearity##
vif(fullmod_gloves)
or_glm(data=COVID_data_notessential,model=fullmod_gloves)


# using hand sanitizer
fullmod_santizer <- glm(leavehomeact___4 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + depression_dichotf + comorbidf_relevel, 
                        data = COVID_data_notessential,
                        family = "binomial")
summary(fullmod_santizer)
##multicollinearity##
vif(fullmod_santizer) 
or_glm(data=COVID_data_notessential,model=fullmod_santizer)


# using disinfectant wipes
fullmod_wipes<- glm(leavehomeact___5 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + depression_dichotf + comorbidf_relevel, 
                    data = COVID_data_notessential,
                    family = "binomial")
summary(fullmod_wipes)
##multicollinearity##
vif(fullmod_wipes) 
or_glm(data=COVID_data_notessential,model=fullmod_wipes)


# washing hands frequently
fullmod_hands<- glm(leavehomeact___6 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + depression_dichotf + comorbidf_relevel, 
                    data = COVID_data_notessential,
                    family = "binomial")
summary(fullmod_hands)
##multicollinearity##
vif(fullmod_hands) 
or_glm(data=COVID_data_notessential,model=fullmod_hands)

################################ ADEOLA: CODE ################################
#Our two new models

## create new binary variable for leavehomeacttotnew
# not cautious = 0
# cautious = 1
COVID_data_notessential <- COVID_data_notessential %>%
  mutate(leavehomeacttotnew_dichot= case_when(leavehomeacttotnew <=4 ~ 0,
                                              leavehomeacttotnew >=5 ~ 1))

##dichotomous version of leavehomeacttotnew#

fullmod_cautious<- glm(leavehomeacttotnew_dichot ~ Classification1 + sexf + age + hhincome + edu1 + phq_sum + comorbidf_relevel, 
                    data = COVID_data_notessential,
                    family = "binomial")
summary(fullmod_cautious)
##multicollinearity##
vif(fullmod_cautious) 
cautious_glm_or = or_glm(data=COVID_data_notessential,model=fullmod_cautious,incr = list(age = 1, hhincome = 1,phq_sum = 1))

#provide care for someone else

fullmod_care<- glm(leavehomereason___2 ~ Classification1 + sexf + age + hhincome + edu1 + phq_sum  + comorbidf_relevel, 
                    data = COVID_data_notessential,
                    family = "binomial")
summary(fullmod_care)
##multicollinearity##
vif(fullmod_care) 
care_glm_or = or_glm(data=COVID_data_notessential,model=fullmod_care,incr = list(age = 1, hhincome = 1,phq_sum = 1))

#Replicate bar plot
barchart.probs = c(0.8822685, 0.6740847, 0.4113424, 0.3725772, 0.09404164)
names(barchart.probs)= c("Grocery Shopping","Exercise","Walking Dog","Other Essential Shopping",
                         "Work")
barchart.percents = 100*barchart.probs

barplot(barchart.percents, xlab = c("Reasons For Leaving Home"),ylab = "Frequency (%)")

#Cautious GLM odds ratios table
cautious_GLM_OR_table = cautious_glm_or[,2:4]


#Care GLM odds ratios table
care_GLM_OR_table = care_glm_or[,2:4]

new.models.results.table = data.frame(rbind(c("0.833 (0.606, 1.145)","0.739 (0.407, 1.293)"),
                                            c("1.037 (0.784, 1.370)","0.883 (0.546, 1.416)"),
                                            c("1.957 (1.502, 2.554)","1.028 (0.647, 1.675)"),
                                            c("1.015 (1.006, 1.025)","1.025 (1.009, 1.041)"),
                                            c("1.051 (1.006, 1.098)","0.989 (0.918, 1.071)"),
                                            c("1.017 (0.727, 1.430)","0.867 (0.454, 1.547)"),
                                            c("0.993 (0.969, 1.018)","1.024 (0.982, 1.067)"),
                                            c("1.242 (0.962, 1.605)","1.124 (0.725, 1.736)")))
colnames(new.models.results.table) = c("Adjusted Odds Ratio (95% CI)","Adjusted Odds Ratio (95% CI)")
rownames(new.models.results.table) = c("Are you rural? (1 = Yes, 0 = No)",
                                       "Are you suburban? (1 = Yes, 0 = No)",
                                       "Female? (1 = Yes, 0 = No) ",
                                       "Age",
                                       "Household Income Level",
                                       "Are you a college graduate? (1 = Yes, 0 = No)",
                                       "Total PHQ-9 Score",
                                       "Do you have at least one comorbidity? (1 = Yes, 0 = No)")


################################ JOSEPH: CODE ################################

####################################################################
# 1. summary statistics
# ----------------forming a table structure -------------------------
table <- data.frame(matrix(NA, nrow=28, ncol=1 ))

colnames(table) <- c("total % (number)")

r <- c("Urban","Suburban","Rural","Missing",
       "Male","Female","Missing ",
       "Non-hispanic white","Non-white"," Missing",
       "18-34","35-49","50 and older","Missing    ",
       "Not a college degree","college graduate or more"," Missing ",
       "<50k","50k-100k","100k-150k",">150k"," Missing  ",
       "None/mild","moderate/severe","  Missing  ",
       "none","one or more","  Missing   ")
rownames(table) <- r

# ----------------data cleaning-------------------------------------
COVID_data_sample <- COVID_data %>%
  filter(localsip==1) %>%
  #filter(!is.na(Zip)) %>%
  filter(is.na(covidsickoutcome) | covidsickoutcome !=2) %>%
  filter(is.na(dis_alone) | dis_alone !=1) %>%
  filter(is.na(essntlsrvcs) | essntlsrvcs !=1)

# ----------------data for summary table-------------------------------------
u1<- length(na.omit( COVID_data_sample$Classification[COVID_data_sample$Classification=="Urban"]) )
u2<- length(na.omit( COVID_data_sample$Classification[COVID_data_sample$Classification=="Suburban"]))
u3<- length(na.omit( COVID_data_sample$Classification[COVID_data_sample$Classification=="Rural"]) )
u4<- length((COVID_data_sample$Classification[COVID_data_sample$Classification=="NA"]) )

s1 <- length(na.omit(COVID_data_sample$sex[COVID_data_sample$sex==1]) )
s2 <- length(na.omit(COVID_data_sample$sex[COVID_data_sample$sex==2]) )
s3<- length(COVID_data_sample$sex[COVID_data_sample$sex=="NA"])

r1 <- length(na.omit(COVID_data_sample$race[COVID_data_sample$race==4]) )
r2 <- length(na.omit(COVID_data_sample$race[COVID_data_sample$race==c(0,1,2,3,5)]) )
r3 <- length((COVID_data_sample$race[COVID_data_sample$race=="NA"]) )

a1 <- length(na.omit(COVID_data_sample$agegroup[COVID_data_sample$agegroup=="18-34"]) )
a2<- length(na.omit(COVID_data_sample$agegroup[COVID_data_sample$agegroup=="35-49"]) )
a3 <- length(na.omit(COVID_data_sample$agegroup[COVID_data_sample$agegroup==">=50"]) )
a4 <- length(COVID_data_sample$agegroup[COVID_data_sample$agegroup=="NA"])

e1 <- length(na.omit( COVID_data_sample$educf[COVID_data_sample$educf=="Notcollegegraduate"]) )
e2 <- length(na.omit( COVID_data_sample$educf[COVID_data_sample$educf=="College" ]) )
e3 <- length( COVID_data_sample$educf[COVID_data_sample$educf=="NA"])

i1 <- length(na.omit( COVID_data_sample$hhincomef[COVID_data_sample$hhincomef=="<50k"]) )
i2 <- length(na.omit( COVID_data_sample$hhincomef[COVID_data_sample$hhincomef=="50-<100k"]) )
i3 <- length(na.omit( COVID_data_sample$hhincomef[COVID_data_sample$hhincomef=="100-150k"]) )
i4 <- length(na.omit( COVID_data_sample$hhincomef[COVID_data_sample$hhincomef==">150k"]) )
i5 <- length(COVID_data_sample$hhincomef[COVID_data_sample$hhincomef=="NA"])

depress <- c("noneormild","moderateorsevere")
d1<- length(na.omit(COVID_data_sample$depressionnewf[COVID_data_sample$depressionnewf=="noneormild"]) )
d2 <- length(na.omit(COVID_data_sample$depressionnewf[COVID_data_sample$depressionnewf=="moderateorsevere"]) )
d3 <- length(COVID_data_sample$depressionnewf[COVID_data_sample$depressionnewf=="NA"])

c1 <- length(na.omit(COVID_data_sample$comorbid[COVID_data_sample$comorbid==2]) )
c2 <- length(na.omit(COVID_data_sample$comorbid[COVID_data_sample$comorbid==1]) )
c3 <- length((COVID_data_sample$comorbid[COVID_data_sample$comorbid=="NA"]) )

# ----------------summary table-------------------------------------
i <- 1
table[i+0,1] <- paste( round( (u1/(u1+u2+u3+u4)), digits = 2)*100, "(",u1,")"    )
table[i+1,1] <- paste( round( (u2/(u1+u2+u3+u4)), digits = 2)*100, "(",u2, ")"    )
table[i+2,1] <- paste( round( (u3/(u1+u2+u3+u4)), digits = 2)*100, "(",u3, ")"    )
table[i+3,1] <- paste( round( (u4/(u1+u2+u3+u4)), digits = 2)*100, "(",u4, ")"    )

table[i+4,1] <- paste( round( (s1/(s1+s2+s3)), digits = 2)*100, "(",s1, ")"    )
table[i+5,1] <- paste( round( (s2/(s1+s2+s3)), digits = 2)*100, "(",s2, ")"    )
table[i+6,1] <- paste( round( (s3/(s1+s2+s3)), digits = 2)*100, "(",s3, ")"    )

table[i+7,1] <- paste( round( (r1/(r1+r2+r3)), digits = 2)*100, "(",r1, ")"    )
table[i+8,1] <- paste( round( (r2/(r1+r2+r3)), digits = 2)*100, "(",r2, ")"    )
table[i+9,1] <- paste( round( (r3/(r1+r2+r3)), digits = 2)*100, "(",r3, ")"    )

table[i+10,1] <- paste( round( (a1/(a1+a2+a3+a4)), digits = 2)*100, "(",a1,")"    )
table[i+11,1] <- paste( round( (a2/(a1+a2+a3+a4)), digits = 2)*100, "(",a2 ,")"   )
table[i+12,1] <- paste( round( (a3/(a1+a2+a3+a4)), digits = 2)*100, "(",a3 ,")"  )
table[i+13,1] <- paste( round( (a4/(a1+a2+a3+a4)), digits = 2)*100, "(",a4 ,")"   )

table[i+14,1] <- paste( round( (e1/(e1+e2+e3)), digits = 2)*100, "(",e1, ")"    )
table[i+15,1] <- paste( round( (e2/(e1+e2+e3)), digits = 2)*100, "(",e2, ")"    )
table[i+16,1] <- paste( round( (e3/(e1+e2+e3)), digits = 2)*100, "(",e3, ")"    )

table[i+17,1] <- paste( round( (i1/(i1+i2+i3+i4+i5)), digits = 2)*100, "(",i1, ")"    )
table[i+18,1] <- paste( round( (i2/(i1+i2+i3+i4+i5)), digits = 2)*100, "(",i2 , ")"   )
table[i+19,1] <- paste( round( (i3/(i1+i2+i3+i4+i5)), digits = 2)*100, "(",i3 , ")"   )
table[i+20,1] <- paste( round( (i4/(i1+i2+i3+i4+i5)), digits = 2)*100, "(",i4, ")"    )
table[i+21,1] <- paste( round( (i5/(i1+i2+i3+i4+i5)), digits = 2)*100, "(",i5, ")"    )

table[i+22,1] <- paste( round( (d1/(d1+d2+d3)), digits = 2)*100, "(",d1, ")"    )
table[i+23,1] <- paste( round( (d2/(d1+d2+d3)), digits = 2)*100, "(",d2, ")"    )
table[i+24,1] <- paste( round( (d3/(d1+d2+d3)), digits = 2)*100, "(",d3, ")"    )

table[i+25,1] <- paste( round( (c1/(c1+c2+c3)), digits = 2)*100, "(",c1, ")"    )
table[i+26,1] <- paste( round( (c2/(c1+c2+c3)), digits = 2)*100, "(",c2, ")"    )
table[i+27,1] <- paste( round( (c3/(c1+c2+c3)), digits = 2)*100, "(",c3, ")"   )


# ----------------plotting-------------------------------------
kbl(table, caption = "Participant characteristics of the sample (N=1393)") %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Urbancity", 1, 4) %>%
  pack_rows("Sex", 5, 7) %>%
  pack_rows("Race", 8, 10)  %>%
  pack_rows("Age", 11, 14) %>%
  pack_rows("Education", 15, 17) %>%
  pack_rows("Income", 18, 22) %>%
  pack_rows("Depressive Symptom Severity", 23, 25) %>%
  pack_rows("Comorbidity", 26, 28)
