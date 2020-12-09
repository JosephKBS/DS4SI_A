#
# Reproducing origianl analysis
# summary statistics

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
library(kableExtra)
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
