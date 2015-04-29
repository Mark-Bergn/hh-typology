library(data.table
        
        )

setwd('household-typology-27April/')


s2009 <- read.csv('s09.csv')
s2010 <- read.csv('s10.csv') 
s2011 <- read.csv('s11.csv')



table(s2009$age_grp)

#Change Sex labels
levels(s2009$sex) <- c("Female", "Male")
levels(s2010$sex) <- c("Female", "Male")
levels(s2011$sex) <- c("Female", "Male")

#Age group is fine

#Job Status
levels(s2009$jbstat)  <- c("Student", "Employed", "Unemployed", "Retired", "Employed", "Unemployed", "Unemployed")
levels(s2010$jbstat)  <- c("Student", "Employed", "Unemployed", "Retired", "Employed", "Unemployed", "Unemployed")
levels(s2011$jbstat)  <- c("Student", "Employed", "Unemployed", "Retired", "Employed", "Unemployed", "Unemployed")


#Qualification
levels(s2009$qual)  <- c("School", "University", "School", "University", "None", "Other", "Other", "School")
levels(s2010$qual) <- c("School", "University", "School", "University", "None", "Other", "Other", "School")
levels(s2011$qual) <- c("School", "University", "School", "University", "None", "Other", "Other", "School")


##Households

levels(s2009$tenure_grp2)  <- c("Renter", "Owner")
levels(s2010$tenure_grp2)  <- c("not applicable", "Renter", "Owner")
levels(s2011$tenure_grp2)  <- c("Renter", "Owner")


#Household Size is fine

levels(s2009$mastat)   <- c("Married or Couple", "Never Married", "Widowed/Divorced/Seperated")
levels(s2010$mastat)   <- c("Married or Couple", "Never Married", "Widowed/Divorced/Seperated")
levels(s2011$mastat)   <- c("Married or Couple", "Never Married", "Widowed/Divorced/Seperated")
s2009$mastat<- factor(s2009$mastat, levels=rev(levels(s2009$mastat)))
s2010$mastat<- factor(s2010$mastat, levels=rev(levels(s2010$mastat)))
s2011$mastat<- factor(s2011$mastat, levels=rev(levels(s2011$mastat)))

#Group number of kids

levels(s2009$nkids)  <- c("None", "1", "2", "3+", "3+", "3+", "not applicable")
levels(s2010$nkids)  <- c("None", "1", "2", "3+", "3+", "3+", "not applicable")
levels(s2011$nkids)  <- c("None", "1", "2", "3+", "3+", "3+", "not applicable")


write.csv(s2009, file='s09.csv')
write.csv(s2010, file='s10.csv')
write.csv(s2011, file='s11.csv')
