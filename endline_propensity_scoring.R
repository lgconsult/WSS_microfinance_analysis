#Rscript for endline survey

install.packages("readr")
install.packages("dplyr")
install.packages("Matching")

# Step 1: Load the 'readr' package and load the relevant endline survey data files into the R environment by specifying the correct file path. 

library(readr)
endline_data <- read.csv("C:/Users/luke.gates/OneDrive - Castalia/Documents/Bangladesh Microfinance WSS/resources/endline_survey_ikea_bangladesh.csv")

#Step 2.1: Rename columns to eliminate spaces
names(endline_data) <- make.names(names(endline_data), unique=TRUE)


#Step 2.2: Subset 'endline_data' dataset to only include the respondent 
endline_data_subset <- endline_data[which(endline_data$Index=="1"),]

#Step3: Select only the relevant variables for the analysis from the 'Finaldata' dataset.

#our treatment variable in the 'Finaldata' dataset is 'ic5__3_' i.e. is your loan approved?
#we can create our treatment variable by applying the following data transformation. 

endline_data_subset$treatment <- ifelse(endline_data_subset$IC6=="1",1,0)
endline_data_subset$treatment[which(is.na(endline_data_subset$treatment))] <- 0

#household size is not measured but can be calculated by counting the number of responses for each 'member id' and 'qslno' from the 'endline_data' dataset. 

householdcount_ <- aggregate(A3~A4, FUN = length, data=endline_data)

#in the  'householdcount' data set, the 'mfi_member_ID' column refers to the household size count. We will use this data to create a variable 'householdsize' in the 'Finaldata' dataset.
endline_data_subset$householdsize <- householdcount_$A3[match(endline_data_subset$A4, householdcount_$A4)]

#subsetting our data for the variables we need
endline_dataforanalysis <- endline_data_subset[,c(16,25,73,74,75,77,374,58,424,246),]

#We can remove any rows that contains NA values as that won't be used in the analysis. This will give us our final dataset for analysis. 
#endline_dataforanalysis <- na.omit(endline_dataforanalysis)

#change column names so they match baseline data

colnames(endline_dataforanalysis) <- c("qslno", "rural_urban", "gender", "age", "education", "marital_status", "household_savings", "toilet", "household_size", "loan_approved")

# eliminate blank results from household_savings column

#endline_dataforanalysis<- endline_dataforanalysis[endline_dataforanalysis$household_savings !="",]

# compare baseline to endline data to determine what fields need to be transformed and their scales
# columns to transform: education and marital status

# Transform education column to numeric values using same scale as baseline data

#baseline data scale
# 1 = "No formal education"
# 2 = "Primary school (Grade 5 or below)"
# 3 = "Junior high school (Grade 6-8)"
# 4 =  "SSC (Grade 9-10)"
# 5 = "HSC (Grade 11-12)"
# 6 = "Bachelor and above"

endline_dataforanalysis[endline_dataforanalysis == "No formal education"] <- 1
endline_dataforanalysis[endline_dataforanalysis == "Primary school (Grade 5 or below)"] <- 2
endline_dataforanalysis[endline_dataforanalysis == "Junior high school (Grade 6-8)"] <- 3
endline_dataforanalysis[endline_dataforanalysis == "SSC (Grade 9-10)"] <- 4
endline_dataforanalysis[endline_dataforanalysis == "HSC (Grade 11-12)"] <- 5
endline_dataforanalysis[endline_dataforanalysis == "Bachelor and above"] <- 6

# Transform marital status column to numeric values using same scale as baseline

#baseline marital status scale
# 1 = "Married"
# 2 = "Unmarried/single"
# 3 = "Divorced/separated"
# 4 = "Widow/widower"

endline_dataforanalysis[endline_dataforanalysis == "Married"] <- 1
endline_dataforanalysis[endline_dataforanalysis == "Unmarried/single"] <- 2
endline_dataforanalysis[endline_dataforanalysis == "Divorced/separated"] <- 3
endline_dataforanalysis[endline_dataforanalysis == "Widow/widower"] <- 4

# write file to csv to compare to baseline for data manipulation
write.csv(endline_dataforanalysis, "C:/Users/luke.gates/Castalia/endline_dataforanalysis.csv", row.names=FALSE)

#Step 4: Estimate a probit model to generate the propensity scores.
#The propensity score is the conditional (predicted) probability of receiving treatment given pre-treatment characteristics such as (age, family size, etc)

#We hypothesize that age, gender education status, marital status, household size, rural and urban status, and household savings all affect the propensity to be selected into the program. 


#attach 'dataforanalysis' dataset into the environment

attach(endline_dataforanalysis)


#estimate the probit model
propensityscores<-glm(formula = treatment ~ age + gender +
                        householdsize, data=endline_dataforanalysis)


#results of the probit model 
summary(propensityscores)

#########################################################################################################

#Step 5: Estimate Average Treatment effect on the treated 

#Install and load 'Matching' package 


library(Matching)

#Estimate the average treatment effect on the treated (ATT), our variable of interest is 's5_q1' i.e. "Does your household have a toilet?"

ATTresults  <- Match(Y = , Tr=treatment, X=propensityscores$fitted.values, estimand = "ATE", M=1, ties = TRUE, replace = TRUE)
#ATT results 

summary(ATTresults)

#########################################################################################################

#End 