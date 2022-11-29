install.packages("dplyr")
library("dplyr")

#merge dataframes

# merge your baseline core data points with your endline data matching on the qslno number
covariate_subset <- left_join(dataforanalysis, endline_dataforanalysis, by = 'qslno', all=TRUE, suffix = c(".baseline", ".endline"))

# merge all other variables from the baseline to your dataset created in the step above
covariate_subset <- left_join(covariate_subset, Finaldata, by = 'qslno', all=TRUE)
# name column 16 "qslno" in the endline_data_subset so that it can be more easily matched to the all_variables data set
names(endline_data_subset)[16] <- 'qslno'
# merge all other variables from the endline to your dataset created in the step above
all_variables <- left_join(covariate_subset, endline_data_subset, by = 'qslno', all=TRUE)

# rename variables so they follow the same format
colnames(all_variables)[5] <- "education.baseline"
colnames(all_variables)[6] <- "marital_status.baseline"
colnames(all_variables)[7] <- "savings.baseline"
names(all_variables)[8] <- "loan_approved.baseline"
names(all_variables)[9] <- "household_size.baseline"
names(all_variables)[10] <- "toilet.baseline"
names(all_variables)[14] <- "education.endline"
names(all_variables)[15] <- "marital_status.endline"
names(all_variables)[16] <- "household_savings.endline"
names(all_variables)[17] <- "toilet.endline"
names(all_variables)[18] <- "household_size.endline"
names(all_variables)[19] <- "loan_approved.endline"


all_variables$loan_approved.endline[is.na(all_variables$loan_approved.endline)] <- all_variables$loan_approved.baseline[is.na(all_variables$loan_approved.endline)]
#group_1_final$toilet.endline[is.na(group_1_final$toilet.endline)] <- group_1_final$toilet.baseline[is.na(group_1_final$toilet.endline)]
all_variables$rural_urban.endline[is.na(all_variables$rural_urban.endline)] <- all_variables$rural_urban.baseline[is.na(all_variables$rural_urban.endline)]
all_variables$gender.endline[is.na(all_variables$gender.endline)] <- all_variables$gender.baseline[is.na(all_variables$gender.endline)]
all_variables$age.endline[is.na(all_variables$age.endline)] <- all_variables$age.baseline[is.na(all_variables$age.endline)]
all_variables$education.endline[is.na(all_variables$education.endline)] <- all_variables$education.baseline[is.na(all_variables$education.endline)]
all_variables$marital_status.endline[is.na(all_variables$marital_status.endline)] <- all_variables$marital_status.baseline[is.na(all_variables$marital_status.endline)]
all_variables$household_size.endline[is.na(all_variables$household_size.endline)] <- all_variables$household_size.baseline[is.na(all_variables$household_size.endline)]
all_variables$toilet.endline[is.na(all_variables$toilet.endline)] <- 0


## separate data into control and treatment groups




# treatment group 1 - those that did not have a toilet at baseline and used a loan for sanitation by the endline

#treatment_1 <- filter(all_variables,
                              #all_variables$toilet.baseline == "0" & all_variables$WC1 == "2")
# filter for columns we are using, including "used a loan for sanitation" variable
#treatment_1 <- treatment_1[,c(1:19, 133),]

# control group 1 - those that did not have a toilet at baseline and did not have a loan approved by endline
#control_1 <- filter(Finalfinal,
                      #Finalfinal$toilet.baseline == "0" & Finalfinal$loan_approved.endline == "0")
# all_variables$s5_q2[all_variables$s5_q2 == '1'] <- 1
all_variables$s5_q2[all_variables$s5_q2 == '2'] <- 1
all_variables$s5_q2[all_variables$s5_q2 == '3'] <- 1
all_variables$s5_q2[all_variables$s5_q2 == '4'] <- 0
all_variables$s5_q2[all_variables$s5_q2 == '5'] <- 1
all_variables$s5_q2[all_variables$s5_q2 == '6'] <- 1
all_variables$s5_q2[all_variables$s5_q2 == '7'] <- 0
all_variables$s5_q2[all_variables$s5_q2 == '8'] <- 1
all_variables$s5_q2[all_variables$s5_q2 == '9'] <- 0
all_variables$s5_q2[all_variables$s5_q2 == '10'] <- 0
all_variables$s5_q2[all_variables$s5_q2 == '-777'] <- 0



all_variables$toilet.baseline[is.na(all_variables$toilet.baseline)] <- 0
group_1 <- filter(all_variables,
                  all_variables$s5_q2 == "0")
# filter for relevant variables - the column where respondents indicated they used the loan for sanitation
group_1 <- group_1[,c(1:19,617, 250, 749)]

# rename WC1 as loan_purpose variable
group_1$loan_purpose <- group_1$WC1
#change NA in column loan_purpose to zeros to indicate that they did not use the loan
group_1$loan_purpose[is.na(group_1$loan_purpose)] <- 0

#group_1$WC1[which(is.na(group_1$WC1))] <- 0

group_1$loan_purpose[group_1$loan_purpose == ''] <- 0
# replace old values in data frame with new values for comparison 
# replace scale to change those that indicated they used the loan for both water supply and sanitation to sanitation so they are included in the results
group_1$loan_purpose[group_1$loan_purpose == '3'] <- 2
group_1$loan_purpose[group_1$loan_purpose == '-99'] <- 0
# Clean out rows of data where people were granted water improvement loans
all_variables <- subset(group_1$loan_purpose, group_1$loan_purpose != 1 & group_1$loan_purpose != -99)

#transform 2s to 1s for treatment group
all_variables$loan_purpose[all_variables$loan_purpose == '2'] <- 1

#transform NAs in endline variables to match baseline variables
all_variables$loan_approved.endline[is.na(all_variables$loan_approved.endline)] <- all_variables$loan_approved.baseline[is.na(all_variables$loan_approved.endline)]
#group_1_final$toilet.endline[is.na(group_1_final$toilet.endline)] <- group_1_final$toilet.baseline[is.na(group_1_final$toilet.endline)]
all_variables$rural_urban.endline[is.na(all_variables$rural_urban.endline)] <- all_variables$rural_urban.baseline[is.na(all_variables$rural_urban.endline)]
all_variables$gender.endline[is.na(all_variables$gender.endline)] <- all_variables$gender.baseline[is.na(all_variables$gender.endline)]
all_variables$age.endline[is.na(all_variables$age.endline)] <- all_variables$age.baseline[is.na(all_variables$age.endline)]
all_variables$education.endline[is.na(all_variables$education.endline)] <- all_variables$education.baseline[is.na(all_variables$education.endline)]
all_variables$marital_status.endline[is.na(all_variables$marital_status.endline)] <- all_variables$marital_status.baseline[is.na(all_variables$marital_status.endline)]
all_variables$household_size.endline[is.na(all_variables$household_size.endline)] <- all_variables$household_size.baseline[is.na(all_variables$household_size.endline)]

# change toilet NAs to zero
all_variables$toilet.endline[is.na(all_variables$toilet.endline)] <- 0
all_variables$WC1 <- NULL
all_variables$s5_q2 <- NULL
# group_1_final$S2 <- NULL
group_1_final_df <- data.frame(all_variables)

# Change column types to numeric
group_1_final_df$household_savings.endline <- as.numeric(group_1_final_df$household_savings.endline)
group_1_final_df$household_size.baseline <- as.numeric(group_1_final_df$household_size.baseline)
group_1_final_df$education.endline <- as.numeric(group_1_final_df$education.endline)
group_1_final_df$marital_status.endline <- as.numeric(group_1_final_df$marital_status.endline)
group_1_final_df$household_size.endline <- as.numeric(group_1_final_df$household_size.endline)
# Change values in household savings column to zero
group_1_final_df$household_savings.endline[which(is.na(group_1_final_df$household_savings.endline))] <- 0
group_1_final_df$household_savings.endline[group_1_final_df$household_savings.endline == ''] <- 0
group_1_final_df$savings.baseline[which(is.na(group_1_final_df$savings.baseline))] <- 0

#Change endline household savings NAs to zero
# group_1_final_df$household_savings.endline[which(is.na(group_1_final_df$household_savings.endline))] <- 0
# Change endline toilet variable to match baseline
group_1_final_df$S2[group_1_final_df$S2 == '2'] <- 1
group_1_final_df$S2[group_1_final_df$S2 == '3'] <- 1
group_1_final_df$S2[group_1_final_df$S2 == '4'] <- 0
group_1_final_df$S2[group_1_final_df$S2 == '5'] <- 1
group_1_final_df$S2[group_1_final_df$S2 == '6'] <- 1
group_1_final_df$S2[group_1_final_df$S2 == '7'] <- 0
group_1_final_df$S2[group_1_final_df$S2 == '8'] <- 1
group_1_final_df$S2[group_1_final_df$S2 == '9'] <- 0
group_1_final_df$S2[group_1_final_df$S2 == '10'] <- 0
group_1_final_df$S2[group_1_final_df$S2 == '-777'] <- 0
group_1_final_df$S2[which(is.na(group_1_final_df$S2))] <- 0

#check for other NAs
table(is.na(group_1_final_df))
group_1_final_df <- na.omit(all_variables)
#group_1_final_df <- data.frame(group_1_final)
#as.data.frame(group_1_final$qslno, group_1_final$rural_urban.baseline)


# write as a csv file
# write.csv(group_1_final_df, "C:/Users/luke.gates/Castalia/group_1.csv", row.names=FALSE)



#hypothesis_1 <- read.csv("C:/Users/luke.gates/Castalia/group_1.csv")



## treatment group 2 - respondents with improved access to water at baseline who accessed a loan for water improvements
# transform water source variable so you can pull it from the all variables dataframe
all_variables$water_source[all_variables$water_source == '1'] <- 1
all_variables$water_source[all_variables$water_source == '2'] <- 1
all_variables$water_source[all_variables$water_source == '3'] <- 1
all_variables$water_source[all_variables$water_source == '4'] <- 1
all_variables$water_source[all_variables$water_source == '5'] <- 1
all_variables$water_source[all_variables$water_source == '6'] <- 0
all_variables$water_source[all_variables$water_source == '7'] <- 1
all_variables$water_source[all_variables$water_source == '8'] <- 0
all_variables$water_source[all_variables$water_source == '9'] <- 0
all_variables$water_source[all_variables$water_source == '10'] <- 0
all_variables$water_source[all_variables$water_source == '11'] <- 0
all_variables$water_source[all_variables$water_source == '12'] <- 0
all_variables$water_source[all_variables$water_source == '13'] <- 0
all_variables$water_source[all_variables$water_source == '14'] <- 0
all_variables$water_source[all_variables$water_source == '15'] <- 0
all_variables$water_source[all_variables$water_source == '16'] <- 0
all_variables$water_source[all_variables$water_source == '17'] <- 1
all_variables$water_source[all_variables$water_source == '18'] <- 1

all_variables$water_source[all_variables$water_source == '-777'] <- 0

group_2 <- filter(all_variables,
                  all_variables$water_source == "1")

group_2 <- group_2[,c(1:19, 119, 650, 617)]

# Rename column WC1 so that we know what variable it is "loan_purpose"
colnames(group_2)[22] <- "loan_purpose"
colnames(group_2)[21] <- "water_source.endline"
#change NA in column loan_purpose to zeros
group_2$loan_purpose[is.na(group_2$loan_purpose)] <- 0
# replace old values in data frame with new values for comparison 
# Replace value "3" with value 1 to indicate those that marked value three used a loan for water supply
group_2$loan_purpose[group_2$loan_purpose == '3'] <- 1

# Clean out rows of data where people were granted sanitation improvement loans
group_2 <- subset(group_2, group_2$loan_purpose != 2 & group_2$loan_purpose != -99)

# Drop rows where respondents did not have access to improved water supply at baseline


#change NA in water_source columns to zeros
group_2$water_source[is.na(group_2$water_source)] <- 0
group_2$water_source.endline[is.na(group_2$water_source.endline)] <- 0

# replace old values in data frame with new values for comparison 

group_2$water_source.endline[group_2$water_source.endline == '1'] <- 1
group_2$water_source.endline[group_2$water_source.endline == '2'] <- 1
group_2$water_source.endline[group_2$water_source.endline == '3'] <- 1
group_2$water_source.endline[group_2$water_source.endline == '4'] <- 1
group_2$water_source.endline[group_2$water_source.endline == '5'] <- 1
group_2$water_source.endline[group_2$water_source.endline == '6'] <- 0
group_2$water_source.endline[group_2$water_source.endline == '7'] <- 1
group_2$water_source.endline[group_2$water_source.endline == '8'] <- 0
group_2$water_source.endline[group_2$water_source.endline == '9'] <- 0
group_2$water_source.endline[group_2$water_source.endline == '10'] <- 0
group_2$water_source.endline[group_2$water_source.endline == '11'] <- 0
group_2$water_source.endline[group_2$water_source.endline == '12'] <- 0
group_2$water_source.endline[group_2$water_source.endline == '13'] <- 0
group_2$water_source.endline[group_2$water_source.endline == '14'] <- 0
group_2$water_source.endline[group_2$water_source.endline == '15'] <- 0
group_2$water_source.endline[group_2$water_source.endline == '16'] <- 0
group_2$water_source.endline[group_2$water_source.endline == '17'] <- 1
group_2$water_source.endline[group_2$water_source.endline == '18'] <- 1
group_2$water_source.endline[group_2$water_source.endline == '-777'] <- 0

#transform NAs in endline variables to match baseline variables
group_2$loan_approved.endline[is.na(group_2$loan_approved.endline)] <- group_2$loan_approved.baseline[is.na(group_2$loan_approved.endline)]
#group_2$toilet.endline[is.na(group_2$toilet.endline)] <- group_2$toilet.baseline[is.na(group_2$toilet.endline)]
group_2$rural_urban.endline[is.na(group_2$rural_urban.endline)] <- group_2$rural_urban.baseline[is.na(group_2$rural_urban.endline)]
group_2$gender.endline[is.na(group_2$gender.endline)] <- group_2$gender.baseline[is.na(group_2$gender.endline)]
group_2$age.endline[is.na(group_2$age.endline)] <- group_2$age.baseline[is.na(group_2$age.endline)]
group_2$education.endline[is.na(group_2$education.endline)] <- group_2$education.baseline[is.na(group_2$education.endline)]
group_2$marital_status.endline[is.na(group_2$marital_status.endline)] <- group_2$marital_status.baseline[is.na(group_2$marital_status.endline)]
group_2$household_size.endline[is.na(group_2$household_size.endline)] <- group_2$household_size.baseline[is.na(group_2$household_size.endline)]


# Drop rows where respondents did not have access to improved water supply at baseline

# group_2 <- subset(group_2, group_2$water_source != 0)

# change data type to numeric for columns that are chr
group_2$education.endline <- as.numeric(group_2$education.endline)
group_2$marital_status.endline <- as.numeric(group_2$marital_status.endline)
group_2$household_savings.endline <- as.numeric(group_2$household_savings.endline)

# Change household savings NA in endline to zeros
group_2$household_savings.endline[which(is.na(group_2$household_savings.endline))] <- 0
group_2$household_savings.endline[group_2$household_savings.endline == ''] <- 0

group_2$toilet.endline[which(is.na(group_2$toilet.endline))] <- 0

table(is.na(group_2$toilet.endline))

group_2_final <- na.omit(group_2)
group_2_final_df <- data.frame(group_2_final)
write.csv(group_2_final_df, "C:/Users/luke.gates/Castalia/group_2.csv", row.names=FALSE)




# treatment group 3
group_3 <- filter(all_variables,
                  all_variables$s5_q2 == "1")

# Take the columns we need for group 3
group_3 <- group_3[,c(1:19,617, 250, 749)]

#change NA in column 133 to zeros to indicate that they did not use the loan
group_3$WC1[is.na(group_3$WC1)] <- 0
group_3$WC1[group_3$WC1 == ''] <- 0
# replace old values in data frame with new values for comparison 
# replace scale to indicate whether people used their loan for sanitation "1" or water supply "0"
group_3$WC1[group_3$WC1 == '3'] <- 2
# Clean out rows of data where people were granted water improvement loans
group_3_final <- subset(group_3, group_3$WC1 != 1 & group_3$WC1 != -99)

#transform 2s to 1s for treatment group
group_3_final$WC1[group_3_final$WC1 == '2'] <- 1

group_3_final$S2[group_3_final$S2 == '2'] <- 1
group_3_final$S2[group_3_final$S2 == '3'] <- 1
group_3_final$S2[group_3_final$S2 == '4'] <- 0
group_3_final$S2[group_3_final$S2 == '5'] <- 1
group_3_final$S2[group_3_final$S2 == '6'] <- 1
group_3_final$S2[group_3_final$S2 == '7'] <- 0
group_3_final$S2[group_3_final$S2 == '8'] <- 1
group_3_final$S2[group_3_final$S2 == '9'] <- 0
group_3_final$S2[group_3_final$S2 == '10'] <- 0
group_3_final$S2[group_3_final$S2 == '-777'] <- 0
group_3_final$S2[which(is.na(group_3_final$S2))] <- 0


#transform NAs in endline variables to match baseline variables
group_3_final$loan_approved.endline[is.na(group_3_final$loan_approved.endline)] <- group_3_final$loan_approved.baseline[is.na(group_3_final$loan_approved.endline)]
# group_3_final$toilet.endline[is.na(group_3_final$toilet.endline)] <- group_3_final$toilet.baseline[is.na(group_3_final$toilet.endline)]
group_3_final$rural_urban.endline[is.na(group_3_final$rural_urban.endline)] <- group_3_final$rural_urban.baseline[is.na(group_3_final$rural_urban.endline)]
group_3_final$gender.endline[is.na(group_3_final$gender.endline)] <- group_3_final$gender.baseline[is.na(group_3_final$gender.endline)]
group_3_final$age.endline[is.na(group_3_final$age.endline)] <- group_3_final$age.baseline[is.na(group_3_final$age.endline)]
group_3_final$education.endline[is.na(group_3_final$education.endline)] <- group_3_final$education.baseline[is.na(group_3_final$education.endline)]
group_3_final$marital_status.endline[is.na(group_3_final$marital_status.endline)] <- group_3_final$marital_status.baseline[is.na(group_3_final$marital_status.endline)]
group_3_final$household_size.endline[is.na(group_3_final$household_size.endline)] <- group_3_final$household_size.baseline[is.na(group_3_final$household_size.endline)]

# transform columsn that are chr to numeric
group_3_final$education.endline <- as.numeric(group_3_final$education.endline)
group_3_final$marital_status.endline <- as.numeric(group_3_final$marital_status.endline)
group_3_final$household_savings.endline <- as.numeric(group_3_final$household_savings.endline)

# Change values in household savings column to zero
group_3_final$household_savings.endline[which(is.na(group_3_final$household_savings.endline))] <- 0
group_3_final$household_savings.endline[group_3_final$household_savings.endline == ''] <- 0

#Change endline household savings NAs to zero
group_3_final$household_savings.endline[which(is.na(group_3_final$household_savings.endline))] <- 0

table(is.na(group_3_final_df))
group_3_final$toilet.endline[is.na(group_3_final$toilet.endline)] <- 0

# omit any remaining NAs from the dataset
group_3_final_df <- na.omit(group_3_final_df)
# make sure table is a dataframe
group_3_final_df <- data.frame(group_3_final)
# save dataframe to computer as .csv file
write.csv(group_3_final_df, "C:/Users/luke.gates/Castalia/group_3.csv", row.names=FALSE)


compare_variables <- all_variables[,c(1:19,617, 618)]
