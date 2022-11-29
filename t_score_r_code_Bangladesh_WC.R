# Propensity score matching for each hypothesis group
install.packages("readr")
install.packages("dplyr")
install.packages("Matching")
install.packages("MatchIt")

library(Matching)
library(MatchIt)
# Hypothesis Group 1
# match variable "used loan for sanitation at end" with respondents who reported not having a toilet at the baseline

#match_obj1 <- matchit(formula = WC1 ~ toilet.endline + rural_urban.endline + gender.endline + age.endline + education.endline + marital_status.endline + household_savings.endline + household_size.endline, data= group_1_final_df, method = "nearest", distance="glm", ratio=1, replace=FALSE)
#summary(match_obj1)
#########################################################################
attach(group_1_final_df)
propensityscores<-glm(formula = loan_purpose ~ S2 + gender.baseline + age.baseline + education.baseline + marital_status.baseline + savings.baseline + household_size.baseline + rural_urban.baseline, data=group_1_final_df)
summary(propensityscores)

ATTresults  <- Match(Y = S2, Tr=loan_purpose, X=propensityscores$fitted.values, estimand = "ATE", M=1, ties = TRUE, replace = TRUE)
summary(ATTresults)
#########################################################################
## Check matching of groups that have high T scores in the matching process
#plot(match_obj1, type = "jitter", interactive = FALSE)
#plot(match_obj1, type = "qq", interactive = FALSE,
#     which.xs = c("toilet.endline", "rural_urban.endline", "marital_status.endline", "household_savings.endline"))

# match variable "used loan for sanitation at end" with respondents who reported not having a toilet at the baseline
#***************************************************************************************************************
# Propensity Score Matching baseline variables with treatment/control groups
match_obj1 <- matchit(formula = loan_purpose ~ rural_urban.baseline + gender.baseline + age.baseline + education.baseline + marital_status.baseline + savings.baseline + household_size.baseline, data= group_1_final_df, method = "nearest", distance="logit", ratio=1, replace=FALSE)
summary(match_obj1, standardize = TRUE)
match_obj1$model

# Estimate the ATE and ATT
# build the dataset based on the matched propensity scores above.
fig1 <- get_matches(match_obj1)

# Estimate the ATE and ATT
attach(fig1)
propensityscores<-glm(formula = loan_purpose ~ S2 + gender.baseline + age.baseline + education.baseline + marital_status.baseline + savings.baseline + household_size.baseline + rural_urban.baseline, data=fig1)
summary(propensityscores)

ATTresults  <- Match(Y = S2, Tr=loan_purpose, X=propensityscores$fitted.values, estimand = "ATE", M=1, ties = TRUE, replace = TRUE)
summary(ATTresults)
# Try another matching method
#m.out2 <- matchit(toilet.endline ~ WC1 + rural_urban.endline + gender.endline + age.endline + education.endline + marital_status.endline + household_savings.endline + household_size.endline, data = m_group_1,
#                  method = "full", distance = "glm", link = "probit")
#summary(m.out2, un = FALSE)
#plot(summary(m.out2))

# Estimate treatment effect ATT using standard regression
#fit_group_1 <- lm(formula = WC1 ~ toilet.endline + rural_urban.endline + gender.endline + age.endline + education.endline + marital_status.endline + household_savings.endline + household_size.endline, data = m_group_1, weights = weights)
#coeftest(fit_group_1, vcov. = vcovCL, cluster = ~subclass)

# Hypothesis Group 2

match_obj2 <- matchit(formula = loan_purpose ~ toilet.baseline + rural_urban.baseline + gender.baseline + age.baseline + education.baseline + marital_status.baseline + savings.baseline + household_size.baseline, data= group_2_final_df, method = "nearest", distance = "logit", ratio = 1, replace=FALSE)
summary(match_obj2)

fig2 <- get_matches(match_obj2)

attach(fig2)
propensityscores<-glm(formula = loan_purpose ~ water_source.endline + toilet.endline + rural_urban.endline + gender.endline + age.endline + education.endline + marital_status.endline + household_savings.endline + household_size.endline, data=fig2)
summary(propensityscores)
ATTresults  <- Match(Y = loan_purpose, Tr=water_source, X=propensityscores$fitted.values, estimand = "ATE", M=1, ties = TRUE, replace = TRUE)



m_group_2 <- match.data(match_obj2)
head(m_group_2)

fit_group_2 <- lm(formula = WC1 ~ toilet.endline + rural_urban.endline + gender.endline + age.endline + education.endline + marital_status.endline + household_savings.endline + household_size.endline, data=m_group_2, weights = weights)
coeftest(fit_group_2, vcov. = vcovCL, cluster = ~subclass)
# Hypothesis Group 3



match_obj3 <- matchit(formula = WC1 ~ rural_urban.baseline + gender.baseline + age.baseline + education.baseline + marital_status.baseline + savings.baseline + household_size.baseline, data= group_3_final_df, method = "nearest", distance = "logit", ratio = 1, replace=FALSE)
summary(match_obj3)

fig3 <- get_matches(match_obj3)

attach(fig3)
propensityscores<-glm(formula = WC1 ~ S2 + rural_urban.endline + gender.endline + age.endline + education.endline + marital_status.endline + household_savings.endline + household_size.endline, data=fig3)
summary(propensityscores)
ATTresults  <- Match(Y = WC1, Tr=S2, X=propensityscores$fitted.values, estimand = "ATE", M=1, ties = TRUE, replace = TRUE)
summary(ATTresults)

#############################################################################################
match_obj3 <- matchit(formula = WC1 ~toilet.endline + rural_urban.endline + gender.endline + age.endline + education.endline + marital_status.endline + household_savings.endline + household_size.endline, data= group_3_final_df, method = "nearest", distance="glm", ratio=1, replace=FALSE)
summary(match_obj3)

attach(group_3_final_df)
propensityscores<-glm(formula = WC1 ~ toilet.endline + rural_urban.endline + gender.endline + age.endline + education.endline + marital_status.endline + household_savings.endline + household_size.endline, data=group_3_final_df)
summary(propensityscores)

m_group_3 <- match.data(match_obj3)
head(m_group_3)

fit_group_3 <- lm(formula = WC1 ~ toilet.endline + rural_urban.endline + gender.endline + age.endline + education.endline + marital_status.endline + household_savings.endline + household_size.endline, data=m_group_3, weights = weights)
coeftest(fit_group_3, vcov. = vcovCL, cluster = ~subclass)
