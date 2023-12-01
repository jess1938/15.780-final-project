rm(list = ls()) # remove all objects from memory
setwd("~/Downloads")
library(dplyr)

voters.df <- read.csv("Voter_Analysis_2008-2018.csv")

#No NA values
table(is.na(voters.df$eligible))
table(is.na(voters.df$participated))
table(is.na(voters.df$age)) 
table(is.na(voters.df$CouncilDistrict.18)) 

# no people younger than 18 
less_than_18 <- subset(voters.df, age <= 17) 

#Found people not eligible
non_eliglible_yet_an_active_voter.df <- filter(voters.df, eligible==0)

#Found NA value
table(is.na(voters.df$RegistrationYear))

table(is.null(voters.df)) #1 null??? 



#To-do: remove one person without registration year 
table(is.na(voters.df$participated))

##removing person without registration year
voters_clean.df <- voters.df %>% filter(!is.na(voters.df$RegistrationYear)) #remove values with no registration year
voters_clean.df <- voters_clean.df %>% filter(!is.null(voters.df)) #remove null values
voters_clean.df <- voters_clean.df %>% filter(eligible>0)
voters_clean.df <- voters_clean.df %>% filter(age<116)

#creating a participation score for 2016 (out of 1 unlike original table)
voters_clean.df$participation_score_2016 <- ifelse(
  is.na(voters_clean.df$GE18) & is.na(voters_clean.df$GE17),
    voters_clean.df$participated/voters_clean.df$eligible,
  ifelse(
    !is.na(voters_clean.df$GE18) & !is.na(voters_clean.df$GE17), 
    (voters_clean.df$participated - voters_clean.df$GE18 - voters_clean.df$GE17)/voters_clean.df$eligible, 
    ifelse(
      is.na(voters_clean.df$GE18) & !is.na(voters_clean.df$GE17),
      (voters_clean.df$participated - voters_clean$GE17)/voters_clean.df$eligible,
      ifelse(
        !is.na(voters_clean.df$GE18) & is.na(voters_clean.df$GE17),
        (voters_clean.df$participated - voters_clean.df$GE18)/voters_clean.df$eligible,
        NA
      )
    )
  )
)

table(is.na(voters_clean.df$participation_score_2016))                       

write.csv(voters_clean.df, file = "voters_clean.csv", row.names = FALSE)


###OLS 

#norm.values <- preProcess(, method=c("center", "scale"))

voters_clean.df$vote_on_score_2016 <- ifelse(voters_clean.df$participation_score_2016 < 0.5, 0, 1)

model <- glm(vote_on_score_2016 ~ age + as.factor(CouncilDistrict.18) + as.factor(PoliticalParty.18), voters_clean.df, family = "binomial")
summary(model)

coef(model)

voters_clean.df$participation_score_2016
mean(voters_clean.df$participation_score_2016)

voters_clean.df$CouncilDistrict.18 <- as.factor(voters_clean.df$CouncilDistrict.18)

#age
selected_vars = select(voters_clean.df,c("vote_on_score_2016","CouncilDistrict.18","PoliticalParty.18"))
summary(voters_clean.df$vote_on_score_2016v)

library(dummies)

#class(selected_vars)
#selected_vars$age <- as.factor(selected_vars$age)

selected_vars <- dummy.data.frame(selected_vars,sep = "_")
#print(selected_vars)


#just location
model <- glm(vote_on_score_2016 ~ ., data=selected_vars, family = "binomial")

print(selected_vars$vote_on_score_2016)

model.predict()
example <- data.frame(voters)
summary(model)
