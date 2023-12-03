rm(list = ls())
setwd("~/Downloads")
library(dplyr)
library(caret)
library(dummies)
library(dummy)

voters_clean_all_elections.df <- read.csv("voters_clean_dec_02.csv")

ge_data <- select(voters_clean_all_elections.df, starts_with("GE"))
prez_ge_data <- select(voters_clean_all_elections.df, c('GE08', 'GE12','GE16'))

#adding eligibility and # of elections voted for general elections only
voters_clean_all_elections.df <- voters_clean_all_elections.df %>%
  mutate(
    num_ge_voted = rowSums(ge_data == 1, na.rm = TRUE),
    num_ge_eligible = 11 - rowSums(is.na(ge_data))
  )

#adding elibility and # of elections voted ge when presidents were being voted \
voters_clean_all_elections.df <- voters_clean_all_elections.df %>%
  mutate(
    num_prez_ge_voted = rowSums(prez_ge_data == 1, na.rm = TRUE),
    num_prez_ge_eligible = 3 - rowSums(is.na(prez_ge_data))
  )

#creating participation score based on # of prez elections voted / # of prez elections eligible for
voters_clean_all_elections.df$prez_participation_score <- ifelse(
  voters_clean_all_elections.df$num_prez_ge_eligible == 0,
  NA,
  voters_clean_all_elections.df$num_prez_ge_voted/voters_clean_all_elections.df$num_prez_ge_eligible
) 
#a lot better -- Mean = 0.64; to note: 102,012 people were not eligible for presidential elections in the data set



######## creating dataset with only people eligible for presidential elections 
voters_clean_elig_prez.df <- voters_clean_all_elections.df
voters_clean_elig_prez.df <- voters_clean_elig_prez.df %>% filter(num_prez_ge_eligible>0)



#creating binary variable on whether would vote or not in a future presidential election
###TO:DO LESS THAN 0.5 OR LESS THAN OR EQUAL TO??????????
voters_clean_elig_prez.df$vote_in_prez_elec <- ifelse(voters_clean_elig_prez.df$prez_participation_score < 0.5, 0, 1) 
unique(voters_clean_elig_prez.df$CommunityDistrict.18)

######### logistic regression with only location
###USE 2018 COUNCIL DISTRICT BC MOST RECENT????
selected_vars = select(voters_clean_elig_prez.df,c("vote_in_prez_elec","CouncilDistrict.18","PoliticalParty.18"))
selected_vars$CouncilDistrict.18 <- factor(selected_vars$CouncilDistrict.18)
selected_vars$PoliticalParty.18 <- factor(selected_vars$PoliticalParty.18)

model <- glm(vote_in_prez_elec ~ ., data=selected_vars, family = "binomial")
summary(model)

######### using assembly district as a location
selected_vars2 = select(voters_clean_elig_prez.df,c("vote_in_prez_elec","AssemblyDistrict.18","PoliticalParty.18","RegistrationYear","age"))

#cleaning data
selected_vars2 <- selected_vars2 %>% filter(!PoliticalParty.18=="NON")
selected_vars2 <- selected_vars2 %>% filter(!is.na(selected_vars2$AssemblyDistrict.18))
selected_vars2 <- selected_vars2 %>% filter(!AssemblyDistrict.18==54)
selected_vars2 <- selected_vars2 %>% filter(!AssemblyDistrict.18==69)

#factorize categorical variables
selected_vars2$PoliticalParty.18 <- factor(selected_vars2$PoliticalParty.18)
selected_vars2$AssemblyDistrict.18 <- factor(selected_vars2$AssemblyDistrict.18)
selected_vars2$vote_in_prez_elec <- factor(selected_vars2$vote_in_prez_elec)

summary(selected_vars2$AssemblyDistrict.18)

#train and test data splitting
train2.index <- sample(c(1:dim(selected_vars2)[1]), dim(selected_vars2)[1]*0.6)
valid2.index <- setdiff((1:dim(selected_vars2)[1]), train.index)
train2.df <- selected_vars2[train.index,]
valid2.df <- selected_vars2[valid.index,]

#normalizing data
preprocess_params2 <- c("range")
# Apply preprocessing
preprocessed_data2 <- preProcess(train2.df, method = preprocess_params2)
# Transform the original data using the computed normalization parameters
train_norm2 <- predict(preprocessed_data2, newdata = train2.df)
valid_norm2 <- predict(preprocessed_data2, newdata = valid2.df)

model2 <- glm(vote_in_prez_elec ~ ., data=train_norm2, family = "binomial")
summary(model2)

predicted_probabilities2 <- predict(model2, newdata = valid_norm2, type="response")
predicted_classes2 <- ifelse(predicted_probabilities2 >= 0.5, 1, 0)
conf_matrix2 <- confusionMatrix(data = as.factor(predicted_classes2), reference = as.factor(valid_norm2$vote_in_prez_elec))

#selected_vars2$PoliticalParty.18 <- factor(selected_vars2$PoliticalParty.18)
#selected_vars2$AssemblyDistrict.18 <- factor(selected_vars2$AssemblyDistrict.18)
#model <- glm(vote_in_prez_elec ~ ., data=selected_vars2, family = "binomial")
#summary(model)








###using election district as a location
selected_vars3 = select(voters_clean_elig_prez.df,c("vote_in_prez_elec","ElectionDistrict.18","PoliticalParty.18"))
selected_vars3$PoliticalParty.18 <- factor(selected_vars3$PoliticalParty.18)
selected_vars3$ElectionDistrict.18 <- factor(selected_vars3$ElectionDistrict.18)
model <- glm(vote_in_prez_elec ~ ., data=selected_vars3, family = "binomial")
summary(model)



####using community district as a location (WITHOUT AGE AND OTHER FACTORS)
selected_vars4 = select(voters_clean_elig_prez.df,c("vote_in_prez_elec","CommunityDistrict.18","PoliticalParty.18"))
selected_vars4$PoliticalParty.18 <- factor(selected_vars4$PoliticalParty.18)
selected_vars4$CommunityDistrict.18 <- factor(selected_vars4$CommunityDistrict.18)
model <- glm(vote_in_prez_elec ~ ., data=selected_vars4, family = "binomial")
summary(model)


####### SPLITTING AND TESTING LOGISTIC REGRESSION

####using community district as a location (WITH ALL OTHER FACTORS INC AGE)
all_selected_vars = select(voters_clean_elig_prez.df,c("vote_in_prez_elec","CommunityDistrict.18","PoliticalParty.18", "RegistrationYear","age"))

#factorize categorical variables
all_selected_vars$PoliticalParty.18 <- factor(all_selected_vars$PoliticalParty.18)
all_selected_vars$CommunityDistrict.18 <- factor(all_selected_vars$CommunityDistrict.18)
all_selected_vars$vote_in_prez_elec <- factor(all_selected_vars$vote_in_prez_elec)

#Filtering out PoliticalParty == NON (only 2 data pts) and NAs in CommunityDistrict (only 28)
all_selected_vars <- all_selected_vars %>% filter(!PoliticalParty.18=="NON")
all_selected_vars <- all_selected_vars %>% filter(!is.na(all_selected_vars$CommunityDistrict.18))
summary(all_selected_vars)



#split training and testing
train.index <- sample(c(1:dim(all_selected_vars)[1]), dim(all_selected_vars)[1]*0.6)
valid.index <- setdiff((1:dim(all_selected_vars)[1]), train.index)
train.df <- all_selected_vars[train.index,]
valid.df <- all_selected_vars[valid.index,]

preprocess_params <- c("range")
# Apply preprocessing
preprocessed_data <- preProcess(train.df, method = preprocess_params)
# Transform the original data using the computed normalization parameters
train_norm <- predict(preprocessed_data, newdata = train.df)
valid_norm <- predict(preprocessed_data, newdata = valid.df)


model <- glm(vote_in_prez_elec ~ ., data=train_norm, family = "binomial")
summary(model)

predicted_probabilities <- predict(model, newdata = valid_norm, type="response")
predicted_classes <- ifelse(predicted_probabilities >= 0.5, 1, 0)
conf_matrix <- confusionMatrix(data = as.factor(predicted_classes), reference = as.factor(valid_norm$vote_in_prez_elec))

#########
selected_vars <- dummy.data.frame(selected_vars,remove_first_dummy = TRUE,sep = "_")

dummy_vars <- dummyVars(~ CouncilDistrict.18, data = selected_vars)

# Create dummy variables
selected_vars_dummies <- predict(dummy_vars, newdata = selected_vars)

# Append the dummy variables to the original data frame
selected_vars <- cbind(selected_vars, selected_vars_dummies)

#selected_vars <- dummy_cols(selected_vars,select_columns = "CouncilDistrict.16",remove_first_dummy = TRUE,sep = "_")
########

