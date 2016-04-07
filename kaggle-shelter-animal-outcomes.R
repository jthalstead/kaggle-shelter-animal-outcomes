rm(list=ls())
library(data.table)
library(stringr)
library(gbm)

train = fread("train.csv")
test = fread("test.csv")

count0 = function(x) {
  return( sum(x == 0) )
}
train$n0 = apply(train, 1, FUN = count0)
test$n0 = apply(test, 1, FUN = count0)

train$OutcomeType = as.factor(train$OutcomeType)
train$name.ind = as.integer(train$Name>'')

train$sex.1 = as.factor(substr(train$SexuponOutcome, regexpr(' ', train$SexuponOutcome), length(train$SexuponOutcome)))
train$sex.2 = as.factor(substr(train$SexuponOutcome, 1, regexpr(' ', train$SexuponOutcome)))

train$mix.ind = as.integer(regexpr('Mix', train$Breed)>0)
train$dom.ind = as.integer(regexpr('Domestic', train$Breed)>0)
train$shorth.ind = as.integer(regexpr('Shorthair', train$Breed)>0)
train$mixcolor.ind = as.integer(regexpr('/', train$Color)>0)
train$dog.ind = as.integer(train$AnimalType=='Dog')

train$age = ifelse(regexpr('year', train$AgeuponOutcome)>0, as.numeric(substr(train$AgeuponOutcome, 1, regexpr(' ', train$AgeuponOutcome))), 
              ifelse(regexpr('month', train$AgeuponOutcome)>0, as.numeric(substr(train$AgeuponOutcome, 1, regexpr(' ', train$AgeuponOutcome)))/12,
                ifelse(regexpr('week', train$AgeuponOutcome)>0, as.numeric(substr(train$AgeuponOutcome, 1, regexpr(' ', train$AgeuponOutcome)))/52,
                       as.numeric(substr(train$AgeuponOutcome, 1, regexpr(' ', train$AgeuponOutcome)))/365)))

train$AnimalID = NULL
train$SexuponOutcome = NULL
train$Name = NULL
train$Breed = NULL
train$Color = NULL
train$AnimalType = NULL
train$AgeuponOutcome = NULL
train$DateTime = NULL
train$OutcomeSubtype = NULL
write.csv(train, file = "train_clean.csv", row.names = F)

gbm.fit = gbm(OutcomeType ~ .,
              data = train,
              distribution = 'multinomial',
              n.trees = 600,
              interaction.depth = 4,
              shrinkage = 0.02,
              train.fraction = 0.7,
              bag.fraction = 0.6,
              verbose = T)

summary(gbm.fit)
gbm.perf(gbm.fit)
plot(gbm.fit)

test$name.ind = as.integer(test$Name>'')

test$sex.1 = as.factor(substr(test$SexuponOutcome, regexpr(' ', test$SexuponOutcome), length(test$SexuponOutcome)))
test$sex.2 = as.factor(substr(test$SexuponOutcome, 1, regexpr(' ', test$SexuponOutcome)))

test$mix.ind = as.integer(regexpr('Mix', test$Breed)>0)
test$dom.ind = as.integer(regexpr('Domestic', test$Breed)>0)
test$shorth.ind = as.integer(regexpr('Shorthair', test$Breed)>0)
test$mixcolor.ind = as.integer(regexpr('/', test$Color)>0)
test$dog.ind = as.integer(test$AnimalType=='Dog')

test$age = ifelse(regexpr('year', test$AgeuponOutcome)>0, as.numeric(substr(test$AgeuponOutcome, 1, regexpr(' ', test$AgeuponOutcome))), 
                   ifelse(regexpr('month', test$AgeuponOutcome)>0, as.numeric(substr(test$AgeuponOutcome, 1, regexpr(' ', test$AgeuponOutcome)))/12,
                          ifelse(regexpr('week', test$AgeuponOutcome)>0, as.numeric(substr(test$AgeuponOutcome, 1, regexpr(' ', test$AgeuponOutcome)))/52,
                                 as.numeric(substr(test$AgeuponOutcome, 1, regexpr(' ', test$AgeuponOutcome)))/365)))

test$SexuponOutcome = NULL
test$Name = NULL
test$Breed = NULL
test$Color = NULL
test$AnimalType = NULL
test$AgeuponOutcome = NULL
test$DateTime = NULL
write.csv(test, file = "test_clean.csv", row.names = F)

preds = as.data.frame(predict(gbm.fit, newdata = test, type = 'response'))

submission = data.table(ID = test$ID, Adoption = preds[, 1], Died = preds[, 2], Euthanasia = preds[, 3], Return_to_owner = preds[, 4], Transfer = preds[, 5])
write.csv(submission, file = choose.files(caption="Save As...", filters = c("Comma Delimited Files (.csv)","*.csv")), row.names = F)
