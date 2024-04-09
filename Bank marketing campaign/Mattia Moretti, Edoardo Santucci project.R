Bank = read.csv("C:/Users/edoar/Desktop/UNICATT/1° anno/Data Analisys/Progetto/bank.csv")

#This data set describes the results of a marketing campaign of a bank.
#The campaign was conducted mainly by telephone, with the aim of getting customers to sign up for a term deposit.
#The objective of the study is therefore to investigate the characteristics of the customers and calls that best classify the variable of interest,
#and provide recommendations to the bank on how to set up future campaigns.

####EDA####

#We start with analyzing the data from different points of view, and extrapolate some informations from the dataset under consideration.

##DISTRIBUTION OF AGE
hist(Bank$age, breaks = 40, main = 'Age Distribution', col = 'red', xlab = 'Age')
summary(Bank$age)
#most of the customers contacted are between 24 and 60 years old
#the average age of customers contacted is 41.24, min: 18 and max: 95

##DISTRIBUTION PDAYS
hist(Bank$pdays, breaks = 30, main = 'pdays Distribution', col = 'blue', xlab = 'pday')
summary(Bank$pdays)
#most customers had never been contacted before this campaign
#the maximum number of days spent before this contact is 854 and the average is 51

##JOB TYPE DISTRIBUTION
barplot(table(Bank$job), 
        main='job type Distribution',
        col= c('red', 'blue', 'green', 'brown', 'yellow', 'orange', 'purple', 'skyblue', 'gray', 'violet', 'black', 'gold'),
        xlab = 'Job',
        ylab = 'Frequence', 
        las = 2,
        cex.names = 0.8,
        width = 0.5)
table(Bank$job)
#People who belong to blue-collar and management are the most frequent costumers in the bank
#Students are the least frequnt among the bank costumers

##DISTRIBUTION OF AGE BASED ON JOB TYPE
library(ggplot2)
ggplot(data = Bank, aes(x = job, y = age)) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  geom_boxplot(  fill = c('blue','red','brown', 'green', 'yellow', 'orange', 'purple', 'skyblue', 'gold', 'black', 'gray', 'violet')) +
  scale_y_continuous(name = 'age') +
  scale_x_discrete(name = 'job') +    
  ggtitle('Distribution of age based on job type') 
#the average age for each type of work is between 38 and 50.
#For students it is around 25 years, for retired around 64/65 years.
#we can see also that there are some outliers for each job with the exception for blue-collar and unemployed


##MARITAL DISTRIBUTION
library(plotrix)
pie3D(table(Bank$marital),labels = round(100*(table(Bank$marital))/sum(table(Bank$marital)), 1),explode = 0.1, main = "Marital Distribution")
legend('topright', c('divorced', 'married', 'single'), fill = rainbow(length(table(Bank$marital))))
table(Bank$marital)
#Most of the clients in the bank are Married (56.9%) or Single (31.5%)

##DEPOSIT BASED ON MARITAL
barplot(table(Bank$deposit, Bank$marital), col = c(2,3), beside = T, main='Deposit based on marital', legend = TRUE)
table(Bank$deposit, Bank$marital)
#Costumers who are single are more likely to subscribe a deposit

##EDUCATION OF CUSTOMER DISTRIBUTION
barplot(table(Bank$education), 
        main='Education of customer Distribution',
        col= c('red', 'blue', 'green', 'brown'),
        xlab = 'Education',
        ylab = 'Frequence', 
        width = 0.5)

pie3D(table(Bank$education),labels = round(100*(table(Bank$education))/sum(table(Bank$education)), 1),explode = 0.1, main = "Education of customer Distribution")
legend('topright', c('primary', 'secondary', 'tertiary', 'unknow'), fill = rainbow(length(table(Bank$education))))
table(Bank$education)
#Most of the customers in the bank have a Secondary education (49.1%) or a Tertiary education (33%)
#only 13.4% of the clients are related to primary education category

##DEPOSIT BASED ON EDUCATION
barplot(table(Bank$deposit, Bank$education), col = c('red','blue'), beside = T, main='Deposit based on education', legend = TRUE,  xlab = 'Education',  ylab = 'Frequence')
table(Bank$deposit, Bank$education)
#Most of the customers with tertiary education tend on subscribe more often the deposit

##CONTACT DISTRIBUTION
pie3D(table(Bank$contact),labels = round(100*(table(Bank$contact))/sum(table(Bank$contact)), 1),explode = 0.1, main = "Contact Distribution")
legend('topright', c('celluler', 'telephone', 'unknown'), fill = rainbow(length(table(Bank$contact))))
table(Bank$contact)
#Most of the customers in the bank are contacted through Cellular 72%
#only 6.93% of the clients are contacted through telephone

##DEPOSIT BASED ON TYPE OF CALL
barplot(table(Bank$deposit, Bank$contact), col = c('red','blue'), beside = T, main='Deposit based on type of call', legend = TRUE)
table(Bank$deposit, Bank$contact)
#If contacted through cellular people subscribe the deposit more often


##LAST CONTACT MONTH OF THE YEAR DISTRIBUTION
barplot(table(Bank$month), 
        main='Last contact month of the year distribution',
        col= c('red', 'blue', 'green', 'brown', 'yellow', 'orange', 'purple', 'skyblue', 'gray', 'violet', 'black', 'gold'),
        xlab = 'Month',
        ylab = 'Frequence', 
        width = 0.5)
table(Bank$month)
#Most of the clients in the bank are contacted in the months of May, Jun, Jul and in Aug last year.
#very few of the clients are contacted in the months of Sep, Mar and in Dec

##DEPOSIT DISTRIBUTION
barplot(table(Bank$deposit), 
        main='Deposit Distribution',
        col= c('red', 'blue'),
        xlab = 'Deposit',
        ylab = 'Frequence')
table(Bank$deposit)
#From the Outcomes of the marketing Campaign 52.6% of the Clients didn't subscribe for the Term Deposits.
#only 47.4% are subsribed to the term deposits

##TERM DEPOSIT BASED ON JOB
barplot(table(Bank$deposit, Bank$job),col = c('red','blue'), beside = T, main="Term depositors based on job type",
        legend = TRUE,
        xlab = 'job',
        ylab = 'Frequence', 
        cex.names = 0.8,
        las = 2)
table(Bank$deposit, Bank$job)
#Clients whose job types in retired,student,unemployed and management are subscribed more to term deposits than other


##DEPOSIT BASED ON LAST CONTACT MONTH
barplot(table(Bank$deposit, Bank$month),col = c('red','blue'), beside = T, main='Deposit based on last contact month',
        legend = TRUE,
        xlab = 'month',
        ylab = 'Frequence',
        cex.names = 0.8)
table(Bank$deposit, Bank$month)
#Most of the clients in the bank are contacted in the months of May, Jun, Jul and in Aug last year. More Clients subscribed to term deposits in this months.
#very few of the clients are contacted in the months of Sep, Mar and in Dec. Even if in these months are contacted less people it is more likely that people subscribe the term deposit in these periods.


##DEPOSIT BASED ON CONTRACT DURATION
summary(Bank$duration)
duration = as.factor(ifelse(Bank$duration < 371,'Below Average','Above Average'))
barplot(table(Bank$deposit, duration), col = c('red','blue'), beside = T, main='Deposit based on contact duration', legend = TRUE,   xlab = 'Duration', ylab = 'Frequence')
table(Bank$deposit, duration)
#if the duration of the call is above average 77.4 % of clients subscribe to term deposits and 22.6 % don't subscribe
#if the duration of the call is below average 31.6 % of clients subscribe to term deposits and 68.4 % don't subscribe

##DEPOSIT BASED ON CAMPAIGN
summary(Bank$campaign)
campaign = as.factor(ifelse(Bank$campaign < 2,'contacted one time','contacted several times'))
barplot(table(Bank$deposit, campaign),col = c('red','blue'), beside = T, main='Deposit based on campaign', legend = TRUE, xlab = 'Number of contacts', ylab = 'Frequence')
table(Bank$deposit, campaign)
# people contacted the first time are more likely to say yes
# also there is a huge number of people who subscribed when contacted the second time or more
# people who didn't subscribed the first time are more likely to say no on the following callS

##DEPOSIT BASED ON BALANCE
summary(Bank$balance)
balance = as.factor(ifelse(Bank$balance < 1529,'Below Average','Above Average'))
barplot(table(Bank$deposit, balance),col = c('red','blue'), beside = T, main='Deposit based on balance', legend = TRUE, xlab = 'Balance', ylab = 'Frequence' )
table(Bank$deposit, balance)
#if the balance is above average 57.3 % of clients subscribe to term deposits and 42.7 % don't subscribe
#if the balance is below average 43.6 % of clients subscribe to term deposits and 56.4% don't subscribe

##DEPOSIT BASED ON HOUSING LOAN
summary(Bank$housing)
Bank$housing = as.factor(Bank$housing)
barplot(table(Bank$deposit, Bank$housing),col = c('red','blue'), beside = T, main='Deposit based on housing loan', legend = TRUE, names.arg = c('no loan', 'have a loan'), xlab = 'Loan', ylab = 'Frequence')
table(Bank$deposit, Bank$housing)
#57% of Clients not having housing loan subscribed to the term deposits and 36.6% of clients having housing loan subscribed to the term deposits.

##DEPOSIT BASED ON PERSONAL LOAN
summary(Bank$loan)
Bank$loan = as.factor(Bank$loan)
barplot(table(Bank$deposit, Bank$loan),col = c('red','blue'), beside = T, main='Deposit based on personal loan', legend = TRUE, names.arg = c('no loan', 'have a loan'), xlab = 'Loan', ylab = 'Frequence')
table(Bank$deposit, Bank$loan)
#49.5 % of Clients not having personal loan subscribed for the term deposits and 33.22 % of clients having personal loan subscribed to the term deposits.

##TERM DEPOSIT BASED ON OUTCOME OF PREVIOUS MARKETING CAMPAING
barplot(table(Bank$deposit, Bank$poutcome),col = c('red','blue'), beside = T, main='Term deposit Based on Outcome of Previous Marketing Campaign',legend = TRUE, xlab = 'Outcome', ylab = 'Frequence',cex.names = 0.8)
table(Bank$deposit,Bank$poutcome)
#From the Outcome of previous Campaign, if the previous campaign had been a Failure, then there is a 50% chance that the client will not subscribe to the term deposit. out of all failure outcomes 50.3 % of clients subscribe and 49.7 % don't subscribe to the term deposits
#From the Outcome of previous Campaign, if the previous campaign had been a Success, then there is a higher chance that the client will subscribe to the term deposit. out of all success outcomes 91.3% of clients subscribe and 8.7% don't subscribe to the term deposit.
#People who subscribed the previous campaign are more likely to subscribe again
#people who didn't subscribed the previous campaign are willing to subscribe the following campaign

##DISTRIBUTION OF AGE BASED ON TERM DEPOSIT STATUS
ggplot(data = Bank, aes(x = deposit, y = age)) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  geom_boxplot(fill = c('red','blue')) +
  scale_y_continuous(name = 'age') +
  scale_x_discrete(name = 'deposit') +    
  ggtitle('Distribution of age based on term Deposit Status') 
#The median age of the clients who have subscribed and haven't subscribed for the term deposit is almost same.


##CORRELATION
library(corrplot)
corrplot(cor(Bank[,c('age', 'balance', 'day', 'duration', 'campaign', 'pdays', 'previous')], method = c("pearson")),method = 'color')
cor(Bank[,c('age', 'balance', 'day', 'duration', 'campaign', 'pdays', 'previous')], method = c("pearson"))
#looking at the pearson correlation matrix we can see that only two attributes are related slightly to each other, pdays and previous: 0.507271588, this mean that there are a discrete linear relationship between this two attributes

##CONCLUSION OF EDA

#most of clients are contacted in the months of may, june, july and august which are the months with the worst ratio yes/no. It could be better to contact clients in september, october, february, march and april because there is a better yes/no ratio.
#Clients who are retired or student subscribe more often to the term deposit.
#From the Outcome of previous Campaign, if the previous campaign had been a Failure, then there is a 50% chance that the client will not subscribe to the term deposit. out of all failure outcomes 50.3 % of clients subscribe and 49.7 % don't subscribe to the term deposits
#From the Outcome of previous Campaign, if the previous campaign had been a Success, then there is a higher chance that the client will subscribe to the term deposit. out of all success outcomes 91.3% of clients subscribe and 8.7% don't subscribe to the term deposit.
#If the balance of the client is above average there is a higher chance that he subscribes to the term deposit.



####BAYES CLASSIFIER####

#Bayes classifier uses a  standard approach and exploit Bayes' theorem and make some sampling assumptions in order to model the 
#conditional probability distributions of different classes given the values of the descriptive attributes.
#They predict, for each new record, the most probable class based on this probability model, infact Bayes Classifier is a model based on probability.
#Naive Bayes classifiers perform very well in practice and are highly valued in domains in which large numbers of descriptive attributes have to be considered.
#This classifier works so well that it is generally used as a baseline for the other model, therefore, we decided to apply it first to our dataset.

set.seed(2023)
library(e1071)
Bank.idx= sample(11162, 8930)#prendo 80%
Bank.train = Bank[Bank.idx,]
Bank.test = Bank[-Bank.idx,]
#We decided to devide our dataset into two part: 80% of the osservations for training set and the remaining 20% for test 
Bank.bc = naiveBayes(deposit ~ ., data=Bank.train)
Bank.bc
#Next we applied the naiveBayes function on the target attribute (deposit) regressed on the whole training set obtained previously and visualized the result. 
predict(Bank.bc, Bank.test[1,])
Bank.pred = predict(Bank.bc, Bank.test)
table(Bank.test$deposit, Bank.pred)
#we used the prediction function with the previous obtained results and the test dataset.

####DECISION TREE####

#The general idea of the decision tree is to find some hierarchical partitioning of the input area space, a hierarchical structure to 
#explain how different areas in the input space correspond to different outcomes,  we want to identify the most relevant subset of the attributes. 
#The focus is that you have asset of attributes, and you automatically find out which are the most important attributes to describe your dataset.
#The decision tree builds a hierarchical decision structure which helps to understand the classification process by traversing the tree from the root node until a leaf is reached, which is the output of the model.

Bank$deposit = as.factor(Bank$deposit)
Bank$poutcome = as.factor(Bank$poutcome)
Bank$month = as.factor(Bank$month)
Bank= Bank[,c('duration', 'month', 'deposit', 'poutcome', 'previous')]
#First of all we Convert string attributes into nominal and reduce the dataset to only the variables of interest to us 

set.seed(2023)
#our idea is to split the data into two subsets:
Bank.idx= sample(11162, 8930)
Bank.train = Bank[Bank.idx,]#training set (80% of the observation) to "train" the algorithm 
Bank.test = Bank[-Bank.idx,]#validation set (20% of the observation) used to validate the accuracy of the algorithms on unknow data.

library(rpart)
library(rpart.plot)
Bank.1 = rpart(deposit ~ ., data=Bank.train)
summary(Bank.1)
#next we applied rpart function to determine the classification tree, operating on the training set.
rpart.plot(Bank.1,extra = 101, cex = 0.6, box.palette = 'RdBu', shadow.col = 'gray', nn = F)
#After building the tree, we can see that the call duration is confirmed as the most significant as it dominates the others and is located at the root of the tree;
#the outcome of the previous marketing campaign and month also seem to be quite relevant, while previous was left out of the model.
Bank.predict = predict(Bank.1, Bank.test, type='class')
table(Bank.test$deposit, Bank.predict)
printcp(Bank.1)
#in the end we do the prediction with the test and come up with the following table:
#     Bank.predict
#        no    yes
#no     921    233
#yes    191    887

#we can denote an error of 47.1% but a result very similar to the Bayes classifier model 


####RANDOM FOREST####

#Decision Tree models might be potentially unstable, this means that small changes to the training datavcan result in drastic changes in the resulting tree.
#This is due to the greedy nature of the underlying algorithm.
#with the random forest model, instead of building one large tree, a set of differently initialized, much smaller 
#decision trees are created, and the classification output is created by committee voting. 
#We are going to build small decision trees for randomly select a subset of attributes, so each of these 
#decision stamps is less accurate but we have a set of decision stamp, and the classification of the model is based on Ensemble methods.
#Forests of trees have the advantage that they are more stable than classical decision trees and offer better performance.

library(randomForest)
Bank.rf = randomForest(deposit ~ ., data=Bank.train, ntree = 100)
Bank.rf_predict = predict(Bank.rf, Bank.test)
table(Bank.test$deposit, Bank.rf_predict)
#after applying the random forest function and making the next prediction:
#        Bank.rf_predict
#            no     yes
#no         947     207
#yes        212     866

#we can see that the forecasts for all three models used are very similar.

####CONCLUSION####

#Although the decision tree model do not guarantee the highest accuracy in prediction, they are a very efficient means, as well as simple to generate and interpret.
#We can therefore confirm the results we have come up with: the most important predictors for classifying the outcomes of a telephone marketing campaign are mainly the duration of the calls,
#the outcomes of previous contacts with the same subject, and the time of year in which the contact is made. 
#A bank implementing such a campaign can use the results of our analysis to maximize its efficiency and learn more about possible customers.








