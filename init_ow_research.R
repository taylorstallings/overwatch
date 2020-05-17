# The first thing we want to do it tell R what folder we will be retrieving data from
# Save your data to a folder and then type the folder path below INBETWEEN QUOTATION MARKS

#Mac

setwd("/Users/tstall14/Desktop/")


# Now you can retrieve your data using the code below
# In this code we are pulling the csv file titled "MidtermData" and naming it "dat1" 

dat1 <- data.frame(read.csv("overwatch.xlsx", header = T))

# Now that you have retrieved the data, you should be able to see the data 
# by clicking on "dat1" located in the "Data" box

# We are now ready to analyze


##############################################
######### Descriptive Statistics #############
##############################################

#To find the minimum, median, and mean replace "Pred1" with the variable name

summary(dat1$Pred1)

# for standard deviation, run the following while replacing "Pred1" with the variable name

sd(dat1$Pred1)


##############################################
######### CHI-SQUARED TEST ###################
##############################################
install.packages("MASS")
library(MASS)


attach(overwatch)
tbl <- table(win, 
             groupsize)

tbl

# run the chi-square test to see the p-value of the relationship
chisq.test(tbl)
##############################################
################ T-Test ######################
##############################################

t.test(`How many deaths did you have?`~`Did you win?`)


##############################################
################ One-Way ANOVA #################
##############################################

fit <- aov(`How many deaths did you have?`~`Did you win?`)
summary(fit)

pairwise.t.test(`Did you win?`, `How many deaths did you have?`, p.adj = "none")


##############################################
################ Two-Way ANOVA #################
##############################################

# replace Pred1 and Pred2 with your Predictor variables and Out1 with your Outcome

# To see if the interaction is significant, run the following and then look at the 
# p-value (Pr(>F)) for dat1$Pred1:dat1$Pred2

fit <- aov(win ~ medals * kills)
summary(fit)

# To see the p-value between each category, run the following while replacing 
# Pred1 and Pred2 with your Predictor variables and Out1 with your Outcome

Int <- interaction(kills, deaths)

pairwise.t.test(win, Int)

# To see the means of each category, run the following while replacing 
# Out1 with your Outcome

aggregate(MonthSales, list(abc), mean)


# Use the code below to see a plot to help visualize (replace Pred1 and Pred2 with your 
# Predictor variables and Out1 with your Outcome)

interaction.plot(dat1$Pred2, dat1$Pred1, dat1$Out1, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="x-axis label", 
                 ylab="y-axis labe;", 
                 main="Interaction of Pred1 and Pred2 on Out1")

##############################################
################ Linear Regression #################
##############################################

# replace Pred1 with your Predictor variable and Out1 with your Outcome
# the p-value is Pr(> |t|)
# beta is "Estimate"

fit <- lm(win ~ groupsize)
summary(fit)

##############################################
################ Binary Logistic Regression #################
##############################################

# replace Pred1 with your Predictor variable and Out1 with your Outcome
# the p-value is Pr(> |t|)
# beta is "Estimate"

fit <- glm(win ~ gold, family = "binomial")
summary(fit)

fit <- glm(win ~ silver, family = "binomial")
summary(fit)

fit <- glm(win ~ bronze, family = "binomial")
summary(fit)

fit <- glm(win ~ kills, family = "binomial")
summary(fit)

fit <- glm(win ~ deaths, family = "binomial")
summary(fit)

fit <- glm(win ~ groupsize, family = "binomial")
summary(fit)

fit <- glm(win ~ medals, family = "binomial")
summary(fit)

fit <- glm(win ~ KDRatio, family = "binomial")
summary(fit)

##############################################
########## Mulitnomial Logistic Regression #################
##############################################

#if you do not have the following packages, you will need to install them

install.packages("nnet")
install.packages("AER")


# Load the packages
library(nnet)
library(AER)


#Replace Out1 with your Outcome variable
#Replace Pred1 with your Predictor variable
#Replace Out_Ref with the name of the Outcome variable condition you want to compare to the others
# Do not replace Y2 with anything

#The betas can be found in the "Estimate" column
# A negative value means an increase in Pred1 will decrease the likelihood of the occurance of a condition (in contrast to the condition you selected to compare)
# A Positive value means an increase in Pred1 will increase the likelihood of the occurance of a condition (in contrast to the condition you selected to compare)

#The p-value can be found in the "Pr(>|z|)" column

Y2 <- relevel(win, ref = "Out_Ref")
fit <- multinom(Y2 ~ gold)

coeftest(fit)