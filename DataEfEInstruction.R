# Install packages
install.packages("eRm")
install.packages("CTT")
install.packages("haven")
install.packages ("dplyr")

# Load packages
library(eRm)
library(CTT)
library(dplyr)

# Import data
library(haven)
DataEfEInstruction <- read_sav("Data.EfE.Instruction.sav")
View(DataEfEInstruction)

# Review data
names(DataEfEInstruction)
head(DataEfEInstruction)
str(DataEfEInstruction)
dim(DataEfEInstruction)
summary(DataEfEInstruction)

# Select Time 1 items only
items <- dplyr::select(DataEfEInstruction,
                       "Q1T1","Q2T1","Q3T1","Q4T1","Q5T1","Q6T1",
                       "Q7T1","Q8T1","Q9T1","Q10T1","Q11T1","Q12T1","Q13T1","Q14T1","Q15T1","Q16T1","Q17T1")

# Review items
names(items)
head(items)
str(items)
dim(items)
summary(items)

# *****Define and estimate Rasch model with conditional maximum likelihood estimation
rm1 <- RM(items)

# *****Short model summary
rm1

# *****Long model summary (in eRm, examine "difficulty" parameter, not "easiness" parameter)
summary(rm1)

# *****Calculate item difficulty for item 1
-sum(rm1$etapar)

# Quasi-exact nonparametric tests (NPtest)/Ponocny’s nonparametric T10 test global model fit
set.seed(210485)
t10 <- NPtest(as.matrix(items),n=1000,method="T10")
t10 
Tpbis1 <- NPtest(as.matrix(items),n=1000,method="Tpbis",idxt=1,idxs=2:6)
Tpbis1 
lrres <- LRtest(rm1,se=TRUE)
plotGOF(lrres,conf=list())

# *****Simpler version of T10
t10 <- NPtest(items, method = "T10")
t10


# *****Quasi-exact nonparametric tests (NPtest)/Ponocny’s T1 for local dependence
t1 <- NPtest(items, method = "T1")
t1


# *****Quasi-exact nonparametric tests (NPtest)/Ponocny’s T11 for local dependence
t11 <- NPtest(items, method = "T11")
t11

# *****Quasi-exact nonparametric tests (NPtest)/Martin-Loef Test for unidimensionality
split <- rep(1:3, each = 10)
NPtest(items, n = 100, method = "MLoef")

# *****Quasi-exact nonparametric tests (NPtest)/Ponocny’s Q3h for local dependence
q3h <- NPtest(items, method = "Q3h")
q3h

# Quasi-exact nonparametric tests (NPtest)/Ponocny’s Q3l for local dependence
q3l <- NPtest(items, method = "Q3l")
q3l

# Goodness-of-fit tests
pres <- person.parameter(rm1)
gof.res <- gofIRT(pres)
gof.res
summary(gof.res)

# *****Andersen's (1973) goodness-of-fit test
andersen <- LRtest(rm1, splitcr = "median", se = TRUE)
andersen


# Item and person fit

# *****Matrices of expected probabilities and residuals, expected probablities
p.res <- person.parameter(rm1)
pmat(p.res)

#difference between expected and actual
residuals <- residuals(p.res)
residuals

# Can export residuals to perform EFA

# *****Item fit (infit and outfit), Infit msq stay between .7-1.3 range for model fit
itemfit(p.res)

# *****Person fit (infit and outfit), use same range as above
personfit(p.res)

# *****Compute person separation reliability (analogous to internal consistency)
# *** what value would indicate high reliablilty?
pers <- person.parameter(RM(items))
res <- SepRel(pers)
res
summary(res)

# *****Estimate person parameters using maximum likelihood (ML) estimation (extrapolation for all right/wrong response patterns)
pres <- person.parameter(rm1)
pres
summary(pres)
pp_ml <- coef(pres)
View(pp_ml)

# Plot all item ICCs individually
plotICC(rm1)

# Plot all item ICCs together
plotjointICC(rm1, cex = .4)
plotjointICC(rm1, item.subset =  1:10, cex = .4)
plotjointICC(rm1, item.subset = 11:17, cex = .4)

# Plot single item ICC
plotICC(rm1, item.subset = "Q2T1")

# Person-item  map
plotPImap(rm1)
plotPImap(rm1, sorted=TRUE)


# Calculate and plot both item (all items together) and test information functions
tinfo <- test_info(rm1)
plotINFO(rm1)

# Calculate and separately plot item (all items together) and test information functions
tinfo <- test_info(rm1)
info <- item_info(rm1)
plotINFO(rm1, type = "item")


# Pathway maps examining item and person misfit
pparm <- person.parameter(rm1)

# Item pathway map
plotPWmap(rm1)

# Person pathway map
plotPWmap(rm1, pmap = TRUE, imap = FALSE)

