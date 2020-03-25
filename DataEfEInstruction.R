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

