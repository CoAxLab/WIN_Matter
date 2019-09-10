# clear your workspace
rm(list=ls());

# Load libraries (clean up afterwards to only those you need)
library("tidyverse")
library("knitr")
library("pander")
library("car")  
library("BayesFactor")
library("leaps")

# Load data

#Load dataset
cns18data = read_csv('/data/dataDB/WIN/database/specific_variables/jasp.csv')

#converts gender to categorical variable
cns18data = cns18data %>% mutate(Sex = factor(Sex)) 



me_BMI=mean(BMI)
sd_BMI=sd(BMI)

SEM_BMI= sd_BMI/sqrt(length(BMI))

me_DXA=mean(DXA)
sd_DXA=sd(DXA)

SEM_DXA= sd_DXA/sqrt(length(DXA))


me_VO2=mean(VO2)
sd_VO2=sd(VO2)

SEM_VO2= sd_VO2/sqrt(length(VO2))
  
  
# Order of data: 
# DVs (rows): v_interference, v_facilitation, a_baseline_mean, z_baseline_mean, t_baselin_mean
# IVs (cols): BMI, DXA, V02, health_PC1 health_PC2


# make a data vector for output: rows are independent variables, cols are dependent health variables
subset.results = matrix(data=NA, nrow = 5, ncol = 5)

# Find our best fitting models.
#Interference
subset.v_interference.BMI = regsubsets(v_interference~BMI+Age+Sex+Education, data=data.frame(cns18data), force.in="BMI")
subset.v_interference.BMI.summary = summary(subset.v_interference.BMI)
subset.results[1,1]=which.min(subset.v_interference.BMI.summary$bic)

subset.v_interference.DXA = regsubsets(v_interference~DXA+Age+Sex+Education, data=data.frame(cns18data), force.in="DXA")
subset.v_interference.DXA.summary = summary(subset.v_interference.DXA)
subset.results[1,2]=which.min(subset.v_interference.DXA.summary$bic)

subset.v_interference.VO2 = regsubsets(v_interference~VO2+Age+Sex+Education, data=data.frame(cns18data), force.in="VO2")
subset.v_interference.VO2.summary = summary(subset.v_interference.VO2)
subset.results[1,3]=which.min(subset.v_interference.VO2.summary$bic)

subset.v_interference.health_PC1 = regsubsets(v_interference~health_PC1+Age+Sex+Education, data=data.frame(cns18data), force.in="health_PC1")
subset.v_interference.health_PC1.summary = summary(subset.v_interference.health_PC1)
subset.results[1,4]=which.min(subset.v_interference.health_PC1.summary$bic)

subset.v_interference.health_PC2 = regsubsets(v_interference~health_PC2+Age+Sex+Education, data=data.frame(cns18data), force.in="health_PC2")
subset.v_interference.health_PC2.summary = summary(subset.v_interference.health_PC2)
subset.results[1,5]=which.min(subset.v_interference.health_PC2.summary$bic)

#Faciliation 
subset.v_facilitation.BMI = regsubsets(v_facilitation~BMI+Age+Sex+Education, data=data.frame(cns18data), force.in="BMI")
subset.v_facilitation.BMI.summary = summary(subset.v_facilitation.BMI)
subset.results[2,1]=which.min(subset.v_facilitation.BMI.summary$bic)

subset.v_facilitation.DXA = regsubsets(v_facilitation~DXA+Age+Sex+Education, data=data.frame(cns18data), force.in="DXA")
subset.v_facilitation.DXA.summary = summary(subset.v_facilitation.DXA)
subset.results[2,2]=which.min(subset.v_facilitation.DXA.summary$bic)

subset.v_facilitation.VO2 = regsubsets(v_facilitation~VO2+Age+Sex+Education, data=data.frame(cns18data), force.in="VO2")
subset.v_facilitation.VO2.summary = summary(subset.v_facilitation.VO2)
subset.results[2,3]=which.min(subset.v_facilitation.VO2.summary$bic)

subset.v_facilitation.health_PC1 = regsubsets(v_facilitation~health_PC1+Age+Sex+Education, data=data.frame(cns18data), force.in="health_PC1")
subset.v_facilitation.health_PC1.summary = summary(subset.v_facilitation.health_PC1)
subset.results[2,4]=which.min(subset.v_facilitation.health_PC1.summary$bic)

subset.v_facilitation.health_PC2 = regsubsets(v_facilitation~health_PC2+Age+Sex+Education, data=data.frame(cns18data), force.in="health_PC2")
subset.v_facilitation.health_PC2.summary = summary(subset.v_facilitation.health_PC2)
subset.results[2,5]=which.min(subset.v_facilitation.health_PC2.summary$bic)


#a_baseline_mean
subset.a_baseline_mean.BMI = regsubsets(a_baseline_mean~BMI+Age+Sex+Education, data=data.frame(cns18data), force.in="BMI")
subset.a_baseline_mean.BMI.summary = summary(subset.a_baseline_mean.BMI)
subset.results[3,1]=which.min(subset.a_baseline_mean.BMI.summary$bic)

subset.a_baseline_mean.DXA = regsubsets(a_baseline_mean~DXA+Age+Sex+Education, data=data.frame(cns18data), force.in="DXA")
subset.a_baseline_mean.DXA.summary = summary(subset.a_baseline_mean.DXA)
subset.results[3,2]=which.min(subset.a_baseline_mean.DXA.summary$bic)

subset.a_baseline_mean.VO2 = regsubsets(a_baseline_mean~VO2+Age+Sex+Education, data=data.frame(cns18data), force.in="VO2")
subset.a_baseline_mean.VO2.summary = summary(subset.a_baseline_mean.VO2)
subset.results[3,3]=which.min(subset.a_baseline_mean.VO2.summary$bic)

subset.a_baseline_mean.health_PC1 = regsubsets(a_baseline_mean~health_PC1+Age+Sex+Education, data=data.frame(cns18data), force.in="health_PC1")
subset.a_baseline_mean.health_PC1.summary = summary(subset.a_baseline_mean.health_PC1)
subset.results[3,4]=which.min(subset.a_baseline_mean.health_PC1.summary$bic)

subset.a_baseline_mean.health_PC2 = regsubsets(a_baseline_mean~health_PC2+Age+Sex+Education, data=data.frame(cns18data), force.in="health_PC2")
subset.a_baseline_mean.health_PC2.summary = summary(subset.a_baseline_mean.health_PC2)
subset.results[3,5]=which.min(subset.a_baseline_mean.health_PC2.summary$bic)


#z_baseline_mean
subset.z_baseline_mean.BMI = regsubsets(z_baseline_mean~BMI+Age+Sex+Education, data=data.frame(cns18data), force.in="BMI")
subset.z_baseline_mean.BMI.summary = summary(subset.z_baseline_mean.BMI)
subset.results[4,1]=which.min(subset.z_baseline_mean.BMI.summary$bic)

subset.z_baseline_mean.DXA = regsubsets(z_baseline_mean~DXA+Age+Sex+Education, data=data.frame(cns18data), force.in="DXA")
subset.z_baseline_mean.DXA.summary = summary(subset.z_baseline_mean.DXA)
subset.results[4,2]=which.min(subset.z_baseline_mean.DXA.summary$bic)

subset.z_baseline_mean.VO2 = regsubsets(z_baseline_mean~VO2+Age+Sex+Education, data=data.frame(cns18data), force.in="VO2")
subset.z_baseline_mean.VO2.summary = summary(subset.z_baseline_mean.VO2)
subset.results[4,3]=which.min(subset.z_baseline_mean.VO2.summary$bic)

subset.z_baseline_mean.health_PC1 = regsubsets(z_baseline_mean~health_PC1+Age+Sex+Education, data=data.frame(cns18data), force.in="health_PC1")
subset.z_baseline_mean.health_PC1.summary = summary(subset.z_baseline_mean.health_PC1)
subset.results[4,4]=which.min(subset.z_baseline_mean.health_PC1.summary$bic)

subset.z_baseline_mean.health_PC2 = regsubsets(z_baseline_mean~health_PC2+Age+Sex+Education, data=data.frame(cns18data), force.in="health_PC2")
subset.z_baseline_mean.health_PC2.summary = summary(subset.z_baseline_mean.health_PC2)
subset.results[4,5]=which.min(subset.z_baseline_mean.health_PC2.summary$bic)




#t_baseline_mean

subset.t_baseline_mean.BMI = regsubsets(t_baseline_mean~BMI+Age+Sex+Education, data=data.frame(cns18data), force.in="BMI")
subset.t_baseline_mean.BMI.summary = summary(subset.t_baseline_mean.BMI)
subset.results[5,1]=which.min(subset.t_baseline_mean.BMI.summary$bic)

subset.t_baseline_mean.DXA = regsubsets(t_baseline_mean~DXA+Age+Sex+Education, data=data.frame(cns18data), force.in="DXA")
subset.t_baseline_mean.DXA.summary = summary(subset.t_baseline_mean.DXA)
subset.results[5,2]=which.min(subset.t_baseline_mean.DXA.summary$bic)

subset.t_baseline_mean.VO2 = regsubsets(t_baseline_mean~VO2+Age+Sex+Education, data=data.frame(cns18data), force.in="VO2")
subset.t_baseline_mean.VO2.summary = summary(subset.t_baseline_mean.VO2)
subset.results[5,3]=which.min(subset.t_baseline_mean.VO2.summary$bic)

subset.t_baseline_mean.health_PC1 = regsubsets(t_baseline_mean~health_PC1+Age+Sex+Education, data=data.frame(cns18data), force.in="health_PC1")
subset.t_baseline_mean.health_PC1.summary = summary(subset.t_baseline_mean.health_PC1)
subset.results[5,4]=which.min(subset.t_baseline_mean.health_PC1.summary$bic)

subset.t_baseline_mean.health_PC2 = regsubsets(t_baseline_mean~health_PC2+Age+Sex+Education, data=data.frame(cns18data), force.in="health_PC2")
subset.t_baseline_mean.health_PC2.summary = summary(subset.t_baseline_mean.health_PC2)
subset.results[5,5]=which.min(subset.t_baseline_mean.health_PC2.summary$bic)


# output for models
model.betas = matrix(data=NA, nrow=5, ncol=5)
model.pvals  = matrix(data=NA, nrow=5, ncol=5)

# Make your models based off of subset selection
#Interference
model.v_interference.BMI.fit = lm(v_interference~BMI+Education, data=data.frame(cns18data))
model.v_interference.BMI.summary = summary(model.v_interference.BMI.fit)
model.betas[1,1]=model.v_interference.BMI.summary$coefficients[2,1]
model.pvals[1,1]=model.v_interference.BMI.summary$coefficients[2,4]

model.v_interference.DXA.fit = lm(v_interference~DXA+Education, data=data.frame(cns18data))
model.v_interference.DXA.summary = summary(model.v_interference.DXA.fit)
model.betas[1,2]=model.v_interference.DXA.summary$coefficients[2,1]
model.pvals[1,2]=model.v_interference.DXA.summary$coefficients[2,4]

model.v_interference.VO2.fit = lm(v_interference~VO2+Education, data=data.frame(cns18data))
model.v_interference.VO2.summary = summary(model.v_interference.VO2.fit)
model.betas[1,3]=model.v_interference.VO2.summary$coefficients[2,1]
model.pvals[1,3]=model.v_interference.VO2.summary$coefficients[2,4]

model.v_interference.health_PC1.fit = lm(v_interference~health_PC1+Education, data=data.frame(cns18data))
model.v_interference.health_PC1.summary = summary(model.v_interference.health_PC1.fit)
model.betas[1,4]=model.v_interference.health_PC1.summary$coefficients[2,1]
model.pvals[1,4]=model.v_interference.health_PC1.summary$coefficients[2,4]


model.v_interference.health_PC2.fit = lm(v_interference~health_PC2, data=data.frame(cns18data))
model.v_interference.health_PC2.summary = summary(model.v_interference.health_PC2.fit)
model.betas[1,5]=model.v_interference.health_PC2.summary$coefficients[2,1]
model.pvals[1,5]=model.v_interference.health_PC2.summary$coefficients[2,4]


#Facilitation
model.v_facilitation.BMI.fit = lm(v_facilitation~BMI+Sex, data=data.frame(cns18data))
model.v_facilitation.BMI.summary = summary(model.v_facilitation.BMI.fit)
model.betas[2,1]=model.v_facilitation.BMI.summary$coefficients[2,1]
model.pvals[2,1]=model.v_facilitation.BMI.summary$coefficients[2,4]

model.v_facilitation.DXA.fit = lm(v_facilitation~DXA+Sex, data=data.frame(cns18data))
model.v_facilitation.DXA.summary = summary(model.v_facilitation.DXA.fit)
model.betas[2,2]=model.v_facilitation.DXA.summary$coefficients[2,1]
model.pvals[2,2]=model.v_facilitation.DXA.summary$coefficients[2,4]

model.v_facilitation.VO2.fit = lm(v_facilitation~VO2+Sex, data=data.frame(cns18data))
model.v_facilitation.VO2.summary = summary(model.v_facilitation.VO2.fit)
model.betas[2,3]=model.v_facilitation.VO2.summary$coefficients[2,1]
model.pvals[2,3]=model.v_facilitation.VO2.summary$coefficients[2,4]

model.v_facilitation.health_PC1.fit = lm(v_facilitation~health_PC1+Sex, data=data.frame(cns18data))
model.v_facilitation.health_PC1.summary = summary(model.v_facilitation.health_PC1.fit)
model.betas[2,4]=model.v_facilitation.health_PC1.summary$coefficients[2,1]
model.pvals[2,4]=model.v_facilitation.health_PC1.summary$coefficients[2,4]


model.v_facilitation.health_PC2.fit = lm(v_facilitation~health_PC2+Sex, data=data.frame(cns18data))
model.v_facilitation.health_PC2.summary = summary(model.v_facilitation.health_PC2.fit)
model.betas[2,5]=model.v_facilitation.health_PC2.summary$coefficients[2,1]
model.pvals[2,5]=model.v_facilitation.health_PC2.summary$coefficients[2,4]


#a_baseline_mean

model.a_baseline_mean.BMI.fit = lm(a_baseline_mean~BMI+Education, data=data.frame(cns18data))
model.a_baseline_mean.BMI.summary = summary(model.a_baseline_mean.BMI.fit)
model.betas[3,1]=model.a_baseline_mean.BMI.summary$coefficients[2,1]
model.pvals[3,1]=model.a_baseline_mean.BMI.summary$coefficients[2,4]

model.a_baseline_mean.DXA.fit = lm(a_baseline_mean~DXA+Education, data=data.frame(cns18data))
model.a_baseline_mean.DXA.summary = summary(model.a_baseline_mean.DXA.fit)
model.betas[3,2]=model.a_baseline_mean.DXA.summary$coefficients[2,1]
model.pvals[3,2]=model.a_baseline_mean.DXA.summary$coefficients[2,4]

model.a_baseline_mean.VO2.fit = lm(a_baseline_mean~VO2+Education, data=data.frame(cns18data))
model.a_baseline_mean.VO2.summary = summary(model.a_baseline_mean.VO2.fit)
model.betas[3,3]=model.a_baseline_mean.VO2.summary$coefficients[2,1]
model.pvals[3,3]=model.a_baseline_mean.VO2.summary$coefficients[2,4]

model.a_baseline_mean.health_PC1.fit = lm(a_baseline_mean~health_PC1+Education, data=data.frame(cns18data))
model.a_baseline_mean.health_PC1.summary = summary(model.a_baseline_mean.health_PC1.fit)
model.betas[3,4]=model.a_baseline_mean.health_PC1.summary$coefficients[2,1]
model.pvals[3,4]=model.a_baseline_mean.health_PC1.summary$coefficients[2,4]


model.a_baseline_mean.health_PC2.fit = lm(a_baseline_mean~health_PC2+Education, data=data.frame(cns18data))
model.a_baseline_mean.health_PC2.summary = summary(model.a_baseline_mean.health_PC2.fit)
model.betas[3,5]=model.a_baseline_mean.health_PC2.summary$coefficients[2,1]
model.pvals[3,5]=model.a_baseline_mean.health_PC2.summary$coefficients[2,4]

#z_baseline_mean

model.z_baseline_mean.BMI.fit = lm(z_baseline_mean~BMI+Education, data=data.frame(cns18data))
model.z_baseline_mean.BMI.summary = summary(model.z_baseline_mean.BMI.fit)
model.betas[4,1]=model.z_baseline_mean.BMI.summary$coefficients[2,1]
model.pvals[4,1]=model.z_baseline_mean.BMI.summary$coefficients[2,4]

model.z_baseline_mean.DXA.fit = lm(z_baseline_mean~DXA+Education, data=data.frame(cns18data))
model.z_baseline_mean.DXA.summary = summary(model.z_baseline_mean.DXA.fit)
model.betas[4,2]=model.z_baseline_mean.DXA.summary$coefficients[2,1]
model.pvals[4,2]=model.z_baseline_mean.DXA.summary$coefficients[2,4]

model.z_baseline_mean.VO2.fit = lm(z_baseline_mean~VO2+Education, data=data.frame(cns18data))
model.z_baseline_mean.VO2.summary = summary(model.z_baseline_mean.VO2.fit)
model.betas[4,3]=model.z_baseline_mean.VO2.summary$coefficients[2,1]
model.pvals[4,3]=model.z_baseline_mean.VO2.summary$coefficients[2,4]

model.z_baseline_mean.health_PC1.fit = lm(z_baseline_mean~health_PC1+Education, data=data.frame(cns18data))
model.z_baseline_mean.health_PC1.summary = summary(model.z_baseline_mean.health_PC1.fit)
model.betas[4,4]=model.z_baseline_mean.health_PC1.summary$coefficients[2,1]
model.pvals[4,4]=model.z_baseline_mean.health_PC1.summary$coefficients[2,4]


model.z_baseline_mean.health_PC2.fit = lm(z_baseline_mean~health_PC2+Education, data=data.frame(cns18data))
model.z_baseline_mean.health_PC2.summary = summary(model.z_baseline_mean.health_PC2.fit)
model.betas[4,5]=model.z_baseline_mean.health_PC2.summary$coefficients[2,1]
model.pvals[4,5]=model.z_baseline_mean.health_PC2.summary$coefficients[2,4]


#t_baseline_mean
model.t_baseline_mean.BMI.fit = lm(t_baseline_mean~BMI+Age, data=data.frame(cns18data))
model.t_baseline_mean.BMI.summary = summary(model.t_baseline_mean.BMI.fit)
model.betas[5,1]=model.t_baseline_mean.BMI.summary$coefficients[2,1]
model.pvals[5,1]=model.t_baseline_mean.BMI.summary$coefficients[2,4]

model.t_baseline_mean.DXA.fit = lm(t_baseline_mean~DXA+Sex, data=data.frame(cns18data))
model.t_baseline_mean.DXA.summary = summary(model.t_baseline_mean.DXA.fit)
model.betas[5,2]=model.t_baseline_mean.DXA.summary$coefficients[2,1]
model.pvals[5,2]=model.t_baseline_mean.DXA.summary$coefficients[2,4]

model.t_baseline_mean.VO2.fit = lm(t_baseline_mean~VO2+Age, data=data.frame(cns18data))
model.t_baseline_mean.VO2.summary = summary(model.t_baseline_mean.VO2.fit)
model.betas[5,3]=model.t_baseline_mean.VO2.summary$coefficients[2,1]
model.pvals[5,3]=model.t_baseline_mean.VO2.summary$coefficients[2,4]

model.t_baseline_mean.health_PC1.fit = lm(t_baseline_mean~health_PC1+Age, data=data.frame(cns18data))
model.t_baseline_mean.health_PC1.summary = summary(model.t_baseline_mean.health_PC1.fit)
model.betas[5,4]=model.t_baseline_mean.health_PC1.summary$coefficients[2,1]
model.pvals[5,4]=model.t_baseline_mean.health_PC1.summary$coefficients[2,4]


model.t_baseline_mean.health_PC2.fit = lm(t_baseline_mean~health_PC2+Age, data=data.frame(cns18data))
model.t_baseline_mean.health_PC2.summary = summary(model.t_baseline_mean.health_PC2.fit)
model.betas[5,5]=model.t_baseline_mean.health_PC2.summary$coefficients[2,1]
model.pvals[5,5]=model.t_baseline_mean.health_PC2.summary$coefficients[2,4]


colnames(model.betas)=c('BMI', 'DXA','VO2','PC1','PC2')
rownames(model.betas)=c('Drift Rate Incongruent-Neutral','Drift Rate Congruent - Neutral','Threshold (a)','Bias (z)','Non-Decision Time (t)')
colnames(model.pvals)=c('BMI', 'DXA','VO2','PC1','PC2')
rownames(model.pvals)=c('Drift Rate Incongruent-Neutral','Drift Rate Congruent - Neutral','Threshold (a)','Bias (z)','Non-Decision Time (t)')

# Bayes Factor (BF)
model.BF  = matrix(data=NA, nrow=5, ncol=5)
#Interference
BF.v_interference.BMI.full = BIC(lm(v_interference~BMI+Education, data=data.frame(cns18data)))
BF.v_interference.BMI.null = BIC(lm(v_interference~Education, data=data.frame(cns18data)))
BF.v_interference.BMI.BayesFactor = exp((BF.v_interference.BMI.full -BF.v_interference.BMI.null)/2)
model.BF[1,1]=BF.v_interference.BMI.BayesFactor


BF.v_interference.DXA.full = BIC(lm(v_interference~DXA+Education, data=data.frame(cns18data)))
BF.v_interference.DXA.null = BIC(lm(v_interference~Education, data=data.frame(cns18data)))
BF.v_interference.DXA.BayesFactor = exp((BF.v_interference.DXA.full -BF.v_interference.DXA.null)/2) 
model.BF[1,2]=BF.v_interference.DXA.BayesFactor

BF.v_interference.VO2.full = BIC(lm(v_interference~VO2+Education, data=data.frame(cns18data)))
BF.v_interference.VO2.null = BIC(lm(v_interference~Education, data=data.frame(cns18data)))
BF.v_interference.VO2.BayesFactor = exp((BF.v_interference.VO2.full -BF.v_interference.VO2.null)/2) 
model.BF[1,3]=BF.v_interference.VO2.BayesFactor

BF.v_interference.health_PC1.full = BIC(lm(v_interference~health_PC1+Education, data=data.frame(cns18data)))
BF.v_interference.health_PC1.null = BIC(lm(v_interference~Education, data=data.frame(cns18data)))
BF.v_interference.health_PC1.BayesFactor = exp((BF.v_interference.health_PC1.full -BF.v_interference.health_PC1.null)/2) 
model.BF[1,4]=BF.v_interference.health_PC1.BayesFactor

BF.v_interference.health_PC2.full = BIC(lm(v_interference~health_PC2, data=data.frame(cns18data)))
BF.v_interference.health_PC2.null = BIC(lm(v_interference~1, data=data.frame(cns18data)))
BF.v_interference.health_PC2.BayesFactor = exp((BF.v_interference.health_PC2.full -BF.v_interference.health_PC2.null)/2)
model.BF[1,5]=BF.v_interference.health_PC2.BayesFactor

#Facilitation
BF.v_facilitation.BMI.full = BIC(lm(v_facilitation~BMI+Sex, data=data.frame(cns18data)))
BF.v_facilitation.BMI.null = BIC(lm(v_facilitation~Sex, data=data.frame(cns18data)))
BF.v_facilitation.BMI.BayesFactor = exp((BF.v_facilitation.BMI.full -BF.v_facilitation.BMI.null)/2)
model.BF[2,1]=BF.v_facilitation.BMI.BayesFactor


BF.v_facilitation.DXA.full = BIC(lm(v_facilitation~DXA+Sex, data=data.frame(cns18data)))
BF.v_facilitation.DXA.null = BIC(lm(v_facilitation~Sex, data=data.frame(cns18data)))
BF.v_facilitation.DXA.BayesFactor = exp((BF.v_facilitation.DXA.full -BF.v_facilitation.DXA.null)/2)
model.BF[2,2]=BF.v_facilitation.DXA.BayesFactor


BF.v_facilitation.VO2.full = BIC(lm(v_facilitation~VO2+Sex, data=data.frame(cns18data)))
BF.v_facilitation.VO2.null = BIC(lm(v_facilitation~Sex, data=data.frame(cns18data)))
BF.v_facilitation.VO2.BayesFactor = exp((BF.v_facilitation.VO2.full -BF.v_facilitation.VO2.null)/2) 
model.BF[2,3]=BF.v_facilitation.VO2.BayesFactor

BF.v_facilitation.health_PC1.full = BIC(lm(v_facilitation~health_PC1+Sex, data=data.frame(cns18data)))
BF.v_facilitation.health_PC1.null = BIC(lm(v_facilitation~Sex, data=data.frame(cns18data)))
BF.v_facilitation.health_PC1.BayesFactor = exp((BF.v_facilitation.health_PC1.full -BF.v_facilitation.health_PC1.null)/2) 
model.BF[2,4]=BF.v_facilitation.health_PC1.BayesFactor

BF.v_facilitation.health_PC2.full = BIC(lm(v_facilitation~health_PC2+Sex, data=data.frame(cns18data)))
BF.v_facilitation.health_PC2.null = BIC(lm(v_facilitation~Sex, data=data.frame(cns18data)))
BF.v_facilitation.health_PC2.BayesFactor = exp((BF.v_facilitation.health_PC2.full -BF.v_facilitation.health_PC2.null)/2) 
model.BF[2,5]=BF.v_facilitation.health_PC2.BayesFactor

#a_baseline_mean
BF.a_baseline_mean.BMI.full = BIC(lm(a_baseline_mean~BMI+Education, data=data.frame(cns18data)))
BF.a_baseline_mean.BMI.null = BIC(lm(a_baseline_mean~Education, data=data.frame(cns18data)))
BF.a_baseline_mean.BMI.BayesFactor = exp((BF.a_baseline_mean.BMI.full -BF.a_baseline_mean.BMI.null)/2)
model.BF[3,1]=BF.a_baseline_mean.BMI.BayesFactor

BF.a_baseline_mean.DXA.full = BIC(lm(a_baseline_mean~DXA+Education, data=data.frame(cns18data)))
BF.a_baseline_mean.DXA.null = BIC(lm(a_baseline_mean~Education, data=data.frame(cns18data)))
BF.a_baseline_mean.DXA.BayesFactor = exp((BF.a_baseline_mean.DXA.full -BF.a_baseline_mean.DXA.null)/2)
model.BF[3,2]=BF.a_baseline_mean.DXA.BayesFactor

BF.a_baseline_mean.VO2.full = BIC(lm(a_baseline_mean~VO2+Education, data=data.frame(cns18data)))
BF.a_baseline_mean.VO2.null = BIC(lm(a_baseline_mean~Education, data=data.frame(cns18data)))
BF.a_baseline_mean.VO2.BayesFactor = exp((BF.a_baseline_mean.VO2.full -BF.a_baseline_mean.VO2.null)/2)
model.BF[3,3]=BF.a_baseline_mean.VO2.BayesFactor


BF.a_baseline_mean.health_PC1.full = BIC(lm(a_baseline_mean~health_PC1+Education, data=data.frame(cns18data)))
BF.a_baseline_mean.health_PC1.null = BIC(lm(a_baseline_mean~Education, data=data.frame(cns18data)))
BF.a_baseline_mean.health_PC1.BayesFactor = exp((BF.a_baseline_mean.health_PC1.full -BF.a_baseline_mean.health_PC1.null)/2) 
model.BF[3,4]=BF.a_baseline_mean.BMI.BayesFactor


BF.a_baseline_mean.health_PC2.full = BIC(lm(a_baseline_mean~health_PC2+Education, data=data.frame(cns18data)))
BF.a_baseline_mean.health_PC2.null = BIC(lm(a_baseline_mean~Education, data=data.frame(cns18data)))
BF.a_baseline_mean.health_PC2.BayesFactor = exp((BF.a_baseline_mean.health_PC2.full -BF.a_baseline_mean.health_PC2.null)/2) 
model.BF[3,5]=BF.a_baseline_mean.health_PC2.BayesFactor


#z_baseline_mean
BF.z_baseline_mean.BMI.full = BIC(lm(z_baseline_mean~BMI+Education, data=data.frame(cns18data)))
BF.z_baseline_mean.BMI.null = BIC(lm(z_baseline_mean~Education, data=data.frame(cns18data)))
BF.z_baseline_mean.BMI.BayesFactor = exp((BF.z_baseline_mean.BMI.full -BF.z_baseline_mean.BMI.null)/2) 
model.BF[4,1]=BF.z_baseline_mean.BMI.BayesFactor

BF.z_baseline_mean.DXA.full = BIC(lm(z_baseline_mean~DXA+Education, data=data.frame(cns18data)))
BF.z_baseline_mean.DXA.null = BIC(lm(z_baseline_mean~Education, data=data.frame(cns18data)))
BF.z_baseline_mean.DXA.BayesFactor = exp((BF.z_baseline_mean.DXA.full -BF.z_baseline_mean.DXA.null)/2) 
model.BF[4,2]=BF.z_baseline_mean.DXA.BayesFactor

BF.z_baseline_mean.VO2.full = BIC(lm(z_baseline_mean~VO2+Education, data=data.frame(cns18data)))
BF.z_baseline_mean.VO2.null = BIC(lm(z_baseline_mean~Education, data=data.frame(cns18data)))
BF.z_baseline_mean.VO2.BayesFactor = exp((BF.z_baseline_mean.VO2.full -BF.z_baseline_mean.VO2.null)/2) 
model.BF[4,3]=BF.z_baseline_mean.VO2.BayesFactor

BF.z_baseline_mean.health_PC1.full = BIC(lm(z_baseline_mean~health_PC1+Education, data=data.frame(cns18data)))
BF.z_baseline_mean.health_PC1.null = BIC(lm(z_baseline_mean~Education, data=data.frame(cns18data)))
BF.z_baseline_mean.health_PC1.BayesFactor = exp((BF.z_baseline_mean.health_PC1.full -BF.z_baseline_mean.health_PC1.null)/2) 
model.BF[4,4]=BF.z_baseline_mean.health_PC1.BayesFactor

BF.z_baseline_mean.health_PC2.full = BIC(lm(z_baseline_mean~health_PC2+Education, data=data.frame(cns18data)))
BF.z_baseline_mean.health_PC2.null = BIC(lm(z_baseline_mean~Education, data=data.frame(cns18data)))
BF.z_baseline_mean.health_PC2.BayesFactor = exp((BF.z_baseline_mean.health_PC2.full -BF.z_baseline_mean.health_PC2.null)/2)
model.BF[4,5]=BF.z_baseline_mean.health_PC2.BayesFactor

#t_baseline_mean
BF.t_baseline_mean.BMI.full = BIC(lm(t_baseline_mean~BMI+Age, data=data.frame(cns18data)))
BF.t_baseline_mean.BMI.null = BIC(lm(t_baseline_mean~Age, data=data.frame(cns18data)))
BF.t_baseline_mean.BMI.BayesFactor = exp((BF.t_baseline_mean.BMI.full -BF.t_baseline_mean.BMI.null)/2) 
model.BF[5,1]=BF.t_baseline_mean.BMI.BayesFactor

BF.t_baseline_mean.DXA.full = BIC(lm(t_baseline_mean~DXA+Sex, data=data.frame(cns18data)))
BF.t_baseline_mean.DXA.null = BIC(lm(t_baseline_mean~Sex, data=data.frame(cns18data)))
BF.t_baseline_mean.DXA.BayesFactor = exp((BF.t_baseline_mean.DXA.full -BF.t_baseline_mean.DXA.null)/2) 
model.BF[5,2]=BF.t_baseline_mean.DXA.BayesFactor

BF.t_baseline_mean.VO2.full = BIC(lm(t_baseline_mean~VO2+Age, data=data.frame(cns18data)))
BF.t_baseline_mean.VO2.null = BIC(lm(t_baseline_mean~Age, data=data.frame(cns18data)))
BF.t_baseline_mean.VO2.BayesFactor = exp((BF.t_baseline_mean.VO2.full -BF.t_baseline_mean.VO2.null)/2) 
model.BF[5,3]=BF.t_baseline_mean.VO2.BayesFactor

BF.t_baseline_mean.health_PC1.full = BIC(lm(t_baseline_mean~health_PC1+Age, data=data.frame(cns18data)))
BF.t_baseline_mean.health_PC1.null = BIC(lm(t_baseline_mean~Age, data=data.frame(cns18data)))
BF.t_baseline_mean.health_PC1.BayesFactor = exp((BF.t_baseline_mean.health_PC1.full -BF.t_baseline_mean.health_PC1.null)/2) 
model.BF[5,4]=BF.t_baseline_mean.health_PC1.BayesFactor


BF.t_baseline_mean.health_PC2.full = BIC(lm(t_baseline_mean~health_PC2+Age, data=data.frame(cns18data)))
BF.t_baseline_mean.health_PC2.null = BIC(lm(t_baseline_mean~Age, data=data.frame(cns18data)))
BF.t_baseline_mean.health_PC2.BayesFactor = exp((BF.t_baseline_mean.health_PC2.full -BF.t_baseline_mean.health_PC2.null)/2) 
model.BF[5,5]=BF.t_baseline_mean.health_PC2.BayesFactor

colnames(model.BF)=c('BMI', 'DXA','VO2','PC1','PC2')
rownames(model.BF)=c('Drift Rate Incongruent-Neutral','Drift Rate Congruent - Neutral','Threshold (a)','Bias (z)','Non-Decision Time (t)')
model.BF

pander(model.BF)

write.csv(model.pvals, file='/data/dataDB/WIN/graphs/poster/pvals.csv')
write.csv(model.betas, file='/data/dataDB/WIN/graphs/poster/betas.csv')
write.csv(model.BF, file='/data/dataDB/WIN/graphs/poster/BF.csv')

writePNG(model.BF)
