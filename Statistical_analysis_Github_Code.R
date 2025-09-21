#This code is used to run Bayesian analysis for each isotope system by sample partition 
#Code adapted for this analysis by G. Busquets-Vass based on Kéry (2010), Introduction to WinBUGS for Ecologists: Bayesian Approach to Regression, ANOVA, mixed models and related analyses. 
#The code is in JAGS language 
 
#Additional instructions for using this code to run analyses: 
#1. replace instances of "isotope" below with the appropriate label (e.g., carbon, nitrogen)
#2. replace instances of "dXisotope" below with the appropriate label (e.g,. d13C, d15N) 
#3. carbon and nitrogen systems must be analysed separately, so these should be uploaded as different .csv files

#### CLEAN EVERYTHING ###
rm(list=ls())
graphics.off() # close all;
gc() # Clear memory

#set working directory 
setwd ("your/directory/here") 

#Install packages required for model 
install.packages("mcmcplots")
install.packages("R2jags")

# read data file (carbon isotope values first - these are Suess corrected) 
ClassBulk <- read.csv("filename_here.csv")

#convert classes to factors 
ClassBulk$Class <- as.factor(ClassBulk$Class)  
levels(ClassBulk$Class)


# Change levels order - adjust level names as required 

ClassBulk$Class <- factor(ClassBulk$Class,
                          levels = c("1_AUS_B",    #abbreviated to AUSB 
                                     "2_FIS_S",     #abbreviated to FISS
                                     "3_NZ_S_Rēkohu",  #abbreviated to NZSR
                                     "4_NZ_S_Māhia",    #abbreviated to NZSM
                                     "5_NZ_B_1_early",   #abbreviated to NZB1E
                                     "6_NZ_B_1_late",    #abbreviated to NZB1L
                                     "7_NZ_B_2"))        #abbreviated to NZB2

#### Text file to save the model: 
sink("model_mean_isotopeBulk.txt")

#model starts here - cat = concatonate and print results 
#dunif = uninformed priors used here 
#produce mean and standard deviation values for each sample partition 

cat("model {

     # Likelihood:
     for (i in 1:n_isotope) {
        N[i] ~ dnorm(mu_isotope[Class[i]], 
        1/pow(sd_isotope[group[i]], 2))
     }

     # Priors:
     for (j in 1:n_Class) {
       mu_isotope[j] ~ dunif(min_mu_isotope, max_mu_isotope)
     }
     for (k in 1:n_group) {
       sd_isotope[k] ~ dunif(min_sd_isotope, max_sd_isotope)
     }

 #Derived quantities (differences between each group/sample partition): adjust group names as required 
     
     diff_sds <- sd_isotope[1] - sd_isotope[2] 
     
     diff_1AUSB_2FISS <-     mu_isotope[1] - mu_isotope[2]
     diff_1AUSB_3_NZSR <-    mu_isotope[1] - mu_isotope[3]
     diff_1AUSB_4_NZSM <-    mu_isotope[1] - mu_isotope[4]
     diff_1AUSB_5_NZB1E <-   mu_isotope[1] - mu_isotope[5]
     diff_1AUSB_6_NZB1L <-   mu_isotope[1] - mu_isotope[6]
     diff_1AUSB_7_NZB2 <-    mu_isotope[1] - mu_isotope[7]
     
     
     diff_2FISS_3_NZSR <-    mu_isotope[2] - mu_isotope[3]
     diff_2FISS_4_NZSM <-    mu_isotope[2] - mu_isotope[4]
     diff_2FISS_5_NZB1E <-   mu_isotope[2] - mu_isotope[5]
     diff_2FISS_6_NZB1L <-   mu_isotope[2] - mu_isotope[6]
     diff_2FISS_7_NZB2 <-    mu_isotope[2] - mu_isotope[7]
     
     
     diff_3_NZSR_4_NZSM <-   mu_isotope[3] - mu_isotope[4]
     diff_3_NZSR_5_NZB1E <-  mu_isotope[3] - mu_isotope[5]
     diff_3_NZSR_6_NZB1L <-  mu_isotope[3] - mu_isotope[6]
     diff_3_NZSR_7_NZB2 <-   mu_isotope[3] - mu_isotope[7]
     
    
     diff_4_NZSM_6_NZB1L <-  mu_isotope[4] - mu_isotope[6]
     diff_4_NZSM_7_NZB2 <-   mu_isotope[4] - mu_isotope[7]
     
     
     diff_5_NZB1E_6_NZB1L <-  mu_isotope[5] - mu_isotope[6]
     diff_5_NZB1E_7_NZB2 <-   mu_isotope[5] - mu_isotope[7]
  
     
     diff_6_NZB1L_7_NZB2 <-   mu_isotope[6] - mu_isotope[7]
     

 }",
    fill=TRUE)

sink() # end of JAGS model

data_list <- list(
  n_isotope = length(ClassBulk$dXisotope),
  N = ClassBulk$dXisotope,
  Class = ClassBulk$Class,
  n_Class = length(unique(ClassBulk$Class)),
  
  group = ClassBulk$Class,
  n_group = length(unique(ClassBulk$Class)),
  
  min_mu_isotope = -25,      #these maximum and minimum values will be different depending on the isotope system being analysed - adjust to set appropriate limits 
  max_mu_isotope = -10,
  
  min_sd_isotope = 0,
  max_sd_isotope = 12
)

monitor <- c("mu_isotope",
             "sd_isotope",
             "diff_sds", 
             "diff_1AUSB_2FISS", 
             "diff_1AUSB_3_NZSR", 
             "diff_1AUSB_4_NZSM",
             "diff_1AUSB_5_NZB1E", 
             "diff_1AUSB_6_NZB1L", 
             "diff_1AUSB_7_NZB2",
             "diff_2FISS_3_NZSR", 
             "diff_2FISS_4_NZSM", 
             "diff_2FISS_5_NZB1E",
             "diff_2FISS_6_NZB1L", 
             "diff_2FISS_7_NZB2",
             "diff_3_NZSR_4_NZSM",
             "diff_3_NZSR_5_NZB1E", 
             "diff_3_NZSR_6_NZB1L", 
             "diff_3_NZSR_7_NZB2",
             "diff_4_NZSM_5_NZB1E", 
             "diff_4_NZSM_6_NZB1L",
             "diff_4_NZSM_7_NZB2",
             "diff_5_NZB1E_6_NZB1L",
             "diff_5_NZB1E_7_NZB2", 
             "diff_6_NZB1L_7_NZB2")




# MCMC control 
n.chains <- 5 
n.iter <- 1000000
n.burnin <- 200000
n.thin <- 50    #how many values the model keeps (1 in 50) to avoid autocorrelation 

# Run analysis:
require(R2jags)

out <- jags(data = data_list,
            # inits = inits.list,
            parameters.to.save = monitor,
            model.file = "model_mean_isotopeBulk.txt",
            n.chains = n.chains,
            n.thin = n.thin,
            n.iter = n.iter,
            n.burnin = n.burnin,
            DIC = T)

#### MODEL RESULTS ###
# Summarize posteriors and statistics:
WhaleID_summary_isotope <- print(out, dig=3) # number of decimals for the output.

# Extract MCMC list: 
mcmc_list <- as.mcmc(out) # For JAGS
require(mcmcplots)
library(MCMCvis)
require(MCMCvis)

# Libraries for diagnostics:
require(ggmcmc)  # For MCMC diagnostics
require(coda)    # For MCMC analysis
require(lattice) # For quick posterior plotting and diagnostics


# Dataframe of posteriors:
post <- as.data.frame(out$BUGSoutput$sims.matrix)

require(ggplot2)
x11()
ggplot()+
  geom_density(data = post,
               aes(x = diff_sds))


# Probabilities of proportions:
#sd:

index_posit <- which(post$diff_sds > 0)
prop_posit <- 100*(length(index_posit)/nrow(post))

#get the positive and negative proportions for the differences in the posterior means of isotope systems 
#for each sample partition, plus the proportion of posterior draws where the difference in means is great than 0

# 1 mus diff_1AUSB_2FISS

index_posit_1AUSBB_2FISS <- which(post$diff_1AUSB_2FISS > 0)
index_negat_1AUSB_2FISS <- which(post$diff_1AUSB_2FISS < 0)
Pos_diff_1AUSB_2FISS <- 100*(length(index_posit_1AUSB_2FISS)/nrow(post))
Neg_diff_1AUSB_2FISS <- 100*(length(index_negat_1AUSB_2FISS)/nrow(post))

# 2 mu diff_1AUSB_3_NZSR

index_posit_1AUSB_3_NZSR <- which(post$diff_1AUSB_3_NZSR > 0)
index_negat_1AUSB_3_NZSR <- which(post$diff_1AUSB_3_NZSR < 0)
Pos_diff_1AUSB_3_NZSR <- 100*(length(index_posit_1AUSB_3_NZSR)/nrow(post))
Neg_diff_1AUSB_3_NZSR <- 100*(length(index_negat_1AUSB_3_NZSR)/nrow(post))

# 3 mu diff_1AUSB_4_NZSM

index_posit_diff_1AUSB_4_NZSM <- which(post$diff_1AUSB_4_NZSM > 0)
index_negat_diff_1AUSB_4_NZSM <- which(post$diff_1AUSB_4_NZSM < 0)
Pos_diff_1AUSB_4_NZSM <- 100*(length(index_posit_diff_1AUSB_4_NZSM)/nrow(post))
Neg_diff_1AUSB_4_NZSM <- 100*(length(index_negat_diff_1AUSB_4_NZSM)/nrow(post))

# 4 mu diff_1AUSB_5_NZB1E

index_posit_diff_1AUSB_5_NZB1E <- which(post$diff_1AUSB_5_NZB1E > 0)
index_negat_diff_diff_1AUSB_5_NZB1E <- which(post$diff_1AUSB_5_NZB1E < 0)
Pos_diff_1AUSB_5_NZB1E <- 100*(length(index_posit_diff_1AUSB_5_NZB1E)/nrow(post))
Neg_diff_1AUSB_5_NZB1E <- 100*(length(index_negat_diff_diff_1AUSB_5_NZB1E)/nrow(post))

# 5 mu diff_1AUSB_6_NZB1L

index_posit_diff_1AUSB_6_NZB1L <- which(post$diff_1AUSB_6_NZB1L > 0)
index_negat_diff_1AUSB_6_NZB1L <- which(post$diff_1AUSB_6_NZB1L < 0)
Pos_diff_1AUSB_6_NZB1L <- 100*(length(index_posit_diff_1AUSB_6_NZB1L)/nrow(post))
Neg_diff_1AUSB_6_NZB1L <- 100*(length(index_negat_diff_1AUSB_6_NZB1L)/nrow(post))

# 6 mu diff_1AUSB_7_NZB2

index_posit_diff_1AUSB_7_NZB2 <- which(post$diff_1AUSB_7_NZB2 > 0)
index_negat_diff_1AUSB_7_NZB2 <- which(post$diff_1AUSB_7_NZB2 < 0)
Pos_diff_1AUSB_7_NZB2 <- 100*(length(index_posit_diff_1AUSB_7_NZB2)/nrow(post))
Neg_diff_1AUSB_7_NZB2 <- 100*(length(index_negat_diff_1AUSB_7_NZB2)/nrow(post))

# 7 mu  diff_2FISS_3_NZSR

index_posit_diff_2FISS_3_NZSR <- which(post$diff_2FISS_3_NZSR > 0)
index_negat_diff_2FISS_3_NZSR <- which(post$diff_2FISS_3_NZSR < 0)
Pos_diff_2FISS_3_NZSR <- 100*(length(index_posit_diff_2FISS_3_NZSR)/nrow(post))
Neg_diff_2FISS_3_NZSR <- 100*(length(index_negat_diff_2FISS_3_NZSR)/nrow(post))

# 8 mu  diff_2FISS_4_NZSM

index_posit_diff_2FISS_4_NZSM <- which(post$diff_2FISS_4_NZSM > 0)
index_negat_diff_2FISS_4_NZSM <- which(post$diff_2FISS_4_NZSM < 0)
Pos_diff_2FISS_4_NZSM <- 100*(length(index_posit_diff_2FISS_4_NZSM)/nrow(post))
Neg_diff_2FISS_4_NZSM <- 100*(length(index_negat_diff_2FISS_4_NZSM)/nrow(post))

# 9 mu diff_2FISS_5_NZB1E

index_posit_diff_2FISS_5_NZB1E <- which(post$diff_2FISS_5_NZB1E > 0)
index_negat_diff_2FISS_5_NZB1E <- which(post$diff_2FISS_5_NZB1E < 0)
Pos_diff_2FISS_5_NZB1E <- 100*(length(index_posit_diff_2FISS_5_NZB1E)/nrow(post))
Neg_diff_2FISS_5_NZB1E <- 100*(length(index_negat_diff_2FISS_5_NZB1E)/nrow(post))

# 10 mu diff_2FISS_6_NZB1L

index_posit_diff_2FISS_6_NZB1L <- which(post$diff_2FISS_6_NZB1L > 0)
index_negat_diff_2FISS_6_NZB1L <- which(post$diff_2FISS_6_NZB1L < 0)
Pos_diff_2FISS_6_NZB1L <- 100*(length(index_posit_diff_2FISS_6_NZB1L)/nrow(post))
Neg_diff_2FISS_6_NZB1L <- 100*(length(index_negat_diff_2FISS_6_NZB1L)/nrow(post))

# 11 mu  diff_2FISS_7_NZB2

index_posit_diff_2FISS_7_NZB2 <- which(post$diff_2FISS_7_NZB2 > 0)
index_negat_diff_2FISS_7_NZB2 <- which(post$diff_2FISS_7_NZB2 < 0)
Pos_diff_2FISS_7_NZB2 <- 100*(length(index_posit_diff_2FISS_7_NZB2)/nrow(post))
Neg_diff_2FISS_7_NZB2 <- 100*(length(index_negat_diff_2FISS_7_NZB2)/nrow(post))


# 12 mu diff_3_NZSR_4_NZSM


index_posit_diff_3_NZSR_4_NZSM <- which(post$diff_3_NZSR_4_NZSM > 0)
index_negat_diff_3_NZSR_4_NZSM <- which(post$diff_3_NZSR_4_NZSM < 0)
Pos_diff_3_NZSR_4_NZSM <- 100*(length(index_posit_diff_3_NZSR_4_NZSM)/nrow(post))
Neg_diff_3_NZSR_4_NZSM <- 100*(length(index_negat_diff_3_NZSR_4_NZSM)/nrow(post))

# 13 mu diff_3_NZSR_5_NZB1E

index_posit_diff_3_NZSR_5_NZB1E <- which(post$diff_3_NZSR_5_NZB1E > 0)
index_negat_diff_3_NZSR_5_NZB1E <- which(post$diff_3_NZSR_5_NZB1E < 0)
Pos_diff_3_NZSR_5_NZB1E <- 100*(length(index_posit_diff_3_NZSR_5_NZB1E)/nrow(post))
Neg_diff_3_NZSR_5_NZB1E <- 100*(length(index_negat_diff_3_NZSR_5_NZB1E)/nrow(post))

# 14 mu diff_3_NZSR_6_NZB1L

index_posit_diff_3_NZSR_6_NZB1L <- which(post$diff_3_NZSR_6_NZB1L > 0)
index_negat_diff_3_NZSR_6_NZB1L <- which(post$diff_3_NZSR_6_NZB1L < 0)
Pos_diff_3_NZSR_6_NZB1L <- 100*(length(index_posit_diff_3_NZSR_6_NZB1L)/nrow(post))
Neg_diff_3_NZSR_6_NZB1L <- 100*(length(index_negat_diff_3_NZSR_6_NZB1L)/nrow(post))

# 15 mu diff_3_NZSR_7_NZB2

index_posit_diff_3_NZSR_7_NZB2 <- which(post$diff_3_NZSR_7_NZB2 > 0)
index_negat_diff_3_NZSR_7_NZB2 <- which(post$diff_3_NZSR_7_NZB2 < 0)
Pos_diff_3_NZSR_7_NZB2 <- 100*(length(index_posit_diff_3_NZSR_7_NZB2)/nrow(post))
Neg_diff_3_NZSR_7_NZB2 <- 100*(length(index_negat_diff_3_NZSR_7_NZB2)/nrow(post))

# 16 mu diff_4_NZSM_5_NZB1E

index_posit_diff_4_NZSM_5_NZB1E <- which(post$diff_4_NZSM_5_NZB1E > 0)
index_negat_diff_4_NZSM_5_NZB1E <- which(post$diff_4_NZSM_5_NZB1E < 0)
Pos_diff_4_NZSM_5_NZB1E <- 100*(length(index_posit_diff_4_NZSM_5_NZB1E)/nrow(post))
Neg_diff_4_NZSM_5_NZB1E <- 100*(length(index_negat_diff_4_NZSM_5_NZB1E)/nrow(post))

# 17 mu diff_4_NZSM_6_NZB1L

index_posit_diff_4_NZSM_6_NZB1L <- which(post$diff_4_NZSM_6_NZB1L > 0)
index_negat_diff_4_NZSM_6_NZB1L <- which(post$diff_4_NZSM_6_NZB1L < 0)
Pos_diff_4_NZSM_6_NZB1L <- 100*(length(index_posit_diff_4_NZSM_6_NZB1L)/nrow(post))
Neg_diff_4_NZSM_6_NZB1L <- 100*(length(index_negat_diff_4_NZSM_6_NZB1L)/nrow(post))

# 18 mu diff_4_NZSM_7_NZB2

index_posit_diff_4_NZSM_7_NZB2 <- which(post$diff_4_NZSM_7_NZB2 > 0)
index_negat_diff_4_NZSM_7_NZB2 <- which(post$diff_4_NZSM_7_NZB2 < 0)
Pos_diff_4_NZSM_7_NZB2 <- 100*(length(index_posit_diff_4_NZSM_7_NZB2)/nrow(post))
Neg_diff_4_NZSM_7_NZB2 <- 100*(length(index_negat_diff_4_NZSM_7_NZB2)/nrow(post))

# 19 mu diff_5_NZB1E_6_NZB1L

index_posit_diff_5_NZB1E_6_NZB1L <- which(post$diff_5_NZB1E_6_NZB1L > 0)
index_negat_diff_5_NZB1E_6_NZB1L <- which(post$diff_5_NZB1E_6_NZB1L < 0)
Pos_diff_5_NZB1E_6_NZB1L <- 100*(length(index_posit_diff_5_NZB1E_6_NZB1L)/nrow(post))
Neg_diff_5_NZB1E_6_NZB1L <- 100*(length(index_negat_diff_5_NZB1E_6_NZB1L)/nrow(post))

# 20 mu diff_5_NZB1E_7_NZB2

index_posit_diff_5_NZB1E_7_NZB2 <- which(post$diff_5_NZB1E_7_NZB2 > 0)
index_negat_diff_5_NZB1E_7_NZB2 <- which(post$diff_5_NZB1E_7_NZB2 < 0)
Pos_diff_5_NZB1E_7_NZB2 <- 100*(length(index_posit_diff_5_NZB1E_7_NZB2)/nrow(post))
Neg_diff_5_NZB1E_7_NZB2 <- 100*(length(index_negat_diff_5_NZB1E_7_NZB2)/nrow(post))

# 21 mu diff_6_NZB1L_7_NZB2

index_posit_diff_6_NZB1L_7_NZB2 <- which(post$diff_6_NZB1L_7_NZB2 > 0)
index_negat_diff_6_NZB1L_7_NZB2 <- which(post$diff_6_NZB1L_7_NZB2 < 0)
Pos_diff_6_NZB1L_7_NZB2 <- 100*(length(index_posit_diff_6_NZB1L_7_NZB2)/nrow(post))
Neg_diff_6_NZB1L_7_NZB2 <- 100*(length(index_negat_diff_6_NZB1L_7_NZB2)/nrow(post))
