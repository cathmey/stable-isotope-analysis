#This code is used to run Bayesian analysis for each isotope system by sample partition 
#Code adapted for this analysis by G. Busquets-Vass based on Kéry (2010), Introduction to WinBUGS for Ecologists: Bayesian Approach to Regression, ANOVA, mixed models and related analyses. 

#The code is in JAGS language 

#note that carbon and nitrogen systems are analysed separately, so these are uploaded as different CSV files 

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
sink("model_mean_carbonBulk.txt")

#model starts here - cat = concatonate and print results 
#dunif = uninformed priors used here 
#produce mean and standard deviation values for each sample partition 

cat("model {

     # Likelihood:
     for (i in 1:n_carbon) {
        N[i] ~ dnorm(mu_carbon[Class[i]], 
        1/pow(sd_carbon[group[i]], 2))
     }

     # Priors:
     for (j in 1:n_Class) {
       mu_carbon[j] ~ dunif(min_mu_carbon, max_mu_carbon)
     }
     for (k in 1:n_group) {
       sd_carbon[k] ~ dunif(min_sd_carbon, max_sd_carbon)
     }

 #Derived quantities (differences between each group/sample partition): adjust group names as required 
     
     diff_sds <- sd_carbon[1] - sd_carbon[2] 
     
     diff_1AUSB_2FISS <-     mu_carbon[1] - mu_carbon[2]
     diff_1AUSB_3_NZSR <-    mu_carbon[1] - mu_carbon[3]
     diff_1AUSB_4_NZSM <-    mu_carbon[1] - mu_carbon[4]
     diff_1AUSB_5_NZB1E <-   mu_carbon[1] - mu_carbon[5]
     diff_1AUSB_6_NZB1L <-   mu_carbon[1] - mu_carbon[6]
     diff_1AUSB_7_NZB2 <-    mu_carbon[1] - mu_carbon[7]
     
     
     diff_2FISS_3_NZSR <-    mu_carbon[2] - mu_carbon[3]
     diff_2FISS_4_NZSM <-    mu_carbon[2] - mu_carbon[4]
     diff_2FISS_5_NZB1E <-  mu_carbon[2] - mu_carbon[5]
     diff_2FISS_6_NZB1L <-   mu_carbon[2] - mu_carbon[6]
     diff_2FISS_7_NZB2 <-   mu_carbon[2] - mu_carbon[7]
     
     
     diff_3_NZSR_4_NZSM <-    mu_carbon[3] - mu_carbon[4]
     diff_3_NZSR_5_NZB1E <-  mu_carbon[3] - mu_carbon[5]
     diff_3_NZSR_6_NZB1L <-   mu_carbon[3] - mu_carbon[6]
     diff_3_NZSR_7_NZB2 <-   mu_carbon[3] - mu_carbon[7]
     
    
     diff_4_NZSM_5_NZB1E <-  mu_carbon[4] - mu_carbon[5]
     diff_4_NZSM_6_NZB1L <-   mu_carbon[4] - mu_carbon[6]
     diff_4_NZSM_7_NZB2 <-   mu_carbon[4] - mu_carbon[7]
     
     
     diff_5_NZB1E_6_NZB1L <-   mu_carbon[5] - mu_carbon[6]
     diff_5_NZB1E_7_NZB2 <-   mu_carbon[5] - mu_carbon[7]
  
     
     diff_6_NZB1L_7_NZB2 <-   mu_carbon[6] - mu_carbon[7]
     

 }",
    fill=TRUE)

sink() # end of JAGS model

data_list <- list(
  n_carbon = length(ClassBulk$d13C),
  N = ClassBulk$d13C,
  Class = ClassBulk$Class,
  n_Class = length(unique(ClassBulk$Class)),
  
  group = ClassBulk$Class,
  n_group = length(unique(ClassBulk$Class)),
  
  min_mu_carbon = -25,      #these maximum and minimum values are different depending on the isotope system being analysed - adjust to set appropriate limits 
  max_mu_carbon = -10,
  
  min_sd_carbon = 0,
  max_sd_carbon = 12
)

monitor <- c("mu_carbon",
             "sd_carbon",
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
            model.file = "model_mean_carbonBulk.txt",
            n.chains = n.chains,
            n.thin = n.thin,
            n.iter = n.iter,
            n.burnin = n.burnin,
            DIC = T)

#### MODEL RESULTS ###
# Summarize posteriors and statistics:
WhaleID_summary_CARB <- print(out, dig=3) # number of decimals for the output.

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
               aes(x = dif_sds))


# Probabilities of proportions:
#sd:

index_posit <- which(post$diff_sds > 0)
prop_posit <- 100*(length(index_posit)/nrow(post))
