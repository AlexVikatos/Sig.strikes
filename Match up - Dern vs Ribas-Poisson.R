##Fight simulation##
##Mackenzie Dern vs. Amanda Ribas
df <- read_csv(file.choose())

Name <- function(name) {
  # Filter rows where the fighter matches either fighter_1_Fighter or fighter_2_Fighter
  relevant_rows <- df[df$fighter_1_Fighter == as.character(name) | df$fighter_2_Fighter == as.character(name), ]
  
  # Extract significant strikes and attempts based on the fighter's position
  sig_strikes <- ifelse(relevant_rows$fighter_1_Fighter == name,
                        relevant_rows$fighter_1_Sig_Strike_Landed,
                        relevant_rows$fighter_2_Sig_Strike_Landed)
  
  sig_strikes_attempts <- ifelse(relevant_rows$fighter_1_Fighter == name,
                                 relevant_rows$fighter_1_Sig_Strike_Attempts,
                                 relevant_rows$fighter_2_Sig_Strike_Attempts)
  
  # Combine the data into a data frame
  result <- data.frame(sig_strikes, sig_strikes_attempts)
  
  return(result)
}


#Data for Mackenzie Dern
Dern <- Name("Mackenzie Dern")

#Data for Amanda Ribas
Ribas=Name("Amanda Ribas")


#Poisson for Dern
lambda_Dern=mean(Dern$sig_strikes)
print(lambda_Dern)

sig_strikes_poisson=function(MM){
  rpois(MM,lambda = lambda_Dern)
}

#Monte Carlo Simulation
set.seed(123)
poisson_dern=replicate(1000,sig_strikes_poisson(1000))


#Poisson for Amanda Ribas
lambda_ribas=mean(Ribas$sig_strikes)
print(lambda_ribas)

sig_strikes_poisson=function(MM){
  rpois(MM,lambda = lambda_ribas)
}

#Monte Carlo Simulation
set.seed(123)
poisson_ribas=replicate(1000,sig_strikes_poisson(1000))



#Comparing the Distributions
fight=data.frame(
  Dern_sig_strikes =as.vector(poisson_dern),
  Ribas_Sig_strikes =as.vector(poisson_ribas)
)

###install.packages("reshape2")
library(reshape2)

# Reshape the data frame into long format for ggplot2
long_fight <- melt(fight, variable.name = "Fighter", value.name = "Significant_Strikes")

# Create a histogram to compare the distributions
ggplot(long_fight, aes(x = Significant_Strikes, fill =Fighter)) +
  geom_histogram(alpha = 0.5, bins = 40, position = "identity") +
  labs(
    title = "Comparison of Fighter Significant Strikes(Poisson Model)",
    x = "Number of Significant Strikes",
    y = "Freq"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("green4", "cadetblue"))

####WHAT ARE THE ODDS THAT RIBAS WILL LAND MORE SIG STRIKES THAN DERN
1/mean(poisson_ribas>poisson_dern)
####WHAT ARE THE ODDS THAT AMANDA RIBAS WILL LAND MORE THAN 24 SIG.STRIKES
1/mean(poisson_ribas>24)
####WHAT ARE THE ODDS THAT AMANDA RIBAS WILL LAND MORE THAN 49 SIG.STRIKES
1/mean(poisson_ribas>49)
####WHAT ARE THE ODDS THAT AMANDA RIBAS WILL LAND MORE THAN 74 SIG.STRIKES
1/mean(poisson_ribas>74)
