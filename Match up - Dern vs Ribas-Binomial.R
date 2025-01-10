##Match Up Dern vs Ribas with Binomial Distribution

#Data for Dern
Dern=Name("Mackenzie Dern")

#Data for Ribas
Ribas=Name("Amanda Ribas")

#Compute Binomial Parameters-Dern
sig_attemps_dern=mean(Dern$sig_strikes_attempts)
accuracy_dern=mean(Dern$sig_strikes/Dern$sig_strikes_attempts)
#Compute Binomial Parameters-Ribas
sig_attemps_ribas=mean(Ribas$sig_strikes_attempts)
accuracy_ribas=mean(Ribas$sig_strikes/Ribas$sig_strikes_attempts)

#Function to generate Binomial-distributed Samples-Dern
sig_strikes_binomial_dern = function(MM) {
  rbinom(MM, size = round(sig_attemps_dern), prob = accuracy_dern)
}

#Function to generate Binomial-distributed Samples-Ribas
sig_strikes_binomial_ribas = function(MM) {
  rbinom(MM, size = round(sig_attemps_ribas), prob = accuracy_ribas)
}


#Monte Carlo Sim
set.seed(123)
dern_binomial=replicate(1000,sig_strikes_binomial_dern(1000))
ribas_binomial=replicate(1000,sig_strikes_binomial_ribas(1000))

#Comparing the Distributions
dern_ribas=data.frame(
  dern_sig_strikes=as.vector(dern_binomial),
  ribas_sig_strikes=as.vector(ribas_binomial)
)

library(reshape2)

long=melt(dern_ribas,variable.name ="Fighter",value.name ="Sig.Strikes" )

##PLOT##
ggplot(long, aes(x = Sig.Strikes, fill =Fighter)) +
  geom_density(alpha = 0.5, bins = 30, position = "identity") +
  labs(
    title = "Comparison of Fighter Significant Strikes(Binomial)",
    x = "Number of Significant Strikes",
    y = "Freq"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("green4", "cadetblue"))




















