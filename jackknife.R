#--------------
# Jackknif in R
#--------------

library(tidyverse)
library(xtable)

setwd("C:/Users/julia/OneDrive/Desktop/github/19. Jackknife")

#----------------------------------------------------
# Example 1: bias of the MLE for an exponential model
#----------------------------------------------------

# 1.1 create data
n <- 200
lambda <- 3
set.seed(2023)
sample1 <- data.frame('x' = rexp(n = n, rate = lambda))

# 1.2 write data in a table in LaTex
print(xtable(sample1, type = "latex"), file = "JB_tables.tex")

# 1.3 save the data in .csv file
write.csv(sample1, "dataset.csv")

# 1.4 mle (biased)
lambda.hat <- 1 / mean(x)
lambda.hat # 3.428671

# 2.1 Jackknife estimate of the bias
theta.jack = numeric(n)
for(i in 1:n) {
  theta.jack[i] <- 1 / mean(sample1$x[-i])
}

bias.J <- (n-1) * (mean(theta.jack) - lambda.hat)
bias.J # [1] 0.1681113

# 3.1 Bootstrap estimate of the bias
set.seed(2023)
B = 10000                                             # B: set the number of bootstrap replicates 
bootstrap_object <- matrix(rep(0, B*length(sample1$x)),  nrow = B)
theta.boot <- numeric(B)

# perform the bootstrap using the function sample() with replacement
for(i in 1:B) {
  
  bootstrap_object[i,] <- sample(sample1$x, size = length(sample1$x), replace = TRUE)
  theta.boot[i] <- 1 / mean(bootstrap_object[i, ]) 
}

# estimate of the bias
bias.B <- mean(theta.boot - lambda.hat) 
bias.B # [1] 0.1535831

# 4. theoretical bias
lambda / (n-1) # [1] 0.1578947

# 5. plots
# histogram of the Jacknife estimates of the bias
p1 <- ggplot(data.frame('bias' = theta.jack)) +
  geom_density(aes(x = theta.jack, colour = theta.jack), lwd = 1.2) +
  scale_x_continuous(breaks = round(seq(min(theta.jack) - 0.02, max(theta.jack) + 0.02, length.out = 10), 2)) +
  labs(title = 'Density of the Jackknife estimates of mle',
       subtitle = paste('artificial dataset. bias mleJackknife: ', round(bias.J, 4)),
       y="count", x="bias") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# histogram of the bootstrap replicates of bias
p2<- ggplot(data.frame('x' = theta.boot)) +
  geom_density(aes(x = theta.boot, colour = theta.boot), lwd = 1.2) +
  scale_x_continuous(breaks = round(seq(min(theta.boot) - 0.02, max(theta.boot) + 0.02, length.out = 10), 2)) +
  labs(title = 'Density of the Bootstrap replicates of the mle',
       subtitle =  paste('artificial dataset. bias mle Bootstrap: ', round(bias.B, 4)),
       y="count", x="bias") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

grid.arrange(p1, p2, nrow = 1)

# 6. summary
summary <- data.frame('bias.Jackknife' = bias.J, 'bias.Bootstrap' = bias.B,
           'bias.theoretical' = lambda / (n-1))

print(xtable(round(summary, 4), type = "latex"), file = "JB_tables.tex")

#----
# end
#----
