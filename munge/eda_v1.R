library(dplyr)
library(ggplot2)
library(evir)
library(qrmtools)
df=read.csv('data/test_vv230225.csv') %>% mutate(return=reactions/potentialReach,loss=-reactions/potentialReach)
# %>% mutate(return= scale(return,center = TRUE,scale = FALSE))
mean_excess_plot(df$return)
df %>% ggplot(aes(x=return))+
  geom_density()


mu <- mean(df$return, na.rm = TRUE)
sigma <- sd(df$return, na.rm = TRUE)

# Confidence Levels
alpha <- c(0.95, 0.99)  # 95% and 99% confidence levels
z_values <- qnorm(alpha)

# Compute VaR
VaR <- mu + z_values * sigma
names(VaR) <- paste0("VaR_", alpha * 100, "%")
print(VaR)
df$potentialReach %>% max() %>% {.*VaR}


##Loss
mu <- mean(df$loss, na.rm = TRUE)
sigma <- sd(df$loss, na.rm = TRUE)


# Compute VaR
alpha <- c(0.95, 0.99) 
VaR <- mu + z_values * sigma
names(VaR) <- paste0("VaR_", alpha * 100, "%")
print(VaR)
df$potentialReach %>% max() %>% {.*-VaR}

mean(df$return>=.025)


##2.2
##Var 99 
alpha=.99
u=.01
scale.u10
fit.u205 <- fit_GPD_MLE(df$return[df$return > u])
(VaR.u025 <- VaR_GPDtail(alpha, threshold = u, p.exceed = mean(df$return > u),
                         shape = shape, scale = scale))
(ES.u025 <- ES_GPDtail(alpha, threshold = u, p.exceed = mean(df$return > u),
                       shape = shape, scale = scale))
print(glue::glue('VaR 99 {VaR.u025}, ES {ES.u025} '))

er_vaquero=df %>% mutate(reaction_rate=reactions/potentialReach) %>% summarise(mean(return))
print(glue::glue('ER {er_vaquero}'))
df$potentialReach %>% max() %>% {.*VaR.u025}

### Loss
mean_excess_plot(df$loss)
alpha=.99
u=-.02
(VaR.u025 <- VaR_GPDtail(alpha, threshold = u, p.exceed = mean(df$loss > u),
                         shape = shape, scale = scale))
(ES.u025 <- ES_GPDtail(alpha, threshold = u, p.exceed = mean(df$loss > u),
                       shape = shape, scale = scale))
print(glue::glue('VaR 99 {VaR.u025}, ES {ES.u025} '))

er_vaquero=df %>% mutate(reaction_rate=reactions/potentialReach) %>% summarise(mean(return))
df$potentialReach %>% max() %>% {.*er_vaquero}
print(glue::glue('ER {er_vaquero}'))

df$potentialReach %>% max() %>% {.*VaR.u025*-1}
