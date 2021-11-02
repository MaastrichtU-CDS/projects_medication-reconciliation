# Investigating sample size criteria Riley et al. (2020)


# Change ##


## Source preprocessed data

source("C:\\Users\\P70070766\\Documents\\Medication reconciliation\\Scripts\\20211027 - Preprocessing.R")
rm(list = setdiff(ls(), "data"))

## Criterion B1: Intercept with a margin of error of 0.05
## Inigo's change
fun.B1 <- function(prop_prior) { 
  ( 1.96 / 0.05 ) ** 2 * prop_prior * ( 1 - prop_prior ) 
  }

data.graph.prop.y <- data[, c("id", "y")]
data.graph.prop.y$cumsum_y <- cumsum(data.graph.prop.y$y)
data.graph.prop.y$entry <- 1:1526
data.graph.prop.y$prop.y <- data.graph.prop.y$cumsum_y / data.graph.prop.y$entry

ggplot(data.graph.prop.y, aes(x = entry, y = prop.y)) +
  geom_point(size = 0.8) +
  geom_point(aes(x = data.graph.prop.y[1077,]$entry, y = data.graph.prop.y[1077,]$prop.y), colour = "orange", shape = 18, size = 5) +
  geom_point(aes(x = data.graph.prop.y[1526,]$entry, y = data.graph.prop.y[1077,]$prop.y), colour = "orangered", shape = 18, size = 5) +
  scale_y_continuous(name = "Proportion of events: composite end-point(%)", breaks = seq(0, 0.080, 0.005), labels = format(seq(0, 8, 0.5), digits = 2)) +
  scale_x_continuous(name = "Entry number", breaks = seq(0, 1600, 100)) +
  theme_minimal() 

max(subset(data.graph.prop.y, entry >= 1000)$prop.y)
min(subset(data.graph.prop.y, entry >= 1000)$prop.y)

n.B1 <- as.data.frame(matrix(nrow = length(seq(0.040, 0.050, 0.001)), ncol = 2))
colnames(n.B1) <- c('prop_prior', 'n')
n.B1$prop_prior <- seq(0.040, 0.050, 0.001)
n.B1$n <- apply(n.B1, 1, function(n.B1) ( 1.96/0.05 ) ** 2 * n.B1[1] * ( 1 - n.B1[1] ))

ggplot(n.B1, aes(x = prop_prior, y = n)) +
  geom_point() +
  scale_y_continuous(name = "Number of patients", breaks = seq(55, 75, 1)) +
  scale_x_continuous(name = "Specified proportion (composite end-point)", breaks = seq(0.040, 0.050, 0.001)) +
  theme_minimal()

## Criterion B2: Mean Absolute Prediction Error (MAPE) of max. 0.05

fun.B2 <- function(prop_prior, p_cand, MAPE) {
  exp( ( -0.508 + 0.259 * log(prop_prior) + 0.504 * log(p_cand) - log(MAPE) ) / 0.544 )
} # Max p_cand of 30; max advised MAPE of 0.050.

prop_prior <- seq(0.040, 0.050, 0.001)
p_cand <- 1:30
MAPE <- seq(0.005, 0.050, 0.005)
n.B2 <- expand.grid(prop_prior, p_cand, MAPE)
colnames(n.B2) <- c('prop_prior', 'p_cand', 'MAPE')
n.B2$n <- apply(n.B2, 1, function(n.B2) exp( ( -0.508 + 0.259 * log(n.B2[1]) + 0.504 * log(n.B2[2]) - log(n.B2[3]) ) / 0.544 ))

max(n.B2$n)
max(subset(n.B2, MAPE == "0.05")$n)
max(subset(n.B2, MAPE == "0.01")$n)
max(subset(n.B2, MAPE == "0.005")$n)

ggplot(subset(n.B2, prop_prior == 0.04), aes(x = MAPE, y = n, colour = as.factor(p_cand))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  scale_y_continuous(name = "Number of patients", breaks = seq(0, 35000, 1000)) +
  scale_x_continuous(name = "Mean Absolute Prediction Error (MAPE)", breaks = seq(0.005, 0.050, 0.005), labels = format(seq(0.005, 0.050, 0.005), digits = 2)) +
  scale_color_discrete(name = "Number of candidate parameters") +
  ggtitle("Specified proportion of events (composite end-point): 4.0 %") + 
  theme_minimal()

ggplot(subset(n.B2, (prop_prior == 0.04) & (MAPE != 0.005)), aes(x = MAPE, y = n, colour = as.factor(p_cand))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  scale_y_continuous(name = "Number of patients", breaks = seq(0, 35000, 1000)) +
  scale_x_continuous(name = "Mean Absolute Prediction Error (MAPE)", breaks = seq(0.010, 0.050, 0.005), labels = format(seq(0.010, 0.050, 0.005), digits = 2)) +
  scale_color_discrete(name = "Number of candidate parameters") +
  ggtitle("Specified proportion of events (composite end-point): 4.0 %") + 
  theme_minimal()

ggplot(subset(n.B2, prop_prior == 0.045), aes(x = MAPE, y = n, colour = as.factor(p_cand))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  scale_y_continuous(name = "Number of patients", breaks = seq(0, 35000, 1000)) +
  scale_x_continuous(name = "Mean Absolute Prediction Error (MAPE)", breaks = seq(0.005, 0.050, 0.005), labels = format(seq(0.005, 0.050, 0.005), digits = 2)) +
  scale_color_discrete(name = "Number of candidate parameters") +
  ggtitle("Specified proportion of events (composite end-point): 4.5 %") + 
  theme_minimal()

ggplot(subset(n.B2, (prop_prior == 0.045) & (MAPE != 0.005)), aes(x = MAPE, y = n, colour = as.factor(p_cand))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  scale_y_continuous(name = "Number of patients", breaks = seq(0, 35000, 1000)) +
  scale_x_continuous(name = "Mean Absolute Prediction Error (MAPE)", breaks = seq(0.010, 0.050, 0.005), labels = format(seq(0.010, 0.050, 0.005), digits = 2)) +
  scale_color_discrete(name = "Number of candidate parameters") +
  ggtitle("Specified proportion of events (composite end-point): 4.5 %") + 
  theme_minimal()

ggplot(subset(n.B2, prop_prior == 0.05), aes(x = MAPE, y = n, colour = as.factor(p_cand))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  scale_y_continuous(name = "Number of patients", breaks = seq(0, 35000, 1000)) +
  scale_x_continuous(name = "Mean Absolute Prediction Error (MAPE)", breaks = seq(0.005, 0.050, 0.005), labels = format(seq(0.005, 0.050, 0.005), digits = 2)) +
  scale_color_discrete(name = "Number of candidate parameters") +
  ggtitle("Specified proportion of events (composite end-point): 5.0 %") + 
  theme_minimal()

ggplot(subset(n.B2, (prop_prior == 0.05) & (MAPE != 0.005)), aes(x = MAPE, y = n, colour = as.factor(p_cand))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  scale_y_continuous(name = "Number of patients", breaks = seq(0, 35000, 1000)) +
  scale_x_continuous(name = "Mean Absolute Prediction Error (MAPE)", breaks = seq(0.010, 0.050, 0.005), labels = format(seq(0.010, 0.050, 0.005), digits = 2)) +
  scale_color_discrete(name = "Number of candidate parameters") +
  ggtitle("Specified proportion of events (composite end-point): 5.0 %") + 
  theme_minimal()

## Criterion B3: Shrinkage factor of 0.90

fun.B3 <- function(cand_p, s, Rsqcs) { 
  ( cand_p ) / ( (s - 1) * log( 1 - Rsqcs / s) ) 
} 

fun.lnLnull <- function(n_events, n_patients) {
  n_events * log( n_events / n_patients ) + ( n_patients - n_events ) * log( 1 - n_events / n_patients )
}

data.lnLnull <- fun.lnLnull(n_events = 69, n_patients = 1526)

fun.maxRsqcs <- function(lnLnull, n_patients) {
  1 - exp( ( 2 * lnLnull ) / n_patients )
}

data.maxRsqcs <- fun.maxRsqcs(lnLnull = data.lnLnull, n_patients = 1526)

Rsqn.data.Rsqcs <- as.data.frame(matrix(nrow = 4, ncol = 2))
colnames(Rsqn.data.Rsqcs) <- c('Rsqn', 'data.Rsqcs')
Rsqn.data.Rsqcs[1, 1] <- 0.15 
Rsqn.data.Rsqcs[1, 2] <- 0.15 * data.maxRsqcs
Rsqn.data.Rsqcs[2, 1] <- 0.10
Rsqn.data.Rsqcs[2, 2] <- 0.10 * data.maxRsqcs
Rsqn.data.Rsqcs[3, 1] <- 0.05
Rsqn.data.Rsqcs[3, 2] <- 0.05 * data.maxRsqcs
Rsqn.data.Rsqcs[4, 1] <- 0.01 
Rsqn.data.Rsqcs[4, 2] <- 0.01 * data.maxRsqcs

n.B3 <- expand.grid(Rsqn.data.Rsqcs$data.Rsqcs, 1:30)
n.B3 <- cbind(n.B3, NA)
colnames(n.B3) <- c('data.Rsqcs', 'p_cand', 'n')
n.B3$n <- apply(n.B3, 1, function(n.B3) ( n.B3[2] ) / ( ( 0.90 - 1) * log( 1 - n.B3[1] / 0.90 ) )) 
n.B3 <- as.data.frame(n.B3)

ggplot(n.B3, aes(x = p_cand, y = n, color = as.factor(data.Rsqcs))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  scale_x_continuous(name = "Number of candidate parameters", breaks = seq(1, 30, 1), labels = seq(1, 30, 1)) +
  scale_y_continuous(name = "Number of participants") +
  scale_colour_discrete(name = "Rsq Nagelkerke", labels = c( "1 %", "5 %", "10 %", "15 %")) +
  ggtitle("Criterion B3: Shrinkage factor of 0.90") +
  theme_minimal()

ggplot(subset(n.B3, data.Rsqcs != min(n.B3$data.Rsqcs)), aes(x = p_cand, y = n, color = as.factor(data.Rsqcs))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  scale_x_continuous(name = "Number of candidate parameters", breaks = seq(1, 30, 1), labels = seq(1, 30, 1)) +
  scale_y_continuous(name = "Number of participants", breaks = seq (0, 18000, 1000), labels = seq(0, 18000, 1000)) +
  scale_colour_discrete(name = "Rsq Nagelkerke", labels = c("5 %", "10 %", "15 %")) +
  ggtitle("Criterion B3: Shrinkage factor of 0.90") +
  theme_minimal() +
  theme(panel.grid.minor = element_line(colour = "white"))

## Criterion B4: Optimism of 0.05

fun.s.optimism <- function(Rsqcs, optimism, maxRsqcs) {
  Rsqcs / ( Rsqcs + optimism * maxRsqcs )
}

n.B4 <- Rsqn.data.Rsqcs
n.B4$data.maxRsqcs <- data.maxRsqcs
n.B4$optimism <- 0.05
n.B4$s.optimism <- apply(n.B4, 1, function(n.B4) n.B4[2] / ( n.B4[2] + n.B4[4] * n.B4[3] )) 
n.B4 <- merge(n.B4, 1:30, by = NULL)
colnames(n.B4)[6] <- 'p_cand'
n.B4$n <- apply(n.B4, 1, function(n.B4) ( n.B4[6] ) / ( ( n.B4[5] - 1 ) * log( 1 - n.B4[2] / n.B4[5] ) ))

ggplot(n.B4, aes(x = p_cand, y = n, colour = as.factor(Rsqn))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  scale_x_continuous(name = "Number of candidate parameters", breaks = seq(1, 30, 1), labels = seq(1, 30, 1)) +
  scale_y_continuous(name = "Number of participants") +
  scale_colour_discrete(name = "Rsq Nagelkerke", labels = c("1 %", "5 %", "10 %", "15 %")) +
  ggtitle("Criterion B4: Optimism of 0.05") +
  theme_minimal() +
  theme(panel.grid.minor = element_line(colour = "white"))

## Checks

### Calculations in paper

fun.B1(prop_prior = 0.50) # Fig. 1, page 9: 384.2

fun.B2(prop_prior = 0.30, p_cand = 10, MAPE = 0.050) # Fig. 2, page 10: 460.9

fun.B3(cand_p = 20, s = 0.90, Rsqcs = 0.10) # Fig. 3, page 10: 1698 

fun.lnLnull(n_events = 50, n_patients = 100) # Supplementary material, S5, page 5: -69.315

fun.maxRsqcs(lnLnull = -69.315, n_patients = 100) # Supplementary material, S5, page 5: 0.75

fun.s.optimism(Rsqcs = 0.20 , optimism = 0.05, maxRsqcs = 0.33) # Fig. 5, page 12: 0.924

fun.B3(cand_p = 20, s = 0.924, Rsqcs = 0.20) # Fig. 5, page 12: 1078.9

### pmsampsize

library('pmsampsize')

pmsampsize(type = "b", rsquared = 0.046220050, parameters = 30, prevalence = .045)

n.B1[6,] # Criterion B1 = Criteria 3 

n.B3[117,] # Criterion B3 = Criteria 1

n.B4[117,] # Criterion B4 = Criteria 2 # Difference likely due to rounding.
fun.B3(cand_p = 30, s = 0.750, Rsqcs = 0.046220050)
fun.B3(cand_p = 30, s = 0.749, Rsqcs = 0.046220050) 



