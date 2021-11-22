########################################################################################################################

# Investigating sample size criteria Riley et al. (2020)

########################################################################################################################

########################################################################################################################

# Source preprocessed data

########################################################################################################################

source("./Preprocessing.R")
rm(list = setdiff(ls(), "data"))

########################################################################################################################

# Criterion B1: Intercept with a margin of error of 0.05

########################################################################################################################

fun.B1 <- function(prop_prior) { 
  ( 1.96 / 0.05 ) ** 2 * prop_prior * ( 1 - prop_prior ) 
}

# y

data.graph.prop.y <- data[, c("id", "y")]
data.graph.prop.y$cumsum_y <- cumsum(data.graph.prop.y$y)
data.graph.prop.y$entry <- 1:1526 # Entries are not chronological! 
data.graph.prop.y$prop.y <- data.graph.prop.y$cumsum_y / data.graph.prop.y$entry

ggplot(data.graph.prop.y, aes(x = entry, y = prop.y)) +
  geom_point(size = 0.8) +
  geom_point(aes(x = data.graph.prop.y[1077,]$entry, y = data.graph.prop.y[1077,]$prop.y), 
             colour = "orange", shape = 18, size = 5) +
  geom_text(data = data.graph.prop.y[1077,], aes(label = "Dataset 1", vjust = -2)) +
  geom_point(aes(x = data.graph.prop.y[1526,]$entry, y = data.graph.prop.y[1077,]$prop.y), 
             colour = "orangered", shape = 18, size = 5) +
  geom_text(data = data.graph.prop.y[1526,], aes(label = "Dataset 2", vjust = -2)) +
  scale_y_continuous(name = "Proportion of events: composite end-point (%)", 
                     breaks = seq(0, 0.080, 0.005), 
                     labels = format(seq(0, 8, 0.5), digits = 2)) +
  scale_x_continuous(name = "Entry number", 
                     breaks = seq(0, 1600, 100)) +
  theme_minimal() 

max(subset(data.graph.prop.y, entry >= 1000)$prop.y)
min(subset(data.graph.prop.y, entry >= 1000)$prop.y)

n.B1.y <- as.data.frame(matrix(nrow = length(seq(0.040, 0.050, 0.001)), ncol = 2))
colnames(n.B1.y) <- c('prop_prior', 'n')
n.B1.y$prop_prior <- seq(0.040, 0.050, 0.001)
n.B1.y$n <- apply(n.B1.y, 1, function(n.B1.y) ( 1.96/0.05 ) ** 2 * n.B1.y[1] * ( 1 - n.B1.y[1] ))

ggplot(n.B1.y, aes(x = prop_prior, y = n)) +
  geom_point() +
  scale_y_continuous(name = "Number of patients", 
                     breaks = seq(55, 75, 1)) +
  scale_x_continuous(name = "Specified proportion: composite end-point (%)", 
                     breaks = seq(0.040, 0.050, 0.001),
                     labels = format(seq(4, 5, 0.1), digits = 2)) +
  theme_minimal()

# y.interaction

data.graph.prop.y.interaction <- data[, c("id", "y.interaction")]
data.graph.prop.y.interaction$cumsum_y.interaction <- cumsum(data.graph.prop.y.interaction$y.interaction)
data.graph.prop.y.interaction$entry <- 1:1526 # Entries are not chronological! 
data.graph.prop.y.interaction$prop.y.interaction <- data.graph.prop.y.interaction$cumsum_y.interaction / data.graph.prop.y.interaction$entry

ggplot(data.graph.prop.y.interaction, aes(x = entry, y = prop.y.interaction)) +
  geom_point(size = 0.8) +
  geom_point(aes(x = data.graph.prop.y.interaction[1077,]$entry, y = data.graph.prop.y.interaction[1077,]$prop.y.interaction), 
             colour = "orange", shape = 18, size = 5) +
  geom_text(data = data.graph.prop.y.interaction[1077,], aes(label = "Dataset 1", vjust = -2)) +
  geom_point(aes(x = data.graph.prop.y.interaction[1526,]$entry, y = data.graph.prop.y.interaction[1077,]$prop.y.interaction), 
             colour = "orangered", shape = 18, size = 5) +
  geom_text(data = data.graph.prop.y.interaction[1526,], aes(label = "Dataset 2", vjust = -2)) +
  scale_y_continuous(name = "Proportion of events: medications interaction (%)", 
                     breaks = seq(0, 0.030, 0.005), 
                     labels = format(seq(0, 3, 0.5), digits = 2)) +
  scale_x_continuous(name = "Entry number", 
                     breaks = seq(0, 1600, 100)) +
  theme_minimal() 

max(subset(data.graph.prop.y.interaction, entry >= 1000)$prop.y.interaction)
min(subset(data.graph.prop.y.interaction, entry >= 1000)$prop.y.interaction)

n.B1.y.interaction <- as.data.frame(matrix(nrow = length(seq(0.018, 0.026, 0.001)), ncol = 2))
colnames(n.B1.y.interaction) <- c('prop_prior', 'n')
n.B1.y.interaction$prop_prior <- seq(0.018, 0.026, 0.001)
n.B1.y.interaction$n <- apply(n.B1.y.interaction, 1, function(n.B1.y.interaction) ( 1.96/0.05 ) ** 2 * n.B1.y.interaction[1] * ( 1 - n.B1.y.interaction[1] ))

ggplot(n.B1.y.interaction, aes(x = prop_prior, y = n)) +
  geom_point() +
  scale_y_continuous(name = "Number of patients", 
                     breaks = seq(25, 40, 1)) +
  scale_x_continuous(name = "Specified proportion: medications interaction (%)", 
                     breaks = seq(0.018, 0.026, 0.001),
                     labels = format(seq(1.8, 2.6, 0.1), digits = 2)) +
  theme_minimal()

# y.revision_wo_interaction

data.graph.prop.y.revision_wo_interaction <- data[, c("id", "y.revision_wo_interaction")]
data.graph.prop.y.revision_wo_interaction$cumsum_y.revision_wo_interaction <- cumsum(data.graph.prop.y.revision_wo_interaction$y.revision_wo_interaction)
data.graph.prop.y.revision_wo_interaction$entry <- 1:1526 # Entries are not chronological! 
data.graph.prop.y.revision_wo_interaction$prop.y.revision_wo_interaction <- data.graph.prop.y.revision_wo_interaction$cumsum_y.revision_wo_interaction / data.graph.prop.y.revision_wo_interaction$entry

ggplot(data.graph.prop.y.revision_wo_interaction, aes(x = entry, y = prop.y.revision_wo_interaction)) +
  geom_point(size = 0.8) +
  geom_point(aes(x = data.graph.prop.y.revision_wo_interaction[1077,]$entry, y = data.graph.prop.y.revision_wo_interaction[1077,]$prop.y.revision_wo_interaction), 
             colour = "orange", shape = 18, size = 5) +
  geom_text(data = data.graph.prop.y.revision_wo_interaction[1077,], aes(label = "Dataset 1", vjust = -2)) +
  geom_point(aes(x = data.graph.prop.y.revision_wo_interaction[1526,]$entry, y = data.graph.prop.y.revision_wo_interaction[1077,]$prop.y.revision_wo_interaction), 
             colour = "orangered", shape = 18, size = 5) +
  geom_text(data = data.graph.prop.y.revision_wo_interaction[1526,], aes(label = "Dataset 2", vjust = -2)) +
  scale_y_continuous(name = "Proportion of events: revision without medications interaction (%)", 
                     breaks = seq(0, 0.080, 0.005), 
                     labels = format(seq(0, 8, 0.5), digits = 2)) +
  scale_x_continuous(name = "Entry number", 
                     breaks = seq(0, 1600, 100)) +
  theme_minimal() 

max(subset(data.graph.prop.y.revision_wo_interaction, entry >= 1000)$prop.y.revision_wo_interaction)
min(subset(data.graph.prop.y.revision_wo_interaction, entry >= 1000)$prop.y.revision_wo_interaction)

n.B1.y.revision_wo_interaction <- as.data.frame(matrix(nrow = length(seq(0.020, 0.027, 0.001)), ncol = 2))
colnames(n.B1.y.revision_wo_interaction) <- c('prop_prior', 'n')
n.B1.y.revision_wo_interaction$prop_prior <- seq(0.020, 0.027, 0.001)
n.B1.y.revision_wo_interaction$n <- apply(n.B1.y.revision_wo_interaction, 1, function(n.B1.y.revision_wo_interaction) ( 1.96/0.05 ) ** 2 * n.B1.y.revision_wo_interaction[1] * ( 1 - n.B1.y.revision_wo_interaction[1] ))

ggplot(n.B1.y.revision_wo_interaction, aes(x = prop_prior, y = n)) +
  geom_point() +
  scale_y_continuous(name = "Number of patients", 
                     breaks = seq(29, 41, 1)) +
  scale_x_continuous(name = "Specified proportion: revision without medications interaction (%)", 
                     breaks = seq(0.020, 0.027, 0.001),
                     labels = format(seq(2.0, 2.7, 0.1), digits = 2)) +
  theme_minimal()

########################################################################################################################

# Criterion B2: Mean Absolute Prediction Error (MAPE) of max 0.05

########################################################################################################################

fun.B2 <- function(prop_prior, p_cand, MAPE) {
  exp( ( -0.508 + 0.259 * log(prop_prior) + 0.504 * log(p_cand) - log(MAPE) ) / 0.544 )
} # Max p_cand of 30; max advised MAPE of 0.050.

p_cand <- 1:30
MAPE <- seq(0.005, 0.050, 0.005)

# y

prop_prior.y <- seq(0.040, 0.050, 0.001)
n.B2.y <- expand.grid(prop_prior.y, p_cand, MAPE)
colnames(n.B2.y) <- c('prop_prior', 'p_cand', 'MAPE')
n.B2.y$n <- apply(n.B2.y, 1, function(n.B2.y) exp( ( -0.508 + 0.259 * log(n.B2.y[1]) + 0.504 * log(n.B2.y[2]) - log(n.B2.y[3]) ) / 0.544 ))

max(n.B2.y$n)
max(subset(n.B2.y, MAPE == "0.05")$n)
max(subset(n.B2.y, MAPE == "0.01")$n)
max(subset(n.B2.y, MAPE == "0.005")$n)

ggplot(subset(n.B2.y, (prop_prior == 0.045) & (MAPE != 0.005)), aes(x = MAPE, y = n, colour = as.factor(p_cand))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  annotate("text", 
           max(subset(n.B2.y, (prop_prior == "0.045") & (MAPE != "0.005"))$MAPE), 
           y = 1526, vjust = -1, 
           label = "n = 1526") +
  scale_y_continuous(name = "Number of patients", 
                     breaks = seq(0, 35000, 1000)) +
  scale_x_continuous(name = "Mean Absolute Prediction Error (MAPE)", 
                     breaks = seq(0.010, 0.050, 0.005), 
                     labels = format(seq(0.010, 0.050, 0.005), digits = 2)) +
  scale_color_discrete(name = "Number of candidate parameters") +
  ggtitle("Specified proportion of events: composite end-point: 4.5 %") + 
  theme_minimal()

# y.interaction

prop_prior.y.interaction <- seq(0.018, 0.026, 0.001)
n.B2.y.interaction <- expand.grid(prop_prior.y.interaction, p_cand, MAPE)
colnames(n.B2.y.interaction) <- c('prop_prior', 'p_cand', 'MAPE')
n.B2.y.interaction$n <- apply(n.B2.y.interaction, 1, function(n.B2.y.interaction) exp( ( -0.508 + 0.259 * log(n.B2.y.interaction[1]) + 0.504 * log(n.B2.y.interaction[2]) - log(n.B2.y.interaction[3]) ) / 0.544 ))

max(n.B2.y.interaction$n)
max(subset(n.B2.y.interaction, MAPE == "0.05")$n)
max(subset(n.B2.y.interaction, MAPE == "0.01")$n)
max(subset(n.B2.y.interaction, MAPE == "0.005")$n)

ggplot(subset(n.B2.y.interaction, (prop_prior == "0.02") & (MAPE != "0.005")), aes(x = MAPE, y = n, colour = as.factor(p_cand))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  annotate("text", 
           max(subset(n.B2.y.interaction, (prop_prior == "0.02") & (MAPE != "0.005"))$MAPE), 
           y = 1526, vjust = -1, 
           label = "n = 1526") +
  scale_y_continuous(name = "Number of patients", 
                     breaks = seq(0, 8000, 1000)) +
  scale_x_continuous(name = "Mean Absolute Prediction Error (MAPE)", 
                     breaks = seq(0.010, 0.050, 0.005), 
                     labels = format(seq(0.010, 0.050, 0.005), digits = 2)) +
  scale_color_discrete(name = "Number of candidate parameters") +
  ggtitle("Specified proportion of events: medications interaction: 2.0 %") + 
  theme_minimal()

# y.revision_wo_interaction

prop_prior.y.revision_wo_interaction <- seq(0.020, 0.027, 0.001)
n.B2.y.revision_wo_interaction <- expand.grid(prop_prior.y.revision_wo_interaction, p_cand, MAPE)
colnames(n.B2.y.revision_wo_interaction) <- c('prop_prior', 'p_cand', 'MAPE')
n.B2.y.revision_wo_interaction$n <- apply(n.B2.y.revision_wo_interaction, 1, function(n.B2.y.revision_wo_interaction) exp( ( -0.508 + 0.259 * log(n.B2.y.revision_wo_interaction[1]) + 0.504 * log(n.B2.y.revision_wo_interaction[2]) - log(n.B2.y.revision_wo_interaction[3]) ) / 0.544 ))

max(n.B2.y.revision_wo_interaction$n)
max(subset(n.B2.y.revision_wo_interaction, MAPE == "0.05")$n)
max(subset(n.B2.y.revision_wo_interaction, MAPE == "0.01")$n)
max(subset(n.B2.y.revision_wo_interaction, MAPE == "0.005")$n)

ggplot(subset(n.B2.y.revision_wo_interaction, (prop_prior == "0.023") & (MAPE != "0.005")), aes(x = MAPE, y = n, colour = as.factor(p_cand))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  annotate("text", 
           max(subset(n.B2.y.revision_wo_interaction, (prop_prior == "0.023") & (MAPE != "0.005"))$MAPE), 
           y = 1526, vjust = -1, 
           label = "n = 1526") +
  scale_y_continuous(name = "Number of patients", 
                     breaks = seq(0, 8000, 1000)) +
  scale_x_continuous(name = "Mean Absolute Prediction Error (MAPE)", 
                     breaks = seq(0.010, 0.050, 0.005), 
                     labels = format(seq(0.010, 0.050, 0.005), digits = 2)) +
  scale_color_discrete(name = "Number of candidate parameters") +
  ggtitle("Specified proportion of events: revision without medications interaction: 2.3 %") + 
  theme_minimal()

########################################################################################################################

# Criterion B3: Shrinkage factor of 0.90

########################################################################################################################

fun.B3 <- function(cand_p, s, Rsqcs) { 
  ( cand_p ) / ( (s - 1) * log( 1 - Rsqcs / s) ) 
} 

fun.lnLnull <- function(n_events, n_patients) {
  n_events * log( n_events / n_patients ) + ( n_patients - n_events ) * log( 1 - n_events / n_patients )
}

fun.maxRsqcs <- function(lnLnull, n_patients) {
  1 - exp( ( 2 * lnLnull ) / n_patients )
}

# y

lnLnull.y <- fun.lnLnull(n_events = 69, n_patients = 1526)

maxRsqcs.y <- fun.maxRsqcs(lnLnull = lnLnull.y, n_patients = 1526)

Rsqn.Rsqcs.y <- as.data.frame(matrix(nrow = 4, ncol = 2))
colnames(Rsqn.Rsqcs.y) <- c('Rsqn', 'Rsqcs')
Rsqn.Rsqcs.y[1, 1] <- 0.15 
Rsqn.Rsqcs.y[1, 2] <- 0.15 * maxRsqcs.y
Rsqn.Rsqcs.y[2, 1] <- 0.10
Rsqn.Rsqcs.y[2, 2] <- 0.10 * maxRsqcs.y
Rsqn.Rsqcs.y[3, 1] <- 0.05
Rsqn.Rsqcs.y[3, 2] <- 0.05 * maxRsqcs.y
Rsqn.Rsqcs.y[4, 1] <- 0.01 
Rsqn.Rsqcs.y[4, 2] <- 0.01 * maxRsqcs.y

n.B3.y <- expand.grid(Rsqn.Rsqcs.y$Rsqcs, 1:30)
n.B3.y <- cbind(n.B3.y, NA)
colnames(n.B3.y) <- c('Rsqcs', 'p_cand', 'n')
n.B3.y$n <- apply(n.B3.y, 1, function(n.B3.y) ( n.B3.y[2] ) / ( ( 0.90 - 1) * log( 1 - n.B3.y[1] / 0.90 ) )) 
n.B3.y <- as.data.frame(n.B3.y)

ggplot(subset(n.B3.y, Rsqcs != min(n.B3.y$Rsqcs)), aes(x = p_cand, y = n, color = as.factor(Rsqcs))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  annotate("text", 
           29, 
           y = 1526, 
           vjust = -1, 
           label = "n = 1526") +
  scale_x_continuous(name = "Number of candidate parameters", 
                     breaks = seq(1, 30, 1), 
                     labels = seq(1, 30, 1)) +
  scale_y_continuous(name = "Number of participants", 
                     breaks = seq (0, 18000, 1000), 
                     labels = seq(0, 18000, 1000)) +
  scale_colour_discrete(name = "Rsq Nagelkerke", 
                        labels = c("5 %", "10 %", "15 %")) +
  ggtitle("Composite end-point") +
  theme_minimal() +
  theme(panel.grid.minor = element_line(colour = "white"))

# y.interaction

lnLnull.y.interaction <- fun.lnLnull(n_events = 33, n_patients = 1526)

maxRsqcs.y.interaction <- fun.maxRsqcs(lnLnull = lnLnull.y.interaction, n_patients = 1526)

Rsqn.Rsqcs.y.interaction <- as.data.frame(matrix(nrow = 4, ncol = 2))
colnames(Rsqn.Rsqcs.y.interaction) <- c('Rsqn', 'Rsqcs')
Rsqn.Rsqcs.y.interaction[1, 1] <- 0.15 
Rsqn.Rsqcs.y.interaction[1, 2] <- 0.15 * maxRsqcs.y.interaction
Rsqn.Rsqcs.y.interaction[2, 1] <- 0.10
Rsqn.Rsqcs.y.interaction[2, 2] <- 0.10 * maxRsqcs.y.interaction
Rsqn.Rsqcs.y.interaction[3, 1] <- 0.05
Rsqn.Rsqcs.y.interaction[3, 2] <- 0.05 * maxRsqcs.y.interaction
Rsqn.Rsqcs.y.interaction[4, 1] <- 0.01 
Rsqn.Rsqcs.y.interaction[4, 2] <- 0.01 * maxRsqcs.y.interaction

n.B3.y.interaction <- expand.grid(Rsqn.Rsqcs.y.interaction$Rsqcs, 1:30)
n.B3.y.interaction <- cbind(n.B3.y.interaction, NA)
colnames(n.B3.y.interaction) <- c('Rsqcs', 'p_cand', 'n')
n.B3.y.interaction$n <- apply(n.B3.y.interaction, 1, function(n.B3.y.interaction) ( n.B3.y.interaction[2] ) / ( ( 0.90 - 1) * log( 1 - n.B3.y.interaction[1] / 0.90 ) )) 
n.B3.y.interaction <- as.data.frame(n.B3.y.interaction)

ggplot(subset(n.B3.y.interaction, Rsqcs != min(n.B3.y.interaction$Rsqcs)), aes(x = p_cand, y = n, color = as.factor(Rsqcs))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  annotate("text", 
           29, 
           y = 1526, 
           vjust = -1, 
           label = "n = 1526") +
  scale_x_continuous(name = "Number of candidate parameters", 
                     breaks = seq(1, 30, 1), 
                     labels = seq(1, 30, 1)) +
  scale_y_continuous(name = "Number of participants", 
                     breaks = seq (0, 30000, 1000), 
                     labels = seq(0, 30000, 1000)) +
  scale_colour_discrete(name = "Rsq Nagelkerke", 
                        labels = c("5 %", "10 %", "15 %")) +
  ggtitle("Medications interaction") +
  theme_minimal() +
  theme(panel.grid.minor = element_line(colour = "white"))

# y.revision_wo_interaction

lnLnull.y.revision_wo_interaction <- fun.lnLnull(n_events = 36, n_patients = 1526)

maxRsqcs.y.revision_wo_interaction <- fun.maxRsqcs(lnLnull = lnLnull.y.revision_wo_interaction, n_patients = 1526)

Rsqn.Rsqcs.y.revision_wo_interaction <- as.data.frame(matrix(nrow = 4, ncol = 2))
colnames(Rsqn.Rsqcs.y.revision_wo_interaction) <- c('Rsqn', 'Rsqcs')
Rsqn.Rsqcs.y.revision_wo_interaction[1, 1] <- 0.15 
Rsqn.Rsqcs.y.revision_wo_interaction[1, 2] <- 0.15 * maxRsqcs.y.revision_wo_interaction
Rsqn.Rsqcs.y.revision_wo_interaction[2, 1] <- 0.10
Rsqn.Rsqcs.y.revision_wo_interaction[2, 2] <- 0.10 * maxRsqcs.y.revision_wo_interaction
Rsqn.Rsqcs.y.revision_wo_interaction[3, 1] <- 0.05
Rsqn.Rsqcs.y.revision_wo_interaction[3, 2] <- 0.05 * maxRsqcs.y.revision_wo_interaction
Rsqn.Rsqcs.y.revision_wo_interaction[4, 1] <- 0.01 
Rsqn.Rsqcs.y.revision_wo_interaction[4, 2] <- 0.01 * maxRsqcs.y.revision_wo_interaction

n.B3.y.revision_wo_interaction <- expand.grid(Rsqn.Rsqcs.y.revision_wo_interaction$Rsqcs, 1:30)
n.B3.y.revision_wo_interaction <- cbind(n.B3.y.revision_wo_interaction, NA)
colnames(n.B3.y.revision_wo_interaction) <- c('Rsqcs', 'p_cand', 'n')
n.B3.y.revision_wo_interaction$n <- apply(n.B3.y.revision_wo_interaction, 1, function(n.B3.y.revision_wo_interaction) ( n.B3.y.revision_wo_interaction[2] ) / ( ( 0.90 - 1) * log( 1 - n.B3.y.revision_wo_interaction[1] / 0.90 ) )) 
n.B3.y.revision_wo_interaction <- as.data.frame(n.B3.y.revision_wo_interaction)

ggplot(subset(n.B3.y.revision_wo_interaction, Rsqcs != min(n.B3.y.revision_wo_interaction$Rsqcs)), aes(x = p_cand, y = n, color = as.factor(Rsqcs))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  annotate("text", 
           29, 
           y = 1526, 
           vjust = -1, 
           label = "n = 1526") +
  scale_x_continuous(name = "Number of candidate parameters", 
                     breaks = seq(1, 30, 1), 
                     labels = seq(1, 30, 1)) +
  scale_y_continuous(name = "Number of participants", 
                     breaks = seq (0, 30000, 1000), 
                     labels = seq(0, 30000, 1000)) +
  scale_colour_discrete(name = "Rsq Nagelkerke", 
                        labels = c("5 %", "10 %", "15 %")) +
  ggtitle("Revision without medications interaction") +
  theme_minimal() +
  theme(panel.grid.minor = element_line(colour = "white"))

########################################################################################################################

# Criterion B4: Optimism of 0.05

########################################################################################################################

fun.s.optimism <- function(Rsqcs, optimism, maxRsqcs) {
  Rsqcs / ( Rsqcs + optimism * maxRsqcs )
}

# y

n.B4.y <- Rsqn.Rsqcs.y
n.B4.y$maxRsqcs <- maxRsqcs.y
n.B4.y$optimism <- 0.05
n.B4.y$s.optimism <- apply(n.B4.y, 1, function(n.B4.y) n.B4.y[2] / ( n.B4.y[2] + n.B4.y[4] * n.B4.y[3] )) 
n.B4.y <- merge(n.B4.y, 1:30, by = NULL)
colnames(n.B4.y)[6] <- 'p_cand'
n.B4.y$n <- apply(n.B4.y, 1, function(n.B4.y) ( n.B4.y[6] ) / ( ( n.B4.y[5] - 1 ) * log( 1 - n.B4.y[2] / n.B4.y[5] ) ))

ggplot(n.B4.y, aes(x = p_cand, y = n, colour = as.factor(Rsqn))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  annotate("text", 
           29, 
           y = 1526, 
           vjust = -1, 
           label = "n = 1526") +
  scale_x_continuous(name = "Number of candidate parameters", 
                     breaks = seq(1, 30, 1), 
                     labels = seq(1, 30, 1)) +
  scale_y_continuous(name = "Number of participants") +
  scale_colour_discrete(name = "Rsq Nagelkerke", 
                        labels = c("1 %", "5 %", "10 %", "15 %")) +
  ggtitle("Composite end-point") +
  theme_minimal() +
  theme(panel.grid.minor = element_line(colour = "white"))

# y.interaction

n.B4.y.interaction <- Rsqn.Rsqcs.y.interaction
n.B4.y.interaction$maxRsqcs <- maxRsqcs.y.interaction
n.B4.y.interaction$optimism <- 0.05
n.B4.y.interaction$s.optimism <- apply(n.B4.y.interaction, 1, function(n.B4.y.interaction) n.B4.y.interaction[2] / ( n.B4.y.interaction[2] + n.B4.y.interaction[4] * n.B4.y.interaction[3] )) 
n.B4.y.interaction <- merge(n.B4.y.interaction, 1:30, by = NULL)
colnames(n.B4.y.interaction)[6] <- 'p_cand'
n.B4.y.interaction$n <- apply(n.B4.y.interaction, 1, function(n.B4.y.interaction) ( n.B4.y.interaction[6] ) / ( ( n.B4.y.interaction[5] - 1 ) * log( 1 - n.B4.y.interaction[2] / n.B4.y.interaction[5] ) ))

ggplot(n.B4.y.interaction, aes(x = p_cand, y = n, colour = as.factor(Rsqn))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  annotate("text", 
           29, 
           y = 1526, 
           vjust = -1, 
           label = "n = 1526") +
  scale_x_continuous(name = "Number of candidate parameters", 
                     breaks = seq(1, 30, 1), 
                     labels = seq(1, 30, 1)) +
  scale_y_continuous(name = "Number of participants") +
  scale_colour_discrete(name = "Rsq Nagelkerke", 
                        labels = c("1 %", "5 %", "10 %", "15 %")) +
  ggtitle("Medications interaction") +
  theme_minimal() +
  theme(panel.grid.minor = element_line(colour = "white"))

# y.revision_wo_interaction

n.B4.y.revision_wo_interaction <- Rsqn.Rsqcs.y.revision_wo_interaction
n.B4.y.revision_wo_interaction$maxRsqcs <- maxRsqcs.y.revision_wo_interaction
n.B4.y.revision_wo_interaction$optimism <- 0.05
n.B4.y.revision_wo_interaction$s.optimism <- apply(n.B4.y.revision_wo_interaction, 1, function(n.B4.y.revision_wo_interaction) n.B4.y.revision_wo_interaction[2] / ( n.B4.y.revision_wo_interaction[2] + n.B4.y.revision_wo_interaction[4] * n.B4.y.revision_wo_interaction[3] )) 
n.B4.y.revision_wo_interaction <- merge(n.B4.y.revision_wo_interaction, 1:30, by = NULL)
colnames(n.B4.y.revision_wo_interaction)[6] <- 'p_cand'
n.B4.y.revision_wo_interaction$n <- apply(n.B4.y.revision_wo_interaction, 1, function(n.B4.y.revision_wo_interaction) ( n.B4.y.revision_wo_interaction[6] ) / ( ( n.B4.y.revision_wo_interaction[5] - 1 ) * log( 1 - n.B4.y.revision_wo_interaction[2] / n.B4.y.revision_wo_interaction[5] ) ))

ggplot(n.B4.y.revision_wo_interaction, aes(x = p_cand, y = n, colour = as.factor(Rsqn))) +
  geom_point() +
  geom_hline(yintercept = 1526) +
  annotate("text", 
           29, 
           y = 1526, 
           vjust = -1, 
           label = "n = 1526") +
  scale_x_continuous(name = "Number of candidate parameters", 
                     breaks = seq(1, 30, 1), 
                     labels = seq(1, 30, 1)) +
  scale_y_continuous(name = "Number of participants") +
  scale_colour_discrete(name = "Rsq Nagelkerke", 
                        labels = c("1 %", "5 %", "10 %", "15 %")) +
  ggtitle("Revision without medications interaction") +
  theme_minimal() +
  theme(panel.grid.minor = element_line(colour = "white"))

########################################################################################################################

# Checks

########################################################################################################################

# Calculations in paper

fun.B1(prop_prior = 0.50) # Fig. 1, page 9: 384.2

fun.B2(prop_prior = 0.30, p_cand = 10, MAPE = 0.050) # Fig. 2, page 10: 460.9

fun.B3(cand_p = 20, s = 0.90, Rsqcs = 0.10) # Fig. 3, page 10: 1698 

fun.lnLnull(n_events = 50, n_patients = 100) # Supplementary material, S5, page 5: -69.315

fun.maxRsqcs(lnLnull = -69.315, n_patients = 100) # Supplementary material, S5, page 5: 0.75

fun.s.optimism(Rsqcs = 0.20 , optimism = 0.05, maxRsqcs = 0.33) # Fig. 5, page 12: 0.924

fun.B3(cand_p = 20, s = 0.924, Rsqcs = 0.20) # Fig. 5, page 12: 1078.9

# pmsampsize

library('pmsampsize')

pmsampsize(type = "b", rsquared = 0.046220050, parameters = 30, prevalence = .045)

n.B1.y[6,] # Criterion B1 = Criteria 3 

n.B3.y[117,] # Criterion B3 = Criteria 1

n.B4.y[117,] # Criterion B4 = Criteria 2 # Difference likely due to rounding.
fun.B3(cand_p = 30, s = 0.750, Rsqcs = 0.046220050)
fun.B3(cand_p = 30, s = 0.749, Rsqcs = 0.046220050) 
