### Agreement with conjoined subjects involving mismatching person features 
### Data analysis vol. I: Power analysis of pilot study data 
### Eva Neu, UMass Amherst, August 2022

library(brms)

### STEP 0: Preparation

# Set working directory to source file location

filepath <- '~/Documents/sacred/ling/Conferences/German/pilot_study/pilot_results.csv'

# This is a user-defined function to read in PCIbex Farm results files; it is provided by PCIbex and should not require any modifications 

read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

# Read in results file, remove unused columns

results <- read.pcibex(filepath)
results <- subset(results, select = -c(EventTime, Latin.Square.Group, Results.reception.time, Inner.element.number, PennElementType, PennElementName, Parameter, Controller.name, Group, Order.number.of.item))

# Create data frame only containing acceptability ratings for critical items, remove more unused data points

critical <- subset(results, Label == "critical" & Condition != "filler" & Value != "Start" & Value != "End")
critical$Value = as.numeric(critical$Value)
critical <- subset(critical, select = -c(Label, Comments, Correct))

# Make subject ID a manageable number

no.of.subjects <- length(unique(critical$MD5.hash.of.participant.s.IP.address)) # number of subjects
items.per.subject <- length(critical$MD5.hash.of.participant.s.IP.address)/(no.of.subjects) # length of data frame 
# divided by no. of subjects gives items per subject 
subject.ids <- rep(seq(1, no.of.subjects), each = items.per.subject) # create vector with subject IDs
critical$Subject <- subject.ids # add to data frame 
critical$MD5.hash.of.participant.s.IP.address <- NULL # delete old subject identifier

# Contrast-code fixed effects

# Condition 1: order of conjuncts
# 23 (1), 32 (-1)

critical <- transform(
  critical, Order = ifelse(Condition == "23_1" | Condition == "23_2" | Condition == "23_3", 1, -1))

# Condition 2: verb agreement 
# First part: 1SG (2), 2PL (-1), 3PL (-1)

critical <- transform(
  critical, Agreement1 = ifelse(Condition == "23_1" | Condition == "32_1", 2, -1))

# Second part: 1SG (0), 2PL (1), 3PL (-1)

critical <- transform(
  critical, Agreement2 = ifelse(Condition == "23_2" | Condition == "32_2", 1, ifelse(Condition == "23_3" | Condition == "32_3", -1, 0)))


### STEP 1: Fit basic model to real data

# This is a regular ordinal regression analysis of the pilot study data using a Bayesian mixed-effects model 

basic.model <- brm(Value ~ Agreement1 + Agreement2 + Order + Order:Agreement1 + Order:Agreement2 +
                  (1 + Agreement1 + Agreement2 + Order + Order:Agreement1 + Order:Agreement2 | Subject) +
                  (1 + Agreement1 + Agreement2 + Order + Order:Agreement1 + Order:Agreement2 | Item_set),
                  data = critical, family = cumulative("probit"), init=0)
summary(basic.model) 

# Results: Significant effect of Agreement1 (test vs. control condition), no significant effect of Agreement2 (second vs. third person 
# agreement) and Order (order of conjuncts). Rhat and ESS are solid


### STEP 2: Generate new data set with posterior_predict

# The following code simulates data based on Bayesian model of pilot study data: if the new subjects in the real study behaved exactly 
# as the pilot study subjects, and if the number of of subjects were num.subjects and the number of items num.items, how would the 
# resulting data set look like?

# Generate new data frame 

num.subj <- 60
num.item <- 60
num.draws <- 100

g1 <- data.frame(
  Item_set = 1:num.item,
  Order = rep(c(1, -1, 1, -1, 1, -1), num.item / 6),
  Agreement1 = rep(c(2, -1, -1, 2, -1, -1), num.item / 6),
  Agreement2 = rep(c(0, 1, -1, 0, 1, -1), num.item / 6)
)
g2 <- data.frame(
  Item_set = 1:num.item,
  Order = rep(c(-1, 1, -1, 1, -1, 1), num.item / 6),
  Agreement1 = rep(c(-1, -1, 2, -1, -1, 2), num.item / 6),
  Agreement2 = rep(c(1, -1, 0, 1, -1, 0), num.item / 6)
)
g3 <- data.frame(
  Item_set = 1:num.item,
  Order = rep(c(1, -1, 1, -1, 1, -1), num.item / 6),
  Agreement1 = rep(c(-1, 2, -1, -1, 2, -1), num.item / 6),
  Agreement2 = rep(c(-1, 0, 1, -1, 0, 1), num.item / 6)
)
g4 <- data.frame(
  Item_set = 1:num.item,
  Order = rep(c(-1, 1, -1, 1, -1, 1), num.item / 6),
  Agreement1 = rep(c(2, -1, -1, 2, -1, -1), num.item / 6),
  Agreement2 = rep(c(0, 1, -1, 0, 1, -1), num.item / 6)
)
g5 <- data.frame(
  Item_set = 1:num.item,
  Order = rep(c(1, -1, 1, -1, 1, -1), num.item / 6),
  Agreement1 = rep(c(-1, -1, 2, -1, -1, 2), num.item / 6),
  Agreement2 = rep(c(1, -1, 0, 1, -1, 0), num.item / 6)
)
g6 <- data.frame(
  Item_set = 1:num.item,
  Order = rep(c(-1, 1, -1, 1, -1, 1), num.item / 6),
  Agreement1 = rep(c(-1, 2, -1, -1, 2, -1), num.item / 6),
  Agreement2 = rep(c(-1, 0, 1, -1, 0, 1), num.item / 6)
)

gp1 <- g1[rep(
  seq_len(nrow(g1)),
  num.subj / 6
), ]
gp2 <- g2[rep(
  seq_len(nrow(g2)),
  num.subj / 6
), ]
gp3 <- g1[rep(
  seq_len(nrow(g3)),
  num.subj / 6
), ]
gp4 <- g2[rep(
  seq_len(nrow(g4)),
  num.subj / 6
), ]
gp5 <- g1[rep(
  seq_len(nrow(g5)),
  num.subj / 6
), ]
gp6 <- g2[rep(
  seq_len(nrow(g6)),
  num.subj / 6
), ]

hypo.frame <- rbind(gp1, gp2, gp3, gp4, gp5, gp6)
hypo.frame$Subject <- rep(1:num.subj, each = num.item)

hypo.data <- posterior_predict(basic.model, newdata = hypo.frame, allow_new_levels = TRUE, ndraws = num.draws)


### STEP 3: Fit basic model to simulated data, see if you can recover the fixed effects of interest

# For each sample (number of samples = num.draws), we build a brms model, store the CI values of the 3 fixed effects in a vector and
# then check for each vector in how many of the samples the CI did not contain 0, i.e., in how many samples the effect was significant  

# Create vectors to store CI values

agreement1.lower <- rep(NA, num.draws) # lower bound of CI 
agreement1.upper <- rep(NA, num.draws) # upper bound of CI
agreement2.lower <-rep(NA, num.draws) # ...
agreement2.upper <- rep(NA, num.draws)
order.lower <- rep(NA, num.draws)
order.upper <- rep(NA, num.draws)

# Loop through samples, fit model and extract CI values. Note that this function builds a brms model for each sample - depending on num.draws,
# this takes time 

for (x in 1:num.draws){
  hypo.frame$Value <- hypo.data[x,]
  hypo.model <- brm(Value ~ Agreement1 + Agreement2 + Order + Order:Agreement1 + Order:Agreement2 +
                    (1 + Agreement1 + Agreement2 + Order + Order:Agreement1 + Order:Agreement2 | Subject) +
                    (1 + Agreement1 + Agreement2 + Order + Order:Agreement1 + Order:Agreement2 | Item_set),
                    data = critical, family = cumulative("probit"), init=0, control = setNames(list(0.99),"adapt_delta"), iter=4000)
  agreement1.lower[x] <- fixef(hypo.model)[5,3]
  agreement1.upper[x] <- fixef(hypo.model)[5,4]
  agreement2.lower[x] <- fixef(hypo.model)[6,3]
  agreement2.upper[x] <- fixef(hypo.model)[6,4]
  order.lower[x] <- fixef(hypo.model)[7,3]
  order.upper[x] <- fixef(hypo.model)[7,4]
  print(paste("Sample no.", x, "completed at", Sys.time()))
}

# Compute proportion of significant effects: in how many of the samples was the effect significant, i.e., did the relevant CI not include 0?

# 1) Agreement1 (test vs. control condition)

agreement1.comparison <- rep(NA, num.draws)
for (x in 1:num.draws){
  agreement1.comparison[x] <- ifelse((agreement1.lower[x] <= 0 & agreement1.upper[x] >= 0) | (agreement1.lower[x] >= 0 & agreement1.upper[x] <= 0), 0, 1)
}
print(sum(agreement1.comparison) / num.draws) 

# 2) Agreement2 (second vs. third person agreement)

agreement2.comparison <- rep(NA, num.draws)
for (x in 1:num.draws){
  agreement2.comparison[x] <- ifelse((agreement2.lower[x] <= 0 & agreement2.upper[x] >= 0) | (agreement2.lower[x] >= 0 & agreement2.upper[x] <= 0), 0, 1)
}
print(sum(agreement2.comparison) / num.draws) 

# 3) Order

order.comparison <- rep(NA, num.draws)
for (x in 1:num.draws){
  order.comparison[x] <- ifelse((order.lower[x] <= 0 & order.upper[x] >= 0) | (order.lower[x] >= 0 & order.upper[x] <= 0), 0, 1)
}
print(sum(order.comparison) / num.draws) 

# Results: Agreement1 is a significant effect in 100% of the samples, Agreement2 and Order in 0%


### STEP 4: Determine test-retest reliability of subject-level effects 

# This is an implementation of the model-based analysis of reliability of subject-level effects from Staub 2021 ("How reliable 
# are individual differences in eye movements in reading?"); a detailed description of the procedure can be found in the paper

# Note that the code below is extremely time-consuming, taking up to a week for each effect depending on settings - running it 
# on an external server and one effect at a time is highly recommended slash necessary

hypo.frame$Value <- NULL

# Part 1: Agreement 1

# Create data frame with even- and odd-level coding

agreement1.frame <- hypo.frame
agreement1.frame$Agreement1_even <- rep(NA, length(hypo.frame$Subject))
agreement1.frame$Agreement1_odd <- rep(NA, length(hypo.frame$Subject))

for (x in 1:length(hypo.frame$Subject)){
  agreement1.frame$Agreement1_even[x] <- ifelse(x%%2 == 0, agreement1.frame$Agreement1[x], 0)
  agreement1.frame$Agreement1_odd[x] <- ifelse(x%%2 == 0, 0, agreement1.frame$Agreement1[x])
}

# Create vectors to store values

agreement1.cor.estimate <- rep(NA, num.draws)
agreement1.cor.lower <- rep(NA, num.draws)
agreement1.cor.upper <- rep(NA, num.draws)

# Loop through data, build model for each simulated sample, extract relevant values

for (x in 1:num.draws){
  agreement1.frame$Value <- hypo.data[x,]
  agreement1.model <- brm(Value ~ Agreement1_odd + Agreement1_even + (1 + Agreement1_odd + Agreement1_even | Subject),
                     data = agreement1.frame, family = cumulative("probit"), control = setNames(list(0.99),"adapt_delta"), iter=4000)
  agreement1.cor.estimate[x] <- as.data.frame(VarCorr(agreement1.model)$Subject$cor)[2,9]
  agreement1.cor.lower[x] <- as.data.frame(VarCorr(agreement1.model)$Subject$cor)[2,11]
  agreement1.cor.upper[x] <- as.data.frame(VarCorr(agreement1.model)$Subject$cor)[2,12]
  print(paste("Sample no.", x, "completed at", Sys.time()))
}

# Calculate average estimate

agreement1.cor.estimate.av <- sum(agreement1.cor.estimate)/num.draws

# Calculate average CI width

agreement1.cor.width <- rep(NA, num.draws)
for (x in 1:num.draws){
  agreement1.cor.width[x] <- (agreement1.cor.upper[x] - agreement1.cor.lower[x])
}
agreement1.cor.width.av <- sum(agreement1.cor.width)/num.draws


# Part 2: Agreement 2

# Create data frame with even- and odd-level coding

agreement2.frame <- hypo.frame
agreement2.frame$Agreement2_even <- rep(NA, length(hypo.frame$Subject))
agreement2.frame$Agreement2_odd <- rep(NA, length(hypo.frame$Subject))

for (x in 1:length(hypo.frame$Subject)){
  agreement2.frame$Agreement2_even[x] <- ifelse(x%%2 == 0, agreement2.frame$Agreement2[x], 0)
  agreement2.frame$Agreement2_odd[x] <- ifelse(x%%2 == 0, 0, agreement2.frame$Agreement2[x])
}

# Create vectors to store values

agreement2.cor.estimate <- rep(NA, num.draws)
agreement2.cor.lower <- rep(NA, num.draws)
agreement2.cor.upper <- rep(NA, num.draws)

# Loop through data, build model for each simulated sample, extract relevant values

for (x in 1:num.draws){
  agreement2.frame$Value <- hypo.data[x,]
  agreement2.model <- brm(Value ~ Agreement2_odd + Agreement2_even + (1 + Agreement2_odd + Agreement2_even | Subject),
                          data = agreement2.frame, family = cumulative("probit"), control = setNames(list(0.99),"adapt_delta"), iter=4000)
  agreement2.cor.estimate[x] <- as.data.frame(VarCorr(agreement2.model)$Subject$cor)[2,9]
  agreement2.cor.lower[x] <- as.data.frame(VarCorr(agreement2.model)$Subject$cor)[2,11]
  agreement2.cor.upper[x] <- as.data.frame(VarCorr(agreement2.model)$Subject$cor)[2,12]
  print(paste("Sample no.", x, "is done"))
}

# Calculate average estimate

agreement2.cor.estimate.av <- sum(agreement2.cor.estimate)/num.draws

# Calculate average CI width

agreement2.cor.width <- rep(NA, num.draws)
for (x in 1:num.draws){
  agreement2.cor.width[x] <- (agreement2.cor.upper[x] - agreement2.cor.lower[x])
}
agreement2.cor.width.av <- sum(agreement2.cor.width)/num.draws


# Part 3: Order

# Create data frame with even- and odd-level coding

order.frame <- hypo.frame
order.frame$Order_even <- rep(NA, length(hypo.frame$Subject))
order.frame$Order_odd <- rep(NA, length(hypo.frame$Subject))

for (x in 1:length(hypo.frame$Subject)){
  order.frame$Order_even[x] <- ifelse(x%%2 == 0, order.frame$Order[x], 0)
  order.frame$Order_odd[x] <- ifelse(x%%2 == 0, 0, order.frame$Order[x])
}

# Create vectors to store values

order.cor.estimate <- rep(NA, num.draws)
order.cor.lower <- rep(NA, num.draws)
order.cor.upper <- rep(NA, num.draws)

# Loop through data, build model for each simulated sample, extract relevant values

for (x in 1:num.draws){
  order.frame$Value <- hypo.data[x,]
  order.model <- brm(Value ~ Order_odd + Order_even + (1 + Order_odd + Order_even | Subject),
                          data = order.frame, family = cumulative("probit"), control = setNames(list(0.99),"adapt_delta"), iter=4000)
  order.cor.estimate[x] <- as.data.frame(VarCorr(order.model)$Subject$cor)[2,9]
  order.cor.lower[x] <- as.data.frame(VarCorr(order.model)$Subject$cor)[2,11]
  order.cor.upper[x] <- as.data.frame(VarCorr(order.model)$Subject$cor)[2,12]
  print(paste("Sample no.", x, "is done"))
}

# Calculate average estimate

order.cor.estimate.av <- sum(order.cor.estimate)/num.draws

# Calculate average CI width

order.cor.width <- rep(NA, num.draws)
for (x in 1:num.draws){
  order.cor.width[x] <- (order.cor.upper[x] - order.cor.lower[x])
}
order.cor.width.av <- sum(order.cor.width)/num.draws


# Print out results

print(paste("Agreement1: ", agreement1.cor.estimate.av, agreement1.cor.width.av))
print(paste("Agreement2: ", agreement2.cor.estimate.av, agreement2.cor.width.av))
print(paste("Order: ", order.cor.estimate.av, order.cor.width.av))

# Results (num.subjects = 60, num.items = 60, num.draws = 100: 
# Agreement1: estimate 0.436538035785049, width 0.619072708945081
# Agreement2: estimate 0.86351181865195, width 0.336066595758394
# Order: estimate -0.0643724785587387, width 1.60110968384372

# For Agreement2 - the effect we are mainly interested in - the estimate is high, indicating that the study has enough power for the test-retest 
# reliability analysis. 
