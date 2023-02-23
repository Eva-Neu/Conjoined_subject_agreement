### Agreement with conjoined subjects involving mismatching person features
### Analysis of experimental data
### Eva Neu, UMass Amherst, December 2022


library(dplyr)
library(ggplot2)
library(gridExtra)
library(brms)


### Step 0: Preparation

# Set working directory to source file location

filepath <- '~/Documents/sacred/ling/Conferences/German/experiment/real_results.csv' 

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
    return(read.csv(filepath, quote = "", comment.char="#", header=FALSE, col.names=cols)) # added quote = "" 
  }
  else{
    return(read.csv(filepath, quote = "", comment.char="#", header=FALSE, col.names=seq(1:n.cols))) # added quote = "" 
  }
}

# Read in results file, remove unused columns

pre.results <- read.pcibex(filepath)
results <- subset(pre.results, select = -c(EventTime, Results.reception.time, Latin.Square.Group, Inner.element.number, PennElementType, PennElementName, Parameter, Controller.name, Order.number.of.item), Value != "Start" & Value != "End" & Label == "critical")

# Make subject ID a manageable number

no.of.subjects <- length(unique(results$MD5.hash.of.participant.s.IP.address)) # number of subjects
items.per.subject <- 180 
subject.ids <- rep(seq(1, no.of.subjects), each = items.per.subject) # create vector with subject IDs
results$Subject <- subject.ids # add to data frame 
results$MD5.hash.of.participant.s.IP.address <- NULL # delete old subject identifier

# Create data frame only containing acceptability ratings for critical items, remove more unused columns

pre.critical <- subset(results, Label == "critical" & Condition != "filler" & Value != "Start" & Value != "End")
pre.critical$Value = as.numeric(pre.critical$Value)
critical <- subset(pre.critical, select = -c(Label, Comments, Correct))

# Change condition labels to something more transparent

critical <- transform(
  critical, cond = ifelse(Condition == "23_1", "2&3_1SG", ifelse(Condition == "32_1", "3&2_1SG", ifelse(Condition == "23_2", "2&3_2PL", 
  ifelse(Condition == "32_2", "3&2_2PL", ifelse(Condition == "23_3", "2&3_3PL", ifelse(Condition == "32_3", "3&2_3PL", NA)))))))

critical$Condition <- NULL 
colnames(critical)[colnames(critical) == "cond"] = "Condition"

# Create data frame only containing fillers

fillers <- subset(results, Condition == "filler" & Value != "Start" & Value != "End")
fillers$Value <- as.numeric(fillers$Value)

# Contrast-code fixed effects

# a) Order (of conjuncts)
# 23 (1), 32 (-1)

critical <- transform(
  critical, Order = ifelse(Condition == "23_1" | Condition == "23_2" | Condition == "23_3", 1, -1))

# b) Verb agreement
# Agreement1, test vs. control conditions: 1SG (2), 2PL (-1), 3PL (-1)

critical <- transform(
  critical, Agreement1 = ifelse(Condition == "23_1" | Condition == "32_1", 2, -1))

# Agreement 2, 2nd vs. 3rd person agreement: 1SG (0), 2PL (1), 3PL (-1)

critical <- transform(
  critical, Agreement2 = ifelse(Condition == "23_2" | Condition == "32_2", 1, ifelse(Condition == "23_3" | Condition == "32_3", -1, 0)))

# Find responses to dialect and feedback questions 

dialect = subset(pre.results, Label == "dialect")
dialect.responses = subset(dialect$Value, dialect$Value != "End" & dialect$Value != "Start" & dialect$Value != "")

feedback = subset(pre.results, Label == "feedback")
feedback.responses = subset(feedback$Value, feedback$Value != "End" & feedback$Value != "Start" & feedback$Value != "")

captcha = subset(pre.results, Label == "captcha")
captcha.responses = subset(captcha$Value, captcha$Value != "End" & captcha$Value != "Start" & captcha$Value != "")

# Check that experimental design worked correctly 

print(table(critical$Group))
print(table(critical$Item_set))
print(table(critical$Subject))
print(table(critical$Condition))

# As a sanity check, compute average responses to grammatical, medium and ungrammatical fillers for each participant

sanity <- aggregate(x = fillers$Value,
                           by = list(fillers$Subject, fillers$Correct),
                           FUN = mean)
print(sanity)

# Exclude participants based on sanity check

critical <- subset(critical, Subject != 11 & Subject != 22 & Subject != 57)


### Step 1: Descriptive statistics

# Compute mean 

mean_critical <- aggregate(x = critical$Value,
          by = list(critical$Condition),
          FUN = mean)

# For comparison, compute mean of fillers 

mean_fillers <- aggregate(x = fillers$Value,
          by = list(fillers$Correct),
          FUN = mean)

# Make bar plot of means

colnames(mean_critical)[1] = "Condition"
colnames(mean_critical)[2] = "Rating"

mean_plot <- ggplot(data=mean_critical, aes(x=Condition, y=Rating)) +
  geom_bar(stat="identity", fill="black")+
  theme(text=element_text(size=22, family="Times New Roman"), axis.title.x = element_text(margin=margin(t=20), size=24), 
        axis.title.y = element_text(margin=margin(t=20), size=24))+
  labs(x="Mean ratings", y="Rating")+ylim(0,5)

mean_plot

# Compute distribution of responses for each test condition separately 

test23_2 <- subset(critical, Condition == "23_2")
test23_3 <- subset(critical, Condition == "23_3")
test32_2 <- subset(critical, Condition == "32_2")
test32_3 <- subset(critical, Condition == "32_3")

df23_2 <- data.frame(table(test23_2$Value))
df23_3 <- data.frame(table(test23_3$Value))
df32_2 <- data.frame(table(test32_2$Value))
df32_3 <- data.frame(table(test32_3$Value))

# Plot distribution of responses per test condition 

plot23_2 <- ggplot(data=df23_2, aes(x=Var1, y=Freq))+
  geom_bar(stat="identity", fill="black")+
  theme(text=element_text(size=24, family="Times New Roman"), axis.title.x = element_text(margin=margin(t=10), size=20),
        axis.title.y = element_text(margin=margin(t=10), size=20))+
  labs(x="Histogram for 2&3_2PL", y="No. of observ.")+ylim(0, 410)
plot23_3 <- ggplot(data=df23_3, aes(x=Var1, y=Freq))+
  geom_bar(stat="identity", fill="black")+
  theme(text=element_text(size=24, family="Times New Roman"), axis.title.x = element_text(margin=margin(t=10), size=20),
        axis.title.y = element_text(margin=margin(t=10), size=20))+
  labs(x="Histogram for 2&3_3PL", y="No. of observ.")+ylim(0, 410)
plot32_2 <- ggplot(data=df32_2, aes(x=Var1, y=Freq))+
  geom_bar(stat="identity", fill="black")+
  theme(text=element_text(size=24, family="Times New Roman"), axis.title.x = element_text(margin=margin(t=10), size=20),
        axis.title.y = element_text(margin=margin(t=10), size=20))+
  labs(x="Histogram for 3&2_2PL", y="No. of observ.")+ylim(0, 410)
plot32_3 <- ggplot(data=df32_3, aes(x=Var1, y=Freq))+
  geom_bar(stat="identity", fill="black")+
  theme(text=element_text(size=24, family="Times New Roman"), axis.title.x = element_text(margin=margin(t=10), size=20), 
        axis.title.y = element_text(margin=margin(t=10), size=20))+
  labs(x="Histogram for 3&2_3PL", y="No. of observ.")+ylim(0, 410)

grid.arrange(plot23_2, plot23_3, plot32_2, plot32_3, ncol=2, nrow=2)


### Step 2: Inferential statistics

### Step 2.1: Bayesian model 

# This is a regular ordinal regression analysis using a Bayesian mixed-effects model 

basic.model <- brm(Value ~ Agreement1 + Agreement2 + Order + Order:Agreement1 + Order:Agreement2 +
                     (1 + Agreement1 + Agreement2 + Order + Order:Agreement1 + Order:Agreement2 | Subject) +
                     (1 + Agreement1 + Agreement2 + Order + Order:Agreement1 + Order:Agreement2 | Item_set),
                   data = critical, family = cumulative("probit"), init=0, iter=4000)
summary(basic.model) 

# Results: Sizeable significant effect of Agreement1 (test vs. control), small but significant effect of Order, no significant 
# effect of Agreement2 (2 vs. 3)


### Step 2.2: Model-based analysis of test-retest reliability 

# This is an implementation of the model-based analysis of reliability of subject-level effects from Staub 2021 ("How reliable 
# are individual differences in eye movements in reading?"); a detailed description of the procedure can be found in the paper

# Part A: Agreement 1

# We add two new fixed effects to data set: a) the effect of Agreement1 in even, and b) the effect of Agreement1 in odd items. That is,
# in one half the items, one level of the critical factor is assigned 0.5 and the other -0.5; in the other half, all items are assigned 0.
# Then we look at the correlation between the effects in the two halves of the experiment. The same is done for Agreement2 and Order

# Add columns to data set

agreement1.even <- rep(NA, length(critical))
agreement1.odd <- rep(NA, length(critical))
critical$Agreement1.even <- agreement1.even
critical$Agreement1.odd <- agreement1.odd 

# Code the columns accordingly

for (x in 1:length(critical$Subject)){
  critical$Agreement1.even[x] <- ifelse(x%%2 == 0, critical$Agreement1[x], 0)
  critical$Agreement1.odd[x] <- ifelse(x%%2 == 0, 0, critical$Agreement1[x])
}

# Build model with these two new fixed effects

agreement1.model <- brm(Value ~ Agreement1.odd + Agreement1.even + (1 + Agreement1.odd + Agreement1.even | Subject),
                        data = critical, family = cumulative("probit"), iter=6000)
summary(agreement1.model)

# Inspect correlation between the two effects

agreement1.cor.estimate <- as.data.frame(VarCorr(agreement1.model)$Subject$cor)[2,9]
agreement1.cor.lower <- as.data.frame(VarCorr(agreement1.model)$Subject$cor)[2,11]
agreement1.cor.upper <- as.data.frame(VarCorr(agreement1.model)$Subject$cor)[2,12]

# Print out the relevant values 

print(paste("Estimate agreement 1:", agreement1.cor.estimate, "lower boundary agreement 1:", agreement1.cor.lower, "upper boundary agreement 1:", agreement1.cor.upper))


# Part B: Agreement 2

# Add columns to data set

agreement2.even <- rep(NA, length(critical))
agreement2.odd <- rep(NA, length(critical))
critical$Agreement2.even <- agreement2.even
critical$Agreement2.odd <- agreement2.odd 

# Code the columns accordingly

for (x in 1:length(critical$Subject)){
  critical$Agreement2.even[x] <- ifelse(x%%2 == 0, critical$Agreement2[x], 0)
  critical$Agreement2.odd[x] <- ifelse(x%%2 == 0, 0, critical$Agreement2[x])
}

# Build model with these two new fixed effects 

agreement2.model <- brm(Value ~ Agreement2.odd + Agreement2.even + (1 + Agreement2.odd + Agreement2.even | Subject),
                        data = critical, family = cumulative("probit"), control = setNames(list(0.99),"adapt_delta"),  iter=4000)
summary(agreement2.model)

# Inspect correlation between the two effects

agreement2.cor.estimate <- as.data.frame(VarCorr(agreement2.model)$Subject$cor)[2,9]
agreement2.cor.lower <- as.data.frame(VarCorr(agreement2.model)$Subject$cor)[2,11]
agreement2.cor.upper <- as.data.frame(VarCorr(agreement2.model)$Subject$cor)[2,12]

# Print out the relevant values 

print(paste("Estimate agreement 2:", agreement2.cor.estimate, "lower boundary agreement 2:", agreement2.cor.lower, "upper boundary agreement 2:", agreement2.cor.upper))


# Part C: Order

# Add columns to data set

order.even <- rep(NA, length(critical))
order.odd <- rep(NA, length(critical))
critical$Order.even <- order.even
critical$Order.odd <- order.odd

# Code the columns accordingly

for (x in 1:length(critical$Subject)){
  critical$Order.even[x] <- ifelse(x%%2 == 0, critical$Order[x], 0)
  critical$Order.odd[x] <- ifelse(x%%2 == 0, 0, critical$Order[x])
}

# Build model with these two new fixed effects 

order.model <- brm(Value ~ Order.odd + Order.even + (1 + Order.odd + Order.even | Subject),
                   data = critical, family = cumulative("probit"), control = setNames(list(0.99),"adapt_delta"),  iter=4000)
summary(order.model)

# Inspect correlation between the two effects

order.cor.estimate <- as.data.frame(VarCorr(order.model)$Subject$cor)[2,9]
order.cor.lower <- as.data.frame(VarCorr(order.model)$Subject$cor)[2,11]
order.cor.upper <- as.data.frame(VarCorr(order.model)$Subject$cor)[2,12]

# Print out the relevant values

print(paste("Estimate order:", order.cor.estimate, "lower boundary order:", order.cor.lower, "upper boundary order:", order.cor.upper))


# Results:

# Agreement1: estimate 0.958969949908789, lower boundary 0.87314838649163, upper boundary 0.996739244912573
# Agreement2: estimate 0.915561396282886, lower boundary 0.752442221651722, upper boundary 0.992971822075763
# Order: estimate -0.183681706671179, lower boundary -0.924135587841976, upper boundary 0.808574504184937

# We find a very strong correlation of the subject-level effects in even and odd items for Agreement1 and Agreement2. As for Order, it
# appears that participants hardly differ with respect to this effect, as confirmed by sd(Order). Since there is no structures variation
# between participants, we do not see any reasonable correlation, which is as expected 


### Step 2.3: Investigate extent of inter-subject variation for Agreement1 and Agreement2 effects

# Having confirmed that subjects adopt consistently different strategies, we now investigate the extent to which subjects differ from each
# other with respect to test vs. control and 2 vs. 3

# First, simple strategy: compute and plot means of individual participants for each agreement pattern (1, 2, 3)

# Add column for agreement (1 vs. 2 vs. 3) (for ease of coding only)

critical <- transform(
  critical, Agreement = ifelse(Condition == "23_2" | Condition == "32_2", "2", ifelse(Condition == "23_3" | Condition == "32_3", "3", "1")))

# Make plot for 1Sg agreement

first.person <- subset(critical, Agreement == "1")
first.person.means <- means.per.participant <- aggregate(x = first.person$Value,
                                                             by = list(first.person$Subject),
                                                             FUN = mean)
first.person.dataframe <- data.frame(first.person.means)
first.person.dataframe <- arrange(first.person.dataframe, x)
first.person.dataframe$num <- seq(1,78)

first.person.plot <- ggplot(data=first.person.dataframe, aes(x=num, y=x))+
  geom_bar(stat="identity", fill="darkgrey")+
  theme(text=element_text(size=24, family="Times New Roman"), axis.title.x = element_text(margin=margin(t=10), size=20))+
  labs(x="Mean ratings per subject for 1Sg agreement", y=NULL)+
  ylim(0, 5)
first.person.plot

# Make plot for 2Pl agreement

second.person <- subset(critical, Agreement == "2")
second.person.means <- means.per.participant <- aggregate(x = second.person$Value,
                                                         by = list(second.person$Subject),
                                                         FUN = mean)
second.person.dataframe <- data.frame(second.person.means)
second.person.dataframe <- arrange(second.person.dataframe, x)
second.person.dataframe$num <- seq(1,78)

second.person.plot <- ggplot(data=second.person.dataframe, aes(x=num, y=x))+
  geom_bar(stat="identity", fill="darkgrey")+
  theme(text=element_text(size=24, family="Times New Roman"), axis.title.x = element_text(margin=margin(t=10), size=20))+
  labs(x="Mean ratings per subject for 2Pl agreement", y=NULL)+
  ylim(0, 5)
second.person.plot

# Make plot for 3Pl agreement 

third.person <- subset(critical, Agreement == "3")
third.person.means <- means.per.participant <- aggregate(x = third.person$Value,
                                                          by = list(third.person$Subject),
                                                          FUN = mean)
third.person.dataframe <- data.frame(third.person.means)
third.person.dataframe <- arrange(third.person.dataframe, x)
third.person.dataframe$num <- seq(1,78)

third.person.plot <- ggplot(data=third.person.dataframe, aes(x=num, y=x))+
  geom_bar(stat="identity", fill="darkgrey")+
  theme(text=element_text(size=24, family="Times New Roman"), axis.title.x = element_text(margin=margin(t=10), size=20))+
  labs(x="Mean ratings per subject for 3Pl agreement", y=NULL)+
  ylim(0, 5)
third.person.plot


# Second, more advanced strategy: compute and plot estimate plus subject slope for each participant 

# Agreement1

# Extract subject slopes 

agreement1.slopes <- coef(basic.model)$Subject[1:78,1,1]

# Store values in data frame

agreement1.per.subject <-rep(-0.67, each = 78)

for (x in seq(1:78)) {
  agreement1.per.subject[x] = agreement1.per.subject[x] + agreement1.slopes[x]
}

agreement1.per.subject <- data.frame(agreement1.per.subject)
names(agreement1.per.subject) <- "est"
agreement1.per.subject <- arrange(agreement1.per.subject, desc(est))
agreement1.per.subject$num <- seq(1:78)

# Plot estimate plus slope for each subject

agreement1.slopes.plot <- ggplot(data=agreement1.per.subject, aes(x=num, y=est))+
  geom_bar(stat="identity", fill="black")+
  theme(text=element_text(size=24, family="Times New Roman"), axis.title.x = element_text(margin=margin(t=10), size=24))+
  labs(x="Estimate of test vs. control (-0.67) + random slopes", y=NULL)+
  theme(axis.text=element_text(size=22))
agreement1.slopes.plot

# Agreement2

# Extract subject slopes

agreement2.slopes <- coef(basic.model)$Subject[1:78,1,2]

# Store values in data frame 

agreement2.per.subject <-rep(-0.05, each = 78)

for (x in seq(1:78)) {
  agreement2.per.subject[x] = agreement2.per.subject[x] + agreement2.slopes[x]
}

agreement2.per.subject <- data.frame(agreement2.per.subject)
names(agreement2.per.subject) <- "est"
agreement2.per.subject <- arrange(agreement2.per.subject, desc(est))
agreement2.per.subject$num <- seq(1:78)

# Plot estimate plus slope for each subject

agreement2.slopes.plot <- ggplot(data=agreement2.per.subject, aes(x=num, y=est))+
  geom_bar(stat="identity", fill="black")+
  # geom_text(aes(label="Rating"), vjust=-0.3, size=6.5, family="Times New Roman")+
  theme(text=element_text(size=24, family="Times New Roman"), axis.title.x = element_text(margin=margin(t=10), size=22))+
  labs(x="Estimate of 2 vs. 3 (-0.05) + random slopes", y=NULL)+
  theme(axis.text=element_text(size=24))
agreement2.slopes.plot

# Results: subject-level estimates for Agreement2 (2 vs. 3) vary more widely than for Agreement1 (test vs. control) indicating 
# genuine differences between subjects with respect to their judgments of second and third person agreement that go beyond
# accidental idiosyncrasies 

