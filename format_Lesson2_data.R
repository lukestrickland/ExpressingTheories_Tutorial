# This script simply cleans some applied data that will be used for the advanced
# example.
# Here we are taking it from its source form into EMC2 compliant format.

#Data taken from Strickland, L., Heathcote, A., Bowden, V. K., Boag, R. J.,
#Wilson, M. K., Khan, S., & Loft, S. (2021). Inhibitory cognitive control allows
#automated advice to improve accuracy while minimizing misuse.
#Psychological Science, 32(11), 1768-1781.

AutoData <- get(load("data/Strickland2021_Automation.RData"))

#Drop unnecessary columns for these purposes
AutoData <- AutoData[,!colnames(AutoData) %in% c("sess", "block")]

#Use column names and factor levels that are EMC2 compliant and
# will be consistent with the next example
colnames(AutoData) <- c("subjects", "AM", "fail", "S", "R", "rt")

AutoData$subjects <- factor(as.character(AutoData$subjects))
AutoData$R <- factor(as.character(AutoData$R))
AutoData$S <- factor(AutoData$S, labels=c("n", "c"))
AutoData$AM <- factor(AutoData$AM, labels=c("auto", "manual"))
AutoData$fail <- factor(AutoData$fail, levels=c("nonf", "fail"),
                        labels=c("AC", "AI"))

save(AutoData, file="data/AutoData.RData")


# Data taken from a submitted empirical paper
# Boag, Strickland, Heathcote & Loft (2025), Under Review.

print(load("data/ATC_Auto-PM_clean.RData"))

# This data has the usual columns, but also special design-specific 
# columns include::
# auto - did participants have an automated decision aid in that block?
# failtrial - on that particular trial, did the decision aid recommend
# a correct decision (nonf) or fail (fail)
# PM_block - PM or control conditions?
head(cleandats)
table(cleandats[,c("auto","S","PM_block")])

#Retain only key columns, data formatting
dat <- cleandats[ , c("s", "auto", "failtrial", "PM_block", "S", "R", "RT") ]
names(dat)[ c(1, 2,3, 4, 7) ] <- c("subjects", "AM", "fail", "PM", "rt")
head(dat)
dat$subjects <- factor(dat$subjects)
#Nicer names for auto correct/incorrect
dat$fail <- factor(dat$fail, labels=c("AC", "AI"))

save(dat, file="data/data_advanced.RData")


