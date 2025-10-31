# This script simply cleans some applied data that will be used for the advanced
# example. Data taken from a submitted empirical paper
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
