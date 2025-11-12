# In Lesson1.Rmd we specify a cognitive process 
#evidence accumulation model within linear-model language in EMC2.
#Specifically, the Prospective Memory (PM) Decision Control model includes psychological processes 
#that map to linear combinations of parameters, embedded in contrasts. 
# This script pre-defines the relevant contrasts and places them in a matrix.

# The code below is used to define a contrast matrix, where: 
#   
#   - **Rows** define linear combinations required for each "mapped" accumulation rate.
# - **Columns** define the parameters used to create the mapped accumulation rates.
# 
# We  create 4 quality/urgency pairs (one for each condition x stimulus type), 
# defining the column and row names for the matrix. We also include three PM-specific parameters, 
# the PM accumulation rate, PM-induced inhibition of ongoing-task accumulation,
# and the PM "false alarm" accumulation rate on non-PM trials.

# Define stimulus types, conditions, and response levels
S <- c("n", "w", "p") # Stimulus types (S): non-word, word, PM (lexical decision task)
cond <- c("C", "PM") # Conditions (cond): control, PM (blocked condition)
Rlevels <- c("N","W","P") # Response levels (Rlevels): non-word, word, PM

# Next, we generate names for the 18 rates based on the fully crossed design. 
# These 18 rates in terms of 11 model parameters

cells <- as.vector(outer(
  outer(
    Rlevels, S,paste0
  ), 
  cond, paste0
)
)
rate_design <- matrix(nrow=18, ncol=11)

colnames(rate_design) <-
  c("qualwC", "urgwC", "qualnC", "urgnC", 
    "qualwPM", "urgwPM", "qualnPM", "urgnPM", "PMR", "inh", "fa")

rownames(rate_design) <- cells

rate_design[1:18, 1:11] <- 0


# After constructing the empty matrix, we then fill out the contrast equations.\
# 
# We theorise about the PM accumulator rate directly. So, the equation for the PM 
# accumulator rate is mapped directly from the PM trial rate.
# A single false alarm rate is estimated for both PM stimuli types *(Not of much 
# interest due to poor estimation. Typical studies observe very few PM fas)*.

rate_design["PpPM", "PMR"] <- 1
rate_design["PnPM", "fa"] <- 1
rate_design["PwPM", "fa"] <- 1


# #### **Inhibition (inh)**
# To model inhibition, we theorise about the difference between ongoing task-accumulation 
# rates on PM trials, compared to nonPM trials. Inhibition from PM inputs should 
# lead to a lower ongoing task accumulation rate on PM trials.\
# 
# The inhibition effect is calculated as the difference between ongoing-task accumulation 
# rates in PM trials compared to non-PM trials. 
# 
# - **Ongoing task rate [PM trial] = Ongoing task rate [non-PM trial] - Inhibition**
#   
#   Thus, in the contrast matrix, inhibition is subtracted from the ongoing-task 
#   accumulation rates on PM trials:
#   To model inhibition from PM inputs, we subtract inhibition from ongoing-task 
#   accumulation on PM trials:

rate_design["WpPM", "inh"] <- -1
rate_design["NpPM", "inh"] <- -1


# 
# Ongoing task capacity inferred from accumulation rates are decomposed into:
#   
# - Urgency (matching + mismatching)
# - Quality (matching - mismatching)\
# 
# So:
#   
# - Matching rate = (0.5 x Urgency) + (0.5 x Quality) 
# - Mismatching rate = (0.5 x Urgency)  - (0.5 x Quality) 
# 
# This structure allows us to estimate capacity separately for PM and control conditions.
# 
# In this example, PM items are words and so they get the same mappings of ongoing-task 
# parameters as words do.

rate_design["NnC", "qualnC"]  <- 0.5
rate_design["WnC", "qualnC"]  <- -0.5
rate_design["NnC", "urgnC"]  <- 0.5
rate_design["WnC", "urgnC"]  <- 0.5

rate_design["NwC", "qualwC"]  <- -0.5
rate_design["WwC", "qualwC"]  <- 0.5
rate_design["NwC", "urgwC"]  <- 0.5
rate_design["WwC", "urgwC"]  <- 0.5

rate_design["NnPM", "qualnPM"]  <- 0.5
rate_design["WnPM", "qualnPM"]  <- -0.5
rate_design["NnPM", "urgnPM"]  <- 0.5
rate_design["WnPM", "urgnPM"]  <- 0.5

rate_design["NwPM", "qualwPM"]  <- -0.5
rate_design["WwPM", "qualwPM"]  <- 0.5
rate_design["NwPM", "urgwPM"]  <- 0.5
rate_design["WwPM", "urgwPM"]  <- 0.5

rate_design["NpPM", "qualwPM"]  <- -0.5
rate_design["WpPM", "qualwPM"]  <- 0.5
rate_design["NpPM", "urgwPM"]  <- 0.5
rate_design["WpPM", "urgwPM"]  <- 0.5

for(i in rownames(rate_design)){
  print(read_rows(rate_design, i))
}

save(rate_design, file="img/rate_contrasts.RData")