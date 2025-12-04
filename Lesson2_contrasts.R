rm(list=ls())
# Here, similar to in Lesson1_contrasts.R, we specify contrasts for 
# the psychological theory, this time for the advanced example (Lesson2.Rmd).
# It would help to have gone through the first example, including 
# Lesson1_contrasts.R, before reviewing this script.

# The contrast matrices we specify are for an example where participants perform
# a PM task but also have the assistance of automated advice. Along the way,
# we also save off a simpler contrast matrix that is for an example with automated
# advice but no PM task. More information can be found in the manuscript and in
# Lesson2.Rmd

# In this example, participants performed an air traffic control conflict detection
# task requiring them to decide whether aircraft would violate safe separation
# standards during their flight. In some conditions, they had a PM task
# to make a special response to particular call signs. Furthermore, in some conditions
# they were provided an automated decision aid that recommended whether aircraft
# were in conflict or not in conflict. This decision aid was sometimes wrong.
# 
# Thus, the design factors include
# Stimulus type (conflict, nonconflict, or PM)
# PM block (control or PM)
# Automation block (manual, i.e., unaided, or automation, i.e., aided)
# Is automation correct? (correct/incorrect)
# 
# The theory is more complex because the contrast equations simultaneously 
# encode theories for automation-use and prospective memory processes. 

# Automation-use affects accumulation rates in two potential ways:
# 1) excitation, where automation inputs increase accumulation towards recommended
# responses and 
# 2) inhibition, where automation inputs decrease accumulation towards opposing 
# responses

# Excitation implies that accumulation rates should be faster than manual
# for the choice automation agrees with. Inhibition implies accumulation rates
# should be slower than manual for the choice automation disagrees with.

# This is coded in addition to the PM inhibition mechanism that was present 
# previously, with effects being additive. Unlike the last example, ongoing-task 
# accumulation rates are not  specified in terms of "quality" and "quantity" 
# here, as there is already a focus on the inhibition and excitation mechanisms

# In this example, proactive control over thresholds will also be coded using 
# contrasts, rather than included as linear-model effects.

# The stringr package will help setup the contrast matrix
library(stringr)
library(EMC2)

# function to print contrast equations out
source("utility_functions.R")

# The data defined in the last script
load("data/data_advanced.RData")

# Set up the contrast matrix in pieces, for clarity.
# Automation and PM will be addressed separately and then matrices stacked

# First, set up a matrix based on automation factors, which essentially handles
# everything in the "control" (i.e., nonPM) conditions

# In this complicated 2x2x4x3 design there are 48 cells
AutoV_RowNames <- as.vector(
  outer(outer(
    outer(
      levels(dat$R), levels(dat$S),paste0
    ), 
    levels(dat$AM), paste0
  ),levels(dat$fail), paste0
  )
)

# The rates are determined by 6 parameters.
exinhcm <- matrix(nrow=length(AutoV_RowNames), ncol=6)
rownames(exinhcm) <- AutoV_RowNames

# Parameters include "manual accumulation rates" (M), that is the accumulation
# rates in the task with no decision aid which uses only 2 accumulators (C and N). 
# The model allows two mechanisms for automation (AUTO) to affect rates, 
# compared with manual, excitation (ex) and inhibition (inh).

colnames(exinhcm) <-
  c("McC", "McN", "MnN", "MnC", "AUTOex", "AUTOinh")

# Set the default entry to zero (i.e., no effect of the parameter on the cell)
exinhcm[1:length(AutoV_RowNames), 1:6] <- 0

# Because there are quite a few cells, fill out the contrast matrix
# programmatically

# First fill out the manual accumulation rates with "dummy coding" (i.e., one
# parameter for each stimulus and latent response)
Mans <- colnames(exinhcm)[
  grepl("M", colnames(exinhcm))
]

# Below string matching to fit the column names above appropriately
# to the design cells they should be assigned to

# Loop over the parameter names specified above
for(i in Mans){
  #Get the stimulus type based on parameter name
  stimi <- str_sub(i, start=2, end=2)
  #Get the latent response type based on parameter name
  ri <- str_sub(i, start=3, end=3)
  
  #Loop the rows of the matrix used to map the parameters 
  for (j in rownames(exinhcm)){
    #Get the stimulus type based on row name (i.e., design cells)
    stimj <- str_sub(j, start=3, end=3)
    #Get the response type based on row name
    Rj <- str_sub(j, start=1, end=1)
    
    # When they match, use this accumulation rate
    if (stimi==stimj & ri==Rj) {
      exinhcm[j,i] <- 1
    }
    
  }
  
}

# Now add excitation appropriately, and subtract inhibition appropriately
# using further string matching

for (j in rownames(exinhcm)){
  # Variables based on row name (i.e., the mapped design cell)
  # Stimulus type (stim), latent response (R), whether it's an
  # automated or manual block (AM), whether automation was correct or 
  # incorrect (failj)
  stimj <- str_sub(j, start=3, end=3)
  Rj <- str_sub(j, start=1, end=1)
  AMj <- str_sub(j, start=4, end=7)
  failj <- str_sub(j, start=-2, end=-1)
  
  #This type of automation aid excitation/inhibition doesn't apply to PM 
  # responses
  if(Rj!="P"){
    #If its the correct response, auto is correct
    # Then it should excite this accumulator
    if (stimj==tolower(Rj) & AMj=="auto" & failj=="AC") {
      exinhcm[j,"AUTOex"] <- 1
    }
    
    #If auto has failed, it should inhibit this accumulator
    if (stimj==tolower(Rj) & AMj=="auto" & failj=="AI") {
      exinhcm[j,"AUTOinh"] <- -1
    }
    
    #If its incorrect response, auto has succeeded, auto should inhibit this
    # accumulator
    if (stimj!=tolower(Rj) & AMj=="auto"& failj=="AC") {
      exinhcm[j,"AUTOinh"] <- -1
    }
    
    #IF it's incorrect response, auto has failed, auto should excite this accumulator
    if (stimj!=tolower(Rj) & AMj=="auto"& failj=="AI") {
      exinhcm[j,"AUTOex"] <- 1
    }   
    
  }

  
}

#Make sure it all looks good so far.
for(i in rownames(exinhcm)){
  print(read_rows(exinhcm, i))
}

#A quick detour here for the automation-only example (no PM task). Remove
# rows related to PM design cells, fix up names for an automation only context,
# save off.
exinhcm_auto <- exinhcm[!grepl("^P|^.p", rownames(exinhcm)), ]
rownames(exinhcm_auto ) <- 
  sub("^(.).(.*)$", "\\1\\2", rownames(exinhcm_auto ))
save(exinhcm_auto, file= "img/exinhcm_auto.RData")


# Continuing now to construct the complex matrix for the unified PM/auto example.
# Similar method to the last contrast specification script here.
# Now we want to account for control vs PM conditions. Use a kronecker product
# to conveniently add another condition and stack the exinhcm matrix diagonally,
# to give separate parameters for control and PM. We will then need to add on
# PM-specific parameters.
# will then need to add PM parameters

A <- matrix(nrow=2, ncol=2, data=c(1,0,0,1), byrow=T)

# The result is a 96 (i.e., 2x48 row matrices) by 12 (i.e, 2x6) where exinhcm
# is filled into the top left and bottom right blocks, with the other two blocks
# containing all zeros. Hence, we now have a 12 parameter model for rates, 6
# for control and 6 for PM.
exinhcm_PM <- kronecker(A, exinhcm)


# Paste the control vs PM aspect into the (cell) rownames used for mapping
rownames(exinhcm_PM) <- c(paste0(rownames(exinhcm), "control"),
                          paste0(rownames(exinhcm), "PM")
)

# Similarly, paste it into parameter names (columns)
colnames(exinhcm_PM) <- c(paste0("control", colnames(exinhcm)),
                          paste0("PM", colnames(exinhcm))
)

# Now define some extra PM parameters that will also be added for the PM conditions,
# these will eventually be joined to the matrix to add 5 new parameter columns.
PMpars <- matrix(nrow=dim(exinhcm_PM)[1], ncol=5)
# initially empty
PMpars [1:nrow(PMpars), 1:ncol(PMpars)] <- 0
rownames(PMpars) <- rownames(exinhcm_PM)

# PM parameters to fill out the PMpars matrix
# PM-induced inhibition for both auto and manual (MAN) conditions, PM 
# accumulator rates (PMV) for both conditions. One PMFA rate assumed to be 
# common to all cells, due to it being  hard to estimate.
colnames(PMpars) <- c("AUTOPMinh","MANPMinh", "AUTOPMV", "MANPMV", "PMFA")

# Loop through the rows that do the mapping
for (j in rownames(PMpars)){
  # Stimulus type, latent response type, 
  # whether PM or control, whether automation or manual block
  stimj <- str_sub(j, start=2, end=2)
  Rj <- str_sub(j, start=1, end=1)
  PMj <- str_sub(j, start=-2, end=-1)
  AMj <- str_sub(j, start=4, end=7)
  
  #If the row corresponds to a PM block, and a PM trial in that block
  if(PMj=="PM"){
   if(stimj=="p"){
     #In automated conditions:
     if(AMj=="auto"){
       if(Rj=="P"){
         # PM accumulator rate
         PMpars[j,"AUTOPMV"] <- 1
       } else{
         # PM inhibition
         PMpars[j,"AUTOPMinh"] <- -1
       }
     } else {
      #In manual conditions
       if(Rj=="P"){
         #PM accumulator rate
         PMpars[j,"MANPMV"] <- 1
       } else{
         #PM inhibition
         PMpars[j,"MANPMinh"] <- -1
       }
       
     }

     
   } else {
     # For any PM responses, give it the PM false alarm parameter.
     # Just one parameter since very rare response.
     if(Rj=="P"){
       PMpars[j,"PMFA"] <- 1
     } 
   }
    
  }
  
  
}

#Check out the PM parameter equations
for(i in rownames(PMpars)){
  print(read_rows(PMpars, i))
}

# Combine the matrices, and inspect it + print its equations as a final check
full_v_cm <- cbind(exinhcm_PM, PMpars)

# 96 x 17 (i.e., 12 + 5 paramters) matrix
print(full_v_cm)

#Print out equations of all active design cells
print_full_v_cm <- full_v_cm[apply(full_v_cm, 1, function(row) any(row != 0)), ]
for(i in rownames(print_full_v_cm )){
  print(read_rows(print_full_v_cm , i))
}

save(full_v_cm, file= "img/exinhcm_PM.RData")


# Finally, proactive control is also embedded in contrasts in the PM+automation
# example. We illustrate below.

# Only need R and PM (2 x 4 = 6 level) condition factor to set this up across 
# PM/control conditions
Proactive_RowNames <- as.vector(
  outer(
      levels(dat$R),
    levels(dat$PM), paste0
  )
)

# The 6 cells are accounted for by 5 parameters
Proactivecm <- matrix(nrow=length(Proactive_RowNames), ncol=5)

# Threshold parameters for each type of latent response, and proactive control
# that could be applied in PM blocks
#
# The automation model does not need to include thresholds to account
# for variation within a block. However, it's possible that automation
# has an overall effect on threshold, or proactive control, which will be 
# accounted for below.
colnames(Proactivecm) <-
  c("C", "N", "paC", "paN",  "P")

rownames(Proactivecm) <- Proactive_RowNames 

# Since these contrasts are fairly simple, just set them up manually
Proactivecm[,"C"] <- c(1,0,0,1,0,0)
Proactivecm[,"N"] <- c(0,1,0,0,1,0)
Proactivecm[,"paC"] <- c(0,0,0,1,0,0)
Proactivecm[,"paN"] <- c(0,0,0,0,1,0)
Proactivecm[,"P"] <- c(0,0,0,0,0,1)

for(i in rownames(Proactivecm)){
  print(read_rows(Proactivecm, i))
}

# Allow for different threshold and proactive-control levels in automation conditions
# To do so, again use the kronecker product with A to make an appropriate
# (12 x 10) matrix

Proactivecm_auto <- kronecker(A, Proactivecm)

# Paste in auto/manual factor to rows that will be used for mapping
rownames(Proactivecm_auto) <- c(paste0(rownames(Proactivecm), "manual"),
                          paste0(rownames(Proactivecm), "auto")
)

# Paste in auto/manual factor to parameter names
colnames(Proactivecm_auto) <- c(paste0("manual", colnames(Proactivecm)),
                          paste0("auto", colnames(Proactivecm))
)


print_Proactivecm_auto <- Proactivecm_auto[
  apply(Proactivecm_auto, 1, function(row) any(row != 0)), 
  ]
for(i in rownames(print_Proactivecm_auto)){
  print(read_rows(print_Proactivecm_auto, i))
}


save(Proactivecm_auto, file= "img/Proactivecm_auto.RData")
