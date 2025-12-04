# In the main lesson, we simulate data to illustrate some principles of 
# cognitive process models. To get appropriate parameter values for that 
# simulation, here we fitted a model to actual data from Strickland et al. (2018)
# Strickland, L., Loft, S., Remington, R. W., & Heathcote, A. (2018). 
# Racing to remember: A theory of decision control in event-based 
# prospective memory. Psychological review, 125(6), 851-887. 
# https://doi.org/10.1037/rev0000113

#Clear environment, load EMC2
rm(list=ls())
library(EMC2)
load("img/rate_contrasts.RData")
load("data/ex1_PR.RData")

# Load in the Psych Review data. From here, coerce the format to be same as in tutorial
# Drop focal condition for these purposes

okdats <- okdats[,c("s", "PM", "day", "S", "R", "RT")]
colnames(okdats) <- c("subjects", "cond", "day", "S", "R", "rt")
okdats <- okdats[okdats$cond!="F",]

okdats$cond <- factor(okdats$cond, levels=c("C", "NF"), labels=c("C", "PM"))

okdats$day <- factor(okdats$day, levels=c("1", "2", "3"), 
                     labels=c("D1", "D2", "D3")
                     )


okdats$S <- factor(okdats$S, levels=c("Nonword", "Word", "PM"), 
                   labels=c("n", "w", "p")
                   )

okdats$R <- factor(okdats$R, levels=c("Nonword", "Word", "PM"), 
                   labels=c("N", "W", "P")
)

save(okdats, file="data/fittedPRdats.RData")

#Drop "day" column for simulation purposes
dats <- okdats[,c("subjects", "cond", "S", "R", "rt")]
save(dats, file="data/simple_data.RData")

#Required by EMC2: a match function that determines accuracy for each
# accumulator for erach trial
match_fun <- function(d)
  as.character(d$S) == tolower(
    as.character(d$lR)
  )

#The design function specifying a model. See Lesson1.Rmd for detailed comments
# on a similar example
design_PM <- design(model=LBA, 
                    data=okdats,
                    functions=list(SlR=function(d) 
                      factor(paste0(d$lR, d$S, d$cond),
                             levels=rownames(rate_design)),
                      RACE=function(d)factor(ifelse(d$cond=="C",2,3)
                      )
                    ),
                    pre_transform = list(func = c(v_SlRinh = "exp")),
                    contrasts = list(SlR=rate_design),
                    matchfun=match_fun,
                    formula=list(v~SlR,B ~ cond*lR +day*lR,A ~ 1,t0 ~ 1,sv ~ lM),
                    constants=c(sv=log(1),v=0,
                                "B_condPM:lRP" = log(1)
                    )
)

#Setting up a prior, here setting population mean values.
p_vector <- sampled_pars(design_PM,doMap=FALSE)
p_vector[grepl("B", names(p_vector))] <- log(1)
p_vector["t0"] <- log(0.3)
p_vector[grepl("inh", names(p_vector))] <- log(0.1)
p_vector[grepl("v_SlRPMR", names(p_vector))] <- 1

p_vector[grepl("qual", names(p_vector))] <- 1
p_vector[grepl("urg", names(p_vector))] <- 2

#Here specifying uncertainty in population means for the prior
n_vs <- length(p_vector[grepl("^v", names(p_vector))])
n_Bs <- length(p_vector[grepl("B", names(p_vector))])


psd=c(
  rep(2,n_vs),
  rep(1,(n_Bs)),
  1, 1,
  1
  
)

#Just print out the prior data frame for a quick look
priordf <- cbind(p_vector, psd)
priordf[grepl("B_", rownames(priordf)),]
priordf[grepl("v_", rownames(priordf)),]
priordf[rownames(priordf) %in% c("A", "t0"),]

#Create EMC2 prior object, find to data and design for sampler object.
# Save. It was dispatched in a separate script with EMC2's "fit" method.
prior_PM  <- prior(design_PM,mu_mean=p_vector, mu_sd=psd)
samplerPM_PR <- make_emc(okdats, design_PM, prior=prior_PM)
save(samplerPM_PR,file="samples/samplerPM_PR.RData")

