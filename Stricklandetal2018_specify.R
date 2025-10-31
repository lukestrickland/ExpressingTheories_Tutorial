rm(list=ls())
library(EMC2)
library(dplyr)

load("img/rate_contrasts.RData")
load("data/ex1_PR.RData")

#Load in the Psych Review data. From here, coerce the format to be same as in tutorial
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

match_fun <- function(d)
  as.character(d$S) == tolower(
    as.character(d$lR)
  )

design_PM <- design(model=LBA, 
                    #The data created last script
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

p_vector <- sampled_pars(design_PM,doMap=FALSE)
p_vector[grepl("B", names(p_vector))] <- log(1)
p_vector["t0"] <- log(0.3)
p_vector[grepl("inh", names(p_vector))] <- log(0.1)
p_vector[grepl("v_SlRPMR", names(p_vector))] <- 1

p_vector[grepl("qual", names(p_vector))] <- 1
p_vector[grepl("urg", names(p_vector))] <- 2

mapped_pars(design_PM, p_vector = p_vector)


n_vs <- length(p_vector[grepl("^v", names(p_vector))])
n_Bs <- length(p_vector[grepl("B", names(p_vector))])


psd=c(
  rep(2,n_vs),
  rep(1,(n_Bs)),
  1, 1,
  1
  
)

priordf <- cbind(p_vector, psd)
priordf[grepl("B_", rownames(priordf)),]
priordf[grepl("v_", rownames(priordf)),]
priordf[rownames(priordf) %in% c("A", "t0"),]


prior_PM  <- prior(design_PM,mu_mean=p_vector, mu_sd=psd)
samplerPM_PR <- make_emc(okdats, design_PM, prior=prior_PM)
save(samplerPM_PR,file="samples/samplerPM_PR.RData")

