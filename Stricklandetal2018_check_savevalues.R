rm(list=ls())
library(EMC2)
library(dplyr)
library(tidyr)
library(ggplot2)

#After dispatch, load and check provides a good account as in our psych review
# paper
load("samples/samplerPM_PR.RData")
load("img/rate_contrasts.RData")
source("utility_functions.R")

#Not need for tutorial purpose - checking fits are reasonable just to make sure
# nothing went wrong.
#Note will likely need less n_cores on a PC, could take a little while
dat <- get_data(emc)
pp <- predict(emc, n_cores = 32)


acc_fun <- function(S, R){
  mean(substr(S,1,1)==tolower(R))
}


acc_df <- calc_stat_postfit(dat, pp, facs=c("S", "cond", "day"),
                            stat_fn=acc_fun,
                            stat_cols=c("S","R")
)

# Fits to major trends in accuracy look reasonable.
acc_df %>% ggplot(aes(cond, postmn_stat)) +
  geom_point(size=3)+ geom_errorbar(aes(ymax = upper, ymin = lower), width=0.5) +
  geom_line(aes(group=1), linetype=2)+
  geom_point(aes(y= stat), pch=21, size=4, colour="black")+
  facet_grid(S~day)  +ylab("Correct RT") +xlab("Block") +ylab("Accuracy")


corRT_fn_list <- c(function(S, R, rt){quantile(rt[toupper(substr(S,1,1))==R], probs=0.1, na.rm=T)},
                   function(S, R, rt){quantile(rt[toupper(substr(S,1,1))==R], probs=0.5, na.rm=T)},
                   function(S, R, rt){quantile(rt[toupper(substr(S,1,1))==R], probs=0.9, na.rm=T)}
)

cols <- c("S", "R", "rt")
stat_col_list <- list(cols,cols,cols)
stat_fn_names <- c("0.1", "0.5", "0.9")


cRT_df <- stack_stat_postfits(
  dat, pp, facs=c("S", "cond", "day"), corRT_fn_list, stat_col_list, stat_fn_names
) 

cRT_df %>%  ggplot(aes(cond, postmn_stat)) +
  geom_point(size=3)+ geom_errorbar(aes(ymax = upper, ymin = lower), width=0.5) +
  geom_line(aes(group=nam), linetype=2)+
  geom_point(aes(y= stat), pch=21, size=4, colour="black")+
  facet_grid(S~day)  +ylab("Correct RT") 


errRT_fn_list <- c(function(S, R, rt){quantile(rt[toupper(substr(S,1,1))!=R], probs=0.1, na.rm=T)},
                   function(S, R, rt){quantile(rt[toupper(substr(S,1,1))!=R], probs=0.5, na.rm=T)},
                   function(S, R, rt){quantile(rt[toupper(substr(S,1,1))!=R], probs=0.9, na.rm=T)}
)

cols <- c("S", "R", "rt")
stat_col_list <- list(cols,cols,cols)
stat_fn_names <- c("0.1", "0.5", "0.9")


eRT_df <- stack_stat_postfits(
  dat, pp, facs=c("S", "cond", "day"), errRT_fn_list, stat_col_list, stat_fn_names
) 

#RT fits quite reasonable
eRT_df %>%  ggplot(aes(cond, postmn_stat)) +
  geom_point(size=3)+ geom_errorbar(aes(ymax = upper, ymin = lower), width=0.5) +
  geom_line(aes(group=nam), linetype=2)+
  geom_point(aes(y= stat), pch=21, size=4, colour="black")+
  facet_grid(S~day)  +ylab("error RT") 


#Parameters inferences comparable to our PR

params <- data.frame(
  get_pars(emc, selection="mu", merge_chains = T)$mu[[1]]
)

param_summaries <- params %>% pivot_longer(cols=colnames(params)) %>% 
  group_by(name) %>% 
  summarize(mn=mean(value), 
            p025= quantile(value, probs=0.025),
            p975=quantile(value, probs=0.975)
  )


# Not clear capacity or urgency effects:
# Lots of overlapping intervals, urgency actually a little lower in control for non-word
# and quality lower in control for word, opposite of what's expected

urg <- param_summaries  %>% filter(grepl(".*SlRurg.*", name))

urg$name <- c("Non-word Urgency, Control",
              "Non-word Urgency, PM",
              "Word Urgency, Control",
              "Word Urgency, PM"
)

urg  %>% 
  ggplot(aes(x=name, y=mn, ymin=p025, ymax=p975))+
  ylab("Effect")+ xlab("") +
  geom_pointrange()+ 
  coord_flip() 


#Again not particularly evidence of capac sharing, lots of overlap.
#Higher for word which is opposite of expected
qual <- param_summaries  %>% filter(grepl(".*SlRqual.*", name))

qual$name <- c("Non-word Quality, Control",
               "Non-word Quality, PM",
               "Word Quality, Control",
               "Word Quality, PM"
)

qual  %>% 
  ggplot(aes(x=name, y=mn, ymin=p025, ymax=p975))+
  ylab("Effect")+ xlab("") +
  geom_pointrange()+ 
  coord_flip() 

#Key effect is strong proactive control for words, a little bit of overall
# proactive control. PM threshold higher than OT

B <- param_summaries  %>% filter(grepl("B_.*", name))
B  %>% 
  ggplot(aes(x=name, y=mn, ymin=p025, ymax=p975))+
  ylab("Effect")+ xlab("") +
  geom_pointrange()+ 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() 


names_full <- names(emc[[1]]$model()[["pre_transform"]]$func)
#Get just parameter names for the simplified simulation
names_sim <-  names_full[!grepl("day", names_full)]

#Get the estimated population means (posterior mean of sampled mu)
p_vector_sim <- colMeans(
  get_pars(emc, selection="mu", merge_chains = T, use_par = names_sim)$mu[[1]]
)

#Get the estimated population covariances (posterior mean of sampled covariance matrices)
Sigma_list <- lapply(
  get_pars(emc, selection="Sigma", merge_chains = T, use_par = names_sim), function(x) 
    colMeans(x[[1]])
)

hierarchical_covs <- do.call("rbind", Sigma_list)
hierarchical_covs <- hierarchical_covs[,colnames(hierarchical_covs) %in% names_sim]
save(p_vector_sim, hierarchical_covs, file="data/sim_values.RData")

