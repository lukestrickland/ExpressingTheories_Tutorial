#Dispatches the relevant fit 
# Usually run in bash
# with nohup R CMD BATCH run_fit.R &
# Run with default EMC2 fitting settings. 
# Number of total CPUs used is cores_for_chains (default 3) x cores_per_chains
# Faster with more cores

library(EMC2)
load("samples/sampler_advanced.RData")
fit(emc,fileName = "samples/sampler_advanced.RData",
    cores_per_chain = 3)

rm(emc)

library(EMC2)
load("samples/sampler_advanced_noPMinh.RData")
fit(emc,fileName = "samples/sampler_advanced_noPMinh.RData",
    cores_per_chain = 3)

rm(emc)

library(EMC2)
load("samples/sampler_advanced_noproactive.RData")
fit(emc,fileName = "samples/sampler_advanced_noproactive.RData",
    cores_per_chain = 3)

rm(emc)

library(EMC2)
load("samples/sampler_advanced_noauto.RData")
fit(emc,fileName = "samples/sampler_advanced_noauto.RData",
    cores_per_chain = 3)

