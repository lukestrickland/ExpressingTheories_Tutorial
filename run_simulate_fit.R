#Dispatches the relevant fit 
# Usually run in bash
# with nohup R CMD BATCH run_simulate_fit.R &
# Run with default EMC2 fitting settings. 
# Number of total CPUs used is cores_for_chains (default 3) x cores_per_chains

library(EMC2)
load("samples/samplerPM.RData")
fit(emc,fileName = "samples/samplerPM.RData",
    cores_per_chain = 3)

rm(emc)
load("samples/samplerPM_noproactive.RData")
fit(emc,fileName = "samples/samplerPM_noproactive.RData",
    cores_per_chain = 3)