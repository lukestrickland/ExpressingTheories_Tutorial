#As requested by reviewer, provide renv version of project
#This script was to setup the renv


# Used by us to save off environment:
# install.packages("renv")
library(renv)
renv::init()
renv::snapshot()

# Tutorial reader porting that environment to their machine:
install.packages("renv")   # only if renv isn’t installed yet
renv::restore()            # reads renv.lock and installs everything cleanly