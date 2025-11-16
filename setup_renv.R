# renv allows you to automatically install all packages
# used for this R project down to the specific versions, specifically
# for working with this project alone. To do so, first restore
# the project environment by running:

# Tutorial reader porting that environment to their machine:
install.packages("renv")   # only if renv isn’t installed yet

# activate the renv. This will restart your Rstudio
renv::activate()   
# reads renv.lock and installs everything cleanly
renv::restore()          

# Above will install all required packages in an isolated project library, 
# ensuring versions match those used when the tutorial was created. 

#You can disable the renv with renv::deactivate()

#Note
#- On Windows, you may need to install Rtools for the renv to build
#- On macOS, you may need to install the command line developer tools.
#- renv may encounter some issues with installing mvtnorm and Matrix packages on 
# macOS in some cases.
#The script *renv_mac_build_fix.R* may help in case you encounter this issue.
