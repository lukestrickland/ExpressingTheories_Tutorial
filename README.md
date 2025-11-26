A tutorial on specifying cognitive process theories in the R package EMC2. For context,
please consult the associated manuscript.

## Pre-computed objects

MCMC samples require compute time. Posterior predictives also require some time, although far less. 
All pre-computed sample and posterior predict objects are available in samples.zip

**Samples should be moved inside of the “samples” directory in order for the scripts to run.**


## Setup Environment

Directions on setting up an appropriate environment to run the scripts can
be found below, and also at the top of *Lesson1.Rmd* and *Lesson2.Rmd*, which
are the primary scripts of interest.

We recommend downloading RStudio and loading this project by opening
*ExpressingTheories_Tutorial.Rproj*. However, any environment that can run R Markdown
should also work.

##### Install packages globally
The primary requirement for these scripts is the EMC2 package, 
which can be installed from CRAN with install.packages("EMC2").Tidyverse packages 
are also used for utility functions - specifically dplyr, tidyr, ggplot2, and 
stringr. These packages can all be installed from CRAN with install.packages().

- On Windows, some packages may require that Rtools is first installed.
- On macOS, some packages may require that command line developer tools are installed.

##### renv
Alternatively, we provide an renv environment, which you can use to automatically 
install all packages for this R project with the specific versions we used. These 
packages are kept isolated to this project and do not affect your global R library.
To use this rather than globally install packages, see the script setup_renv.R
If you have issues with renv, we recommend trying installing packages globally
instead.

## Troubleshooting
If you have any problems running the project, you can contact the first
author at *luke.strickland@curtin.edu.au*. In case you wish to quickly view the 
code output from the lessons but encounter issues running the code, we have 
provided pre-knitted outputs in *Lesson1.html* and *Lesson2.html*.