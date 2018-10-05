#####################################
# Checking to see if packages are installed
# then installing packages which are not installed
#####################################

packages <- c("ggplot2", "dplyr", "MASS", "ordinal", "sure","rms","VGAM","PResiduals","generalhoslem","devtools")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

sessionInfo()

