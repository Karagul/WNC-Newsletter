#!/bin/sh

#This script will check for R and install the necessary dependencies
#for the WNC Newsletter.
#NOTE: macOS or Linux only.

if which R
then echo "Looks like you have R installed."
else exit "You don't seem to have R installed.  Please install R: https://cloud.r-project.org/" 
fi


echo 'Installing packages (You will be asked for your password).'
ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
brew install openssl@1.1
brew install geos
brew install pandoc

sudo R -e "install.packages(c('lubridate', 'dplyr', 'jsonlite', 'ggplot2', 'GISTools', 'rgdal', 'formattable', 'blsAPI', 'TTR', 'tidyr', 'reshape2', 'htmltools', 'webshot'), repos='http://mirrors.nics.utk.edu/cran/')" || echo 'Package installation failed.'
sudo R -e "webshot::install_phantomjs()"




