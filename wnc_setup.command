#!/bin/sh

#This script will check for R and install the necessary dependencies
#for the WNC Newsletter.
#NOTE: macOS or Linux only.

echo "Installing Homebrew..."
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

if which R
then echo "Looks like you have R installed."
else echo "Installing R for you." | brew install R 
fi


echo 'Installing packages and dependencies (You will probably be asked for your password).'
brew install openssl@1.1
brew install geos
brew install pandoc

sudo R -e "install.packages(c('lubridate', 'dplyr', 'jsonlite', 'ggplot2', 'GISTools', 'rgdal', 'formattable', 'blsAPI', 'TTR', 'tidyr', 'htmlwidgets', 'webshot'), repos='http://mirrors.nics.utk.edu/cran/')" || echo 'Package installation failed.'
sudo R -e "webshot::install_phantomjs()"




