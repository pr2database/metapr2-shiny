# https://www.theorsociety.com/media/3832/data-visualisation-workshop-uploading-a-shiny-app-to-a-server-_14062018122240.pdf

# Key is in directory /shiny
# Key pass is metapr2

# Install for R server

sudo apt-get update
# sudo apt-get upgrade

sudo apt install libgdal-dev

sudo apt-get install r-base r-base-dev

sudo su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""

# Run install_packages.sh (libraries need to be installed for root!)

# Install  shiny server
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.17.973-amd64.deb
sudo gdebi shiny-server-1.5.17.973-amd64.deb

sudo systemctl start shiny-server
sudo systemctl enable shiny-server

# Test Shiny server at http://34.126.91.69:3838/

# Secure the files and restart the server
sudo chmod 755 -R /srv/shiny-server
sudo systemctl restart shiny-server


# Move the files to directory /srv/shiny-server/metapr2 using WinSCP

sudo chmod 777 -R /srv/shiny-server

# To access the logs
sudo chmod 777 -R /var/log/shiny-server

# Start app with http://34.126.91.69:3838/metapr2

# Install rstudio

wget https://download2.rstudio.org/server/bionic/amd64/rstudio-server-1.4.1717-amd64.deb
sudo gdebi rstudio-server-1.4.1717-amd64.deb

sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev

sudo addgroup metapr2
sudo adduser daniel
# password is daniel

# Start app with http://34.126.91.69:8787
