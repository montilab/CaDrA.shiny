
# Get shiny+tidyverse+devtools packages from rocker image
FROM rocker/shiny-verse:4.2.2

# Set up the maintainer information
MAINTAINER Reina Chau (lilychau999@gmail.com)
    
# Set up a volume directory
VOLUME /srv/shiny-server/   

# Set up working directory to the app
WORKDIR /srv/shiny-server/

# Define a system argument
ARG DEBIAN_FRONTEND=noninteractive

# Install system libraries of general use
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    librsvg2-dev \
    libudunits2-dev \
    libv8-dev \
    libsodium-dev \
    libbz2-dev \
    liblzma-dev \
    tcl8.6-dev \
    tk8.6-dev 
    
# Install required bioconductor packages to run CaDrA
RUN R -e "BiocManager::install('Biobase')"
RUN R -e "BiocManager::install('GSVA')"
RUN R -e "BiocManager::install('SummarizedExperiment')"

# Install required R packages to run CaDrA
RUN R -e "install.packages('doParallel', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gplots', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('misc3d', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plyr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ppcor', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('R.cache', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reshape2', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Install CaDrA package with all dependencies already installed previously
RUN R -e "devtools::install_github('montilab/CaDrA', dependencies=TRUE)"

# Install additional packages to run CaDrA-shiny applications
RUN R -e "install.packages('shinyBS', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('heatmaply', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('future', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Install additional packages to run CaDrA-Plumber-API
RUN R -e "install.packages('unix', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plumber', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Make ShinyApp available at port 3838
EXPOSE 3838

# Copy bash file to Docker image
COPY inst/shinyapp/shiny-server.sh /usr/bin/shiny-server.sh

# Allow permissions
RUN ["chmod", "+rwx", "/srv/shiny-server/"]
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

# Execute the app
CMD ["/usr/bin/shiny-server.sh"]
