# Create a cadra_version argument
# to allow the package to build according to its R version
ARG R_VERSION
ARG R_VERSION=${R_VERSION:-4.3.1}

ARG CADRA_BUILD
ARG CADRA_BUILD=${CADRA_BUILD:-shiny}

############# Build Stage: Base ##################

# Get shiny+tidyverse+devtools packages from rocker image
FROM rocker/shiny-verse:${R_VERSION} as base

# Set up the maintainer information
MAINTAINER Reina Chau (lilychau999@gmail.com)

# Define a system argument
ARG DEBIAN_FRONTEND=noninteractive

# Install system libraries of general use
RUN apt-get update && apt-get -y --no-install-recommends install \
    librsvg2-dev \
    libudunits2-dev \
    libv8-dev \
    libsodium-dev \
    libbz2-dev \
    liblzma-dev \
    tcl8.6-dev \
    tk8.6-dev \
    && apt-get clean \
	&& rm -rf /var/lib/apt/lists/*

############# Build Stage: Shinyapp ##################

# Build from the base image for shinyapp
From base as shiny

# Create working directory variable
ENV SERVER_KICKOFF=inst/shinyapp/shiny-server.sh

############# Build Stage: Plumber API ##################

# Build from the base image for shinyapp
From base as plumber

# Create working directory variable
ENV SERVER_KICKOFF=inst/plumber/shiny-server.sh

############# Build Stage: Final ##################

# Build the final image 
FROM ${CADRA_BUILD} as final

# Create package directory 
ENV PACKAGE=/CaDrA.shiny  

# Create working directory 
ENV DATA=/data

# Set up volume directory for package
VOLUME ${PACKAGE}

# Set up volume directory for data
VOLUME ${DATA}

# Set up working directory in docker
WORKDIR ${PACKAGE}

# Install the required bioconductor packages to run CaDrA
RUN R -e "BiocManager::install('SummarizedExperiment')"
RUN R -e "BiocManager::install('GSVA')"

# Install required R packages to run CaDrA
RUN R -e "install.packages('doParallel', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gplots', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('misc3d', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plyr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ppcor', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('R.cache', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reshape2', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Install CaDrA package without dependencies (we are already done so in previous steps)
RUN R -e "devtools::install_github('montilab/CaDrA', dependencies=FALSE)"

# Install additional packages for shiny applications
RUN R -e "install.packages('shinyBS', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('heatmaply', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('future', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Install packages for plumber API 
RUN R -e "install.packages('unix', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plumber', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Make ShinyApp/API available at port 3838
EXPOSE 3838

# Copy package code to Docker image
COPY . ${PACKAGE}

# Copy bash script to Docker image
COPY ${SERVER_KICKOFF} /user/bin/shiny-server.sh

# Allow permissions
RUN chmod a+rwx ${PACKAGE}
RUN chmod a+rwx /user/bin/shiny-server.sh

# Execute the app
CMD ["/bin/bash", "-c", "/user/bin/shiny-server.sh"]
