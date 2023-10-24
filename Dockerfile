# Create a cadra_version argument
# to allow the package to build according to its R version
ARG R_VERSION
ARG R_VERSION=${R_VERSION:-4.3.1}

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

############# Build Stage: Final ##################

# Build the final image 
FROM base as final

# Create package directory 
ENV PACKAGE=/CaDrA.shiny  

# Set up a volume directory to store package code and data
VOLUME ${PACKAGE}

# Make package location as working directory
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

# Install CaDrA package without dependencies as we are already done so in previous steps
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

# Make Shiny App/Plumber API available at port 3838
EXPOSE 3838

# Copy package code to Docker image
COPY . ${PACKAGE}

# Copy bash script that starts shiny-server to Docker image
COPY inst/shiny/shiny-server.sh /user/bin/shiny-server.sh

# Allow permissions to read/write/execute the package
RUN chmod a+rwx ${PACKAGE}

# Allow permissions to execute the bash script
RUN chmod a+x /user/bin/shiny-server.sh

# Execute the bash script to launch shiny app or plumber api
CMD ["/bin/bash", "-c", "/user/bin/shiny-server.sh"]
