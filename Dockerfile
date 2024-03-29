# Build according to a specified version of R
ARG R_VERSION
ARG R_VERSION=${R_VERSION:-4.3.0}

############# Build Stage: CaDrA ##################

# Get shiny+tidyverse+devtools packages from rocker image
FROM rocker/shiny-verse:${R_VERSION} as base

# Define a system argument
ARG DEBIAN_FRONTEND=noninteractive

# Install system libraries of general use
RUN apt-get update --allow-releaseinfo-change --fix-missing \
  && apt-get -y --no-install-recommends install \
  librsvg2-dev \
  libudunits2-dev \
  libv8-dev \
  libsodium-dev \
  libbz2-dev \
  liblzma-dev \
  tcl8.6-dev \
  tk8.6-dev \
  ca-certificates \
  git \
  && apt clean autoclean \
  && apt autoremove --yes \
	&& rm -rf /var/lib/{apt,dpkg,cache,log}/

# Build according to a specified release of CaDrA
ARG CADRA_BRANCH
ENV CADRA_BRANCH=${CADRA_BRANCH:-devel}

# Set working directory to install CaDrA
WORKDIR / 

# Clone CaDrA repo
RUN git clone https://github.com/montilab/CaDrA.git

# Set working directory to CaDrA
WORKDIR /CaDrA 

# Checkout the desired branch for the build
RUN git checkout ${CADRA_BRANCH} && git pull

# Install CaDrA denpendencies
RUN Rscript "/CaDrA/install_r_packages.R"

############# Build Stage: CaDrA.shiny ##################
FROM base as final 

# Create package directory 
ENV PACKAGE_DIR=/CaDrA.shiny  

# Make package as working directory
WORKDIR ${PACKAGE_DIR}

# Copy package code to Docker image
COPY . ${PACKAGE_DIR}

# Load CaDrA package and install CaDrA.shiny dependencies
RUN Rscript -e "library('devtools'); devtools::load_all('/CaDrA');" \
  && Rscript "${PACKAGE_DIR}/install_r_packages.R"

# Install additional R packages for plumber API 
RUN R -e "install.packages('unix', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plumber', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Make Shiny App/Plumber API available at port 3838
EXPOSE 3838

# Copy bash script that starts shiny-server to Docker image
COPY inst/shiny/shiny-server.sh /user/bin/shiny-server.sh

# Allow permissions to read/write/execute the package
RUN chmod a+rwx ${PACKAGE_DIR}

# Allow permissions to execute the bash script
RUN chmod a+x /user/bin/shiny-server.sh

# Execute the bash script to launch shiny app or plumber api
CMD ["/bin/bash", "-c", "/user/bin/shiny-server.sh"]
