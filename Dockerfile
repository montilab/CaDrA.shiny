
# Create a cadra_version argument
# to allow the package to build according to its R version
ARG R_VERSION
ARG R_VERSION=${R_VERSION:-4.3.0}

# Get shiny+tidyverse+devtools packages from rocker image
FROM rocker/shiny-verse:${R_VERSION}

# Set up the maintainer information
MAINTAINER Reina Chau (lilychau999@gmail.com)
    
# Set up a volume directory
VOLUME /citation-worldmap   

# Set up working directory to the app
WORKDIR /citation-worldmap

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
    tk8.6-dev 

# Install additional packages to run citation-worldmap applications
RUN R -e "install.packages('DT', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('future', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('colourpicker', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Make ShinyApp available at port 3838
EXPOSE 3838

# Copy bash file to Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

# Allow permissions
RUN ["chmod", "+rwx", "/citation-worldmap"]
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

# Execute the app
CMD ["/usr/bin/shiny-server.sh"]
