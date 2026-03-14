# Dev Container for R + Shiny + tidyverse + rio + ggplot2
FROM rocker/r-ver:latest

# Install system dependencies for shiny
RUN apt-get update && \
    apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shinyjs','cookies','bslib','googlesheets4', 'tidyverse', 'rio', 'ggplot2',), repos='https://cloud.r-project.org/')"

# Expose Shiny default port
EXPOSE 3838

# Set default workdir
WORKDIR /workspaces/E26
