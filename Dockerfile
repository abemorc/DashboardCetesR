# Base R Shiny image
FROM rocker/shiny

RUN apt-get update && apt-get install -y libglpk-dev 
RUN apt-get install -y r-cran-igraph
# Make a directory in the container

RUN mkdir /home/shiny-app

# Install R dependencies

RUN R -e "install.packages(c('tidyverse', 'shinydashboard', 'highcharter', 'xts', 'readxl', 'prophet'), dependencies = TRUE)"

# Copy the Shiny app code
COPY app.R /home/shiny-app/app.R

COPY inflacionfmi.xls /home/shiny-app/inflacionfmi.xls 

COPY lista_bonos.Rds /home/shiny-app/lista_bonos.Rds

# Expose the application port
EXPOSE 8080

# Run the R Shiny app
CMD Rscript /home/shiny-app/app.R
