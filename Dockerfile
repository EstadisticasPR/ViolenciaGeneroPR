FROM rocker/shiny:latest
RUN install2.r --error \
    shiny \
    here \
    dplyr \
    readxl \
    stringr \
    tidyr \
    shinythemes \
    plotly \
    DT \
    shinyWidgets \
    RColorBrewer \
    rsconnect \
    sf \
    zoo
WORKDIR /home/ViolenciaGeneroPR
COPY ui.R ui.R 
COPY server.R server.R 
COPY data data
COPY global.R global.R
COPY utils.R utils.R
COPY www www
COPY deploy.R deploy.R
CMD Rscript deploy.R
