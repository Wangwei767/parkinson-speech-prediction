
FROM rocker/shiny:latest
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'plotly', 'DT', 'ggplot2', 'dplyr', 'caret', 'shinyWidgets', 'shinyjs'))"
COPY app.R /srv/shiny-server/
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
