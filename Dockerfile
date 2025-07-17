# 使用官方R镜像
FROM rocker/shiny:4.3.2

# 设置工作目录
WORKDIR /srv/shiny-server

# 安装系统依赖
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# 安装R包
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'plotly', 'ggplot2', 'dplyr', 'patchwork', 'shinyWidgets', 'shinyjs'), repos='https://cloud.r-project.org')"

# 复制应用文件
COPY app.R .

# 设置权限
RUN chmod -R 755 /srv/shiny-server

# 暴露端口
EXPOSE 3838

# 启动应用
CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=3838)"]
