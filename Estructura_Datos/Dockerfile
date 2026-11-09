FROM public.ecr.aws/posit/rstudio-sagemaker:ubuntu-26.04-std

RUN apt-get update && apt-get install -y \
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org/')"

WORKDIR /app
COPY renv.lock /app/renv.lock
RUN R -e "renv::restore(lockfile = '/app/renv.lock', library = '/usr/local/lib/R/site-library')"
COPY entrypoint.sh /app/entrypoint.sh
RUN sed -i 's/\r$//' /app/entrypoint.sh && chmod +x /app/entrypoint.sh
ENTRYPOINT ["/app/entrypoint.sh"]