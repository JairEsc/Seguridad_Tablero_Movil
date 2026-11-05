FROM rocker/r-ver:4.3.2

# Instalar Git y librerías de sistema para R
RUN apt-get update && apt-get install -y \
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes requeridos en R
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org/')"

# 3. Copiar el archivo renv.lock desde tu computadora al contenedor
COPY Estructura_Datos/renv.lock /app/renv.lock

# 4. Restaurar las librerías en el directorio global de R dentro del contenedor
RUN R -e "renv::restore(library = '/usr/local/lib/R/site-library')"

WORKDIR /app
COPY Estructura_Datos/entrypoint.sh /app/entrypoint.sh
RUN chmod +x /app/entrypoint.sh

ENTRYPOINT ["/app/entrypoint.sh"]