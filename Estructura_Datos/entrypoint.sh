#!/bin/bash
set -e

# Configuración del bot de Git
git config --global user.name "AWS Automation Bot"
git config --global user.email "bot@sigeh.com"
export RENV_AUTOLOAD_ENABLED=FALSE
# Clonar repo mediante el Token de GitHub
git clone --depth 1 --single-branch --branch main https://${GH_TOKEN}@github.com/JairEsc/Seguridad_Tablero_Movil.git /app/repo
#cd /app/repo
mkdir -p /app/repo/Datos/Preliminares
cd /app/repo/Estructura_Datos
# Copiar scripts actualizados a la carpeta del repo si están fuera, o ejecutarlos directo
rm -f .Rprofile
Rscript codigos/00-descargar.R
Rscript codigos/01-procesar.R
cd /app/repo

# Verificar si hay cambios antes de hacer commit
git add .
if ! git diff-index --quiet HEAD; then
  git commit -m "Auto-update data: $(date +'%Y-%m-%d')"
  git push origin main
else
  echo "No se detectaron cambios en los datos descargados."
fi