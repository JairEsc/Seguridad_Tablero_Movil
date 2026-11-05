#!/bin/bash
set -e

# Configuración del bot de Git
git config --global user.name "AWS Automation Bot"
git config --global user.email "bot@sigeh.com"

# Clonar repo mediante el Token de GitHub
git clone https://${GH_TOKEN}@https://github.com/JairEsc/Seguridad_Tablero_Movil.git /app/repo
#cd /app/repo
cd /app/repo/Estructura_Datos
# Copiar scripts actualizados a la carpeta del repo si están fuera, o ejecutarlos directo
Rscript Estructura_Datos/codigos/00-descargar.R
Rscript Estructura_Datos/codigos/01-procesar.R

cd /app/repo

# Verificar si hay cambios antes de hacer commit
git add .
if ! git diff-index --quiet HEAD; then
  git commit -m "Auto-update data: $(date +'%Y-%m-%d')"
  git push origin main
else
  echo "No se detectaron cambios en los datos descargados."
fi