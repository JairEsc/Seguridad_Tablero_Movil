
excel=list()
excel[['estatal_homicidio']]=datos_estatal_2025 |> 
  dplyr::filter(`Tipo de delito`=='Homicidio') |> 
  dplyr::filter(Año=='2026') |> 
  dplyr::group_by(Entidad,`Subtipo de delito`,Modalidad) |> 
  dplyr::summarise(total=sum(total)) |> 
  dplyr::arrange(dplyr::desc(total))
excel[['municipal_Hidalgo']]=datos_estatal_2025 |> 
  dplyr::filter(`Tipo de delito`=='Homicidio') |> 
  dplyr::filter(Entidad=='Hidalgo') |> 
  dplyr::filter(Año=='2026') |> 
  dplyr::group_by(Municipio,`Subtipo de delito`,Modalidad) |> 
  dplyr::summarise(total=sum(total)) |> 
  dplyr::arrange(dplyr::desc(total))

excel |> openxlsx::write.xlsx("../../../datos_secretariado_homicidio.xlsx")


"../../llamadas911/inputs/llamadas9112025/Estadístico 911 2025.xlsx" |> 
  readxl::read_excel()->llamadas911


llamadas911$`Delito / Emergencia` |> unique() |> sort()

llamadas911_lesiones=llamadas911 |> 
  dplyr::filter(`Delito / Emergencia`%in%c(
    "DETONACIÓN DE ARMA DE FUEGO",
    "LESIONADO POR ARMA BLANCA",
    "PERSONA TIRADA EN VÍA PÚBLICA" 
  ))

llamadas911_lesiones$`Hora Incidente` |> 
  lubridate::hour()
municipios=sf::st_read("../../../Reutilizables/Cartografia/municipiosjair.shp")

library(sf)
llamadas911_lesiones$lati

llamadas911_lesiones |> 
  dplyr::mutate(
    `Latitud Emergencia`=as.numeric(gsub("\\'", "",`Latitud Emergencia`)),
    `Longitud Emergencia`=as.numeric(gsub("\\'", "",`Longitud Emergencia`))
    ) |> 
  dplyr::filter(!is.na(`Latitud Emergencia`) & !is.na(`Longitud Emergencia`)) |> 
  st_as_sf(coords=c("Longitud Emergencia","Latitud Emergencia"),crs=4326) |> 
  st_join(y = municipios |> dplyr::select(NOM_MUN,geometry),join = st_nearest_feature) |> 
  dplyr::mutate(hora_incidente_n=lubridate::hour(`Hora Incidente`)) |> 
  dplyr::group_by(NOM_MUN,Colonia,hora_incidente_n,`Delito / Emergencia`) |> 
  dplyr::summarise(num_incidentes=dplyr::n())->z
z |> st_drop_geometry() |> openxlsx::write.xlsx("../../../datos_911_proxy_homicidio.xlsx")
