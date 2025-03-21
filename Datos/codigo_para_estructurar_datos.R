
##### nacional 2015
intercensal_nac_2015=readxl::read_excel("Github/private.gob/Seguridad_Tablero_Movil/Estructura_Datos/intercensal_2015.xls")
intercensal_nac_2015_2=intercensal_nac_2015|>
  dplyr::select(`Entidad federativa`,`Grupos quinquenales de edad`,`Población total`,Estimador)|>
  dplyr::filter(!is.na(`Entidad federativa`) & `Entidad federativa`!='Estados Unidos Mexicanos')|>
  dplyr::filter(Estimador=='Valor')|>
  dplyr::filter(`Grupos quinquenales de edad`=='Total')|>
  dplyr::mutate(`Entidad federativa`=substr(`Entidad federativa`,4,nchar(`Entidad federativa`)))|>
  dplyr::select(-Estimador,-`Grupos quinquenales de edad`)

##nacional 2020
nacional2020=read_excel("Github/private.gob/Seguridad_Tablero_Movil/Estructura_Datos/nacional_2015_2020.xlsx")
nacional2020=nacional2020|>
  dplyr::select(`Entidad federativa`,`Grupo quinquenal de edad`,`2020...9`)|>
  dplyr::filter(`Entidad federativa`!='Estados Unidos Mexicanos')|>
  dplyr::filter(`Grupo quinquenal de edad`=='Total')|>
  dplyr::select(-`Grupo quinquenal de edad`)


## municipal 2015
intercensal_mun_2015=readxl::read_excel("Github/private.gob/Seguridad_Tablero_Movil/Estructura_Datos/intercensal_hidalgo_2015.xls")
intercensal_mun_2015_2=intercensal_mun_2015|>
  dplyr::select(Municipio,`Grupos quinquenales de edad`,Estimador,`Población total`)|>
  dplyr::filter(!is.na(`Municipio`) & Municipio!='Total')|>
  dplyr::filter(Estimador=='Valor')|>
  dplyr::filter(`Grupos quinquenales de edad`=='Total')|>
  dplyr::mutate(Municipio=substr(Municipio,5,nchar(Municipio)))|>
  dplyr::select(-Estimador)|>
  dplyr::select(-`Grupos quinquenales de edad`)



## municipal 2020
intercensal_mun_2020=readxl::read_excel("Github/private.gob/Seguridad_Tablero_Movil/Estructura_Datos/Banco de datos infografias _Eduardo.xlsx")
intercensal_mun_2020=intercensal_mun_2020|>
  dplyr::select(Municipio,`Población total`)|>
  dplyr::filter(!is.na(Municipio)& Municipio!='Estatal')

##Le pegamos la poblacion a cada bloque de 5 años. 
datos_estatal_2025=read.csv("Github/private.gob/Seguridad_Tablero_Movil/Municipal-Delitos-2015-2025_feb2025.csv",check.names = F,fileEncoding = "latin1")
datos_estatal_2025$total=rowSums(datos_estatal_2025|>dplyr::select(Enero:Diciembre),na.rm = T)
datos_estatal_2025=datos_estatal_2025|>
  dplyr::select(Año,Entidad,`Tipo de delito`,Enero:total)|>
  dplyr::group_by(Año,Entidad,`Tipo de delito`)|>
  dplyr::summarise_all(sum)
datos_estatal_2025$pobtot=rep(NA,nrow(datos_estatal_2025))
datos_estatal_2025$pobtot[datos_estatal_2025$Año%in%c(2015:2019)]=
intercensal_nac_2015_2$`Población total`|>lapply(FUN = \(x) rep(x,40))|>unlist()|>rep(5)
#el de 2020:2025 tambien
datos_estatal_2025$pobtot[datos_estatal_2025$Año%in%c(2020:2025)]=
  nacional2020$`2020...9`|>lapply(FUN = \(x) rep(x,40))|>unlist()|>rep(6)
##Corregimos la tasa
datos_estatal_2025=datos_estatal_2025|>
  dplyr::mutate(tasa=1000*total/as.numeric(pobtot))



######################Lo mismo pero para la municipal
datos_municipal_2025=read.csv("Github/private.gob/Seguridad_Tablero_Movil/Municipal-Delitos-2015-2025_feb2025.csv",check.names = T,fileEncoding = "latin1")
hidalgo_municipal_2025=datos_municipal_2025|>
  dplyr::filter(Clave_Ent==13)


hidalgo_municipal_2025$total=rowSums(hidalgo_municipal_2025|>dplyr::select(Enero:Diciembre),na.rm = T)

hidalgo_municipal_2025=hidalgo_municipal_2025|>
  dplyr::select(Año,Municipio,Tipo.de.delito,Enero:Diciembre,total)

##Le pegamos del 2015 a 2019 la poblacion
hidalgo_municipal_2025$pobtot=rep(0,nrow(hidalgo_municipal_2025))
hidalgo_municipal_2025=hidalgo_municipal_2025|>
  dplyr::arrange(Año,Municipio,Tipo.de.delito)
hidalgo_municipal_2025_2=hidalgo_municipal_2025|>
  dplyr::group_by(Año,Municipio,Tipo.de.delito)|>
  dplyr::summarise_all(sum)
##Rellenamos 2015
hidalgo_municipal_2025_2$pobtot[hidalgo_municipal_2025_2$Año%in%c(2015:2019)]=
  intercensal_mun_2015_2$`Población total`|>lapply(FUN = \(x) rep(x,40))|>unlist()|>rep(5)
##Rellenamos 2020
hidalgo_municipal_2025_2$pobtot[hidalgo_municipal_2025_2$Año%in%c(2020:2025)]=
  intercensal_mun_2020$`Población total`|>lapply(FUN = \(x) rep(x,40))|>unlist()|>rep(6)

#Calculamos tasa 
hidalgo_municipal_2025_2=hidalgo_municipal_2025_2|>
  dplyr::mutate(pobtot=as.numeric(pobtot))|>
  dplyr::mutate(tasa=1000*total/pobtot)


##Ahora sí generamos los CSV consumibles por js.

datos_estatal_2025|>
  dplyr::ungroup()|>
  dplyr::filter(Entidad=='Hidalgo')|>
  dplyr::select(Año,`Tipo de delito`,total,pobtot,tasa)|>
  write.csv("Github/private.gob/Seguridad_Tablero_Movil//Datos/CSVs_2/Hidalgo_Año_y_Tipo.csv",row.names = F,fileEncoding = "UTF-8")


datos_estatal_2025|>
  dplyr::ungroup()|>
  dplyr::filter(Entidad=='Hidalgo')|>
  dplyr::select(-Entidad)|>
  tidyr::pivot_longer(cols = c(Enero:Diciembre),names_to = "Mes",values_to = "Conteo")|>
  dplyr::select(Año, `Tipo de delito`,Mes,Conteo)|>
  dplyr::mutate(Conteo=ifelse(is.na(Conteo),0,Conteo))|>
  write.csv("Github/private.gob/Seguridad_Tablero_Movil/Datos/CSVs_2/delitos por mes_15-24_estatal.csv",row.names = F,fileEncoding = "UTF-8")


datos_estatal_2025|>
  dplyr::ungroup()|>
  dplyr::select(Año,`Tipo de delito`,tasa)|>
  dplyr::group_by(Año,`Tipo de delito`)|>
  dplyr::summarise(tasa_media=mean(tasa),tasa_mediana=median(tasa))|>
  write.csv("Github/private.gob/Seguridad_Tablero_Movil/Datos/CSVs_2/tasa_media_nacional.csv",fileEncoding = "UTF-8",row.names = F)


hidalgo_municipal_2025_2|>
  tidyr::pivot_longer(cols = Enero:Diciembre,names_to = "Mes",values_to = "Conteo")|>
  dplyr::select(Año,Municipio,Tipo.de.delito,Mes,Conteo)|>
  dplyr::mutate(Conteo=ifelse(is.na(Conteo),0,Conteo))|>
  write.csv("Github/private.gob/Seguridad_Tablero_Movil/Datos/CSVs_2/delitos por mes_15-24.csv",fileEncoding = "UTF-8",row.names = F)


hidalgo_municipal_2025_2|>
  dplyr::select(Año,Municipio,Tipo.de.delito,total,tasa)|>
  write.csv("Github/private.gob/Seguridad_Tablero_Movil/Datos/CSVs_2/Municipal_Año_y_Tipo.csv",fileEncoding = "UTF-8",row.names = F)



#IDM=read.csv("Peticiones especiales/IDM_NM_ene25.csv",check.names = F,fileEncoding = "latin1")
