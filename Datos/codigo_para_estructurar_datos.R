
##### nacional 2015
intercensal_nac_2015=readxl::read_excel("intercensal_2015.xls")
intercensal_nac_2015_2=intercensal_nac_2015|>
  dplyr::select(`Entidad federativa`,`Grupos quinquenales de edad`,`Población total`,Estimador)|>
  dplyr::filter(!is.na(`Entidad federativa`) & `Entidad federativa`!='Estados Unidos Mexicanos')|>
  dplyr::filter(Estimador=='Valor')|>
  dplyr::filter(`Grupos quinquenales de edad`=='Total')|>
  dplyr::mutate(`Entidad federativa`=substr(`Entidad federativa`,4,nchar(`Entidad federativa`)))|>
  dplyr::select(-Estimador,-`Grupos quinquenales de edad`)

##nacional 2020
nacional2020=readxl::read_excel("nacional_2015_2020.xlsx")
nacional2020=nacional2020|>
  dplyr::select(`Entidad federativa`,`Grupo quinquenal de edad`,`2020...9`)|>
  dplyr::filter(`Entidad federativa`!='Estados Unidos Mexicanos')|>
  dplyr::filter(`Grupo quinquenal de edad`=='Total')|>
  dplyr::select(-`Grupo quinquenal de edad`)


## municipal 2015
intercensal_mun_2015=readxl::read_excel("intercensal_hidalgo_2015.xls")
intercensal_mun_2015_2=intercensal_mun_2015|>
  dplyr::select(Municipio,`Grupos quinquenales de edad`,Estimador,`Población total`)|>
  dplyr::filter(!is.na(`Municipio`) & Municipio!='Total')|>
  dplyr::filter(Estimador=='Valor')|>
  dplyr::filter(`Grupos quinquenales de edad`=='Total')|>
  dplyr::mutate(Municipio=substr(Municipio,5,nchar(Municipio)))|>
  dplyr::select(-Estimador)|>
  dplyr::select(-`Grupos quinquenales de edad`)



## municipal 2020
intercensal_mun_2020=readxl::read_excel("Banco de datos infografias _Eduardo.xlsx")
intercensal_mun_2020=intercensal_mun_2020|>
  dplyr::select(Municipio,`Población total`)|>
  dplyr::filter(!is.na(Municipio)& Municipio!='Estatal')
archivo_2026=list.files("../Datos/Preliminares/",pattern = "RNID-Delitos",full.names = T) |> rev()
archivo_2025=list.files("../Datos/Preliminares/",pattern = "2025",full.names = T)
victimas_2026=list.files("../Datos/Preliminares/",pattern = "RNID-Víctimas",full.names = T)
datos_victimas_2026=read.csv(victimas_2026,check.names = F,fileEncoding = "latin1")
##Le pegamos la poblacion a cada bloque de 5 años. 
datos_estatal_2025=read.csv(archivo_2025[1],check.names = F,fileEncoding = "latin1") |> 
  rbind(read.csv(archivo_2026[1],check.names = F,fileEncoding = "latin1"))
hidalgo_municipal_2025=datos_estatal_2025|>
  dplyr::filter(Clave_Ent==13) |> 
  dplyr::filter(`Cve. Municipio`<13100)
datos_estatal_2025 |> colnames()
datos_estatal_2025=datos_estatal_2025 |> 
  dplyr::mutate(dplyr::across(Enero:Diciembre,as.numeric))
datos_estatal_2025$total=rowSums(datos_estatal_2025|>dplyr::select(Enero:Diciembre),na.rm = T)
datos_estatal_2025$`Tipo de delito`[datos_estatal_2025$`Tipo de delito`%in%c(
  "Violación equiparada","Violación simple" 
)] ="Violación" 
datos_estatal_2025=datos_estatal_2025|>
  dplyr::select(Año,Entidad,`Tipo de delito`,Enero:total)|>
  dplyr::group_by(Año,Entidad,`Tipo de delito`)|>
  dplyr::summarise_all(sum)
##Lo hacemos con joins mejor


#el de 2020:2025 tambien
nacional2020 =nacional2020 |> dplyr::rename(`Población total`=`2020...9`)
poblacion_por_años=c(2015:2026) |> lapply(
  \(z){
    if(z>=2020){
      w=nacional2020
    }
    else{
      w=intercensal_nac_2015_2
    }
    w$Año=z
    return(w)
  }
)

poblacion_por_años=do.call(rbind,poblacion_por_años)
datos_estatal_2025$Entidad |> unique() |> lapply(\(w){w%in%poblacion_por_años$`Entidad federativa`}) |> unlist() |> all()


datos_estatal_2025=datos_estatal_2025|> 
  merge(poblacion_por_años |> dplyr::mutate(`Población total`=as.numeric(`Población total`)),by.x=c('Entidad','Año'),by.y=c('Entidad federativa',"Año") )

##Corregimos la tasa
datos_estatal_2025=datos_estatal_2025|>
  dplyr::mutate(tasa=1000*total/as.numeric(`Población total`))



######################Lo mismo pero para la municipal
##hidalgo_municipal_2025 ya lo definimos arriba


hidalgo_municipal_2025$total=rowSums(hidalgo_municipal_2025|>dplyr::select(Enero:Diciembre),na.rm = T)

hidalgo_municipal_2025=hidalgo_municipal_2025|>
  dplyr::select(Año,Municipio,`Tipo de delito`,Enero:Diciembre,total)

##Le pegamos del 2015 a 2019 la poblacion
hidalgo_municipal_2025$pobtot=rep(0,nrow(hidalgo_municipal_2025))
hidalgo_municipal_2025=hidalgo_municipal_2025|>
  dplyr::arrange(Año,Municipio,`Tipo de delito`)
hidalgo_municipal_2025$`Tipo de delito`[hidalgo_municipal_2025$`Tipo de delito`%in%c(
  "Violación equiparada","Violación simple" 
)] ="Violación" 
hidalgo_municipal_2025_2=hidalgo_municipal_2025|>
  dplyr::group_by(Año,Municipio,`Tipo de delito`)|>
  dplyr::summarise_all(sum)
##Rellenamos poblaciones

poblacion_por_años_municipal=c(2015:2026) |> lapply(
  \(z){
    if(z>=2020){
      w=intercensal_mun_2020|> dplyr::mutate(Municipio=stringr::str_squish (gsub('\\*','',Municipio)))
    }
    else{
      w=intercensal_mun_2015_2 |> dplyr::mutate(Municipio=stringr::str_squish (gsub('\\*','',Municipio)))
    }
    w$Año=z
    return(w)
  }
)
poblacion_por_años_municipal=do.call(rbind,poblacion_por_años_municipal)
hidalgo_municipal_2025_2$Municipio |> unique() |> lapply(\(w){w%in%poblacion_por_años_municipal$Municipio}) |> unlist() |> all()

hidalgo_municipal_2025_2=hidalgo_municipal_2025_2 |> 
  merge(poblacion_por_años_municipal |> dplyr::mutate(`Población total`=as.numeric(`Población total`)),by.x=c('Municipio','Año'),by.y=c('Municipio',"Año") )

#Calculamos tasa 
hidalgo_municipal_2025_2=hidalgo_municipal_2025_2|>
  dplyr::mutate(tasa=1000*total/`Población total`)


##Ahora sí generamos los CSV consumibles por js.

datos_estatal_2025$`Tipo de delito` |> unique()

datos_estatal_2025|>
  dplyr::ungroup()|>
  dplyr::filter(Entidad=='Hidalgo')|>
  dplyr::select(Año,`Tipo de delito`,total,`Población total`,tasa)|>
  write.csv("../Datos/CSVs_2/Hidalgo_Año_y_Tipo.csv",row.names = F,fileEncoding = "UTF-8")

incidencia_mensual=datos_estatal_2025|>
  dplyr::ungroup()|>
  dplyr::filter(Entidad=='Hidalgo')|>
  dplyr::select(-Entidad)|>
  tidyr::pivot_longer(cols = c(Enero:Diciembre),names_to = "Mes",values_to = "Conteo")|>
  dplyr::select(Año, `Tipo de delito`,Mes,Conteo)|>
  dplyr::mutate(Conteo=ifelse(is.na(Conteo),0,Conteo))

datos_estatal_2025|>
  dplyr::ungroup()|>
  dplyr::filter(Entidad=='Hidalgo')|>
  dplyr::select(-Entidad)|>
  tidyr::pivot_longer(cols = c(Enero:Diciembre),names_to = "Mes",values_to = "Conteo")|>
  dplyr::select(Año, `Tipo de delito`,Mes,Conteo)|>
  dplyr::mutate(Conteo=ifelse(is.na(Conteo),0,Conteo))|>
  write.csv("../Datos/CSVs_2/delitos por mes_15-24_estatal.csv",row.names = F,fileEncoding = "UTF-8")


#//Cambio. Guardamos las tasas de todos los estados
tasa_ent=datos_estatal_2025 |>
  dplyr::ungroup() |>
  dplyr::select(Año, `Tipo de delito`, Entidad, total, tasa) |>
  tidyr::pivot_wider(
    names_from = Entidad,
    values_from = c(total, tasa),
    names_glue = "{.value}_{Entidad}"
  )

tasa_hgo=datos_estatal_2025|>
  dplyr::ungroup()|>
  dplyr::select(Entidad,Año,`Tipo de delito`,tasa,total)|>
  #dplyr::filter(Entidad=='Hidalgo') |> 
  dplyr::group_by(Año,`Tipo de delito`)|>
  dplyr::summarise(tasa_media_hgo=mean(tasa),total_hgo=sum(total))
tasa_nac=datos_estatal_2025|>
  dplyr::ungroup()|>
  dplyr::select(Entidad,Año,`Tipo de delito`,tasa,total)|>
  dplyr::group_by(Año,`Tipo de delito`)|>
  dplyr::summarise(tasa_media_nac=mean(tasa),total_nac=sum(total))
tasa_ent=tasa_nac |> 
  merge(tasa_ent,by=c('Año','Tipo de delito'))
# tasa_ent=tasa_ent |> 
#   dplyr::mutate(prop_totales=round(100*total_hgo/total_nac,2))
tasa_ent |> write.csv("../Datos/CSVs_2/tasa_media_nacional.csv",fileEncoding = "UTF-8",row.names = F)


hidalgo_municipal_2025_2|>
  dplyr::select(Municipio:total) |> 
  #tidyr::pivot_longer(cols = Enero:Diciembre,names_to = "Mes",values_to = "Conteo")|>
  #dplyr::select(Año,Municipio,`Tipo de delito`,Mes,Conteo)|>
  #dplyr::mutate(Conteo=ifelse(is.na(Conteo),0,Conteo))|>
  write.csv("../Datos/CSVs_2/delitos por mes_15-24.csv",fileEncoding = "UTF-8",row.names = F)

##Modificamos el de tasas para el histórico municipal y agregar el promedio municipal y el promedio estatal
tasa_hgo=hidalgo_municipal_2025_2|>
  dplyr::select(Año,Municipio,`Tipo de delito`,total,tasa) |> 
  dplyr::group_by(Año,`Tipo de delito`) |> 
  dplyr::summarise(tasa_hgo=mean(tasa),
                   total_hgo=sum(total))
tasa_mpio=hidalgo_municipal_2025_2|>
  dplyr::select(Año,Municipio,`Tipo de delito`,total,tasa)|>
  dplyr::rename(tasa_mpio=tasa)
tasa_mpio=tasa_mpio |> 
  merge(tasa_hgo,by=c("Año","Tipo de delito"))
tasa_mpio |> 
  write.csv("../Datos/CSVs_2/Municipal_Año_y_Tipo.csv",fileEncoding = "UTF-8",row.names = F)

delitos_2015_2025=(datos_estatal_2025 |> 
  dplyr::filter(Año!=2026))$`Tipo de delito` |> unique()
delitos_2026=(datos_estatal_2025 |> 
  dplyr::filter(Año==2026))$`Tipo de delito` |> unique()
delitos_sin_cambios=delitos_2015_2025[(delitos_2015_2025 |> lapply(\(z){z%in% delitos_2026}) |> unlist())]
delitos_con_cambios=delitos_2015_2025[!(delitos_2015_2025 |> lapply(\(z){z%in% delitos_2026}) |> unlist())]
#"Violación equiparada" "Violación simple"   ahora son Violación 
##O sea que podemos reclasificar todos los delitos antiguos en los nuevos. 

##Los nuevos son: 
delitos_nuveos=delitos_2026[!(delitos_2026 |> lapply(\(z){z%in% delitos_2015_2025}) |> unlist())]
delitos_nuveos[delitos_nuveos!='Violación']

##O sea que tengo 39+8 delitos
#40 con seguimiento de 2015 - 2026 y
#8 con seguimiento de 2026->

#IDM=read.csv("Peticiones especiales/IDM_NM_ene25.csv",check.names = F,fileEncoding = "latin1")
