//supon que existe: 

//csvTasaMedia
//"Año","Tipo de delito","tasa_media_nac","total_nac","tasa_media_hgo","total_hgo","prop_totales"

//mesesEstatalCsvCargado
//"Año","Tipo de delito","Mes","Conteo"

function generarInsumosHistorico(tipo_de_delito,entidad='Hidalgo'){
    //Regresa un vector de (longitud 2015-2025/2026) x 2 (tasa Hgo y tasa Nacional)
    const header=data_tasa_media[0].split(",").map((x)=>x.replace(/"/g,''))
    const filtroPorTipoDeDelito=data_tasa_media.filter((r)=>{//Comentario para Jair del futuro: Los .csv están ordenados por fecha 2015->>. Los filter bien pordían detenerse después de la última coincidencia
        return(r.replace(/"/g,'').split(",")[1]==tipo_de_delito && r.replace(/"/g,'').split(",")[0]!='2026')})
    const tasa_hidalgo_idx=header.indexOf("tasa_"+entidad)
    return filtroPorTipoDeDelito.map((r)=>{return[parseInt(r.split(",")[0]),parseFloat(r.split(",")[tasa_hidalgo_idx]),parseFloat(r.split(",")[2])]})//Año, Hgo, Nacional
}

function generarInsumosIncidenciaAnual(año){
    //Regresa un vector de (tasa de delitos del año seleccionado) x 1 (tasas de cada delito)
    const filtroPorAño=data_tasa_media.filter((r)=>{
        return(r.replace(/"/g,'').split(",")[0]==año)})
    return filtroPorAño.map((r)=>{return[(r.split(",")[1].replace(/"/g,'')),parseFloat(r.split(",")[2])]})//Año, Hgo, Nacional
}

function generarInsumosIncidenciaMensual(año,tipo_de_delito){
    //Regresa un vector de 12 x 1
    return data_meses_estatal.filter((r)=>{
        return (r.replace(/"/g,'').split(",")[0]==año && r.replace(/"/g,'').split(",")[1]==tipo_de_delito)
    }).map((r)=>{return([r.split(",")[2].replace(/"/g,''),parseInt(r.split(",")[3])])})//Mes, Conteo
}
//data_municipal_año_tipo_municipio
//"Año","Tipo de delito","Municipio","total","tasa_mpio","tasa_hgo","total_hgo"
function generarInsumosColorearMapa(año,tipo_de_delito){
    //Regresa un vector de 84 x 1
    const filtroAñoTipo = data_municipal_año_tipo_municipio.filter((r)=>{
        const tipo_de_delito_r=r.replace(/"/g,'').split(",")[1];
        const año_r=r.replace(/"/g,'').split(",")[0];
        const municipio_r=r.replace(/"/g,'').split(",")[2];
        return (año_r==año && tipo_de_delito_r==tipo_de_delito)
    })
    return filtroAñoTipo
}
function actualizarPropiedadesGeojson(municipio, listaMunicipales,tasasMunicipales,totalesMunicipales, ranking_map) {

    const municipioIndex = listaMunicipales.indexOf(municipio);
    const tasa = municipioIndex !== -1 ? tasasMunicipales[municipioIndex] : null;
    const total = municipioIndex !== -1 ? totalesMunicipales[municipioIndex] : null;
    const ranking = tasa !== null ? ranking_map.get(tasa) : null;
    return([ranking,tasa, total])
}
function generarInsumosHistoricoMunicipal(tipo_de_delito,municipio){
    //Regresa un vector de (longitud 2015-2025/2026) x 2 (tasa Hgo y tasa Nacional)
    const filtroPorTipoDeDelito=data_municipal_año_tipo_municipio.filter((r)=>{
        const tipo_de_delito_r=r.replace(/"/g,'').split(",")[1];
        //const año_r=r.replace(/"/g,'').split(",")[0];
        const municipio_r=r.replace(/"/g,'').split(",")[2];
        return(municipio_r==municipio && tipo_de_delito_r==tipo_de_delito && r.replace(/"/g,'').split(",")[0]!=2026)
    })
    return filtroPorTipoDeDelito.map((r)=>{return[parseInt(r.split(",")[0]),parseFloat(r.split(",")[4]),parseFloat(r.split(",")[5])]})//Año, Hgo, Nacional
}

function generarInsumosIncidenciaAnualMunicipal(año,municipio){
    //Regresa un vector de (tasa de delitos del año seleccionado) x 1 (tasas de cada delito)
    const filtroPorAño=data_municipal_año_tipo_municipio.filter((r)=>{
        const tipo_de_delito_r=r.replace(/"/g,'').split(",")[1];
        const año_r=r.replace(/"/g,'').split(",")[0];
        const municipio_r=r.replace(/"/g,'').split(",")[2];

        return(año_r==año && municipio_r==municipio)
    })
    return filtroPorAño.map((x)=>{return([(x.split(",")[1].replace(/"/g,'')),parseFloat(x.split(",")[4])])})//Año, Hgo, Nacional
}
//data_mensual_año_tipo_municipio
//"Municipio","Año","Tipo de delito","Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre","total"

function generarInsumosIncidenciaMensualMunicipal(año,tipo_de_delito,municipio){
    //Regresa un vector de 12 x 1
    const header = data_mensual_año_tipo_municipio[0].split(",").map(h => h.replace(/"/g,''))
    const filtroAñoTipoMunicipio = data_mensual_año_tipo_municipio.filter((r)=>{
        const tipo_de_delito_r=r.replace(/"/g,'').split(",")[2];
        const año_r=r.replace(/"/g,'').split(",")[1];
        const municipio_r=r.replace(/"/g,'').split(",")[0];
        return (año_r==año && tipo_de_delito_r==tipo_de_delito && municipio_r==municipio)
    })
    if(filtroAñoTipoMunicipio.length==0){
        return [header, [0,0,0,0,0,0,0,0,0,0,0,0],true]
    }
    const values = filtroAñoTipoMunicipio[0].split(",").map((v, i) => i < 3 ? v.replace(/"/g,'') : parseInt(v)) || [];
    return [header, values]
}

function colorearMapaMunicipios(delito_actual='Aborto',año_actual='2026'){
    let arr_area_promesa_actual=[]
    let arr_absoluto_promesa_actual=[]
    let Promesa_Actual_Actualizamos_Area = new Promise((resolve, reject) => {
    //actualizamos el campo área con el ranking
    
    //Esto sería si quisiera ponerles el valor que les corresponde. Quiero el ranking. 
    //replicamos el vector pero en lugar de valor tiene el ranking sobre los valores unicos
    //e.g. [0,0,0,1,2,2,3]-> [1,1,1,2,3,3,4]
    const tasasMunicipiales = generarInsumosColorearMapa(año_actual,delito_actual).map(//Municipal
        (x)=>{
            return(
            [x.split(",")[2].replace(/[\r\n"']/g, "").trim(),//municipio
            parseFloat(x.split(",")[4].replace(/[\r\n"']/g, "").trim()),//tasa_mpio
            parseInt(x.split(",")[3].replace(/[\r\n"']/g, "").trim()),//total
            ]
            )
        }
        )
    let valores_unicos = [...new Set(tasasMunicipiales.map((x)=>{return(x[1])}))].sort((a, b) => b-a); // Ordenamos de mayor a menor
    let ranking_map = new Map(
        valores_unicos.map((valor, index) => [valor, index + 1])
    ); // Asignamos ranking
        // Asignamos el ranking a cada municipio en Leaflet
        poligonos_map_h.eachLayer((layer) => {
        if(valores_unicos.length==0){//Por la clasificación de delitos nuevos
            layer.feature.properties.Area =1
            layer.feature.properties.COV_ID =1
            layer.feature.properties.COV_ =0
            layer.feature.properties.PERIMETER =0
        }else{
            const valoresActualizables_prev=actualizarPropiedadesGeojson(municipio=layer.feature.properties.NOM_MUN, 
            tasasMunicipiales.map((x)=>x[0]),
            tasasMunicipiales.map((x)=>x[1]),
            tasasMunicipiales.map((x)=>x[2]), ranking_map=ranking_map)
            const valoresActualizables= valoresActualizables_prev[0]===null?[1,0,0]:valoresActualizables_prev
            //console.log(valoresActualizables)//Ranking, tasa, total

            layer.feature.properties.Area =//Este 
            (valoresActualizables[0])/ranking_map.get(valores_unicos[valores_unicos.length - 1])//
            layer.feature.properties.COV_ID =
            valoresActualizables[0]
            layer.feature.properties.COV_ =
            Math.round(
                parseFloat(
                10000 *
                    valoresActualizables[1]
                )
            ) / 10000; // Asignamos ranking en lugar del valor
            layer.feature.properties.PERIMETER =
            valoresActualizables[2];
            }
        });
        resolve();
        
    });
    Promesa_Actual_Actualizamos_Area.then(()=>{
    poligonos_map_h.eachLayer((layer)=>{
        layer.unbindTooltip(); // Elimina tooltip anterior
        layer.bindTooltip(
        'Municipio: ' + layer.feature.properties.NOM_MUN + '<br>' +
        'Ranking: ' + layer.feature.properties.COV_ID + '<br>' +
        'Tasa de delitos por cada mil: ' + layer.feature.properties.COV_ + '<br>'+
        'Delitos registrados (seleccionado): ' + layer.feature.properties.PERIMETER
        )});
    poligonos_map_h.resetStyle()
    refrescarSeleccionMunicipios()
    })
}
function colorearMapaEntidades(delito_actual='Aborto',año_actual='2026'){
      
        const header = data_tasa_media[0].split(",").map((x) => x.replace(/"/g, ""));
        const tasasEstatales = data_tasa_media.filter((row,idx)=>{
          return(idx===0 || row.replace(/"/g,'').split(",")[1]==delito_actual)
        })
        const tasasEstatalesAñoAtual_prev=tasasEstatales.filter((row)=>{
            return(row.split(",")[0]==año_actual)
        })
        if(tasasEstatalesAñoAtual_prev.length==0){
            poligonos_map.eachLayer((layer)=>{
                layer.feature.properties.Area =1 })
            return 0 }
        
        const tasasEstatalesAñoAtual=tasasEstatalesAñoAtual_prev[0].split(",").slice(4).slice(32).map((x)=>parseFloat(x.replace(/\r/g, ""))) // Solo tasas de entidades
          let valores_unicos = [...new Set(tasasEstatalesAñoAtual)].sort((a, b) => b-a); // Ordenamos de mayor a menor
          let ranking_map = new Map(
            valores_unicos.map((valor, index) => [valor, index + 1])
          ); // Asignamos ranking
          //console.log("Valores únicos:", valores_unicos);
          //console.log("Ranking Map:", ranking_map);
            // Asignamos el ranking a cada municipio en Leaflet
            poligonos_map.eachLayer((layer) => {
              //console.log(layer.feature.properties.NOMGEO)
              if(valores_unicos.length==0){//Por la clasificación de delitos nuevos
                layer.feature.properties.Ranking=1
              }else{
                const valoresActualizables_prev=actualizarPropiedadesGeojson(municipio=layer.feature.properties.NOMGEO, 
                  tasasEstatales[0].split(",").slice(4).slice(32).map((x)=>x.replace("tasa_","").replace(/"/g,"").replace(/\r/g, "")),
                  tasasEstatalesAñoAtual,
                  tasasEstatalesAñoAtual, 
                  ranking_map=ranking_map)
                  //console.log(valoresActualizables_prev)//Ranking, tasa, total
                const valoresActualizables= valoresActualizables_prev[0]===null?[1,0,0]:valoresActualizables_prev

                layer.feature.properties.Area =//Este 
                  (valoresActualizables[0])/ranking_map.get(valores_unicos[valores_unicos.length-1])//
                layer.feature.properties.Ranking=valoresActualizables[0]
                layer.feature.properties.Valor=valoresActualizables[1]
                }

            })
          poligonos_map.eachLayer((layer)=>{
            layer.unbindTooltip(); // Elimina tooltip anterior
            layer.bindTooltip(
              'Entidad: ' + layer.feature.properties.NOMGEO + '<br>' +
              'Ranking: ' + layer.feature.properties.Ranking + '<br>' +
              'Tasa: ' + layer.feature.properties.Valor + '<br>' 
            )});
          poligonos_map.resetStyle()
}