//El Jair del presente cree que está fácil refactorizar el código del Jair del pasado. 
//supon que existe: 

//csvTasaMedia
//"Año","Tipo de delito","tasa_media_nac","total_nac","tasa_media_hgo","total_hgo","prop_totales"

//mesesEstatalCsvCargado
//"Año","Tipo de delito","Mes","Conteo"

function generarInsumosHistorico(tipo_de_delito){
    //Regresa un vector de (longitud 2015-2025/2026) x 2 (tasa Hgo y tasa Nacional)
    const filtroPorTipoDeDelito=data_tasa_media.filter((r)=>{//Comentario para Jair del futuro: Los .csv están ordenados por fecha 2015->>. Los filter bien pordían detenerse después de la última coincidencia
        return(r.replace(/"/g,'').split(",")[1]==tipo_de_delito)})
    return filtroPorTipoDeDelito.map((r)=>{return[parseInt(r.split(",")[0]),parseFloat(r.split(",")[4]),parseFloat(r.split(",")[2])]})//Año, Hgo, Nacional
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
        return(municipio_r==municipio && tipo_de_delito_r==tipo_de_delito)
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

