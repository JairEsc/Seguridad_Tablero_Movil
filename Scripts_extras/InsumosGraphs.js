//El Jair del presente cree que está fácil refactorizar el código del Jair del pasado. 
//supon que existe: 

//csvTasaMedia
//"Año","Tipo de delito","tasa_media_nac","total_nac","tasa_media_hgo","total_hgo","prop_totales"

//mesesEstatalCsvCargado
//"Año","Tipo de delito","Mes","Conteo"

function generarInsumosHistorico(tipo_de_delito){
    //Regresa un vector de (longitud 2015-2025/2026) x 2 (tasa Hgo y tasa Nacional)
    const filtroPorTipoDeDelito=data_tasa_media_fetched_and_splitted.filter((r)=>{//Comentario para Jair del futuro: Los .csv están ordenados por fecha 2015->>. Los filter bien pordían detenerse después de la última coincidencia
        return(r.replace(/"/g,'').split(",")[1]==tipo_de_delito)})
    return filtroPorTipoDeDelito.map((r)=>{return[parseInt(r.split(",")[0]),parseFloat(r.split(",")[4]),parseFloat(r.split(",")[2])]})//Año, Hgo, Nacional
}

function generarInsumosIncidenciaAnual(año){
    //Regresa un vector de (tasa de delitos del año seleccionado) x 1 (tasas de cada delito)
    const filtroPorAño=data_tasa_media_fetched_and_splitted.filter((r)=>{
        return(r.replace(/"/g,'').split(",")[0]==año)})
    return filtroPorAño.map((r)=>{return[parseInt(r.split(",")[0]),parseFloat(r.split(",")[4]),parseFloat(r.split(",")[2])]})//Año, Hgo, Nacional
}

function generarInsumosIncidenciaMensual(año,tipo_de_delito){
    //Regresa un vector de 12 x 1
    return data_meses_estatal_fetched_and_splitted.filter((r)=>{
        return (r.replace(/"/g,'').split(",")[0]==año && r.replace(/"/g,'').split(",")[1]==tipo_de_delito)
    }).map((r)=>{return([r.split(",")[2],parseInt(r.split(",")[3])])})//Mes, Conteo
}