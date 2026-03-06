
let data_año_tipo_estatal; // Variable global para almacenar los datos procesados
let data_meses_estatal; // Variable global para almacenar los datos procesados
let data_tasa_media; // Variable global para almacenar los datos procesados
let tipos_de_delito;

let cargandoDataTasaMedia = new Promise((resolve, reject) => {
  fetch("Datos/CSVs_2/tasa_media_nacional.csv")
    .then((response) => response.text())
    .then((data) => {
      data_tasa_media = data.split("\n");
      resolve(); // La promesa se resuelve cuando los datos están listos
    });
});
let cargandoDataAñoTipo = new Promise((resolve, reject) => {
  fetch("Datos/CSVs_2/Hidalgo_Año_y_Tipo.csv")
    .then((response) => response.text())
    .then((data) => {
      data_año_tipo_estatal = data.split("\n");
      tipos_de_delito = data_año_tipo_estatal
        .slice(-48,-1)
        .map((x)=>x.replace(/"/g,'').split(",")[1]);
      resolve(); // La promesa se resuelve cuando los datos están listos
    });
});
let cargadoDataMesesEstatal= new Promise((resolve, reject) => {
  fetch("Datos/CSVs_2/delitos por mes_15-24_estatal.csv")
    .then((response) => response.text())
    .then((data) => {
      data_meses_estatal = data.split("\n");
      resolve(); // La promesa se resuelve cuando los datos están listos
    });
});

generate_values_tasa_media = function (delito_sel) {//datos para gráfica de meses dada elecciones de año y delito
  //console.log("ESTATAL: datos para gráfica de meses dada elecciones de año y delito")
  //console.log(data_meses_estatal_fetched_and_splitted.slice(40*12*year_sel_modulo2015+1,40*12*(year_sel_modulo2015+1)+1).slice(12*delito_sel,12*(delito_sel+1)))
  arr_tasa=[]
  for(let www=0;www<11;www++){//Hasta 2025
    arr_tasa.push(data_tasa_media[delito_sel+1+www*39])
  }
  console.log(arr_tasa)
  return(generarInsumosHistorico(tipos_de_delito[delito_sel]))
  return(arr_tasa)
}
generate_values_meses_estatal = function (year_sel,delito_sel) {//datos para gráfica de meses dada elecciones de año y delito
  //year_sel_modulo2015=(year_sel-2015)
  //console.log("ESTATAL: datos para gráfica de meses dada elecciones de año y delito")
  //console.log(data_meses_estatal_fetched_and_splitted.slice(40*12*year_sel_modulo2015+1,40*12*(year_sel_modulo2015+1)+1).slice(12*delito_sel,12*(delito_sel+1)))
  
  return(generarInsumosIncidenciaMensual(year_sel,tipos_de_delito[delito_sel]))
  return(data_meses_estatal.slice(40*12*year_sel_modulo2015+1,40*12*(year_sel_modulo2015+1)+1)
.slice(12*delito_sel,12*(delito_sel+1)))
}
generate_values_Año = function (year_sel) {
  //console.log("ESTATAL: datos para gráfica de tipos de delito dado año")

  //Codigo para generar valores al seleccionar el año en la pestaña: 'barplot_entidad'
  const inicio = 39 * (year_sel - 2015) + 1;
  const fin = inicio + 39;
  //console.log(data_fetched_and_splitted.slice(inicio, fin))
  if (!data_año_tipo_estatal) {
    return [];
  }
  return(generarInsumosIncidenciaAnual(year_sel))
  return data_año_tipo_estatal.slice(inicio, fin).map((x) =>
    (
      Math.round(
        100000 *
          parseFloat(
            x
              .split(",")[4]
              .replace(/[\r\n"']/g, "")
              .trim()
          )
      ) / 100000
    )
      .toString()
      .replace(",", ".")
  ); // Extraer columna 4
};
const kkk = 100000;
generate_values_Tipo = function (tipo_sel) {//notar que no es consistente con la tasa nacional. Este incluye header. 
  //Codigo para generar valores al seleccionar el año en la pestaña: 'historico_entidad'
  arr = [];
  años=[]
  for (k = 0; k < 11; k++) {//En Enero de 2026 va a cambiar.Por ahora solo se consumen de 2015 a 2024
    arr.push(
      Math.round(
        kkk *
          parseFloat(
            data_año_tipo_estatal[k * 39 + tipo_sel]
              .split(",")[4]
              .replace(/[\r\n"']/g, "")
              .trim()
          )
      ) / kkk
    );
    años.push(data_año_tipo_estatal[k * 39 + tipo_sel].split(",")[0])
  }
  //console.log(arr);
  //console.log(años);
  return arr;
};