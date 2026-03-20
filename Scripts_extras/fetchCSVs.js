
let data_año_tipo_estatal; // Variable global para almacenar los datos procesados
let data_meses_estatal; // Variable global para almacenar los datos procesados
let data_tasa_media; // Variable global para almacenar los datos procesados
let tipos_de_delito;

let cargandoDataTasaMedia = new Promise((resolve, reject) => {
  fetch("Datos/CSVs_2/tasa_media_nacional.csv")
    .then((response) => response.text())
    .then((data) => {
      data_tasa_media = data.split("\n");
      
      resolve();
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


/*Municipales */
let data_municipal_año_tipo_municipio; // Variable global para almacenar los datos procesados
let data_mensual_año_tipo_municipio; // Variable global para almacenar los datos procesados

let LargeCsvCargado = new Promise((resolve, reject) => {
  fetch("Datos/CSVs_2/Municipal_Año_y_Tipo.csv")
    .then((response) => response.text())
    .then((data) => {
      data_municipal_año_tipo_municipio = data.split("\n");
      resolve(); // La promesa se resuelve cuando los datos están listos
    });
});
let VeryLargeCsvCargado = new Promise((resolve, reject) => {
  fetch("Datos/CSVs_2/delitos por mes_15-24.csv")
    .then((response) => response.text())
    .then((data) => {
      data_mensual_año_tipo_municipio = data.split("\n");
      resolve(); // La promesa se resuelve cuando los datos están listos
    });
});