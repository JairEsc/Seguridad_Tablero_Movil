//Recibe la actualización del Nav Visible

//Hay que tener cuidado con la selección porque están por orden alfabético los municipios.
//fetch de los datos necesarios

let data_municipal_fetched_and_splitted; // Variable global para almacenar los datos procesados
let data_mensual_fetched_and_splitted; // Variable global para almacenar los datos procesados
let LargeCsvCargado = new Promise((resolve, reject) => {
  fetch("Datos/CSVs_2/Municipal_Año_y_Tipo.csv")
    .then((response) => response.text())
    .then((data) => {
      data_municipal_fetched_and_splitted = data.split("\n");
      resolve(); // La promesa se resuelve cuando los datos están listos
    });
});
let VeryLargeCsvCargado = new Promise((resolve, reject) => {
  fetch("Datos/CSVs_2/delitos por mes_15-24.csv")
    .then((response) => response.text())
    .then((data) => {
      data_mensual_fetched_and_splitted = data.split("\n");
      resolve(); // La promesa se resuelve cuando los datos están listos
    });
});

generate_values_Mun_Año = function (year_sel, municipio_sel) {
  //Codigo para generar valores al seleccionar el año en la pestaña: 'barplot_entidad'
  const inicio = 40 * 84 * (year_sel - 2015)+1; //notar que NO incluye al header.
  const fin = inicio + 40 * 84;
  if (!data_municipal_fetched_and_splitted) {
    return [];
  }
  console.log("MUNICIPAL. datos incidencia dado año y municipio")
  console.log(data_municipal_fetched_and_splitted
    .slice(inicio, fin)
    .slice(40 * municipio_sel, 40 * (municipio_sel + 1)))
  return data_municipal_fetched_and_splitted
    .slice(inicio, fin)//filtro al año
    .slice(40 * municipio_sel, 40 * (municipio_sel + 1))//filtro al municipio
    .map(
      (
        x //notar que aquí ya no tiene header
      ) =>
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
    ); 
};
generate_values_Mun_Tipo = function (tipo_sel, municipio_sel) {
  //el municipio es numerico,  desde cero(acatlán) hasta 83 zimapán
  //Codigo para generar valores al seleccionar el año en la pestaña: 'historico_entidad'
  console.log("MUNICIPAL. datos historico dado tipo de delito y municipio")

  arr = [];
  año=[]
  for (let w = 0; w < 10; w++) {
    arr.push(
      Math.round(
        kkk *
          parseFloat(
            data_municipal_fetched_and_splitted[
              w * 40 * 84 + tipo_sel + 40 * municipio_sel
            ]
              .split(",")[4]
              .replace(/[\r\n"']/g, "")
              .trim()
          )
      ) / kkk
    );
    año.push(data_municipal_fetched_and_splitted[
      w * 40 * 84 + tipo_sel + 40 * municipio_sel
    ])
  }
  console.log(año)
  return arr;
};
generate_values_Año_Mun_Tipo = function (year_sel, municipio_sel,tipo_sel) {
  console.log("MUNICIPAL. datos de___ dado año, municipio y tipo de delito")
  year_sel_modulo2015=(year_sel-2015)
  //el municipio es numerico,  desde cero(acatlán) hasta 83 zimapán
  //Codigo para generar valores al seleccionar el año en la pestaña: 'historico_entidad'
 
  return(data_mensual_fetched_and_splitted.slice(84*40*12*(year_sel_modulo2015)+1,84*40*12*(year_sel_modulo2015+1)+1)//Ya no trae header. 
  .slice(40*12*(municipio_sel),40*12*(municipio_sel+1))//filtro a municipio
  .slice(12*tipo_sel,12*(tipo_sel+1)))
};
let data_mun;
let data_meses_mun;
//Vamos a hacer un primera  llamada a los datos para alimentar a las gráficas por default.

LargeCsvCargado.then(() => {
  //Aquí alimentamos las gráficas por default. Y de paso nos aseguramos que los csv ya se leyeron.
  ///
  let primeros40_Mun = generate_values_Mun_Año(2024, 45);
  //console.log("Primeros 40 valores:", primeros40);
  primeros40_Mun_ordenados_estatal=ordenarPorValores(tipos_de_delito,primeros40_Mun.map((x)=> {return(x)}))//filtrar valores muy pequeños?

  ///grafica de prueba
  data_mun = {
    labels: primeros40_Mun_ordenados_estatal.tiposOrdenados.map((x)=>{if(sub_labels_clasificacion[x]){return(x+'...')}else{return(x)}}),
    datasets: [
      {
        axis: "y",
        label: "Tasa de delito por cada mil habitantes",
        data: primeros40_Mun_ordenados_estatal.valoresOrdenados,
        fill: false,
        backgroundColor: [
          "rgba(98,17,50,0.1)",
          "rgba(157,36,73,0.1)",
          "rgba(112,144,144,0.1)",
          "rgba(212,193,156,0.1)",
          "rgba(179,142,93,0.1)",
          "rgba(29,29,27,0.1)",
          "rgba(9, 86, 70,0.1)",
        ],
        borderColor: [
          "rgb(98,17,50)",
          "rgb(157,36,73)",
          "rgb(112,144,144)",
          "rgb(212,193,156)",
          "rgb(179,142,93)",
          "rgb(29,29,27)",
          "rgb(9, 86, 70)",
        ],
        borderWidth: 1,
      },
    ],
  };
  const ctx_mun = document
    .getElementById("barplot_tipo_por_año_municipal")
    .getContext("2d"); //inicio a crear la gráfica

  chart_barplot_mun_tipos_por_año = new Chart(ctx_mun, {
    type: "bar",
    data: data_mun,
    
    responsive: true,
    options: {interaction:{intersect: false,
      mode:'y'
    },
      indexAxis: "y",
      maintainAspectRatio: false,
      scales: {
        y: {
          
          ticks: {
            precision:0,
            mirror: true,
            color: "black",
            font: { size: 15 },
          },
        },
        x: { position: "top",
         },
      },
      locale: "en-EN",
      plugins: {
        tooltip: {
          callbacks: {
            title: (tooltipItems) => {
              // Obtener el label original
              let originalLabel = tooltipItems[0].label;
              if(sub_labels_clasificacion[originalLabel.substring(0,originalLabel.length-3)]){
                return(sub_labels_clasificacion[originalLabel.substring(0,originalLabel.length-3)])
              }
              else{
                return originalLabel
              }
              
            }
          }
        }
      }
    },
    plugins:plugin_actualizar_eleccion_cruzada
  });

  //Otra gráfica de Prueba
  let primer_historico_mun = generate_values_Mun_Tipo(1, 45);
  //console.log("Primeros historico:", primer_historico);
  const ctx_hist_mun = document
    .getElementById("lineplot_año_por_tipo_municipal")
    .getContext("2d");
  chart_mun = new Chart(ctx_hist_mun, {
    type: "line",
    data: {
      labels: Array.from({ length: 10 }, (_, i) => 2015 + i),
      datasets: [
        {
          data: primer_historico_mun,
          backgroundColor: "rgba(179,142,93,0.8)",
          borderColor: "rgb(9, 86, 70)",
          borderWidth: 1,
          spanGaps: true,
          label: ["Tasa de delito por cada mil habitantes"],
        },
      ],
    },
    options: {
      locale: "en-EN",
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        tooltip: {
          callbacks: {
            label: (ctx) => (`${ctx.dataset.label}: ${ctx.raw}`)
          }
        },
        chartArea: {
          backgroundColor: "rgba(240, 240, 240, 1)", // Cambia este color a lo que desees
        },
      },
      scales: {
        y: {
          beginAtZero: false,
        },
      },
    },
  });

  var año_sel_promesa = 2025;
  var tipo_sel_promesa = 0;
  ///Hasta aquí ya se crearon las gráficas por default del municipio pachuca (o pacula tal vez)
  let arr_area_promesa = [];
  let arr_absoluto_promesa = [];
  let Promesa_Actualizamos_Area = new Promise((resolve, reject) => {
    //actualizamos el campo área con el ranking

    //slice a año seleccionado
    const inicio_promesa = 40 * 84 * (año_sel_promesa - 2015) + 1; //notar que no incluye al header.
    const fin_promesa = inicio_promesa + 40 * 84;

    //ciclo 84 con tamaño de paso 40
    data_año_seleccionado = data_municipal_fetched_and_splitted.slice(
      inicio_promesa,
      fin_promesa
    );
    for (let ww = 0; ww < 84; ww++) {
      arr_area_promesa.push(
        parseFloat(
          data_año_seleccionado[ww * 40 + tipo_sel_promesa]
            .split(",")[4]
            .replace(/[\r\n"']/g, "")
            .trim()
        )
      );
      arr_absoluto_promesa.push(
        parseFloat(
          data_año_seleccionado[ww * 40 + tipo_sel_promesa]
            .split(",")[3]
            .replace(/[\r\n"']/g, "")
            .trim()
        )
      );
    }
    //sé que están en el orden del csv.
    //que es el orden del array municipios.
    /*poligonos_map_h.eachLayer((layer) => {
      layer.feature.properties.Area = arr_area_promesa[municipios.indexOf(layer.feature.properties.NOM_MUN)];
    });*/
    //Esto sería si quisiera ponerles el valor que les corresponde. Quiero el ranking.
    //replicamos el vector pero en lugar de valor tiene el ranking sobre los valores unicos
    //e.g. [0,0,0,1,2,2,3]-> [1,1,1,2,3,3,4]

    let valores_unicos = [...new Set(arr_area_promesa)].sort((a, b) => a - b); // Ordenamos de menor a mayor
    let ranking_map = new Map(
      valores_unicos.map((valor, index) => [valor, index + 1])
    ); // Asignamos ranking
    // Asignamos el ranking a cada municipio en Leaflet
    poligonos_map_h.eachLayer((layer) => {
      let area_valor =
        arr_area_promesa[municipios.indexOf(layer.feature.properties.NOM_MUN)];
      layer.feature.properties.Area =
        (ranking_map.get(valores_unicos[valores_unicos.length - 1]) +
          1 -
          ranking_map.get(area_valor)) /
        ranking_map.get(valores_unicos[valores_unicos.length - 1]); // Asignamos ranking en lugar del valor
      layer.feature.properties.COV_ID =
        ranking_map.get(valores_unicos[valores_unicos.length - 1]) +
        1 -
        ranking_map.get(area_valor); // Asignamos ranking en lugar del valor
      layer.feature.properties.COV_ =
        Math.round(
          parseFloat(
            10000 *
              arr_area_promesa[
                municipios.indexOf(layer.feature.properties.NOM_MUN)
              ]
          )
        ) / 10000; // Asignamos ranking en lugar del valor
      layer.feature.properties.PERIMETER =
        arr_absoluto_promesa[
          municipios.indexOf(layer.feature.properties.NOM_MUN)
        ];
    });
    resolve();
  });
  Promesa_Actualizamos_Area.then(() => {
    poligonos_map_h.eachLayer((layer) => {
      layer.unbindTooltip(); // Elimina tooltip anterior
      layer.bindTooltip(
        "Municipio: " +
          layer.feature.properties.NOM_MUN +
          "<br>" +
          "Ranking: " +
          layer.feature.properties.COV_ID +
          "<br>" +
          "Tasa de delitos por cada mil: " +
          layer.feature.properties.COV_ +
          "<br>" +
          "Delitos en Total: " +
          layer.feature.properties.PERIMETER
      );
    });
    poligonos_map_h.resetStyle();

  });


});

VeryLargeCsvCargado.then(()=>{
  











  ///Vamos a crear la función por meses. Asume selección de año, municipio y delito.

  //Referenciamos a los valores de la selección. 
  //Aqui ya sabemos que es municipio 45
  //Año 2024(10)
  //Delito 0
  datos_año_mun_delito=generate_values_Año_Mun_Tipo(2024,45,0)

  //console.log(datos_año_mun_delito.map((x)=>{return parseFloat((x.split(","))[4].replace(/[\r\n"']/g, "").trim())}))

const meses = ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"];
//revisa si los datos son constantes. 
if(datos_año_mun_delito.map((x)=>{return parseFloat((x.split(","))[4].replace(/[\r\n"']/g, "").trim())}).reduce((partialSum, a) => partialSum + a, 0)==0){
  console.log("era cero")
  document.getElementById('barplot_meses_mun').style.backgroundImage='url(Datos/no_data.png)';
}
data_meses_mun = {
  labels: meses,
  datasets: [{
    label: "Total de Delitos (Aborto 2025)",
    data: datos_año_mun_delito.map((x)=>{return parseFloat((x.split(","))[4].replace(/[\r\n"']/g, "").trim())}),
    fill: false,
        backgroundColor: [
          "rgb(98,17,50)",
          "rgb(157,36,73)",
          "rgb(112,144,144)",
          "rgb(212,193,156)",
          "rgb(179,142,93)",
          "rgb(29,29,27)",
          "rgb(9, 86, 70)",
        ],
        borderColor:[
          "rgba(98,17,50,0.1)",
          "rgba(157,36,73,0.1)",
          "rgba(112,144,144,0.1)",
          "rgba(212,193,156,0.1)",
          "rgba(179,142,93,0.1)",
          "rgba(29,29,27,0.1)",
          "rgba(9, 86, 70,0.1)",
        ] ,
    borderWidth: 1
  }]
};
const ctx_meses_mun = document
    .getElementById("barplot_meses_mun")
    .getContext("2d"); //inicio a crear la gráfica
stackedBar_meses = new Chart(ctx_meses_mun, {
  type: 'bar',
  data: data_meses_mun,
  options: {
    locale: "en-EN",
      scales: {
          x: {
              stacked: true,
          },
          y: {
              stacked: true,
              ticks:{precision:0},
          }
      },
      maintainAspectRatio:false,
  }
});

})