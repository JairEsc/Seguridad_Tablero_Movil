/* Variar el width del selector según la seleccion. */
const select = document.getElementById("tipo_dropdown");

select.addEventListener("change", function () {
  const dummySelect = document.createElement("select");
  dummySelect.classList.add("dummy");
  const dummyOption = document.createElement("option");
  dummyOption.innerHTML = this.value;
  dummySelect.appendChild(dummyOption);
  document.body.appendChild(dummySelect);
  select.style.width = `${dummySelect.offsetWidth}px`;
  document.body.removeChild(dummySelect);
});
select.dispatchEvent(new Event("change"));
/* ------------------------------ */

//Todavía falta hacer dependiente de la selección las gráficas. Ahí, se deberá agregar la actualización de CVE_MUN de hidalgo para actualizar los colores.

let data_fetched_and_splitted; // Variable global para almacenar los datos procesados
let data_meses_estatal_fetched_and_splitted; // Variable global para almacenar los datos procesados
let data_tasa_media_fetched_and_splitted; // Variable global para almacenar los datos procesados

const plugin_actualizar_eleccion_cruzada=[{
  id: 'customEventListener',
  afterEvent: (chart, evt) => {
      console.log("Evento detectado:", evt.event.type);
      if (evt.event.type == 'click') {
          const points = chart.getElementsAtEventForMode(evt.event, 'y', { intersect: false }, true);
          if (points.length > 0) {
              const datasetIndex = points[0].datasetIndex;  // Índice del dataset
              const index = points[0].index;  // Índice de la barra clickeada
              
              let label = chart.data.labels[index];  // Obtener etiqueta de la barra
              console.log(label)
              

              document.getElementById('tipo_dropdown').value = label.replace('...','');  // Cambiar el valor del dropdown
              document.getElementById('tipo_dropdown').dispatchEvent(new Event('change'));
          }
      }
  }
}]
let csvTasaMedia = new Promise((resolve, reject) => {
  fetch("Datos/CSVs_2/tasa_media_nacional.csv")
    .then((response) => response.text())
    .then((data) => {
      data_tasa_media_fetched_and_splitted = data.split("\n");
      resolve(); // La promesa se resuelve cuando los datos están listos
    });
});
let csvCargado = new Promise((resolve, reject) => {
  fetch("Datos/CSVs_2/Hidalgo_Año_y_Tipo.csv")
    .then((response) => response.text())
    .then((data) => {
      data_fetched_and_splitted = data.split("\n");
      resolve(); // La promesa se resuelve cuando los datos están listos
    });
});
let mesesEstatalCsvCargado = new Promise((resolve, reject) => {
  fetch("Datos/CSVs_2/delitos por mes_15-24_estatal.csv")
    .then((response) => response.text())
    .then((data) => {
      data_meses_estatal_fetched_and_splitted = data.split("\n");
      resolve(); // La promesa se resuelve cuando los datos están listos
    });
});
ordenarPorValores=function(tipos_de_delito, valores) {
  let ordenado = valores.map((v, i) => ({ valor: v, delito: tipos_de_delito[i] }))
                        .sort((a, b) =>  b.valor-a.valor );

  return {
      valoresOrdenados: ordenado.map(obj => obj.valor),
      tiposOrdenados: ordenado.map(obj => obj.delito)
  };
}

generate_values_tasa_media = function (delito_sel) {//datos para gráfica de meses dada elecciones de año y delito
  //console.log("ESTATAL: datos para gráfica de meses dada elecciones de año y delito")
  //console.log(data_meses_estatal_fetched_and_splitted.slice(40*12*year_sel_modulo2015+1,40*12*(year_sel_modulo2015+1)+1).slice(12*delito_sel,12*(delito_sel+1)))
  arr_tasa=[]
  for(let www=0;www<10;www++){
    arr_tasa.push(data_tasa_media_fetched_and_splitted[delito_sel+1+www*40])
  }
  return(arr_tasa)
}
generate_values_meses_estatal = function (year_sel,delito_sel) {//datos para gráfica de meses dada elecciones de año y delito
  year_sel_modulo2015=(year_sel-2015)
  //console.log("ESTATAL: datos para gráfica de meses dada elecciones de año y delito")
  //console.log(data_meses_estatal_fetched_and_splitted.slice(40*12*year_sel_modulo2015+1,40*12*(year_sel_modulo2015+1)+1).slice(12*delito_sel,12*(delito_sel+1)))
  return(data_meses_estatal_fetched_and_splitted.slice(40*12*year_sel_modulo2015+1,40*12*(year_sel_modulo2015+1)+1)
.slice(12*delito_sel,12*(delito_sel+1)))
}
generate_values_Año = function (year_sel) {
  console.log("ESTATAL: datos para gráfica de tipos de delito dado año")

  //Codigo para generar valores al seleccionar el año en la pestaña: 'barplot_entidad'
  const inicio = 40 * (year_sel - 2015) + 1;
  const fin = inicio + 40;
  //console.log(data_fetched_and_splitted.slice(inicio, fin))
  if (!data_fetched_and_splitted) {
    return [];
  }
  return data_fetched_and_splitted.slice(inicio, fin).map((x) =>
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
  //console.log("ESTATAL: datos para gráfica histórica dado tipo de delito")
  arr = [];
  años=[]
  for (k = 0; k < 10; k++) {//En Enero de 2026 va a cambiar.Por ahora solo se consumen de 2015 a 2024
    arr.push(
      Math.round(
        kkk *
          parseFloat(
            data_fetched_and_splitted[k * 40 + tipo_sel]
              .split(",")[4]
              .replace(/[\r\n"']/g, "")
              .trim()
          )
      ) / kkk
    );
    años.push(data_fetched_and_splitted[k * 40 + tipo_sel].split(",")[0])
  }
  //console.log(arr);
  //console.log(años);
  return arr;
};
let tipos_de_delito;
let data;
let data_meses;
//Vamos a hacer un primera  llamada a los datos para alimentar a las gráficas por default.
const sub_labels_clasificacion={
  'Robo':'Robo simple, Casa-Habitación, Vehículo, Autopartes, Ganado ...',
  'Otros delitos contra el patrimonio':'Apropiación de bienes abandonados, Ocultación de artículos robados',
  'Otros delitos contra la sociedad':'Inducción a la mendicidad, Explotación de grupos socialmente desfavorecidos, Proporcionar inmuebles destinados al comercio carnal',
  'Otros delitos que atentan contra la libertad personal':'Auto secuestro, Retención y sustracción de incapaces, Actos relacionados',
  'Otros delitos que atentan contra la libertad y la seguridad sexual': 'Estupro, Ultraje a la moral pública, Exhibicionismo obseno, Lenocinio',
  'Otros delitos que atentan contra la vida y la integridad corporal':'Inducción o Ayuda al suicidio, Peligro de contagio, Inseminación artificial no consentida'
}
const inverted_sub_labels = Object.fromEntries(
  Object.entries(sub_labels_clasificacion).map(([key, value]) => [value, key])
);

Promise.all([csvCargado,csvTasaMedia]).then(() => {
  //Aquí alimentamos las gráficas por default. Y de paso nos aseguramos que los csv ya se leyeron.
  //Esto nada más ocurre la primera vez----
  //definimos un objeto para las sub-labels
  
  //------
  tipos_de_delito = data_fetched_and_splitted
    .slice(1, 41)
    .map((x) => x.split(",")[1].replace(/["]/g, ""));

  var optns = document.getElementById("tipo_dropdown");

  for (element in tipos_de_delito) {
    var opt = document.createElement("option");
    opt.value = tipos_de_delito[element];
    opt.innerHTML = tipos_de_delito[element];
    if(sub_labels_clasificacion[tipos_de_delito[element]]){
      opt.title=sub_labels_clasificacion[tipos_de_delito[element]]
      opt.innerHTML+='...'
      opt.style.backgroundColor='rgb(230, 230, 230)'
    }
    optns.appendChild(opt);
  }

  ///
  let primeros40 = generate_values_Año(2025);
  console.log("Primeros 40 valores:", primeros40);

  primeros40_ordenados_estatal=ordenarPorValores(tipos_de_delito,primeros40.map((x)=> {return(x)}))//filtrar valores muy pequeños?

  //
  data = {
    labels: primeros40_ordenados_estatal.tiposOrdenados.map((x)=>{if(sub_labels_clasificacion[x]){return(x+'...')}else{return(x)}}),//tipos_de_delito
    datasets: [
      {
        axis: "y",
        label: "Tasa de delito por cada mil habitantes",
        data: primeros40_ordenados_estatal.valoresOrdenados,//primeros40
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
        borderWidth: 2,
      },
    ],
  };
  const ctx = document
    .getElementById("barplot_tipo_por_año_estatal")
    .getContext("2d"); //inicio a crear la gráfica

  chart_barplot_tipos_por_año = new Chart(ctx, {
    type: "bar",
    data: data,
    responsive: true,
    options: {interaction:{intersect: false,
      mode:'y'
    },
      indexAxis: "y",
      maintainAspectRatio: false,
      scales: {
        y: {
          ticks: {
            mirror: true,
            color: "black",
            font: { size: 15 },
          },
        },
        x: { position: "top" },
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
    plugins: plugin_actualizar_eleccion_cruzada
    
  });

  //Otra gráfica de Prueba
  let primer_historico = generate_values_Tipo(1);
  //console.log("Primeros historico:", primer_historico);
  const ctx_hist = document
    .getElementById("lineplot_año_por_tipo_estatal")
    .getContext("2d");
  chart = new Chart(ctx_hist, {
    type: "line",
    data: {
      labels: Array.from({ length: 10 }, (_, i) => 2015 + i),
      datasets: [
        {
          data: primer_historico,
          backgroundColor: "rgba(179,142,93,0.8)",
          borderColor: "rgb(9, 86, 70)",
          borderWidth: 1,
          spanGaps: true,
          label: ["Tasa de delito por cada mil habitantes"],
        },
        {
          data: [],
          backgroundColor: "rgb(98, 17, 50)",
          borderColor: "rgba(0, 0, 0, 0.8)",
          borderWidth: 1,
          spanGaps: true,
          label: ["Promedio Nacional"],
        },
      ],
    },
    options: {
      locale: "en-EN",
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        chartArea: {
          backgroundColor: "rgba(240, 240, 240, 1)", // Cambia este color a lo que desees
        },
        tooltip: {
          callbacks: {
            label: (ctx) => (`${ctx.dataset.label}: ${ctx.raw}`)
          }
        },
      },
      scales: {
        y: {
          beginAtZero: false,
        },
      },
    },
  });
//console.log("Graficas por default estatal. Creada")
//una más: 
//Tasa media para las 32 entidades. 
chart.data.datasets[1].data=generate_values_tasa_media(0).map((x)=>{return(Math.round(parseFloat(x.split(",")[2])*100000)/100000)})
chart.update();
});

mesesEstatalCsvCargado.then(()=>{
  const meses = ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre"];

data_estatal_año_tipo=generate_values_meses_estatal(2025,0)
//console.log(data_estatal_año_tipo.map((x)=>{return parseFloat((x.split(","))[3].replace(/[\r\n"']/g, "").trim())}))
data_meses = {
  labels: meses,
  datasets: [{
    label: 'Delitos en Hidalgo (Aborto 2025)',
    data: data_estatal_año_tipo.map((x)=>{return parseFloat((x.split(","))[3].replace(/[\r\n"']/g, "").trim())}),
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
const image=new Image()
image.src = 'url(Datos/no_data.png)';
//Revisar si el primer default tiene datos

const ctx_meses = document
    .getElementById("barplot_meses")
    .getContext("2d"); //inicio a crear la gráfica
stackedBar = new Chart(ctx_meses, {
  type: 'bar',
  data: data_meses,
  options: {
    locale: "en-EN",
      scales: {
          x: {
              stacked: true,
          },
          y: {
            ticks:{precision:0},
              stacked: true
          }
      },
      maintainAspectRatio:false,
      
  },
});
})
//Va a haber un código equivalente para alimentar las gráficas por default del nivel municipal.
