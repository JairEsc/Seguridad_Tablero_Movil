//Renombramos a "InicializacionTableroEstatal.js".



//Todavía falta hacer dependiente de la selección las gráficas. Ahí, se deberá agregar la actualización de CVE_MUN de hidalgo para actualizar los colores.

  
const plugin_actualizar_eleccion_cruzada=[{
  id: 'customEventListener',
  afterEvent: (chart, evt) => {
      //console.log("Evento detectado:", evt.event.type);
      if (evt.event.type == 'click') {
          const points = chart.getElementsAtEventForMode(evt.event, 'y', { intersect: false }, true);
          if (points.length > 0) {
              const datasetIndex = points[0].datasetIndex;  // Índice del dataset
              const index = points[0].index;  // Índice de la barra clickeada
              
              let label = chart.data.labels[index];  // Obtener etiqueta de la barra
              //console.log(label)
              

              document.getElementById('tipo_dropdown').value = label.replace('...','');  // Cambiar el valor del dropdown
              document.getElementById('tipo_dropdown').dispatchEvent(new Event('change'));
          }
      }
  }
}]



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

Promise.all([cargandoDataAñoTipo,cargandoDataTasaMedia]).then(() => {
  //Aquí alimentamos las gráficas por default. Y de paso nos aseguramos que los csv ya se leyeron.
  //Esto nada más ocurre la primera vez----
  //definimos un objeto para las sub-labels
  alimentarDropdownTiposDelito(tipos_de_delito)//Afecta a document.getElementById("tipo_dropdown")
  //Primer coloreado del mapa. 
  colorearMapaEntidades(delito_actual=delito_actual,año_actual='2026');
  map.fitBounds(poligonos_map_h.getBounds())
  seleccionarHidalgo()
  ///
  let primeros40 = generarInsumosIncidenciaAnual(2026);
  primeros40_ordenados_estatal=ordenarPorValores(primeros40.map((x)=> {return(x[0])}),primeros40.map((x)=> {return(x[1])}))//filtrar valores muy pequeños?
  //
  dataGraficaTiposPorAño = inicializarDataGraficaTiposPorAño(primeros40_ordenados_estatal)
  
  const ctx = document
    .getElementById("barplot_tipo_por_año_estatal")
    .getContext("2d"); //inicio a crear la gráfica

  chart_barplot_tipos_por_año = new Chart(ctx, {
    type: "bar",
    data: dataGraficaTiposPorAño,
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
    
  });//Inicializar la grafica
  const dataGraficaHistorico = generarInsumosHistorico(delito_actual)
  const ctx_hist = document
    .getElementById("lineplot_año_por_tipo_estatal")
    .getContext("2d");
  chart_lineplot_año_por_tipo_estatal = new Chart(ctx_hist, {
    type: "line",
    data: {
      labels: dataGraficaHistorico.map((x)=>{return(x[0])}),
      datasets: [
        {
          data: dataGraficaHistorico.map((x)=>{return(Math.round(parseFloat(x[1])*100000)/100000)}),
          backgroundColor: "rgba(179,142,93,0.8)",
          borderColor: "rgb(9, 86, 70)",
          borderWidth: 1,
          spanGaps: true,
          label: "Tasa de delito por cada mil habitantes (Hidalgo)",
        },
        {
          data: [],
          backgroundColor: "rgb(98, 17, 50)",
          borderColor: "rgba(0, 0, 0, 0.8)",
          borderWidth: 1,
          spanGaps: true,
          label: "Promedio Nacional",
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
  });//Inicializar la grafica

  chart_lineplot_año_por_tipo_estatal.data.datasets[1].data=dataGraficaHistorico.map((x)=>{return(Math.round(parseFloat(x[2])*100000)/100000)})
  chart_lineplot_año_por_tipo_estatal.update();
});

Promise.all([cargandoDataAñoTipo,cargadoDataMesesEstatal]).then(()=>{

    data_estatal_año_tipo=generarInsumosIncidenciaMensual(2026,delito_actual)
    dataGraficaMensual = inicializarDataGraficaMensual(data_estatal_año_tipo)
    const meses = ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre"];

    //Revisar si el primer default tiene datos
    const ctx_meses = document
        .getElementById("barplot_meses")
        .getContext("2d"); //inicio a crear la gráfica
    stackedBar = new Chart(ctx_meses, {
      type: 'bar',
      data: {
      labels: meses,
      datasets: [{
        label: 'Delitos en Hidalgo ('+delito_actual+' 2026)',
        data: data_estatal_año_tipo.map((x)=>{return parseFloat(x[1])}),
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
        },
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
