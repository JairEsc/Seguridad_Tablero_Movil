//Recibe la actualización del Nav Visible

//Hay que tener cuidado con la selección porque están por orden alfabético los municipios.
//fetch de los datos necesarios

let data_mun;
let data_meses_mun;
//Vamos a hacer un primera  llamada a los datos para alimentar a las gráficas por default.

LargeCsvCargado.then(() => {//Barra horizontal de tipos por año.
                              //Historico
  //Aquí alimentamos las gráficas por default. Y de paso nos aseguramos que los csv ya se leyeron.
  ///
  let primeros40_Mun = generarInsumosIncidenciaAnualMunicipal(2026, 'Pachuca de Soto');
  //console.log("Primeros 40 valores:", primeros40);
  primeros40_Mun_ordenados_estatal=ordenarPorValores(primeros40_Mun.map((x)=>{return(x[0])}),primeros40_Mun.map((x)=>{return(x[1])}))//filtrar valores muy pequeños?

  ///grafica de prueba
  const ctx_mun = document
    .getElementById("barplot_tipo_por_año_municipal")
    .getContext("2d"); //inicio a crear la gráfica

  chart_barplot_mun_tipos_por_año = new Chart(ctx_mun, {
    type: "bar",
    data: {
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
      },
    
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
  const primer_historico_mun = generarInsumosHistoricoMunicipal('Aborto','Pachuca de Soto');
  //console.log("Primeros historico:", primer_historico);
  const ctx_hist_mun = document
    .getElementById("lineplot_año_por_tipo_municipal")
    .getContext("2d");
  chart_mun = new Chart(ctx_hist_mun, {
    type: "line",
    data: {
      labels: primer_historico_mun.map((x) => x[0]), 
      datasets: [
        {
          data: primer_historico_mun.map((x) => x[1]),
          backgroundColor: "rgba(179,142,93,0.8)",
          borderColor: "rgb(9, 86, 70)",
          borderWidth: 1,
          spanGaps: true,
          label: ["Tasa de delito por cada mil habitantes ("+municipio_actual+')'],
        },
        {
          data: primer_historico_mun.map((x) => x[2]),
          backgroundColor: "rgb(98, 17, 50)",
          borderColor: "rgb(9, 86, 70)",
          borderWidth: 1,
          spanGaps: true,
          label: ["Promedio Estatal"],
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

      },
      scales: {
        y: {
          beginAtZero: false,
        },
      },
    },
  });

  var año_sel_promesa = 2026;
  var tipo_sel_promesa = 'Aborto';
  ///Hasta aquí ya se crearon las gráficas por default del municipio pachuca (o pacula tal vez)
  let arr_area_promesa = [];
  let arr_absoluto_promesa = [];
  let Promesa_Actualizamos_Geojson = new Promise((resolve, reject) => {
    //actualizamos el campo área con el ranking
    tasasMunicipiales = generarInsumosColorearMapa(año_sel_promesa,tipo_sel_promesa).map(
      (x)=>{
        return(
          [x.split(",")[2].replace(/[\r\n"']/g, "").trim(),//municipio
          parseFloat(x.split(",")[4].replace(/[\r\n"']/g, "").trim()),//tasa_mpio
          parseInt(x.split(",")[3].replace(/[\r\n"']/g, "").trim()),//total
        ]
        )
      }
    )

    //sé que están en el orden del csv.
    //que es el orden del array municipios.
    /*poligonos_map_h.eachLayer((layer) => {
      layer.feature.properties.Area = arr_area_promesa[municipios.indexOf(layer.feature.properties.NOM_MUN)];
    });*/
    //Esto sería si quisiera ponerles el valor que les corresponde. Quiero el ranking.
    //replicamos el vector pero en lugar de valor tiene el ranking sobre los valores unicos
    //e.g. [0,0,0,1,2,2,3]-> [1,1,1,2,3,3,4]

    let valores_unicos = [...new Set(tasasMunicipiales.map((x)=>{return(x[1])}))].sort((a, b) => a - b); // Ordenamos de menor a mayor
    let ranking_map = new Map(
      valores_unicos.map((valor, index) => [valor, index + 1])
    ); // Asignamos ranking
    // Asignamos el ranking a cada municipio en Leaflet
    poligonos_map_h.eachLayer((layer) => {
      const valoresActualizables=actualizarPropiedadesGeojson(municipio=layer.feature.properties.NOM_MUN, 
        tasasMunicipiales.map((x)=>x[0]),
        tasasMunicipiales.map((x)=>x[1]),
        tasasMunicipiales.map((x)=>x[2]), ranking_map=ranking_map)
        //console.log(valoresActualizables)//Ranking, tasa, total

      layer.feature.properties.Area =
        (ranking_map.get(valores_unicos[valores_unicos.length - 1])+1-valoresActualizables[0])/ranking_map.get(valores_unicos[valores_unicos.length - 1])//
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
    });
    resolve();
  });
  Promesa_Actualizamos_Geojson.then(() => {
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

VeryLargeCsvCargado.then(()=>{//Gráfico mensual
  ///Vamos a crear la función por meses. Asume selección de año, municipio y delito.

  //Referenciamos a los valores de la selección. 
  //Aqui ya sabemos que es municipio 45
  //Año 2024(10)
  //Delito 0
  datos_año_mun_delito=generarInsumosIncidenciaMensualMunicipal(2026,'Aborto','Pachuca de Soto')

  //console.log(datos_año_mun_delito.map((x)=>{return parseFloat((x.split(","))[4].replace(/[\r\n"']/g, "").trim())}))
  //revisa si los datos son constantes. 
  if(datos_año_mun_delito[1].slice(3,15).reduce((partialSum, a) => partialSum + a, 0)==0){
    //console.log("era cero")
    document.getElementById('barplot_meses_mun').style.backgroundImage='url(Datos/no_data.png)';
  }
  data_meses_mun = {
    labels: datos_año_mun_delito[0].slice(3,15),
    datasets: [{
      label: "Total de Delitos (Aborto 2026)",
      data: datos_año_mun_delito[1].slice(3,15),
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
  seleccionarMunicipioDefault('Pachuca de Soto')
})