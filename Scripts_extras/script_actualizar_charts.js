///
//Ahora le toca refactor al que renombraremos "ActualizadorGraficasEstatal/Municipal.js"
//Una función para cada gráfica. Una para estatal y otra para municipal.
ActualizarGraficaHistoricoEstatal=function(tipo_de_delito){
  //
  document.getElementById('lineplot_año_por_tipo_estatal').style.backgroundImage='none'
  const historico_actual =generarInsumosHistorico(tipo_de_delito)
  //console.log(historico_actual)
  //actualizamos "data"
  chart_lineplot_año_por_tipo_estatal.destroy();
  const ctx_hist = document
    .getElementById("lineplot_año_por_tipo_estatal")
    .getContext("2d");
  chart_lineplot_año_por_tipo_estatal = new Chart(ctx_hist, {
    type: "line",
    data: {
      labels: historico_actual.map((x)=>{return(x[0])}),
      datasets: [
        {
          data: historico_actual.map((x)=>{return(Math.round(parseFloat(x[1])*100000)/100000)}),
          backgroundColor: "rgba(179,142,93,0.8)",
          borderColor: "rgb(9, 86, 70)",
          borderWidth: 1,
          spanGaps: true,
          label: ["Tasa de delito por cada mil habitantes (Hidalgo)"],
        },
        {
          data: [],
          backgroundColor: "rgb(98, 17, 50)",
          borderColor: "rgba(0, 0, 0, 0.8)",
          borderWidth: 1,
          spanGaps: true,
          label: ["Tasa Media Nacional"],
        },
      ],
    },
    options: {
      locale: "en-EN",
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        chartArea: {
          backgroundColor: "rgba(240, 240, 240, 1)",
        },
        tooltip: {
          callbacks: {
            label: (ctx) => (`${ctx.dataset.label}: ${ctx.raw}`)
          }
        },
      },
      scales: {
        y: {
          ticks: {
        precision:4
      },
          beginAtZero: false,
        },
      },
    },
  });
  chart_lineplot_año_por_tipo_estatal.data.datasets[1].data=historico_actual.map((x)=>{return(Math.round(parseFloat(x[2])*100000)/100000)})
  chart_lineplot_año_por_tipo_estatal.update();
  if(historico_actual.length==1){
      //console.log("Ni siquiera hay datos porque es delito nuevo")
      //"Ni siquiera hay datos porque es delito nuevo"
      const canvas = document.getElementById('lineplot_año_por_tipo_estatal');

      // Configuración de la imagen (siempre visible o condicional)
      canvas.style.backgroundImage = 'url("Datos/few_data.png")';
      canvas.style.backgroundRepeat = 'no-repeat';
      canvas.style.backgroundPosition = 'center';
      canvas.style.backgroundSize = 'contain'; 
      canvas.style.backgroundColor = 'transparent'; // Asegura transparencia
      //document.getElementById('lineplot_año_por_tipo_municipal').style.backgroundImage='url(Datos/no_data.png)';
  }
}
const coloresRandom=[
            "rgb(98,17,50)",
            "rgb(157,36,73)",
            "rgb(112,144,144)",
            "rgb(212,193,156)",
            "rgb(179,142,93)",
            "rgb(29,29,27)",
            "rgb(9, 86, 70)",
          ]
PushGraficaHistoricoEstatal=function(entidad,tipo_de_delito){
  const entidades_actuales=chart_lineplot_año_por_tipo_estatal.data.datasets.map((x)=>x.label)
  const index=entidades_actuales.indexOf("Tasa de delito por cada mil habitantes ("+entidad+')')
  if(index==-1){
    const datasetsActualesLength=chart_lineplot_año_por_tipo_estatal.data.datasets.length
    chart_lineplot_año_por_tipo_estatal.data.datasets.push(structuredClone(chart_lineplot_año_por_tipo_estatal.data.datasets[0]))
    //Datos nuevos
    const historicoEntidad=generarInsumosHistorico(tipo_de_delito=tipo_de_delito,entidad=entidad)
    chart_lineplot_año_por_tipo_estatal.data.datasets[datasetsActualesLength].data=historicoEntidad.map((x)=>{
      return(x[1])
    }).map((x)=>{return(Math.round(parseFloat(x)*100000)/100000)})
    chart_lineplot_año_por_tipo_estatal.data.datasets[datasetsActualesLength].label="Tasa de delito por cada mil habitantes ("+entidad+')'
    chart_lineplot_año_por_tipo_estatal.data.datasets[0].borderColor=coloresRandom[(datasetsActualesLength-1)%coloresRandom.length]
    chart_lineplot_año_por_tipo_estatal.update();
  }
}
PopGraficaHistoricoEstatal=function(entidad,tipo_de_delito){
  const entidades_actuales=chart_lineplot_año_por_tipo_estatal.data.datasets.map((x)=>x.label)
  const index=entidades_actuales.indexOf("Tasa de delito por cada mil habitantes ("+entidad+')')
  if(index>-1){
    chart_lineplot_año_por_tipo_estatal.data.datasets.splice(index, 1)
    chart_lineplot_año_por_tipo_estatal.update();
  }
}
PushGraficaHistoricoMunicipal=function(municipio,tipo_de_delito){
  const entidades_actuales=chart_mun.data.datasets.map((x)=>x.label)
  const index=entidades_actuales.indexOf("Tasa de delito por cada mil habitantes ("+municipio+')')
  if(index==-1){
    const datasetsActualesLength=chart_mun.data.datasets.length
    chart_mun.data.datasets.push(structuredClone(chart_mun.data.datasets[0]))
    //Datos nuevos
    const historicoEntidad=generarInsumosHistoricoMunicipal(tipo_de_delito=tipo_de_delito,entidad=municipio)
    chart_mun.data.datasets[datasetsActualesLength].data=historicoEntidad.map((x)=>{
      return(x[1])
    }).map((x)=>{return(Math.round(parseFloat(x)*100000)/100000)})
    chart_mun.data.datasets[datasetsActualesLength].label="Tasa de delito por cada mil habitantes ("+municipio+')'
    chart_mun.data.datasets[0].borderColor=coloresRandom[(datasetsActualesLength-1)%coloresRandom.length]
    chart_mun.update();
  }
}
PopGraficaHistoricoMunicipal=function(municipio,tipo_de_delito){
  const entidades_actuales=chart_mun.data.datasets.map((x)=>x.label)
  const index=entidades_actuales.indexOf("Tasa de delito por cada mil habitantes ("+municipio+')')
  if(index>-1){
    chart_mun.data.datasets.splice(index, 1)
    chart_mun.update();
  }
}
ActualizarGraficaHistoricoMunicipal=function(tipo_de_delito){
  //
  document.getElementById('lineplot_año_por_tipo_municipal').style.backgroundImage='none'
  const historico_actual_mun = generarInsumosHistoricoMunicipal(
    tipo_de_delito,
    municipio_actual
  );
  
  
  chart_mun.destroy();
  const ctx_hist_mun = document
    .getElementById("lineplot_año_por_tipo_municipal")
    .getContext("2d");
  chart_mun = new Chart(ctx_hist_mun, {
    type: "line",
    data: {
      labels: historico_actual_mun.map((x) => x[0]),
      datasets: [
        {
          data: historico_actual_mun.map((x) => x[1]),
          backgroundColor: "rgba(179,142,93,0.8)",
          borderColor: "rgb(9, 86, 70)",
          borderWidth: 1,
          spanGaps: true,
          label: ["Tasa de delito por cada mil habitantes (Pachuca de Soto)"],
        },
        {
          data: historico_actual_mun.map((x) => x[2]),
          backgroundColor: "rgb(98, 17, 50)",
          borderColor: "rgb(9, 86, 70)",
          borderWidth: 1,
          spanGaps: true,
          label: ["Promedio Estatal"],
        },
      ],
    },
    options: {locale: "en-EN",
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        tooltip: {
          callbacks: {
            label: (ctx) => (`${ctx.dataset.label}: ${ctx.raw}`)
          }
        },

      },
      ticks: {
        precision:4
      },
      scales: {
        y: {
          beginAtZero: false,
        },
      },
    },
  });
  //console.log(historico_actual_mun)
  if(historico_actual_mun.length==1){
      console.log("Ni siquiera hay datos porque es delito nuevo")
      //"Ni siquiera hay datos porque es delito nuevo"
      const canvas = document.getElementById('lineplot_año_por_tipo_municipal');

      // Configuración de la imagen (siempre visible o condicional)
      canvas.style.backgroundImage = 'url("Datos/few_data.png")';
      canvas.style.backgroundRepeat = 'no-repeat';
      canvas.style.backgroundPosition = 'center';
      canvas.style.backgroundSize = 'contain'; 
      canvas.style.backgroundColor = 'transparent'; // Asegura transparencia
      //document.getElementById('lineplot_año_por_tipo_municipal').style.backgroundImage='url(Datos/no_data.png)';
  }
}
ActualizarGraficaIncidenciaAnualEstatal=function(año){
  //
  const los40Actuales = generarInsumosIncidenciaAnual(parseInt(año)); //generamos los valores para la estatal de año
  //console.log(los40Actuales)
  //actualizamos "data"
  const los40Actuales_ordenados_estatal=ordenarPorValores(los40Actuales.map((x)=> {return(x[0])}),los40Actuales.map((x)=> {return(x[1])}))//filtrar valores muy pequeños?
  ////primeros40_ordenados_estatal=ordenarPorValores(primeros40.map((x)=> {return(x[0])}),primeros40.map((x)=> {return(x[1])}))//filtrar valores muy pequeños?
  const dataGraficaTiposPorAño = inicializarDataGraficaTiposPorAño(los40Actuales_ordenados_estatal)

  chart_barplot_tipos_por_año.destroy(); //Destruimos estatal de año
  const ctx = document
    .getElementById("barplot_tipo_por_año_estatal")
    .getContext("2d");
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
        },
      }
    },
    plugins: plugin_actualizar_eleccion_cruzada
  })
  
    
  }
ActualizarGraficaIncidenciaAnualMunicipal=function(año){
  //
  const primeros40_Mun = generarInsumosIncidenciaAnualMunicipal(parseInt(año),municipio_actual); //Aquí todavía falta la elección del municipio. Lo posponemos

  los40Actuales_ordenados_municipal=ordenarPorValores(primeros40_Mun.map((x)=>{return(x[0])}),primeros40_Mun.map((x)=>{return(x[1])}))
  //data_mun.datasets[0].data = los40Actuales_ordenados_municipal.valoresOrdenados; //Por ahora todo lo demás se queda igual que en el default
  //data_mun.labels = los40Actuales_ordenados_municipal.tiposOrdenados.map((x)=>{if(sub_labels_clasificacion[x]){return(x+'...')}else{return(x)}}); //Por ahora todo lo demás se queda igual que en el default
  chart_barplot_mun_tipos_por_año.destroy();
  const ctx_mun = document
    .getElementById("barplot_tipo_por_año_municipal")
    .getContext("2d"); //inicio a crear la gráfica

  chart_barplot_mun_tipos_por_año = new Chart(ctx_mun, {
    type: "bar",
    data: {
    labels: los40Actuales_ordenados_municipal.tiposOrdenados.map((x)=>{if(sub_labels_clasificacion[x]){return(x+'...')}else{return(x)}}),
    datasets: [
      {
        axis: "y",
        label: "Tasa de delito por cada mil habitantes ("+municipio_actual+')',
        data: los40Actuales_ordenados_municipal.valoresOrdenados,
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
}
ActualizarGraficaIncidenciaMensualEstatal=function(valor_tipo,valor_año){
  //
    
   
  //Cuando ocurre el cambio de alguna, la gráfica de meses de municipio 
  const meses_actual_est=generarInsumosIncidenciaMensual(valor_año,valor_tipo)
  //console.log(meses_actual_est)
  stackedBar.destroy();
  document.getElementById('barplot_meses').style.backgroundImage='none'
  //console.log("Nuevos datos de meses: ")
  //console.log(data_meses.datasets[0].data)
  if(meses_actual_est.reduce((partialSum, a) => partialSum + a[1], 0)==0){
    //console.log("era cero")
    document.getElementById('barplot_meses').style.backgroundImage='url(Datos/no_data.png)';
  }

  const ctx_meses = document
    .getElementById("barplot_meses")
    .getContext("2d");
    stackedBar= new Chart(ctx_meses, {
      type: 'bar',
      data: {
      labels: meses_actual_est.map((x)=>{return(x[0])}),
      datasets: [{
        label: 'Delitos en Hidalgo' +'('+ valor_tipo + ' ' + valor_año+')',
        data: meses_actual_est.map((x)=>{return parseFloat(x[1])}),
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
      options: {locale: "en-EN",
          scales: {
              x: {
                  stacked: true
              },
              y: {
                ticks:{precision:0},
                  stacked: true
              }
          },
          maintainAspectRatio:false,
      }
    });

// y estatal va a cambiar.
}
ActualizarGraficaIncidenciaMensualMunicipal=function(valor_tipo,valor_año){
  //
  let arr_area_promesa_actual=[]
  let arr_absoluto_promesa_actual=[]
  let Promesa_Actual_Actualizamos_Area = new Promise((resolve, reject) => {
  //actualizamos el campo área con el ranking
  
  //Esto sería si quisiera ponerles el valor que les corresponde. Quiero el ranking. 
  //replicamos el vector pero en lugar de valor tiene el ranking sobre los valores unicos
  //e.g. [0,0,0,1,2,2,3]-> [1,1,1,2,3,3,4]
  const tasasMunicipiales = generarInsumosColorearMapa(valor_año,valor_tipo).map(
      (x)=>{
        return(
          [x.split(",")[2].replace(/[\r\n"']/g, "").trim(),//municipio
          parseFloat(x.split(",")[4].replace(/[\r\n"']/g, "").trim()),//tasa_mpio
          parseInt(x.split(",")[3].replace(/[\r\n"']/g, "").trim()),//total
        ]
        )
      }
    )
  let valores_unicos = [...new Set(tasasMunicipiales.map((x)=>{return(x[1])}))].sort((a, b) => a - b); // Ordenamos de menor a mayor
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
})
  let meses_actual_mun = generarInsumosIncidenciaMensualMunicipal(
      valor_año,valor_tipo,
      municipio_actual
    );
    stackedBar_meses.destroy();
    //data_meses_mun.datasets[0].data = meses_actual_mun.map((x)=>{return parseFloat((x.split(","))[4].replace(/[\r\n"']/g, "").trim())});
    //console.log("datos mensuales: ")
    //console.log(meses_actual_mun)
    document.getElementById('barplot_meses_mun').style.backgroundImage='none'

    if(meses_actual_mun[1].slice(3,15).reduce((partialSum, a) => partialSum + a, 0)==0){
    //console.log("era cero")
    document.getElementById('barplot_meses_mun').style.backgroundImage='url(Datos/no_data.png)';
    
  }
    //data_meses_mun.datasets[0].label = 'Total de Delitos ('+valor_tipo+' '+valor_año+')'
    //if(data_meses_mun.datasets[0].data.reduce((partialSum, a) => partialSum + a, 0)==0){
      //console.log("era cero")
      //document.getElementById('barplot_meses_mun').style.backgroundImage='url(Datos/no_data.png)';
    //}
    const ctx_meses_mun = document
      .getElementById("barplot_meses_mun")
      .getContext("2d");
      stackedBar_meses = new Chart(ctx_meses_mun, {
        type: 'bar',
        data: {
    labels: meses_actual_mun[0].slice(3,15),
    datasets: [{
      label: "Total de Delitos en "+municipio_actual +'('+ valor_tipo + ' ' + valor_año+')',
      data: meses_actual_mun[1].slice(3,15),
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
                    stacked: true
                },
                y: {
                  ticks:{precision:0},
                    stacked: true
                }
            },
            maintainAspectRatio:false,
        }
      });
}
  //---------------------------------------------------
$("#año_dropdown").change(function () {
  //Cambia el valor del año. //O sea que solo actualizamos las gráficas superiores
  //generamos los nuevos valores para barplot de año
  //actualizamos el objeto data que guarda los valores para el barplot de año
  //destruimos la gráica anterior
  //Creamos una con los datos actualizados
  //console.log("Año actualizado a " + this.value);
   //Creamos una nueva chart estatal año

  ///Corregir.

  ActualizarGraficaIncidenciaAnualEstatal(this.value)

  //Hacemos exactamente lo mismo pero para municipal año
  ActualizarGraficaIncidenciaAnualMunicipal(this.value)

  //El código está bien pero los cambios de display no se reflejan directamente.
  // voy a simular un reforzamiento de nav_active
  click_on_nav(
    document.getElementsByClassName("active_nav_seguridad")[0].innerHTML
  );
});

$("#tipo_dropdown").change(function () {
  //Cambia el valor del tipo. //O sea que solo actualizamos las gráficas inferiores
  //generamos los nuevos valores para lineplot de tipo
  //actualizamos el objeto data que guarda los valores para el lineplot de tipo
  //destruimos la gráica anterior
  //Creamos una con los datos actualizados
  ActualizarGraficaHistoricoEstatal(this.value)
  delito_actual=this.value
  //Repetimos para la municipal
  ActualizarGraficaHistoricoMunicipal(this.value)
  // Forzar actualización .

  click_on_nav(
    document.getElementsByClassName("active_nav_seguridad")[0].innerHTML
  );
  limpiarTodasLasSelecciones();
  limpiarMunicipios()
});

//creamos una promesa de ordenar los municipios según la seleccion. Año y Tipo. 

//cuando se crean las default se crea la primer promesa. 
//se filtra y se obtiene una lista de tamaño 84.
//se "empareja" con los nombres/cves de los municipios
//se ordena de manera que se pueda asignar el rank a cada municipio.
//el orden: Puede ser el número de valores únicos (>2)
//pendiente-. 


//una vez modificado el poligonos_h (campo área), se llama a resetStyle() dentro de la promesa y se cumple. 

$("#año_dropdown, #tipo_dropdown").change(function (){
  
  let valor_tipo = $("#tipo_dropdown").val();  
  let valor_año = $("#año_dropdown").val(); 
  ActualizarGraficaIncidenciaMensualEstatal(valor_tipo,valor_año)

  ///Todo esto se puede simplificar. Solo es cambiar los colores del geojson dependiendo del año y tipo de delito.
  //si cambia cualquiera de los dos, los anteriores lidian con las gráficas. Ahora generamos la promesa

  ActualizarGraficaIncidenciaMensualMunicipal(valor_tipo, valor_año)

  
  // // Forzar actualización .

  click_on_nav(
    document.getElementsByClassName("active_nav_seguridad")[0].innerHTML
  );
  // document.getElementById('scroll_de_barplot_tipos').scrollTop=0
  colorearMapaEntidades(delito_actual=valor_tipo,año_actual=valor_año)
  //limpiarTodasLasSelecciones()
  refrescarSeleccionadas()
})
