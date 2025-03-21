//---------------------------------------------------
$("#año_dropdown").change(function () {
  //Cambia el valor del año. //O sea que solo actualizamos las gráficas superiores
  //generamos los nuevos valores para barplot de año
  //actualizamos el objeto data que guarda los valores para el barplot de año
  //destruimos la gráica anterior
  //Creamos una con los datos actualizados
  console.log("Año actualizado a " + this.value);
  let los40Actuales = generate_values_Año(parseInt(this.value)); //generamos los valores para la estatal de año
  //actualizamos "data"
  los40Actuales_ordenados_estatal=ordenarPorValores(tipos_de_delito,los40Actuales.map((x)=> {return(x<0.0001?0:x)}))

  data.datasets[0].data = los40Actuales_ordenados_estatal.valoresOrdenados; //Por ahora todo lo demás se queda igual que en el default
  data.labels = los40Actuales_ordenados_estatal.tiposOrdenados.map((x)=>{if(sub_labels_clasificacion[x]){return(x+'...')}else{return(x)}}); //Por ahora todo lo demás se queda igual que en el default
  chart_barplot_tipos_por_año.destroy(); //Destruimos estatal de año
  const ctx = document
    .getElementById("barplot_tipo_por_año_estatal")
    .getContext("2d");
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
  }); //Creamos una nueva chart estatal año

  ///Corregir.



  //Hacemos exactamente lo mismo pero para municipal año
  let primeros40_Mun = generate_values_Mun_Año(
    parseInt(this.value),
    municipio_actual
  ); //Aquí todavía falta la elección del municipio. Lo posponemos
  //console.log(primeros40_Mun)//






  los40Actuales_ordenados_municipal=ordenarPorValores(tipos_de_delito,primeros40_Mun.map((x)=> {return(x<0.0001?0:x)}))

  data_mun.datasets[0].data = los40Actuales_ordenados_municipal.valoresOrdenados; //Por ahora todo lo demás se queda igual que en el default
  data_mun.labels = los40Actuales_ordenados_municipal.tiposOrdenados.map((x)=>{if(sub_labels_clasificacion[x]){return(x+'...')}else{return(x)}}); //Por ahora todo lo demás se queda igual que en el default
  chart_barplot_mun_tipos_por_año.destroy();
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
  });

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
  let historico_actual = generate_values_Tipo(
    tipos_de_delito.indexOf(this.value) + 1
  ); //generamos los valores para la estatal de tipo
  //actualizamos "data"
  chart.destroy();
  const ctx_hist = document
    .getElementById("lineplot_año_por_tipo_estatal")
    .getContext("2d");
  chart = new Chart(ctx_hist, {
    type: "line",
    data: {
      labels: Array.from({ length: 10 }, (_, i) => 2015 + i),
      datasets: [
        {
          data: historico_actual,
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
  chart.data.datasets[1].data=generate_values_tasa_media(tipos_de_delito.indexOf(this.value)).map((x)=>{return(Math.round(parseFloat(x.split(",")[2])*100000)/100000)})
  chart.update();


  //Repetimos para la municipal
  let historico_actual_mun = generate_values_Mun_Tipo(
    tipos_de_delito.indexOf(this.value) + 1,
    municipio_actual
  );
  chart_mun.destroy();
  const ctx_hist_mun = document
    .getElementById("lineplot_año_por_tipo_municipal")
    .getContext("2d");
  chart_mun = new Chart(ctx_hist_mun, {
    type: "line",
    data: {
      labels: Array.from({ length: 10 }, (_, i) => 2015 + i),
      datasets: [
        {
          data: historico_actual_mun,
          backgroundColor: "rgba(179,142,93,0.8)",
          borderColor: "rgb(9, 86, 70)",
          borderWidth: 1,
          spanGaps: true,
          label: ["Tasa de delito por cada mil habitantes"],
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
        chartArea: {
          backgroundColor: "rgba(240, 240, 240, 1)", // Cambia este color a lo que desees
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
  // Forzar actualización .

  click_on_nav(
    document.getElementsByClassName("active_nav_seguridad")[0].innerHTML
  );
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
  
  
//si cambia cualquiera de los dos, los anteriores lidian con las gráficas. Ahora generamos la promesa
let arr_area_promesa_actual=[]
let arr_absoluto_promesa_actual=[]
let Promesa_Actual_Actualizamos_Area = new Promise((resolve, reject) => {
  //actualizamos el campo área con el ranking
  let valor_tipo = $("#tipo_dropdown").val();  
  let valor_año = $("#año_dropdown").val();  
  //slice a año seleccionado
  const inicio_promesa = 40 * 84*(valor_año - 2015) +1;//notar que no incluye al header.
  const fin_promesa = inicio_promesa + 40*84;
  
  //ciclo 84 con tamaño de paso 40
  data_año_seleccionado=data_municipal_fetched_and_splitted.slice(inicio_promesa, fin_promesa)
  for (let ww = 0; ww < 84; ww++) {
    arr_area_promesa_actual.push(
          parseFloat(
            data_año_seleccionado[ww*40+tipos_de_delito.indexOf(valor_tipo)]
              .split(",")[4]
              .replace(/[\r\n"']/g, "")
              .trim()
          )
    );
    arr_absoluto_promesa_actual.push(
          parseFloat(
            data_año_seleccionado[ww*40+tipos_de_delito.indexOf(valor_tipo)]
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
  
  let valores_unicos = [...new Set(arr_area_promesa_actual)].sort((a, b) => a - b); // Ordenamos de menor a mayor
  let ranking_map = new Map(valores_unicos.map((valor, index) => [valor, index + 1])); // Asignamos ranking
  // Asignamos el ranking a cada municipio en Leaflet
  poligonos_map_h.eachLayer((layer) => {
    let area_valor = arr_area_promesa_actual[municipios.indexOf(layer.feature.properties.NOM_MUN)];
    layer.feature.properties.Area = (ranking_map.get(valores_unicos[valores_unicos.length - 1])+1-ranking_map.get(area_valor))/(ranking_map.get(valores_unicos[valores_unicos.length - 1])); // Asignamos ranking en lugar del valor
    layer.feature.properties.COV_ID =ranking_map.get(valores_unicos[valores_unicos.length - 1])+1- (ranking_map.get(area_valor)); // Asignamos ranking en lugar del valor
    layer.feature.properties.COV_ =Math.round(parseFloat(100000*arr_area_promesa_actual[municipios.indexOf(layer.feature.properties.NOM_MUN)]))/100000; // Asignamos ranking en lugar del valor
    layer.feature.properties.PERIMETER =arr_absoluto_promesa_actual[municipios.indexOf(layer.feature.properties.NOM_MUN)]
  });




  resolve()
    
});
Promesa_Actual_Actualizamos_Area.then(()=>{
  poligonos_map_h.eachLayer((layer)=>{
    layer.unbindTooltip(); // Elimina tooltip anterior
    layer.bindTooltip(
      'Municipio: ' + layer.feature.properties.NOM_MUN + '<br>' +
      'Ranking: ' + layer.feature.properties.COV_ID + '<br>' +
      'Tasa de delitos por cada mil: ' + layer.feature.properties.COV_ + '<br>'
    )});
  poligonos_map_h.resetStyle()
})

let valor_tipo = $("#tipo_dropdown").val();  
let valor_año = $("#año_dropdown").val();  
//Cuando ocurre el cambio de alguna, la gráfica de meses de municipio 
let meses_actual_est = generate_values_meses_estatal(
  valor_año,
  tipos_de_delito.indexOf(valor_tipo)
);
stackedBar.destroy();
document.getElementById('barplot_meses').style.backgroundImage='none'
data_meses.datasets[0].data = meses_actual_est.map((x)=>{return parseFloat((x.split(","))[3].replace(/[\r\n"']/g, "").trim())});
data_meses.datasets[0].label = 'Delitos en Hidalgo ('+valor_tipo+' '+valor_año+')'
console.log("Nuevos datos de meses: ")
console.log(data_meses.datasets[0].data)
if(data_meses.datasets[0].data.reduce((partialSum, a) => partialSum + a, 0)==0){
  console.log("era cero")
  document.getElementById('barplot_meses').style.backgroundImage='url(Datos/no_data.png)';
}

const ctx_meses = document
  .getElementById("barplot_meses")
  .getContext("2d");
  stackedBar= new Chart(ctx_meses, {
    type: 'bar',
    data: data_meses,
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


let meses_actual_mun = generate_values_Año_Mun_Tipo(
  valor_año,
  municipio_actual,tipos_de_delito.indexOf(valor_tipo)
);
stackedBar_meses.destroy();
data_meses_mun.datasets[0].data = meses_actual_mun.map((x)=>{return parseFloat((x.split(","))[4].replace(/[\r\n"']/g, "").trim())});
console.log("datos mensuales: ")
console.log(meses_actual_mun)
document.getElementById('barplot_meses_mun').style.backgroundImage='none'

data_meses_mun.datasets[0].label = 'Total de Delitos ('+valor_tipo+' '+valor_año+')'
if(data_meses_mun.datasets[0].data.reduce((partialSum, a) => partialSum + a, 0)==0){
  console.log("era cero")
  document.getElementById('barplot_meses_mun').style.backgroundImage='url(Datos/no_data.png)';
}
const ctx_meses_mun = document
  .getElementById("barplot_meses_mun")
  .getContext("2d");
  stackedBar_meses = new Chart(ctx_meses_mun, {
    type: 'bar',
    data: data_meses_mun,
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
// Forzar actualización .

click_on_nav(
  document.getElementsByClassName("active_nav_seguridad")[0].innerHTML
);
document.getElementById('scroll_de_barplot_tipos').scrollTop=0
})
