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

/*Alimentar dropdown de los tipos de delitos */
function alimentarDropdownTiposDelito(tipos_de_delitos_array){
  var optns = document.getElementById("tipo_dropdown");//Alimentamos el dropdown de delitos
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
}
/* ------------------------------ */

function inicializarDataGraficaTiposPorAño(primeros40_ordenados_estatal){
    return (
   {
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
  })
}
function inicializarDataGraficaMensual(data_estatal_año_tipo){
    const meses = ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre"];

    return (
   {
      labels: meses,
      datasets: [{
        label: 'Delitos en Hidalgo (Aborto 2025)',
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
    })
}



ordenarPorValores=function(tipos_de_delito, valores) {
  let ordenado = valores.map((v, i) => ({ valor: v, delito: tipos_de_delito[i] }))
                        .sort((a, b) =>  b.valor-a.valor );

  return {
      valoresOrdenados: ordenado.map(obj => obj.valor),
      tiposOrdenados: ordenado.map(obj => obj.delito)
  };
}

