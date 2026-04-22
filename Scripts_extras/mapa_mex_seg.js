var map = L.map('map_tablero_seguridad',{
});
getGradientColor = function(startColor, endColor, percent){
    // strip the leading # if it's there
    startColor = startColor.replace(/^\s*#|\s*$/g, '');
    endColor = endColor.replace(/^\s*#|\s*$/g, '');

    // convert 3 char codes --> 6, e.g. `E0F` --> `EE00FF`
    if (startColor.length === 3) {
      startColor = startColor.replace(/(.)/g, '$1$1');
    }

    if (endColor.length === 3) {
      endColor = endColor.replace(/(.)/g, '$1$1');
    }

    // get colors
    const startRed = parseInt(startColor.substr(0, 2), 16),
      startGreen = parseInt(startColor.substr(2, 2), 16),
      startBlue = parseInt(startColor.substr(4, 2), 16);

    const endRed = parseInt(endColor.substr(0, 2), 16),
      endGreen = parseInt(endColor.substr(2, 2), 16),
      endBlue = parseInt(endColor.substr(4, 2), 16);

    // calculate new color
    let diffRed = endRed - startRed;
    let diffGreen = endGreen - startGreen;
    let diffBlue = endBlue - startBlue;

    diffRed = ((diffRed * percent) + startRed);
    diffGreen = ((diffGreen * percent) + startGreen);
    diffBlue = ((diffBlue * percent) + startBlue);

    let diffRedStr = diffRed.toString(16).split('.')[0];
    let diffGreenStr = diffGreen.toString(16).split('.')[0];
    let diffBlueStr = diffBlue.toString(16).split('.')[0];

    // ensure 2 digits by color
    if (diffRedStr.length === 1) diffRedStr = '0' + diffRedStr;
    if (diffGreenStr.length === 1) diffGreenStr = '0' + diffGreenStr;
    if (diffBlueStr.length === 1) diffBlueStr = '0' + diffBlueStr;

    return '#' + diffRedStr + diffGreenStr + diffBlueStr;
}
function getColor_h(d) {
    return getGradientColor('#FF0000','#00FF00', d)
}
L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
	minZoom: 4,
	maxZoom: 15,
	attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
	ext: 'png'
}).addTo(map);


function style_ent(feature) {
        return {
            fillColor: getColor_h(feature.properties.Area),
            weight: 5,
            opacity: 0.4,
            color: '#ffffff',
            dashArray: '0',
            fillOpacity: 0.4
        };
}
poligonos_map = L.geoJson(mexico, {
    style: style_ent,
    onEachFeature: onEachFeature,
}).addTo(map)
//map.fitBounds(poligonos_map.getBounds());
var ultimo_seleccionado='Hidalgo'
var capasSeleccionadas = new Set(); 

function style_seleccionado(layer) {
    layer.setStyle({
        weight: 5,
        color: '#000000', 
        fillOpacity: 0.7
    });
    
    // Validamos que la capa tenga un método para traer al frente
    if (layer.bringToFront) {
        layer.bringToFront();
    }
}
function SelectFeature(e) {
    var layer = e.target;
    if (capasSeleccionadas.has(layer)) {
        // Si ya está, lo quitamos (Deseleccionar)
        capasSeleccionadas.delete(layer);
        poligonos_map.resetStyle(layer);
        PopGraficaHistoricoEstatal(layer.feature.properties.NOMGEO,document.getElementById("tipo_dropdown").value||delito_actual)
    } else {
        // Si no está, lo agregamos (Seleccionar)
        capasSeleccionadas.add(layer);
        style_seleccionado(layer);
        PushGraficaHistoricoEstatal(layer.feature.properties.NOMGEO,document.getElementById("tipo_dropdown").value||delito_actual)

    }
}

function resetHighlight(e) {
    var layer = e.target;
    // Solo reseteamos si NO está en nuestra lista de seleccionados
    if (!capasSeleccionadas.has(layer)) {
        poligonos_map.resetStyle(layer);
    }
}

function highlightFeature(e) {
    var layer = e.target;
    // Aplicamos hover visual siempre que no esté seleccionado
    if (!capasSeleccionadas.has(layer)) {
        layer.setStyle({
            weight: 5,
            color: '#999', 
            fillOpacity: 0.6
        });
    }
}
function onEachFeature(feature, layer) {


    // 2. Asignar los eventos normales
    layer.on({
        mouseover: highlightFeature,
        mouseout: resetHighlight,
        click: SelectFeature
    });
}
function limpiarTodasLasSelecciones() {
    poligonos_map.resetStyle();
    capasSeleccionadas.clear();
    seleccionarHidalgo();
    
}
function seleccionarHidalgo(){
    poligonos_map.eachLayer(function(layer) {
    // Verificamos si la propiedad coincide (ajusta 'Entidad' al nombre de tu columna)
    if (layer.feature.properties.NOMGEO === ultimo_seleccionado) {
        capasSeleccionadas.add(layer);
        style_seleccionado(layer);
    }
});
}
function refrescarSeleccionadas() {
    // Recorremos todas las capas del GeoJSON
    poligonos_map.eachLayer(function(layer) {
        if (capasSeleccionadas.has(layer)) {
            style_seleccionado(layer);
        } else {
            poligonos_map.resetStyle(layer);
        }
    });
}
//Marca de Agua
L.Control.Watermark = L.Control.extend({
    onAdd: function(map_h) {
        var img = L.DomUtil.create('img');

        img.src = 'Datos/logo lab.png';
        img.style.width = '14vw';
        img.style.marginBottom='4vh'

        return img;
    },

});

L.control.watermark = function(opts) {
    return new L.Control.Watermark(opts);
}

L.control.watermark({ position: 'bottomleft' }).addTo(map);
