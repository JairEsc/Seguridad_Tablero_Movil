var map = L.map('map_tablero_seguridad',{
    maxBoundsViscosity: 0.8
});
function getColor(d) {
    return d==1? "#a50026":
    d==2? "#b50f26":
    d==3? "#c51f27":
    d==4? "#d52e27":
    d==5? "#df422f":
    d==6? "#e95538":
    d==7? "#f26941":
    d==8? "#f67e4b":
    d==9? "#f99354":
    d==10? "#fca85e":
    d==11? "#fdb96a":
    d==12? "#fec978":
    d==13? "#feda86":
    d==14? "#fee695":
    d==15? "#fff0a6":
    d==16? "#fffab7":
    d==17? "#f9fcb7":
    d==18? "#edf7a6":
    d==19? "#e0f295":
    d==20? "#d2ec87":
    d==21? "#c2e57c":
    d==22? "#b2de71":
    d==23? "#a0d669":
    d==24? "#8bcd67":
    d==25? "#77c465":
    d==26? "#61bb62":
    d==27? "#49af5c":
    d==28? "#30a356":
    d==29? "#19964f":
    d==30? "#118747":
    d==31? "#08773f":
    d==32? "#006837":'#bfbfbf'
}
L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
	minZoom: 4,
	maxZoom: 15,
	attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
	ext: 'png'
}).addTo(map);


function style_ent(feature) {
    
        return {
            fillColor: getColor(feature.properties.CVEGEO),
            weight: 5,
            opacity: 1,
            color: '#666',
            dashArray: '0',
            fillOpacity: 0.7
        };
    
    
}
poligonos_map = L.geoJson(mexico, {
    style: style_ent,
    onEachFeature: onEachFeature,
}).addTo(map)
map.fitBounds(poligonos_map.getBounds());
var ultimo_seleccionado='Hidalgo'

function highlightFeature(e) {
    var layer = e.target;

    layer.setStyle({
        weight: 5,
        color: '#666',
        fillOpacity: 0.7
    });

    layer.bringToFront();
}

function resetHighlight(e) {
    poligonos_map.resetStyle();
}
function onEachFeature(feature, layer) {
    layer.on({
        mouseover: highlightFeature,
        mouseout: resetHighlight,
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

    onRemove: function(map) {
        // Nothing to do here
    }
});

L.control.watermark = function(opts) {
    return new L.Control.Watermark(opts);
}

L.control.watermark({ position: 'bottomleft' }).addTo(map);

/*
var info_nacional = L.control();

info_nacional.onAdd = function (map) {
    this._div = L.DomUtil.create('div', 'info_tablero_seg'); // create a div with a class "info"
    this.update();
    return this._div;
};

// method that we will use to update the control based on feature properties passed
info_nacional.update = function (props) {
    this._div.innerHTML = '<h1>'+'Estado de Hidalgo'+'</h1>'+'<h4>'+'Estado Seleccionado'+'</h4>' 
};

info_nacional.addTo(map);



var controlSearch = new L.Control.Search({
    position:'bottomleft',		
    layer: poligonos_map,
    initial: false,
    zoom: 12,
    marker: false,
    propertyName: 'NOM_MUN',
});

map.addControl(controlSearch);

var legend_nac = L.control({position: 'bottomright'});

legend_nac.onAdd = function (map) {

    var div_nac = L.DomUtil.create("div", "info_tablero_seg legend legend_seguridad"),
      colors = ["#00FF00", "#7FFF00", "#FFFF00", "#FFBF00", "#FF4000", "#FF0000"]; // Verde → Rojo

    // Crear el gradiente
    var gradient_nac = "linear-gradient(to right, " + colors.join(", ") + ")";

    // Agregar el título y el gradiente
    div_nac.innerHTML =
    '<strong>Tasa de delitos por cada mil habitantes</strong><br>' +
    '<div style="width: 10vw; height: 10px; background: ' + gradient_nac + ';"></div>';

    // Agregar los valores de referencia debajo del gradiente
    
    return div_nac;
};

legend_nac.addTo(map);
*/