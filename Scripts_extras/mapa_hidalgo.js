let municipio_actual = 'Pachuca de Soto';

var map_h = L.map('map_tablero_seguridad_hidalgo',{
    maxBoundsViscosity: 0.8
});
map_h.createPane('municipios'); // Pane normal
map_h.createPane('municipioActual'); // Pane para el municipio seleccionado

// Asignar prioridad: municipioActual estará arriba de municipios
map_h.getPane('municipios').style.zIndex = 400;
map_h.getPane('municipioActual').style.zIndex = 500;


municipios=["Acatlán","Acaxochitlán","Actopan","Agua Blanca de Iturbide","Ajacuba","Alfajayucan","Almoloya","Apan","Atitalaquia","Atlapexco","Atotonilco de Tula","Atotonilco el Grande","Calnali","Cardonal","Chapantongo","Chapulhuacán","Chilcuautla","Cuautepec de Hinojosa","El Arenal","Eloxochitlán","Emiliano Zapata","Epazoyucan","Francisco I. Madero","Huasca de Ocampo","Huautla","Huazalingo","Huehuetla","Huejutla de Reyes","Huichapan","Ixmiquilpan","Jacala de Ledezma","Jaltocán","Juárez Hidalgo","La Misión","Lolotla","Metepec","Metztitlán","Mineral de la Reforma","Mineral del Chico","Mineral del Monte","Mixquiahuala de Juárez","Molango de Escamilla","Nicolás Flores","Nopala de Villagrán","Omitlán de Juárez","Pachuca de Soto","Pacula","Pisaflores","Progreso de Obregón","San Agustín Metzquititlán","San Agustín Tlaxiaca","San Bartolo Tutotepec","San Felipe Orizatlán","San Salvador","Santiago de Anaya","Santiago Tulantepec de Lugo Guerrero","Singuilucan","Tasquillo","Tecozautla","Tenango de Doria","Tepeapulco","Tepehuacán de Guerrero","Tepeji del Río de Ocampo","Tepetitlán","Tetepango","Tezontepec de Aldama","Tianguistengo","Tizayuca","Tlahuelilpan","Tlahuiltepa","Tlanalapa","Tlanchinol","Tlaxcoapan","Tolcayuca","Tula de Allende","Tulancingo de Bravo","Villa de Tezontepec","Xochiatipan","Xochicoatlán","Yahualica","Zacualtipán de Ángeles","Zapotlán de Juárez","Zempoala","Zimapán"]
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

//console.log(getGradientColor('#FF0000', '#00FF00', 48/84))
L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
	minZoom: 4,
	maxZoom: 15,
	attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
	ext: 'png'
}).addTo(map_h);
function style_ent_h(feature) {

    return {

        fillColor: getColor_h(parseFloat(feature.properties.Area)),//cambia a area

        opacity: 1,

        color: feature.properties.NOM_MUN==municipio_actual?"#667":'white',

        dashArray: feature.properties.NOM_MUN==municipio_actual?'0':'5',

        fillOpacity: feature.properties.NOM_MUN==municipio_actual?0.4:0.4,

        pane: feature.properties.NOM_MUN == municipio_actual ? 'municipioActual' : 'municipios'

    };

}
function getColor_h(d) {
    return getGradientColor('#FF0000','#00FF00', d)
}//color basado en gradiente
poligonos_map_h = L.geoJson(hidalgo, {
    style: style_ent_h, // Usamos tu función original de gradientes
    onEachFeature: onEachFeature_h,
}).addTo(map_h);
var municipiosSeleccionados = new Set(); 

function style_seleccionado_h(layer) {
    layer.setStyle({
        weight: 4,
        color: '#000000', // Borde negro para resaltar
        fillOpacity: 0.7,
        dashArray: '0' 
    });
    if (layer.bringToFront) layer.bringToFront();
}

function SelectFeature_h(e) {
    var layer = e.target;
    var nombreMun = layer.feature.properties.NOM_MUN;
    var delito = document.getElementById("tipo_dropdown").value || delito_actual;

    if (municipiosSeleccionados.has(layer)) {
        municipiosSeleccionados.delete(layer);
        poligonos_map_h.resetStyle(layer);
        PopGraficaHistoricoMunicipal(nombreMun, delito);
    } else {
        municipiosSeleccionados.add(layer);
        style_seleccionado_h(layer);
        PushGraficaHistoricoMunicipal(nombreMun, delito);
    }

    if (typeof info !== 'undefined') info.update(layer.feature.properties);
    municipio_actual=layer.feature.properties.NOM_MUN
    ActualizarGraficaIncidenciaAnualMunicipal(año=document.getElementById("año_dropdown").value,municipio_actual)
    //force_click_on_nav();
    //ActualizarGraficaIncidenciaMens(año=document.getElementById("año_dropdown").value,municipio_actual)
    ActualizarSoloGraficaIncidenciaMensualMunicipal(valor_tipo=document.getElementById("tipo_dropdown").value,año=document.getElementById("año_dropdown").value,municipio_actual)
    force_click_on_nav();
}

function highlightFeature_h(e) {
    var layer = e.target;
    // Solo mostramos hover si no está ya seleccionado
    if (!municipiosSeleccionados.has(layer)) {
        layer.setStyle({
            weight: 3,
            color: '#666',
            fillOpacity: 0.5
        });
    }
}

function resetHighlight_h(e) {
    var layer = e.target;
    if (!municipiosSeleccionados.has(layer)) {
        poligonos_map_h.resetStyle(layer);
    }
}

function onEachFeature_h(feature, layer) {
    // Tooltip dinámico
    layer.bindTooltip('<b>Municipio:</b> ' + feature.properties.NOM_MUN + '<br>' +
                      '<b>Dato:</b> ' + feature.properties.Area);

    layer.on({
        mouseover: highlightFeature_h,
        mouseout: resetHighlight_h,
        click: SelectFeature_h
    });
}

function seleccionarMunicipioDefault(nombre) {
    poligonos_map_h.eachLayer(function(layer) {
        if (layer.feature.properties.NOM_MUN === nombre) {
            municipiosSeleccionados.add(layer);
            style_seleccionado_h(layer);
            // Opcional: Push inicial a la gráfica
            // PushGraficaHistoricoMunicipal(nombre, delito_actual);
        }
    });
}

function limpiarMunicipios() {
    poligonos_map_h.resetStyle();
    municipiosSeleccionados.clear();
    // Volvemos a seleccionar el default si es necesario
    seleccionarMunicipioDefault('Pachuca de Soto');
    info.update(); 
}
function refrescarSeleccionMunicipios() {
    poligonos_map_h.eachLayer(function(layer) {
        if (municipiosSeleccionados.has(layer)) {
            style_seleccionado_h(layer);
        } else {
            poligonos_map_h.resetStyle(layer);
        }
    });
}

var info = L.control();

info.onAdd = function (map_h) {
    this._div = L.DomUtil.create('div', 'info_tablero_seg'); // create a div with a class "info"
    this.update();
    return this._div;
};

// method that we will use to update the control based on feature properties passed
info.update = function (props) {
    this._div.innerHTML = '<h1 style="font-size:large">'+(props? props.NOM_MUN: 'Pachuca de Soto')+'</h1>'+'<h4>'+'Municipio Seleccionado'+'</h4>' 
};

info.addTo(map_h);



var controlSearch_h = new L.Control.Search({
    position:'topleft',		
    layer: poligonos_map_h,
    initial: false,
    zoom: 12,
    marker: false,
    propertyName: 'NOM_MUN',
});

map_h.addControl(controlSearch_h);

var legend_h = L.control({position: 'bottomright'});

legend_h.onAdd = function (map) {

    var div = L.DomUtil.create("div", "info_tablero_seg legend legend_seguridad"),
      colors = ["#00FF00", "#7FFF00", "#FFFF00", "#FFBF00", "#FF4000", "#FF0000"]; // Verde → Rojo

    // Crear el gradiente
    var gradient = "linear-gradient(to right, " + colors.join(", ") + ")";

    // Agregar el título y el gradiente
    div.innerHTML =
    '<strong>Tasa de delitos por cada mil habitantes</strong><br>' +
    '<div style="height: 10px; background: ' + gradient + ';"></div>';

    // Agregar los valores de referencia debajo del gradiente
    
    return div;
};

legend_h.addTo(map_h);


//Marca de Agua
L.Control.Watermark = L.Control.extend({
    onAdd: function(map_h) {
        var img = L.DomUtil.create('img');

        img.src = 'Datos/logo lab.png';
        img.style.width = '20vw';
        img.style.marginBottom = '4vh';

        return img;
    },

    onRemove: function(map) {
        // Nothing to do here
    }
});

L.control.watermark = function(opts) {
    return new L.Control.Watermark(opts);
}

L.control.watermark({ position: 'bottomleft' }).addTo(map_h);

