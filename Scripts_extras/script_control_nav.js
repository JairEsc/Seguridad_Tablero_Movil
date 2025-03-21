graph_estatal_año=document.getElementById("barplot_tipo_por_año_estatal")
graph_estatal_tipo=document.getElementById("lineplot_año_por_tipo_estatal")
graph_estatal_meses=document.getElementById("barplot_meses")
graph_municipal_año=document.getElementById("barplot_tipo_por_año_municipal")
graph_municipal_tipo=document.getElementById("lineplot_año_por_tipo_municipal")
graph_municipal_meses=document.getElementById("barplot_meses_mun")
click_on_nav=function(nav_clickeada){
    document.getElementById('scroll_de_barplot_tipos').scrollTop=0
    //se clickeó alguna
    navs=document.getElementsByClassName("nav_seguridad")
    maps=document.getElementsByClassName("mapa_web_seguridad")
    navs[0].className=navs[0].className.replace(" active_nav_seguridad", "")
    navs[1].className=navs[1].className.replace(" active_nav_seguridad", "")
    
    //Que cuando haga el cambio se alterne el display de los canvas. 
    if(nav_clickeada=='Estatal'){
        
        navs[0].className+=' active_nav_seguridad'
        maps[0].style.display='block'
        maps[1].style.display='none'

        let interval = setInterval(() => {
            if (maps[0].offsetWidth > 0 && maps[0].offsetHeight > 0) {
                map.fitBounds(poligonos_map_h.getBounds());
                clearInterval(interval);
            }
        }, 50); // Intenta cada 50ms
        graph_estatal_año.style.display='block'
        graph_municipal_año.style.display='none'
        graph_estatal_tipo.style.display='block'
        graph_municipal_tipo.style.display='none'
        graph_estatal_meses.style.display='block'
        graph_municipal_meses.style.display='none'
    }
    else{
        navs[1].className+=' active_nav_seguridad'
        maps[0].style.display='none'
        maps[1].style.display='block'
        let interval = setInterval(() => {
            if (maps[1].offsetWidth > 0 && maps[1].offsetHeight > 0) {
                map_h.fitBounds(poligonos_map_h.getBounds());
                clearInterval(interval);
            }
        }, 50); 
        graph_estatal_año.style.display='none'
        graph_municipal_año.style.display='block'
        graph_estatal_tipo.style.display='none'
        graph_municipal_tipo.style.display='block'
        graph_estatal_meses.style.display='none'
        graph_municipal_meses.style.display='block'

    }
    window.dispatchEvent(new Event("resize"));
}
