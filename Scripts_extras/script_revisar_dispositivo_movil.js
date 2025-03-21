cambiar_tamaños=function(id,height_new,width_new='100vw'){
    elemento_id=document.getElementById(id)
    elemento_id.style.height=height_new
    elemento_id.style.width=width_new
}
cambiar_display=function(id,display_new){
    elemento_id=document.getElementById(id)
    elemento_id.style.display=display_new
}
if(Math.min(window.screen.width, window.screen.height) < 768
){
    console.log("F")
    //procedemos a cambiar styles
    ids_cambiar_tamaño=[
        'contenedor_global',//'100vw',50
        'primera_mitad',//'100vw',50
        'map_tablero_seguridad',//45,'100vw'
        'map_tablero_seguridad_hidalgo',//45,'100vw'
        'segunda_mitad',//'100vw',50
        'contenedor_historico',//50,'100vw'
        'contenedor_historico_titulo_select',//2,'100vw'
        'contenedor_historico_canvas',//48,'100vw'
        'contenedor_barplot_horizontal',//50,'100vw'
        'contenedor_barplot_horizontal_titulo_select',//2,'100vw'
        'scroll_de_barplot_tipos',//50,'100vw'
        'contenedor_scroll_canvas',//180,'100vw'
        'contenedor_meses_titulo_canvas',//50,'100vw'
        'meses_titulo',//2,'100vw'
        'contenedor_meses_canvas',//48,'100vw'
    ]
    //aplicamos cambio de tamaño
    //cambiar_tamaños(ids_cambiar_tamaño[0],'100vw',50)
    cambiar_tamaños('primera_mitad','50vh','100vw')
    //falta el header (nav) de 5vh
    cambiar_tamaños('map_tablero_seguridad','45vh','100vw')
    cambiar_tamaños('map_tablero_seguridad_hidalgo','45vh','100vw')
    cambiar_tamaños('segunda_mitad','45vh','100vw')
    cambiar_tamaños('contenedor_historico','45vh','100vw')
    cambiar_tamaños('contenedor_historico_titulo_select','2vh','100vw')
    //Cambiar el tamaño de la letra a más pequeña 
    historico_titulo_select=document.getElementById('contenedor_historico_titulo_select')
    historico_titulo_select.children[0].style.fontSize='small'
    historico_titulo_select.children[0].style.maxWidth='70vw'
    historico_titulo_select.children[1].style.width='30vw'
    historico_titulo_select.children[1].style.minWidth='10vw'
    historico_titulo_select.children[1].style.maxWidth='30vw'


    //overflow x para la segunda mitad
    segunda_mitad=document.getElementById('segunda_mitad')
    segunda_mitad.style.overflowX='auto'

    //Cambiar el tamaño de la letra a más pequeña (barplot horizontal)
    barplot_horizontal_titulo_select=document.getElementById('contenedor_barplot_horizontal_titulo_select')
    barplot_horizontal_titulo_select.children[0].style.fontSize='small'
    barplot_horizontal_titulo_select.children[0].style.maxWidth='80vw'
    barplot_horizontal_titulo_select.children[1].style.width='20vw'
    barplot_horizontal_titulo_select.children[1].style.minWidth='10vw'
    barplot_horizontal_titulo_select.children[1].style.maxWidth='20vw'


    //cambiar el tamaño del titulo y del select. 
    cambiar_tamaños('contenedor_historico_canvas','43vh','100vw')
    cambiar_tamaños('contenedor_barplot_horizontal','43vh','100vw')
    cambiar_tamaños('contenedor_barplot_horizontal_titulo_select','2vh','100vw')
    cambiar_tamaños(ids_cambiar_tamaño[10],'43vh','100vw')
    cambiar_tamaños(ids_cambiar_tamaño[11],'180vh','100vw')
    cambiar_tamaños(ids_cambiar_tamaño[12],'43vh','100vw')
    cambiar_tamaños(ids_cambiar_tamaño[13],'2vh','100vw')
    cambiar_tamaños(ids_cambiar_tamaño[14],'43vh','100vw')


    //Le quitamos algunos flex
    cambiar_display(ids_cambiar_tamaño[0],'block')
    cambiar_display(ids_cambiar_tamaño[4],'flex')
    cambiar_display('contenedor_historico_titulo_select','flex')


    //Nav Estado-Municipal-Fuente: 
    nav_div=document.getElementById('contenedor_nav_est_mun_fuente')
    //Tiene dos hijos.
    nav_div.children[0].style.width='66vw'
    nav_div.children[0].children[0].style.width='33vw'
    nav_div.children[0].children[0].children[0].style.width='33vw'
    nav_div.children[0].children[1].style.width='33vw'
    nav_div.children[0].children[1].children[0].style.width='33vw'
    //El segundo hijo tiene lo de la fuente en dos p
    nav_div.children[1].style.width='34vw'
    nav_div.children[1].children[0].style.width='34vw'
    nav_div.children[1].children[0].children[0].style.marginBlockStart=0//Fuente
    nav_div.children[1].children[0].children[0].style.marginBlockEnd=0//Fuente
    nav_div.children[1].children[0].children[0].style.fontSize='x-small'//Fuente
    nav_div.children[1].children[0].children[1].style.fontSize='xx-small'//Secretariado...
    nav_div.children[1].children[0].children[1].style.marginBlockStart=0//Secretariado...
    nav_div.children[1].children[0].children[1].style.marginBlockEnd=0//Secretariado...
    
    
    
    
    //Al final simular in click en el nav activo
    click_on_nav(
        document.getElementsByClassName("active_nav_seguridad")[0].innerHTML
      );

    ///Detalles menores en el mapa de leaflet
      //Solo aplica para el mapa municipal.
      //topright control 
      topright_control_municipal=document.getElementsByClassName('info_tablero_seg leaflet-control')[0]
      topright_control_municipal.children[0].style.fontSize='large'

}