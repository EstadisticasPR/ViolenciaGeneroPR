// =============================================================================
//  Manejadores del mapa leaflet
// =============================================================================
// A diferencia de los otros scripts, este expone un objeto global
// (window.MapHandlers) con un método init() que se llama desde el código R/Leaflet, 
// pasándole el mapa ya creado y un objeto de configuración.
//
// init(map, cfg) configura tres comportamientos sobre el mapa:
//   Bloqueo de zoom mínimo -> el usuario no puede alejarse más allá del encuadre completo de la isla.
//   Caja de información -> muestra la región y su total al pasar el cursor.
//   Botón de reinicio -> vuelve al encuadre inicial de la isla.
//
// Contrato del objeto cfg:
//    {
//      bounds:      [[sur, oeste], [norte, este]],  // límites de la isla
//      totals:      { <idRegión>: <número>, ... },  // total por región
//      legendTitle: "texto",   // etiqueta del valor mostrado
//      infoTitle:   "texto"    // título de la caja en estado inactivo
//    }
// =============================================================================

window.MapHandlers = {
  init: function(map, cfg) {
    
    // Límites geográficos de la isla, como objeto Leaflet.
    var islandBounds = L.latLngBounds(cfg.bounds[0], cfg.bounds[1]);
    
    // --- Bloqueo de zoom mínimo ---
    // Calcula el nivel de zoom en el que la isla completa cabe justo en el
    // contenedor y lo fija como zoom mínimo. Así el usuario puede acercarse,
    // pero no alejarse hasta perder de vista la isla.
    function lockMinZoom() {
      map.setMinZoom(map.getBoundsZoom(islandBounds, false));
    }
    
    // Se aplica cuando el mapa está listo.
    map.whenReady(lockMinZoom);
    
    // y se recalcula al cambiar el tamaño del contenedor, porque el zoom
    // que "encaja" depende del tamaño en píxeles del mapa.
    map.on('resize', lockMinZoom);
    
    // --- Caja de información (hover) ---
    // Actualiza el panel #info_box. Con un `id` de región, muestra su nombre y
    // su total; sin `id` (null), muestra el mensaje inicial de instrucción.
    function updateInfo(id) {
      var box = document.getElementById('info_box');
      if (!box) return; // Si el panel no existe, no hacemos nada.
      if (id) {
        // Total de la región; si no existe en cfg.totals, se usa 0.
        var t = cfg.totals[id] != null ? cfg.totals[id] : 0;
        box.innerHTML = '<b>Región Policiaca: ' + id + '</b><br>' + cfg.legendTitle + ': ' + t;
      } else {
        box.innerHTML = '<b>' + cfg.infoTitle + '</b><br>Pasa el cursor sobre una región';
      }
    }
    
    // Recorre todas las capas del mapa y, en las que tienen layerId (las regiones), 
    // enlaza los eventos de entrada/salida del cursor.
    map.eachLayer(function(layer) {
      if (layer.options && layer.options.layerId) {
        layer.on('mouseover', function() { updateInfo(this.options.layerId); });
        layer.on('mouseout', function() { updateInfo(null); });
      }
    });
    
    // --- Botón de reinicio ---
    // Control personalizado de Leaflet: un botón "⌂" que devuelve el mapa al
    // encuadre inicial de la isla.
    var ResetControl = L.Control.extend({
      options: { position: 'topleft' },
      onAdd: function() {
        var btn = L.DomUtil.create('button', 'leaflet-bar reset-map-btn');
        btn.innerHTML = '⌂';
        btn.title = 'Volver a la vista inicial';
        btn.style.cssText = 'width:34px;height:34px;cursor:pointer;font-size:16px;background:#fff;border:none;';
        
        // Al hacer click: detiene la propagación del evento (para que el mapa
        // no lo interprete como click/arrastre) y reajusta a la isla.
        L.DomEvent.on(btn, 'click', function(e) {
          L.DomEvent.stop(e);
          map.fitBounds(islandBounds);
        });
        return btn;
      }
    });
    map.addControl(new ResetControl());
  }
};