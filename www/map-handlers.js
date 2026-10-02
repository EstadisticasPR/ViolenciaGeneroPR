// =============================================================================
// Manejadores del mapa Leaflet
// =============================================================================

window.MapHandlers = {

  init: function(map, cfg) {

    // =====================================================
    // LÍMITES DE PUERTO RICO
    // =====================================================

    var islandBounds = L.latLngBounds(
      cfg.bounds[0],
      cfg.bounds[1]
    );


    // =====================================================
    // DETECTAR DISPOSITIVO
    // =====================================================

    function esMovil() {
      return window.innerWidth <= 600;
    }


    // =====================================================
    // CONFIGURAR VISTA INICIAL
    // =====================================================

    function ajustarVistaInicial() {

      map.invalidateSize({
        animate: false
      });


      // ---------------------------------------------------
      // MÓVIL
      // ---------------------------------------------------

      if (esMovil()) {

        map.setView(
          [18.20, -66.50],
          8,
          {
            animate: false
          }
        );

      }


      // ---------------------------------------------------
      // DESKTOP
      // ---------------------------------------------------

      else {

        map.fitBounds(
          islandBounds,
          {
            padding: [10, 10],
            animate: false
          }
        );

      }


      // ===================================================
      // OBTENER EL ZOOM REAL
      // ===================================================

      var zoomInicial = map.getZoom();


      // ===================================================
      // LIMITES RELATIVOS
      // ===================================================

      map.setMinZoom(
        zoomInicial - 1
      );

      map.setMaxZoom(
        zoomInicial + 1
      );


      console.log(
        "Zoom inicial:",
        zoomInicial,
        " | Min:",
        zoomInicial - 1,
        " | Max:",
        zoomInicial + 1
      );

    }


    // =====================================================
    // INICIALIZACIÓN
    // =====================================================

    map.whenReady(function() {

      setTimeout(function() {

        ajustarVistaInicial();

      }, 500);

    });


    // =====================================================
    // RESIZE
    // =====================================================

    var resizeTimeout;

    if (window.ResizeObserver) {

      var resizeObserver =
        new ResizeObserver(function() {

          clearTimeout(resizeTimeout);

          resizeTimeout = setTimeout(function() {

            map.invalidateSize({
              animate: false
            });

          }, 200);

        });


      resizeObserver.observe(
        map.getContainer()
      );

    }


    // =====================================================
    // CAJA DE INFORMACIÓN
    // =====================================================

    function updateInfo(id) {

      var box =
        document.getElementById('info_box');

      if (!box) return;


      if (id) {

        var t =
          cfg.totals[id] != null
            ? cfg.totals[id]
            : 0;


        box.innerHTML =
          '<b>Región Policiaca: ' +
          id +
          '</b><br>' +
          cfg.legendTitle +
          ': ' +
          t;

      }

      else {

        box.innerHTML =
          '<b>' +
          cfg.infoTitle +
          '</b><br>' +
          'Pasa el cursor sobre una región';

      }

    }


    // =====================================================
    // EVENTOS DE LAS REGIONES
    // =====================================================

    map.eachLayer(function(layer) {

      if (
        layer.options &&
        layer.options.layerId
      ) {

        layer.on(
          'mouseover',
          function() {

            updateInfo(
              this.options.layerId
            );

          }
        );


        layer.on(
          'mouseout',
          function() {

            updateInfo(null);

          }
        );

      }

    });


    // =====================================================
    // BOTÓN RESET
    // =====================================================

    var ResetControl =
      L.Control.extend({

        options: {
          position: 'topleft'
        },


        onAdd: function() {

          var btn =
            L.DomUtil.create(
              'button',
              'leaflet-bar reset-map-btn'
            );


          btn.innerHTML = '⌂';

          btn.title =
            'Volver a la vista inicial';


          btn.style.cssText =
            'width:34px;' +
            'height:34px;' +
            'cursor:pointer;' +
            'font-size:16px;' +
            'background:#fff;' +
            'border:none;';


          L.DomEvent.on(
            btn,
            'click',
            function(e) {

              L.DomEvent.stop(e);


              map.invalidateSize({
                animate: false
              });


              // =========================================
              // RESTAURAR VISTA
              // =========================================

              if (esMovil()) {

                map.setView(
                  [18.20, -66.50],
                  7,
                  {
                    animate: true
                  }
                );

              }

              else {

                map.fitBounds(
                  islandBounds,
                  {
                    padding: [10, 10],
                    animate: true
                  }
                );

              }


              // =========================================
              // RECALCULAR LIMITES
              // =========================================

              setTimeout(function() {

                var zoomInicial =
                  map.getZoom();


                map.setMinZoom(
                  zoomInicial - 1
                );


                map.setMaxZoom(
                  zoomInicial + 1
                );


                console.log(
                  "RESET → Zoom:",
                  zoomInicial,
                  " | Min:",
                  zoomInicial - 1,
                  " | Max:",
                  zoomInicial + 1
                );

              }, 300);

            }
          );


          return btn;

        }

      });


    map.addControl(
      new ResetControl()
    );

  }

};