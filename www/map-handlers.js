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
    // AJUSTAR VISTA
    // =====================================================

    function ajustarVista() {

      map.invalidateSize({
        animate: false
      });


      // ---------------------------------------------------
      // MÓVIL
      // ---------------------------------------------------

      if (esMovil()) {

        map.setView(
          [18.20, -66.50],
          6,
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
            maxZoom: 8,
            animate: false
          }
        );

      }

    }


    // =====================================================
    // CONFIGURAR LÍMITES DE ZOOM
    // =====================================================

    function configurarZoom() {

      if (esMovil()) {

        // En móvil:
        // permite alejarse hasta zoom 5
        // pero no acercarse más allá de zoom 7

        map.setMinZoom(5);
        map.setMaxZoom(7);

      }

      else {

        // Desktop

        map.setMinZoom(6);
        map.setMaxZoom(18);

      }

    }


    // =====================================================
    // INICIALIZACIÓN
    // =====================================================

    map.whenReady(function() {

      setTimeout(function() {

        configurarZoom();

        ajustarVista();

      }, 500);

    });


    // =====================================================
    // CAMBIO DE TAMAÑO
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
    // RESIZE DEL MAPA
    // =====================================================

    map.on('resize', function() {

      clearTimeout(resizeTimeout);

      resizeTimeout = setTimeout(function() {

        map.invalidateSize({
          animate: false
        });

      }, 200);

    });


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


              if (esMovil()) {

                map.setView(
                  [18.20, -66.50],
                  6,
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
                    maxZoom: 8,
                    animate: true
                  }
                );

              }

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