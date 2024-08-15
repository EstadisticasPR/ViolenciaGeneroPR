# Authenticate
rsconnect::setAccountInfo(name='proyectosestadisticosiepr', 
                          token='BEABB8A60DD899C508F752F523FCCBA6', 
                          secret='Nwqn/uXs5WyrCF6e+PhnQr68qA8V1WIj7uIdpPV7')
# Deploy
rsconnect::deployApp(appFiles = c("ui.R", "server.R", "data",
                       "global.R", "utils.R", "www"), 
                     forceUpdate = TRUE)
