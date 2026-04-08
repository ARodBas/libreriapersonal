#' Función rectangulo
#'
#' Crea polígono rectangular y buffer
#'
#' @param lon1 Límite oeste
#' @param lon2 Límite este
#' @param lat1 Límite sur
#' @param lat2 Límite norte
#' @param b buffer
#' @return polígono espacial
#' @import sf
#' @export
#'
#' @examples
#' # creo un polígono base de El Cachucho de 0.3º de buffer
#' rectangulo(b=0.3)
#' # creo un polígono base de Avilés sin buffer
#' rectangulo(lon1=-6.6,lon2=-5.6,lat1=43.6807,lat2=44.06)
#'
rectangulo <- function(lon1=-5.8,lon2=-4,lat1=43.3,lat2=44.4,b=0){

  ## Devolver error si valores no numéricos
  if (!inherits(c(lon1,lon2,lat1,lat2,b), c("numeric"))) {
    stop("Las longitudes, latitudes y buffer deben ser
          numéricos")
  }

  lon1<-lon1-b
  lon2=lon2+b
  lat1<-lat1-b
  lat2=lat2+b

  ## Asegurar que las longitudes están entre -180 y 180
  if((lon1<(-180))|(lon1>180) |
     (lon2<(-180))|(lon2>180))  stop("Longitudes fuera de rango")

  ## Asegurar que las latitudes están entre -90 y 90
  if((lat1<(-90))|(lat1>90) |
     (lat2<(-90))|(lat2>90))  stop("Latitudes fuera de rango")


  pol = sf::st_polygon(
    list(
      cbind(
        c(lon1,lon2,lon2,lon1,lon1),
        c(lat1,lat1,lat2,lat2,lat1))
    )
  )
  pol_crs = st_sfc(pol, crs=4326)
  cat("El polígono está en WGS84")  # Información
  return(pol_crs)
}
