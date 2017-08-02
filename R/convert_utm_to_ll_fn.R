#' Convert from UTM to Lat-long
#'
#' If eastings and northings have been flipped
#' around dateline, will convert back and provide estimtes of "original" lat lon
#' Values should be viewed as estimates for plotting and general pairing
#'
#' @param eastings eastings of points, paired to northings
#' @param northings northings of points,  paired to eastings
#' @param zone UTM zone of points
#' @param flip_around_dateline logical stating if points have been flipped around the dateline
#' @param meters_per_utm_unit meters per easting/northing
#'
#' @return a data.frame with
#' \describe{
#' \item{approx_long}{the approximate transformed longitude of each UTM point}
#' \item{approx_lat}{the approximate transformed latitude of each UTM point}
#' }
#' @export
#'
#' @examples
#' eastings <- Spatial_List$loc_x[,'E_km']
#'
#' northings <- Spatial_List$loc_x[,'N_km']
#'
#' zone <- Extrapolation_List$zone
#'
#' flip_around_dateline <- Extrapolation_List$flip_around_dateline
#'
#' latlongs <- convert_utm_to_ll_fn(eastings = eastings, northings = northings, zone = zone, flip_around_dateline = flip_around_dateline)
#'
#' ggmap::qmplot(x = approx_long, y = approx_lat, data = latlongs, color = 'red')

convert_utm_to_ll_fn <-
  function(eastings,
           northings,
           zone,
           flip_around_dateline = T,
           meters_per_utm_unit = 1000) {
    require(dplyr)
    # on.exit(detach('package:dplyr'))

if (length(eastings) != length(northings)){
  stop('eastings and northings are not same length, make sure that they represent paired coordinates')
}

    utm_coords <-
      data.frame(easting = eastings, northing = northings) %>%
      dplyr::mutate(geometry = purrr::map2(eastings, northing, ~ sf::st_point(
        x = c(.x * meters_per_utm_unit, .y * meters_per_utm_unit), dim = 'XY'
      ))) %>% # assign a spatial geometry to each knot
      ungroup() %>%
      mutate(geometry = sf::st_sfc(geometry, crs = paste0(
        "+proj=utm +zone=", zone
      ))) %>%
      sf::st_sf() # convert to a sf object in UTM projection for supplied zone


    ll_coords <-
      sf::st_transform(utm_coords, crs = "+proj=longlat") #convert to long-lat projection

    ll_geometry <- sf::st_geometry(ll_coords)

    extract_ll <- function(x) {
      data.frame(approx_long = x[1], approx_lat = x[2])
    }
    ll_coords <-  purrr::map_df(ll_geometry, extract_ll) # extract transformed long/lat for knots

    if (flip_around_dateline == T) { # if UTM coordinates were flipped around deadline, reverse flip
      ll_coords <-
        dplyr::mutate(ll_coords, approx_long = purrr::map_dbl(approx_long, ~ ifelse(.x > 0, .x - 180, .x + 180)))
    }
    return(ll_coords)

  }