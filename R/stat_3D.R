#' @importFrom magrittr "%>%"
#' @importFrom scales "rescale"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "bind_rows"
#' @importFrom plot3D "perspbox"
#' @importFrom plot3D "trans3D"

Stat3D <- ggproto("Stat3D", Stat,

                  setup_params = function(data, params) {

                    params$xrange <- range(data$x)
                    params$yrange <- range(data$y)
                    params$zrange <- range(data$z)

                    params
                  },

                  compute_group = function(data, scales, theta=135, phi=60, xrange=c(0,1), yrange=c(0,1), zrange=c(0,1)) {

                    data = data %>%
                      mutate(
                        x = rescale(x, from=xrange, to=c(0,1)),
                        y = rescale(y, from=yrange, to=c(0,1)),
                        z = rescale(z, from=zrange, to=c(0,1)))

                    pmat = perspbox(z=diag(2), plot=F, theta=theta, phi=phi)

                    XY = trans3D(
                      x = data$x,
                      y = data$y,
                      z = data$z,
                      pmat = pmat) %>%
                      data.frame()

                    data$x = XY$x
                    data$y = XY$y

                    data
                  },

                  required_aes = c("x", "y", "z")
)

#' Draw 3D Geoms
#'
#' This function adds 3D geoms such as points and paths to a ggplot2 plot.
#'
#' @param theta The azimuthal direction in degrees.
#' @param phi The colatitude in degrees.
#' @param geom The geom type to use *ie. "point", "path", "line"*
#' @param ... Arguements passed on to layer.
#' These are often aesthetics, used to set an
#' aesthetic to a fixed value, like color = "red" or size = 3.
#' @export
stat_3D <- function(mapping = NULL, data = NULL, geom = "point",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = Stat3D, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
