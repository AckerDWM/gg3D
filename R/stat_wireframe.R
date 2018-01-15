#' @importFrom magrittr "%>%"
#' @importFrom scales "rescale"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "bind_rows"
#' @importFrom plot3D "perspbox"
#' @importFrom plot3D "trans3D"

StatWireframe <- ggproto("StatWireframe", Stat,

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

                           x_wires = sapply(unique(data$x), function(xi) {
                             df_xi = filter(data, x == xi) %>%
                               arrange(x, y)
                             XY <- trans3D(
                               x = df_xi$x,
                               y = df_xi$y,
                               z = df_xi$z,
                               pmat = pmat) %>%
                               data.frame() %>%
                               mutate(z = df_xi$z)
                             XY = rbind(c(NA, NA, NA), XY, c(NA, NA, NA))
                             XY
                           }, simplify=F) %>%
                             bind_rows()

                           y_wires = sapply(unique(data$y), function(yi) {
                             df_yi = filter(data, y == yi) %>%
                               arrange(x, y)
                             XY <- trans3D(
                               x = df_yi$x,
                               y = df_yi$y,
                               z = df_yi$z,
                               pmat = pmat) %>%
                               data.frame() %>%
                               mutate(z = df_yi$z)
                             XY = rbind(c(NA, NA, NA), XY, c(NA, NA, NA))
                             XY
                           }, simplify=F) %>%
                             bind_rows()

                           df = bind_rows(x_wires, y_wires)
                           df
                         },

                         required_aes = c("x", "y", "z")
)

#' Wireframe Plot
#'
#' This function adds a 3D wireframe to a ggplot2 plot.
#'
#' @param theta The azimuthal direction in degrees.
#' @param phi The colatitude in degrees.
#' @param ... Arguements passed on to layer.
#' These are often aesthetics, used to set an
#' aesthetic to a fixed value, like color = "red" or size = 3.
#' @export
stat_wireframe <- function(mapping = NULL, data = NULL, geom = "path",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  layer(
    stat = StatWireframe, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
