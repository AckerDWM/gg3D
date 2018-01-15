#' @importFrom magrittr "%>%"
#' @importFrom scales "rescale"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "bind_rows"
#' @importFrom plot3D "perspbox"
#' @importFrom plot3D "trans3D"

Axes3D = ggproto("Axes3D", Stat,
                  compute_group = function(data, scales, theta=135, phi=60) {

                    pmat = perspbox(z=diag(2), plot=F, theta=theta, phi=phi)

                    x_axis = trans3D(x = 0:1, y = 0, z = 0, pmat = pmat) %>%
                      data.frame() %>%
                      mutate(axis="x")
                    y_axis = trans3D(x = 0, y = 0:1, z = 0, pmat = pmat) %>%
                      data.frame() %>%
                      mutate(axis="y")
                    z_axis = trans3D(x = 0, y = 0, z = 0:1, pmat = pmat) %>%
                      data.frame() %>%
                      mutate(axis="z")

                    Axes = bind_rows(x_axis, y_axis, z_axis)

                    Axes
                  },

                  required_aes = c("x", "y", "z")
)

#' Draw 3D Axes
#'
#' This function adds 3D axes to a ggplot2 plot.
#'
#' @param theta The azimuthal direction in degrees.
#' @param phi The colatitude in degrees.
#' @param ... Arguements passed on to layer.
#' These are often aesthetics, used to set an
#' aesthetic to a fixed value, like color = "red" or size = 3.
#' @export
axes_3D = function(mapping = aes(group=1), data = NULL, geom = "path",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = Axes3D, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = FALSE, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
