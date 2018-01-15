#' @importFrom magrittr "%>%"
#' @importFrom scales "rescale"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "bind_rows"
#' @importFrom plot3D "perspbox"
#' @importFrom plot3D "trans3D"

AxisLabels3D <- ggproto("AxisLabels3D", Stat,

                        compute_group = function(data, scales, theta=135, phi=60) {

                          pmat = perspbox(z=diag(2), plot=F, theta=theta, phi=phi)

                          XY = trans3D(
                            x = c(1,.1,0,0,0,0),
                            y = c(0,0,1,.1,0,0),
                            z = c(0,0,0,0,1,.1),
                            pmat = pmat) %>%
                            data.frame()

                          XY = sapply(1:nrow(XY), function(i) {
                            XY_i = XY[i,]
                            x = XY_i$x
                            y = XY_i$y

                            XY_i$hjust = ifelse(x > 0, -.1, 1.1)
                            XY_i$vjust = ifelse(y > 0, 1.1, -.1)
                            XY_i
                          }, simplify=F) %>%
                            bind_rows()

                          df = XY
                          df$label = c(max(data$x), min(data$x), max(data$y), min(data$y), max(data$z), min(data$z))

                          df
                        },

                        required_aes = c("x", "y", "z")
)

#' 3D Axis Numbering
#'
#' This function adds 3D axis numbering to ggplot2 plots.
#'
#' @param theta The azimuthal direction in degrees.
#' @param phi The colatitude in degrees.
#' @param ... Arguements passed on to layer.
#' These are often aesthetics, used to set an
#' aesthetic to a fixed value, like color = "red" or size = 3.
#' @export
axis_labs_3D <- function(mapping = aes(group=1), data = NULL, geom = "text",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  layer(
    stat = AxisLabels3D, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = FALSE, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
