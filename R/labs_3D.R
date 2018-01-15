#' @importFrom magrittr "%>%"
#' @importFrom scales "rescale"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "bind_rows"
#' @importFrom plot3D "perspbox"
#' @importFrom plot3D "trans3D"

Label3D <- ggproto("Label3D", Stat,

                   compute_group = function(data, scales, theta=135, phi=60, labs=c("x-axis", "y-axis", "z-axis")) {

                     pmat = perspbox(z=diag(2), plot=F, theta=theta, phi=phi)

                     XY = trans3D(
                       x = c(1,0,0),
                       y = c(0,1,0),
                       z = c(0,0,1),
                       pmat = pmat) %>%
                       data.frame()

                     df = XY
                     df$label = labs

                     df
                   },

                   required_aes = c("x", "y", "z")
)

#' 3D Axis Labels
#'
#' This function adds 3D axis labels to ggplot2 plots.
#'
#' @param theta The azimuthal direction in degrees.
#' @param phi The colatitude in degrees.
#' @param label The labels to add. A vector of three where the first
#' element is x, the second is y, and the third is z.
#' @param ... Arguements passed on to layer.
#' These are often aesthetics, used to set an
#' aesthetic to a fixed value, like color = "red" or size = 3.
#' @export
labs_3D <- function(mapping = aes(group=1), data = NULL, geom = "text",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = Label3D, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = FALSE, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
