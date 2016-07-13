
#' Calculate weights for bilinear interpolation
#'
#' \code{bilinear_interp} calculates three coefficients, used to calculate height via blinear interpolation from height and location of three neighbors
#'
#' @param xy1, 2D location and height of first neighbor
#' @param xy2, 2D location and height of second neighbor
#' @param xy3, 2D location and height of third neighbor
#' @param xypred, 2D location of location to interpolate

#' @return Tagged list of useful output
#' \describe{
#'   \item{zpred}{height at location \code{xypred}}
#'   \item{phi}{Coefficients for interpolation}
#' }

#' @examples
#'
#'   \dontrun{
#'   bilinear_interp( xyz1=c(0,0,0), xyz2=c(1,-1,1), xyz3=c(0,1,2), xypred=c(0.1,0.7))
#'   # Should equal 1.7
#'   }

#' @export
bilinear_interp = function( xyz1, xyz2, xyz3, xypred ){
  # Make constaint matrix
  #  1st row:  sum to one
  #  2nd row:  sum to x locations
  #  3rd row:  sum to y location
  B = rbind( rep(1,3), cbind(xyz1[1:2],xyz2[1:2],xyz3[1:2]) )

  # Calculate weighting vector that satisfies these constraints
  phi = solve(B) %*% c(1,xypred)

  # Calculate interpolated value
  zpred = c(xyz1[3],xyz2[3],xyz3[3]) %*% phi
  Return = list("phi"=phi, "zpred"=zpred)
  return( Return )
}
# bilinear_interp( xyz1=c(0,0,0), xyz2=c(1,0,1), xyz3=c(0,1,2), xypred=c(0.25,0.5))

