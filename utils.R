z_scale <- function(x) {
  (x - mean(x, na.rm=T)) / sd(x, na.rm=T)
}
