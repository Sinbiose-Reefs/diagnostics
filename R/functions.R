# first letter in a string to upper case
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# create the function to get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# poison smooth 
poison_smooth <- function(...) {
  geom_smooth(method = "gam", 
              method.args = list(family = "negbin(1)"),...)
  # geom_smooth(method = "glm", method.args = list(family = "poisson"), ...) # glm option
}
