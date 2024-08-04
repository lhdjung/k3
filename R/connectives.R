
nand <- function(x, y) {
  !(x & y)
}

nor <- function(x, y) {
  !(x | y)
}

# TODO: Rethink this -- it doesn't seem to fit with the other logical
# connectives right here...
xnor <- function(x, y, strict = FALSE) {
  equivalent(x, y, strict)
}

