## pmap
fmapn<- function(.l, .f, ...) {
  do.call(Map, c(list(.f), .l, ...))
}
