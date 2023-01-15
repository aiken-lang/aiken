fn substract(x, y) {
  x - y
}

test flip_1() {
  flip(substract)(3, 2) == -1
}
