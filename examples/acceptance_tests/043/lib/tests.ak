fn subtract(x, y) {
  x - y
}

test flip_1() {
  flip(subtract)(3, 2) == -1
}
