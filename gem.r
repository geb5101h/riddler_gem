library(magrittr)
a = 1 / 2
b = 1 / 3
c = 1 / 6


Ea = (1 + a
      + b * (1 / c - 1) * a / (a + b)
      + c * (1 / b - 1) * a / (a + c)) / (1 - a)

Eb = (c * 1  + a * (1 + (1 / c - 1) * a / (a + b))) / (1 - b)
Ec = (b * 1  + a * (1 + (1 / b - 1) * a / (a + c))) / (1 - c)

a * Ea + b * Eb + c * Ec

gemCountSim <- function() {
  vecCount = c(1,0,0)
  while (TRUE) {
    draw = rmultinom(1,1,c(a,b,c)) %>% as.vector
    vecCount = vecCount + draw
    if (vecCount %>% min > 0)
      return(vecCount[1])
  }
}

replicate(10000,gemCountSim(),simplify = "array") %>% mean
