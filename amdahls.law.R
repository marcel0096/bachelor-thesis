## ------------------------------------------------------------------------------------------------------------ ##
                                                  # Amdahl's Law 
## ------------------------------------------------------------------------------------------------------------ ##

# calculating the potential speed up of a workload according to amdahls law
# with p being the percentage of the workload that can be parallelized and n the number of machines
# (https://en.wikipedia.org/wiki/Amdahl%27s_law)

amdahl <- function(p, n) {
  1 / ((1 - p) + (p / n))
}

# amdahls formula solved by n to determine the number of machines necessary to achieve a speedup of x
amdahl.reversed <- function(p, x) {
  p / ((1 / x) - (1-p))
}