# Please run the main code before running this code
source("election-data-analysis.R")

library(tidyverse)
library(patchwork)
library(rddensity)
library(rdrobust)
library(fixest)
library(modelsummary)
library(showtext)
font_add_google("Fira Sans")
showtext_auto()


#______________________________________________________________#
# chunk that will let you read rdrobust as modelsummary:
tidy.rdrobust <- function(object, ...){
  ret <- data.frame(term = row.names(object$coef), 
                    estimate = object$coef[, 1], 
                    std.error = object$se[, 1], 
                    statistic = object$z[, 1],
                    p.value = object$pv[, 1], 
                    conf.low = object$ci[,1],
                    conf.high = object$ci[, 2])
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(object, ...){
  ret <- data.frame(nobs.left = object$N[1],
                    nobs.right = object$N[2],
                    nobs.effective.left = object$N_h[1],
                    nobs.effective.right = object$N_h[2],
                    cutoff = object$c,
                    order.regression = object$q,
                    order.bias = object$q,
                    kernel = object$kernel,
                    bwselect = object$bwselect)
  ret
}
#______________________________________________________________#

#'
#' Is there failure of McCrary density test in non-BJP states as well?
#' 
#' 
# let us first examine the bandwidth with `DCdensity()` command
DCdensity(data_fnb$Win_Margin, ext.out = T) 
# the optimal bandwidth is 0.222
# let's check what happen when we choose a narrow window 
# standard is to look at half the bandwidth size
DCdensity(data_fnb$Win_Margin, bw = 0.111, ext.out = T)

test_density_nonBJP <- rddensity(data_fnb$Win_Margin, c = 0,
                          massPoints = F)
summary(test_density_nonBJP)
plotdensitytest_nonBJP <- rdplotdensity(
  rdd = test_density_nonBJP,
  X = data_fnb$Win_Margin,
  type = "both"
)


# on the other hand, in the BJP states
test_density_BJP <- rddensity(data_fb$Win_Margin, c = 0,
                                 massPoints = F)
summary(test_density_BJP)
plotdensitytest_BJP <- rdplotdensity(
  rdd = test_density_BJP,
  X = data_fb$Win_Margin,
  type = "both"
)

# add a dummy for BJP win
data_e_winner <-
  data_e |>
  mutate(
    winBJP = (Win_Margin >= 0)
  )


# Growth of Muslim voters regressed on BJP win margin
rdrobust(
  y = data_e$g,
  x = data_e$Win_Margin,
  c = 0
) |> 
  summary()

p01 <- 
  rdplot(
  y = data_e$g,
  x = data_e$Win_Margin,
  c = 0
)

p01 <- p01$rdplot

p01 + labs(x = "BJP victory margin",
           y = "Growth rate of Muslim voters",
           title = "") +
  theme(
    # Special Elite for both axis title and plot title
    axis.title = element_text(family = "Fira Sans"),
    title = element_text(family = "Fira Sans")
  )


# Since everything so far has been done using automated procedure, 
# people may
# think that our results are driven by bandwidth choice.
# let's do this using different bandwidths

rdbwselect(
  y = data_e$g,
  x = data_e$Win_Margin,
  c = 0,
  all = TRUE
) |>
  summary()

# MSE RD bandwidth
m1 <- rdrobust(
  y = data_e$g,
  x = data_e$Win_Margin,
  c = 0,
  bwselect = "mserd"
)

# CER-SUM bandwidth
m2 <- rdrobust(
  y = data_e$g,
  x = data_e$Win_Margin,
  c = 0,
  bwselect = "cersum")

# CER-COMB1 bandwidth
m3 <- rdrobust(
  y = data_e$g,
  x = data_e$Win_Margin,
  c = 0,
  bwselect = "cercomb1"
)

# put together these models into a list
m_diffbw <- list("Bandwidth = 0.127 (MSERD)" = m1, 
                 "Bandwidth = 0.106 (CER-SUM)" = m2, 
                 "Bandwidth = 0.094 (CER-COMB1)" = m3)
msummary(m_diffbw,
             stars = T)

# adjust kernel
m4 <-
  rdrobust(
  y = data_e$g,
  x = data_e$Win_Margin,
  c = 0,
  kernel = "triangular"
)

m5 <- 
  rdrobust(
  y = data_e$g,
  x = data_e$Win_Margin,
  c = 0,
  kernel = "epanechnikov"
)

m6 <- 
  rdrobust(
  y = data_e$g,
  x = data_e$Win_Margin,
  c = 0,
  kernel = "uniform"
) 

m_diffkr <- list(
  "Kernel: Triangular" = m4,
  "Kernel: Epanechnikov" = m5,
  "Kernel: Uniform" = m6
)

msummary(
  m_diffkr,
  stars = T
)

# Donut-hole RD design
# trim data for the win margin between +/_ 0.5%.

msummary(rdrobust(data_e$g[data_e$Win_Margin<=-.005 | 
                             .005 <= data_e$Win_Margin],
                  data_e$Win_Margin[data_e$Win_Margin<=-.005 |
                                      .005 <= data_e$Win_Margin]),
         stars = T)