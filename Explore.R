#! /usr/bin/env Rscript

input_name.default <- "output/explore.tsv"
"#
input_name.default <- 'output/20140307-cache-envs.tsv'
input_name.default <- 'output/20140524-explore.tsv'
input_name.default <- 'output/20140605-explore.tsv'
"#
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

if (FALSE) {
  setwd("/Users/tobias/dev/pypy/lamb-bench")
  cmd.line <- FALSE
} else {
  cmd.line <- TRUE
}

pkgs = c(
  "rgl",
  "tidyverse",
  "car",
  "scatterplot3d",
  NULL
)
#------------------------------------------------
source("./help.R")


bench <- read_tsv(input_name, comment='#',
                  col_names=c('timestamp', 'value', 'unit', 'criterion', 'benchmark', 'vm', 'suite', 'extra_args', 'warump', 'cores', 'input_sizes', 'variable_values'),
                  col_types=cols(timestamp=col_skip(),value=col_number(),unit=col_factor(),criterion=col_factor(),benchmark=col_factor(), vm=col_factor(), suite=col_factor(), extra_args=col_guess(), warump=col_logical(), cores=col_number(), input_sizes=col_integer(), variable_values=col_integer()))


# --- shaping data
bench %<>%
  select(benchmark,input_sizes, variable_values,criterion, value) %>%
  spread_measurements %>%
  select(benchmark, input_sizes, variable_values, cpu, total) %>%
  group_by(benchmark)


#dat <- bench %>% filter(benchmark == "tree")
#dat <- bench %>% filter(benchmark == "append")
#dat <- bench %>% filter(benchmark == "map")
#dat <- bench %>% filter(benchmark == "reverse")
dat <- bench %>% filter(benchmark == "arbitraty_precision_ints")
#dat <- bench %>% filter(benchmark == "filter")
dat %<>% arrange(input_sizes, variable_values, cpu, total)

###############
# p <- wireframe(total ~ variable_values * input_sizes, data=dat)
# npanel <- c(4, 2)
# rotx <- c(-50, -80)
# rotz <- seq(30, 300, length = npanel[1]+1)
# update(p[rep(1, prod(npanel))], layout = npanel,
#        panel = function(..., screen) {
#          panel.wireframe(..., screen = list(z = rotz[current.column()],
#                                             x = rotx[current.row()]))
#        })
###############

#plot3d(dat$input_sizes, dat$variable_values, dat$cpu)

###############
#scatter3d(cpu ~ input_sizes + variable_values | benchmark, data=bench)
#scatter3d(cpu ~ input_sizes + variable_values | benchmark, data=bench, fit=c("linear","additive"))

scatter3d(cpu ~ input_sizes + variable_values, data=dat, fit=c("quadratic"), point.col="blue", surface=TRUE)

##############
#scatterplot3d(dat$input_sizes,dat$variable_values,dat$cpu)


