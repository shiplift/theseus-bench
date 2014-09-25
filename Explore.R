#! /usr/bin/env Rscript

pkgs = c(
  "reshape2",
  "plyr",
  #"beanplot",
  "boot",
  "Hmisc",
  "ggplot2",
  "tools",
  "quantreg",
  "lattice",
  "doBy",
  "rgl",
  "car",
  "scatterplot3d"
)

use <- function(pkg) {
  if (!require(pkg, character.only=TRUE)) { install.packages(pkg) }
  library(pkg,character.only=TRUE)
}
sapply(pkgs, use)
if (!require(extrafont)) {
  install.packages("devtools")
  library(devtools)
  install_github("Rttf2pt1", "wch")
  install_github("extrafont", "wch")
}
library(extrafont)
loadfonts(quiet=TRUE)
if (length(fonts()) == 0) {
  font_import(prompt=FALSE)
}


# ---- cmd line ----

if (FALSE) {
  setwd("/Users/tobias/dev/pypy/lamb-bench")
}

if (length(commandArgs(trailingOnly=TRUE)) > 0) {
  tsv_name = commandArgs(trailingOnly=TRUE)[1]
} else {
  # tsv_name <- "output/20140307-cache-envs.tsv"
  tsv_name <- "output/explore.tsv"
}

if (!file.exists(tsv_name)) {
  stop("Cannot open input file ", tsv_name)
}

input.basename = file_path_sans_ext(tsv_name)

bench <- read.delim(tsv_name, comment.char = "#", header=FALSE,
                    col.names=c('timestamp', 'value', 'unit', 'criterion', 'benchmark', 'vm', 'suite', 'extra_args', 'warump', 'cores', 'input_sizes', 'variable_values'))

#------------------------------------------------
# ------ functions -----
confInterval095Error <- function (samples) {
  if (length(sample) < 30)
    qnorm(0.975) * sd(samples) / sqrt(length(samples))
  else
    qt(0.975, df=length(samples)-1) * sd(samples) / sqrt(length(samples))
}

div_mean_x_y = function(data, indices, extraid) {
  indexx = indices[extraid == "x"]
  indexy = indices[extraid == "y"]
  mean(data[indexx]) / mean(data[indexy])
}

normalize_value_bootstrap_confidence = function(x, y, R=1000) {
  # x and y need to be *vectors* of all the values
  # the result is the confidence interval of mean(x) / mean(y)
  total <- c(x,y)
  id <- as.factor(c(rep("x",length(x)),rep("y",length(y))))
  
  b <- boot(total, div_mean_x_y, strata=id, R=R, extraid=id)
  norm <- boot.ci(b, type=c("norm"))$normal
  dimnames(norm) <- list(NULL,c("conf", "upper", "lower"))
  norm
}

bootstrapTo <- function(df, supergroup, group, val, var) {
  cmp <- df[(df[[group]] == val),]
  doIt <- function(X) {
    ident <- (X[[supergroup]])[[1]]
    subs <- cmp[(cmp[[supergroup]] == ident),]
    comparee <- subs[[var]]
    normalize_value_bootstrap_confidence(X[[var]], comparee)
  }
  ddply(df, c(supergroup,group), doIt)  
}

normalizeTo <- function(df, supergroup, group, val, var, vars=c(var)) {
  data <- df
  sg <- droplevels(df[[supergroup]])
  indexes <- which(df[[group]] == val)
  norm.factor <- (df[[var]])[indexes]
  names(norm.factor) <- sg[indexes]
  for (normvar in vars) {
    divis <- norm.factor[ sg ]
    res <- df[[normvar]]/divis
    data[[paste0(normvar,".norm")]] <- res
  }
  data
}

# --- shaping data
bench <- bench[c('criterion','benchmark','value', 'unit', 'input_sizes', 'variable_values')]
bench <- orderBy(~variable_values+input_sizes, bench)
bench.tot <- bench[bench$criterion == 'total',,drop=TRUE]
bench.cpu <- bench[bench$criterion == "cpu",,drop=TRUE]
bench.mem <- bench[bench$criterion == "mem",,drop=TRUE]

#dat <- bench.cpu[bench.cpu$benchmark == "tree",]
#dat <- bench.cpu[bench.cpu$benchmark == "append",]
#dat <- bench.cpu[bench.cpu$benchmark == "map",]
#dat <- bench.cpu[bench.cpu$benchmark == "reverse",]
dat <- bench.cpu[bench.cpu$benchmark == "arbitraty_precision_ints",]
#dat <- bench.cpu[bench.cpu$benchmark == "filter",]


###############
p <- wireframe(as.numeric(value) ~ variable_values * input_sizes, data=dat)
npanel <- c(4, 2)
rotx <- c(-50, -80)
rotz <- seq(30, 300, length = npanel[1]+1)
update(p[rep(1, prod(npanel))], layout = npanel,
       panel = function(..., screen) {
         panel.wireframe(..., screen = list(z = rotz[current.column()],
                                            x = rotx[current.row()]))
       })
###############
plot3d(dat$input_sizes, dat$variable_values, dat$value)

###############
scatter3d(value ~ input_sizes + variable_values | benchmark, data=bench)
scatter3d(value ~ input_sizes + variable_values | benchmark, data=bench, fit=c("linear","additive"))

scatter3d(value ~ input_sizes + variable_values, data=dat, fit=c("quadratic"), point.col="blue", surface=TRUE)

##############
scatterplot3d(dat$input_sizes,dat$variable_values,dat$value)


