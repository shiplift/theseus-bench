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
  "EnvStats",
  "tidyverse",
  "scatterplot3d",
  "plot3Drgl",
  "texreg",
  NULL
)
#------------------------------------------------
source("./help.R")


bench.s <- read_tsv(input_name, comment='#',
                  col_names=c('timestamp', 'value', 'unit', 'criterion', 'benchmark', 'vm', 'suite', 'extra_args', 'warump', 'max_shape_depth', 'substitution_threshold', 'max_storage_width'),
                  col_types=cols(timestamp=col_skip(),value=col_number(),unit=col_factor(),criterion=col_factor(),benchmark=col_factor(), vm=col_factor(), suite=col_factor(), extra_args=col_guess(), warump=col_logical(), max_shape_depth=col_number(), substitution_threshold=col_integer(), max_storage_width=col_integer()))
"
bench.s <- read_tsv(clipboard(), comment='#',
                  col_names=c('timestamp', 'value', 'unit', 'criterion', 'benchmark', 'vm', 'suite', 'extra_args', 'warump', 'max_shape_depth', 'substitution_threshold', 'max_storage_width'),
                  col_types=cols(timestamp=col_skip(),value=col_number(),unit=col_factor(),criterion=col_factor(),benchmark=col_factor(), vm=col_factor(), suite=col_factor(), extra_args=col_guess(), warump=col_logical(), max_shape_depth=col_integer(), substitution_threshold=col_integer(), max_storage_width=col_integer()))
"
# --- shaping data
bench <- bench.s %>%
  select(benchmark, max_shape_depth, max_storage_width,substitution_threshold,criterion, value) %>%
  spread_measurements %>%
  filter(max_shape_depth<30&max_storage_width<34) %>% #old data
  select(benchmark, max_shape_depth, max_storage_width, substitution_threshold, cpu, total, mem) %>%
  group_by(benchmark)

dat <- bench %>% filter(benchmark == "reverse")
"
#dat <- bench %>% filter(benchmark == 'tree')
dat <- bench %>% filter(benchmark == 'append')
dat <- bench %>% filter(benchmark == 'map')
dat <- bench %>% filter(benchmark == 'reverse')
dat <- bench %>% filter(benchmark == 'reversen')
#dat <- bench %>% filter(benchmark == 'arbitraty_precision_ints')
dat <- bench %>% filter(benchmark == 'filter')
"
dat %<>% arrange(max_shape_depth, max_storage_width, substitution_threshold)

# n.outliers <- rosnerTest(dat$cpu,k=min(10, floor(length(dat$cpu) * 0.1)),warn=FALSE)$n.outliers
# cap <- sort(dat$cpu,decreasing=TRUE)[n.outliers+1]
# dat %<>% filter(cpu < 4000)
###############
# p <- wireframe(total ~ max_storage_width * substitution_threshold, data=dat)
# npanel <- c(4, 2)
# rotx <- c(-50, -80)
# rotz <- seq(30, 300, length = npanel[1]+1)
# update(p[rep(1, prod(npanel))], layout = npanel,
#        panel = function(..., screen) {
#          panel.wireframe(..., screen = list(z = rotz[current.column()],
#                                             x = rotx[current.row()]))
#        })
###############

#plot3d(dat$substitution_threshold, dat$max_storage_width, dat$cpu)

###############
#scatter3d(cpu ~ substitution_threshold + max_storage_width | benchmark, data=bench)
#scatter3d(cpu ~ substitution_threshold + max_storage_width | benchmark, data=bench, fit=c("linear","additive"))

# car::scatter3d(cpu ~ substitution_threshold + max_shape_depth, data=dat
#                #, fit=c("quadratic"), point.col="blue", surface=TRUE
#                ,surface=FALSE
#                )

"
.vc <- viridis::viridis(ceiling(max(dat$cpu)), direction=-1)
.vm <- viridis::viridis(ceiling(max(dat$mem)), direction=-1)
"

v2c <- function(value, ...) {
  cols <- viridis::viridis(ceiling(diff(range(dat$cpu))), ...)
  cols[findInterval(value, vec=seq(from=min(value), to=max(value), length.out=min(10000, length(cols))))]
}

colorrange <- function (x) 10^ceiling(diff(log10(range(x,na.rm=TRUE))))

"

dat %<>% filter(cpu < 5000)
"
with(dat, scatter3Drgl(x=max_shape_depth, y=max_storage_width, z=substitution_threshold,
                    xlab='max shape depth',ylab='max storage width', zlab='threshold',
                    ticktype='detailed', nticks=30,
                    colvar=cpu,col=viridis::viridis(colorrange(cpu), direction=-1)))
# add another point
scatter3Drgl(x = 0, y = 0, z = 0, add = TRUE, colkey = FALSE,
          pch = 18, cex = 3, col = "black")

with(dat, scatter3Drgl(max_shape_depth, max_storage_width, substitution_threshold,
                       xlab='max shape depth',ylab='max storage width', zlab='threshold',
                       ticktype='detailed', nticks=30,
                       colvar=mem, col=viridis::viridis(colorrange(mem), direction=-1)))
# add another point
scatter3Drgl(x = 0, y = 0, z = 0, add = TRUE, colkey = FALSE,
             pch = 18, cex = 3, col = "black")


single_out <- function(.data, arg) {
  grouped <- .data %>% ungroup %>% select(-benchmark) %>%
      group_by_at(vars({{ arg }}, -cpu,-total,-mem))
  groupvars <- sapply(group_vars(grouped),as.name)
  summed <- grouped %>%
    summarize_at(vars(cpu,total,mem), list(~max(.),~min(.),~mean(.)))
  summed %>% ungroup %>%
    select(!!!groupvars,starts_with('cpu'),starts_with('total'),starts_with('mem'))
}

matrix_for_criterion <- function(.data, criterion='cpu', part='mean') {
  component <- paste(criterion,part,sep='_')
  m <- matrix(nrow=max(.data[,1]),ncol=max(.data[,2]))
  to.matrix <- function(row, what) {
    x <- row[[1]]
    y <- row[[2]]
    val <- row[[what]]
    m[x,y] <<-val
  }
  apply(.data, 1, to.matrix, component)
  m
}

.m <- dat %>% single_out(-max_shape_depth) %>% matrix_for_criterion('cpu','min')
hist3Drgl(z=.m,col=viridis::viridis(colorrange(.m), direction=-1))


##############
#scatterplot3d(dat$substitution_threshold,dat$max_storage_width,dat$cpu)

"
summary(dat$cpu)
.tmp <- dat %>% filter(TRUE)
.tmp <- dat %>% filter(cpu > 5000)
.tmp <- dat %>% filter(cpu > 1000)
.tmp <- dat %>% filter(cpu < 5000)
.tmp <- dat %>% filter(cpu < 300)
.tmp <- dat %>% filter(cpu < 250)
.tmp <- dat %>% filter(cpu < 236)
.tmp <- dat %>% filter(cpu < 200)
.tmp <- dat %>% filter(cpu < 100)
summary(.tmp$cpu)
hist(.tmp$substitution_threshold, breaks=max(.tmp$substitution_threshold)-min(.tmp$substitution_threshold))
hist(.tmp %>% filter(substitution_threshold<30) %>% .$substitution_threshold,breaks=max(.tmp %>% filter(substitution_threshold<30) %>% .$substitution_threshold))
hist(.tmp %>% filter(substitution_threshold<25) %>% .$substitution_threshold,breaks=max(.tmp %>% filter(substitution_threshold<25) %>% .$substitution_threshold))
hist(.tmp %>% filter(substitution_threshold<80) %>% .$substitution_threshold,breaks=max(.tmp %>% filter(substitution_threshold<80) %>% .$substitution_threshold))
hist(.tmp %>% filter(substitution_threshold<500) %>% .$substitution_threshold,breaks=max(.tmp %>% filter(substitution_threshold<500) %>% .$substitution_threshold))
hist(.tmp %>% filter(substitution_threshold>500) %>% .$substitution_threshold,breaks=max(.tmp %>% filter(substitution_threshold>500) %>% .$substitution_threshold))
hist(.tmp$max_storage_width, breaks=max(.tmp$max_storage_width)-min(.tmp$max_storage_width))
hist(.tmp$max_shape_depth, breaks=max(.tmp$max_shape_depth)-min(.tmp$max_shape_depth))

.baddie <- bench %>% filter(cpu>4000)
.baddie2 <- .baddie %>% filter(substitution_threshold<128)

#BAD [>5000]
#reverse: vv[3, 17?, >31], co>30!!, is[>100/500!]
#reversen: vv[23!], co>23!, is[>100]
#append: vv[>30], co>30! is[>500/>120]
#map: is[128!], ??
#filter is>512
"

# fopt <- function(.data, criterion='cpu') {
#
#   m3 <- array(dim=list(max(.data$max_shape_depth),max(.data$substitution_threshold),max(.data$max_storage_width)))
#   to.array <- function(max_shape_depth, substitution_threshold, max_storage_width, value) {
#     x <- max_shape_depth
#     y <- substitution_threshold
#     z <- max_storage_width
#     m3[[x,y,z]] <<-value
#     #print(paste(x,y,z,m3[x,y,z]))
#     #print(m3)
#   }
#   .data %>% ungroup %>%
#     select(max_shape_depth,substitution_threshold,max_storage_width,value=matches(!!criterion)) %>%
#     pwalk(to.array)
#
#
#   fun <- function(p) {
#     #print(p)
#     p <- pmax(1, p)
#     x <- p[[1]]
#     y <- p[[2]]
#     z <- p[[3]]
#     m3[[x,y,z]]
#   }
#   grad <- function(p) {
#     #r <- function() rnorm(1,0,0.5)
#     #r <- function() rnorm(1,0,1)
#     #r <- function() rt(1,df=Inf)/3
#     #r <- function() runif(1,-1,1)
#     r <- function() rt(1,df=Inf)
#     wiggle <- c(
#       round((dim(m3)[[1]]) * 0.2 * r()),
#       round((dim(m3)[[2]]) * 0.2 * r()),
#       round((dim(m3)[[3]]) * 0.2 * r()),
#       NULL)
#     #print(wiggle)
#     p[[1]] <- min(dim(m3)[[1]], max(1, p[[1]] + wiggle[[1]]))
#     p[[2]] <- min(dim(m3)[[2]], max(1, p[[2]] + wiggle[[2]]))
#     p[[3]] <- min(dim(m3)[[3]], max(1, p[[3]] + wiggle[[3]]))
#     p
#   }
#
#   list(par=sapply(list(.data$max_shape_depth,.data$substitution_threshold,.data$max_storage_width), median),
#        fn=fun,
#        gr=grad)
# }
#
# report_optimize <- function(.data, criterion='cpu', silent=FALSE) {
#   if (!silent)
#     print(paste(' --- ', criterion, ' --- '))
#
#   driver <- fopt(.data, criterion=criterion)
#   fopt.res <- optim(driver$par, driver$fn, driver$gr,
#                     method='SANN', control=list(maxit = 50000, temp = 20,ndeps=c(1,1,1)))
#   if (!silent) {
#     print(paste(
#       c('max shape depth:','max sotrage width:', 'substitution threshold', 'optimal value', 'min of data', 'diff'),
#       c(fopt.res$par, fopt.res$value, min(.data[[criterion]]), abs(fopt.res$value - min(.data[[criterion]])))))
#   }
#   list(params=list(max_shape_depth=fopt.res$par[[1]],
#                    max_sotrage_width=fopt.res$par[[2]],
#                    substitution_threshold=fopt.res$par[[3]],
#                    deviation=abs(fopt.res$value - min(.data[[criterion]]))/min(.data[[criterion]])),
#        optim_result=fopt.res)
# }


report_optimize <- function(.data, criterion='cpu', silent=FALSE) {
  v <- sym(criterion)
  d <- .data %>% arrange(!!v) %>% head(1)
  print(d)
    list(params=list(max_shape_depth=2,
                     max_sotrage_width=2,
                     substitution_threshold=2,
                     deviation=0),
         optim_result=NULL)
}

print("=-=-=-=-=")

optimize_pars_for_benchmark <- function(benchmark,data, silent=FALSE) {
  if (!silent)
    print(paste("====",benchmark,"===="))
  optimals <- tibble(criterion=character(),
                     max_shape_depth=integer(),
                     max_sotrage_width=integer(),
                     substitution_threshold=integer(),
                     deviation=numeric())
  for (criterion in c('cpu','total','mem')) {
    opt <- data %>%  report_optimize(criterion, silent=silent)
    r <- c(criterion=criterion,  opt$params)
    optimals %<>% add_row(!!!r)
  }
  if (!silent)
    print("=-=-=-=-=")
  optimals
}
optimize_pars_for_benchmark_silent <- function(benchmark, data) optimize_pars_for_benchmark(benchmark, data, silent=TRUE)

bench.optimals <- bench %>% nest %>%
  # mutate(data=map2(benchmark, data, optimize_pars_for_benchmark)) %>%
  mutate(data=map2(benchmark, data, optimize_pars_for_benchmark_silent)) %>%
  unnest(cols=c(data))

bench.optimals.c <- bench.optimals %>% ungroup %>% group_by(criterion) %>% arrange(criterion)

bench.optimals.c %<>% filter(criterion!='total')


base_family='Helvetica'
bench.optimals.c %>%
  pivot_longer(c('max_shape_depth','max_sotrage_width','substitution_threshold'), names_to='parameter') %>%
  ggplot(aes(
    x=parameter,y=value
  )) + default.theme.t(fakeLegend = FALSE) +
  geom_boxplot() +
  geom_jitter() +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,NA),
                     breaks=function(x) { seq(floor(x[[1]]),ceiling(x[[2]]),1)},
                     minor_breaks = NULL) +
  facet_wrap(criterion~.)

"=-=-=-="

# use('rms')
# require(rms)
# row <- 0
# with(dat %>% ungroup %>% select(cpu, max_shape_depth, max_storage_width, substitution_threshold),
#   # for(gvar in list(max_shape_depth, max_storage_width, substitution_threshold)) {
#   #   row <- row + 1; col <- 0
#     for(fun in list(qlogis, qnorm, function(y) -log(-log(y)))) {
#       # col <- col + 1
#       # print(Ecdf(~cpu, groups=gvar, fun=fun,
#                  # main=paste(c('depth','width', 'threshold')[row],
#                  #            c('logit','probit','log-log')[col])),
#             # split=c(col,row,3,3), more=row < 3 | col < 3
#             # E))
#     }
#   # }
# )

#o <- orm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=dat)
l   <-  lm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=dat)
# g   <- glm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=dat)
l2  <-  lm(cpu~I(max_shape_depth^2) + I(max_storage_width^2) + I(substitution_threshold^2), data=dat)
# g2  <- glm(cpu~I(max_shape_depth^2) + I(max_storage_width^2) + I(substitution_threshold^2), data=dat)
l2a <-  lm(cpu~poly(max_shape_depth,2) + poly(max_storage_width,2) + poly(substitution_threshold,2), data=dat)
# g2a <- glm(cpu~poly(max_shape_depth,2) + poly(max_storage_width,2) + poly(substitution_threshold,2), data=dat)
ls  <-  lm(sqrt(cpu)~max_shape_depth + max_storage_width + substitution_threshold, data=dat)
# gs  <- glm(sqrt(cpu)~max_shape_depth + max_storage_width + substitution_threshold, data=dat)

l2c <-  lm(cpu~poly(max_shape_depth,2) + max_storage_width + substitution_threshold, data=dat)
l2s <-  lm(sqrt(cpu)~poly(max_shape_depth,2) + max_storage_width + substitution_threshold, data=dat)
l3c <-  lm(cpu~poly(max_shape_depth,3) + max_storage_width + substitution_threshold, data=dat)
l3s <-  lm(sqrt(cpu)~poly(max_shape_depth,3) + max_storage_width + substitution_threshold, data=dat)
l4c <-  lm(cpu~poly(max_shape_depth,4) + max_storage_width + substitution_threshold, data=dat)
l4s <-  lm(sqrt(cpu)~poly(max_shape_depth,4) + max_storage_width + substitution_threshold, data=dat)
# g2c <- glm(cpu~poly(max_shape_depth,2) + max_storage_width + substitution_threshold, data=dat)


lb  <- lm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=bench)
lbs <- lm(sqrt(cpu)~max_shape_depth + max_storage_width + substitution_threshold, data=bench)

# AIC(l   ,g   ,l2  ,g2  ,l2a ,g2a ,ls  ,gs, l2c, g2c )
# BIC(l   ,g   ,l2  ,g2  ,l2a ,g2a ,ls  ,gs, l2c, g2c )
AIC(l, l2, l2a, ls, l2c)
BIC(l, l2, l2a, ls, l2c)

screenreg(l = list(l,ls,l2a,l2c,l4s))

# ls0 <-  lm(sqrt(cpu)~0 + max_shape_depth + max_storage_width + substitution_threshold, data=dat)
# gs0 <- glm(sqrt(cpu)~0 + max_shape_depth + max_storage_width + substitution_threshold, data=dat)
#r <- lrm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=dat)
# f
# with(dat, {
#   dd <- datadist(max_shape_depth, max_storage_width, substitution_threshold)
# })
# options(datadist=NULL)
# plot(summary(f), log=TRUE)

