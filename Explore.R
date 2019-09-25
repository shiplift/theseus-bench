#! /usr/bin/env Rscript

input_name.default <- "output/explore.tsv"
"#
input_name.default <- 'output/20140307-cache-envs.tsv'
input_name.default <- 'output/20140524-explore.tsv'
input_name.default <- 'output/20140605-explore.tsv'
input_name.default <- 'output/20190921-explore.tsv'
input_name.default <- 'output/20190924-explore.tsv'
"#
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

if (FALSE) {
  setwd("/Users/tobias/dev/pypy/lamb-bench")
  cmd.line <- FALSE
} else {
  cmd.line <- TRUE
}

pkgs = c(
  'rgl',
  'EnvStats',
  'tidyverse',
  'scatterplot3d',
  'plot3Drgl',
  #'texreg',
  'locfit',
  'ztable',
  'shadowtext',
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
  select(vm, benchmark, max_shape_depth, max_storage_width,substitution_threshold,criterion, value) %>%
  spread_measurements %>%
  filter(max_shape_depth<30&max_storage_width<34) %>% #old data
  factorize(.add_visuals = FALSE) %>%
  select(-vm,-gc) %>%
  group_by(benchmark)


dat <- bench %>% filter(benchmark == "map[n]")
"
dat <- bench %>% filter(benchmark == 'tree[E]')
dat <- bench %>% filter(benchmark == 'tree[n]')
dat <- bench %>% filter(benchmark == 'append[E]')
dat <- bench %>% filter(benchmark == 'append[n]')
dat <- bench %>% filter(benchmark == 'map[E]')
dat <- bench %>% filter(benchmark == 'map[n]')
dat <- bench %>% filter(benchmark == 'reverse[E]')
dat <- bench %>% filter(benchmark == 'reverse[n]')
dat <- bench %>% filter(benchmark == 'filter[E]')
dat <- bench %>% filter(benchmark == 'filter[n]')
#dat <- bench %>% filter(benchmark == 'arbitraty_precision_ints')
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


if (FALSE) {
"

dat %<>% filter(cpu < 5000)
"
with(dat, scatter3Drgl(x=max_shape_depth, y=max_storage_width, z=substitution_threshold,
                    xlab='max shape depth',ylab='max storage width', zlab='threshold',
                    ticktype='detailed', nticks=30, cex=3,
                    colvar=cpu,col=viridis::viridis(colorrange(cpu), direction=-1)))
# add another point
scatter3Drgl(x = 0, y = 0, z = 0, add = TRUE, colkey = FALSE,
          pch = 18, cex = 3, col = "black")

with(dat, scatter3Drgl(max_shape_depth, max_storage_width, substitution_threshold,
                       xlab='max shape depth',ylab='max storage width', zlab='threshold',
                       ticktype='detailed', nticks=30, cex=3,
                       colvar=mem, col=viridis::viridis(colorrange(mem), direction=-1)))
# add another point
scatter3Drgl(x = 0, y = 0, z = 0, add = TRUE, colkey = FALSE,
             pch = 18, cex = 3, col = "black")

.m <- dat %>% single_out(-max_shape_depth) %>% matrix_for_criterion('cpu','min')
hist3Drgl(z=.m,col=viridis::viridis(colorrange(.m), direction=-1))

}
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
#                    max_storage_width=fopt.res$par[[2]],
#                    substitution_threshold=fopt.res$par[[3]],
#                    deviation=abs(fopt.res$value - min(.data[[criterion]]))/min(.data[[criterion]])),
#        optim_result=fopt.res)
# }


# report_optimize <- function(.data, criterion='cpu', silent=FALSE) {
#   v <- sym(criterion)
#   d <- .data %>% arrange(!!v) %>% head(1)
#   print(d)
#     list(params=list(max_shape_depth=2,
#                      max_storage_width=2,
#                      substitution_threshold=2,
#                      deviation=0),
#          optim_result=NULL)
# }
#
# print("=-=-=-=-=")
#
# optimize_pars_for_benchmark <- function(benchmark,data, silent=FALSE) {
#   if (!silent)
#     print(paste("====",benchmark,"===="))
#   optimals <- tibble(criterion=character(),
#                      max_shape_depth=integer(),
#                      max_storage_width=integer(),
#                      substitution_threshold=integer(),
#                      deviation=numeric())
#   for (criterion in c('cpu','total','mem')) {
#     opt <- data %>%  report_optimize(criterion, silent=silent)
#     r <- c(criterion=criterion,  opt$params)
#     optimals %<>% add_row(!!!r)
#   }
#   if (!silent)
#     print("=-=-=-=-=")
#   optimals
# }
# optimize_pars_for_benchmark_silent <- function(benchmark, data) optimize_pars_for_benchmark(benchmark, data, silent=TRUE)
#
# bench.optimals <- bench %>% nest %>%
#   # mutate(data=map2(benchmark, data, optimize_pars_for_benchmark)) %>%
#   mutate(data=map2(benchmark, data, optimize_pars_for_benchmark_silent)) %>%
#   unnest(cols=c(data))
#
# bench.optimals.c <- bench.optimals %>% ungroup %>% group_by(criterion) %>% arrange(criterion)
#
# bench.optimals.c %<>% filter(criterion!='total')

"
base_family='Helvetica'
"
# bench.optimals.c %>%
#   pivot_longer(c('max_shape_depth','max_storage_width','substitution_threshold'), names_to='parameter') %>%
#   ggplot(aes(
#     x=parameter,y=value
#   )) + default.theme.t(fakeLegend = FALSE) +
#   geom_boxplot() +
#   geom_jitter() +
#   scale_y_continuous(expand=c(0,0),
#                      limits=c(0,NA),
#                      breaks=function(x) { seq(floor(x[[1]]),ceiling(x[[2]]),1)},
#                      minor_breaks = NULL) +
#   facet_wrap(criterion~.)

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
#
# #o <- orm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=dat)
# l   <-  lm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=dat)
# # g   <- glm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=dat)
# l2  <-  lm(cpu~I(max_shape_depth^2) + I(max_storage_width^2) + I(substitution_threshold^2), data=dat)
# # g2  <- glm(cpu~I(max_shape_depth^2) + I(max_storage_width^2) + I(substitution_threshold^2), data=dat)
# l2a <-  lm(cpu~poly(max_shape_depth,2) + poly(max_storage_width,2) + poly(substitution_threshold,2), data=dat)
# # g2a <- glm(cpu~poly(max_shape_depth,2) + poly(max_storage_width,2) + poly(substitution_threshold,2), data=dat)
# ls  <-  lm(sqrt(cpu)~max_shape_depth + max_storage_width + substitution_threshold, data=dat)
# # gs  <- glm(sqrt(cpu)~max_shape_depth + max_storage_width + substitution_threshold, data=dat)
#
# l2c <-  lm(cpu~poly(max_shape_depth,2) + max_storage_width + substitution_threshold, data=dat)
# l2s <-  lm(sqrt(cpu)~poly(max_shape_depth,2) + max_storage_width + substitution_threshold, data=dat)
# l3c <-  lm(cpu~poly(max_shape_depth,3) + max_storage_width + substitution_threshold, data=dat)
# l3s <-  lm(sqrt(cpu)~poly(max_shape_depth,3) + max_storage_width + substitution_threshold, data=dat)
# l4c <-  lm(cpu~poly(max_shape_depth,4) + max_storage_width + substitution_threshold, data=dat)
# l4s <-  lm(sqrt(cpu)~poly(max_shape_depth,4) + max_storage_width + substitution_threshold, data=dat)
# # g2c <- glm(cpu~poly(max_shape_depth,2) + max_storage_width + substitution_threshold, data=dat)
#
#
# lb  <- lm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=bench)
# lbs <- lm(sqrt(cpu)~max_shape_depth + max_storage_width + substitution_threshold, data=bench)
#
# # AIC(l   ,g   ,l2  ,g2  ,l2a ,g2a ,ls  ,gs, l2c, g2c )
# # BIC(l   ,g   ,l2  ,g2  ,l2a ,g2a ,ls  ,gs, l2c, g2c )
# AIC(l, l2, l2a, ls, l2c)
# BIC(l, l2, l2a, ls, l2c)

#screenreg(l = list(l,ls,l2a,l2c,l4s))

# ls0 <-  lm(sqrt(cpu)~0 + max_shape_depth + max_storage_width + substitution_threshold, data=dat)
# gs0 <- glm(sqrt(cpu)~0 + max_shape_depth + max_storage_width + substitution_threshold, data=dat)
#r <- lrm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=dat)
# f
# with(dat, {
#   dd <- datadist(max_shape_depth, max_storage_width, substitution_threshold)
# })
# options(datadist=NULL)
# plot(summary(f), log=TRUE)

bench.models <- bench %>% group_modify(function (x,y) {
  .mc <- lm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=x)
  .mm <-lm(mem~max_shape_depth + max_storage_width + substitution_threshold, data=x)
  tribble(
    ~model_cpu, ~anova_cpu, ~model_mem, ~anova_mem,
    .mc, anova(.mc), .mm, anova(.mm))
})

bench.models.p <- bench.models %>% group_modify(function (x, y) {
  cd <- x$anova_cpu[[1]]$`Pr(>F)`[1]
  cw <- x$anova_cpu[[1]]$`Pr(>F)`[2]
  ct <- x$anova_cpu[[1]]$`Pr(>F)`[3]
  md <- x$anova_mem[[1]]$`Pr(>F)`[1]
  mw <- x$anova_mem[[1]]$`Pr(>F)`[2]
  mt <- x$anova_mem[[1]]$`Pr(>F)`[3]
  tribble(
    ~p_cpu_max_shape_depth,
    ~p_cpu_max_storage_width,
    ~p_cpu_substitution_threshold,
    ~p_mem_max_shape_depth,
    ~p_mem_max_storage_width,
    ~p_mem_substitution_threshold,
    cd,cw,ct,md,mw,mt)})


# TODO: print model results
# forall bench: non-correlation hypothesis cannot be rejected for any substitution_threshold.

#--- >> -----
bench.dw <- bench %>% single_out(-substitution_threshold, .keep_benchmark = TRUE, .make_min_default = TRUE)
bench.dw.orig <- bench.dw %>% filter(TRUE)
bench.dw %<>% (function(.data) {
  # filer out order-of-magnitude outliers
  if (nrow(.data[.data$benchmark == 'reverse[n]' & .data$cpu > 5e4,]) > 0) {
    res <- .data %>% filter(TRUE)
    res[res$benchmark == 'reverse[n]' & res$cpu > 5e4,]$cpu <- NA
    res
  } else {
    .data
  }
})

#--- << -----

bench.cpu <- bench.dw %>% arrange(cpu, .by_group=TRUE) %>% top_n(-3,cpu)
bench.mem <- bench.dw %>% arrange(mem, .by_group=TRUE) %>% top_n(-3,mem)

bench.minimals <- rbind(
  bench.cpu %>% mutate(criterion='cpu'),
  bench.mem %>% mutate(criterion='mem')) %>%
  mutate(criterion=as_factor(criterion))

"
base_family='Helvetica'
"
bench.minimals %>%
  pivot_longer(c('max_shape_depth','max_storage_width'), names_to='parameter') %>%
  ggplot(aes(
    x=parameter,y=value
  )) + default.theme.t(fakeLegend = FALSE) +
  geom_boxplot() +
  geom_dotplot(binaxis='y',
               stackdir='center',
               dotsize = .5,
               fill="red") +
  # geom_jitter() +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,NA),
                     breaks=function(x) { seq(floor(x[[1]]),ceiling(x[[2]]),1)},
                     minor_breaks = NULL) +
  facet_null()
  # facet_wrap(criterion~niladic)
  # facet_wrap(criterion~.)
  # facet_wrap(niladic~.)

"
bench.sw.pretend <- bench %>% ##! filter(max_storage_width<22) %>%
  single_out(-substitution_threshold, .keep_benchmark = TRUE, .make_min_default=TRUE) %>%
  single_out(-max_shape_depth, .keep_benchmark = TRUE)

bench.sw.pretend %>%
  mutate(is_min = min(cpu_min) == cpu_min) %>%
  ggplot(aes(x=max_storage_width, y=cpu_min)) + default.theme.t() +
    geom_col(fill='gray') +
    geom_linerange(aes(ymin=cpu_min,ymax=cpu_max),  color=I('black'), size=.2) +
    scale_x_continuous(expand=c(0,0),
                       breaks=function (l) seq(0,ceiling(l[2]),5),
                       minor_breaks=function (l) seq(0,ceiling(l[2]),1)) +
    geom_point(aes(color = is_min)) +
    scale_color_manual(values = c(NA, 'red'), guide=FALSE) +
    coord_cartesian(ylim=c(0,10000)) +
    facet_wrap(vars(benchmark),labeller=label_parsed,scales='free')

bench.sw.pretend %>%
  mutate(is_min = min(mem_min) == mem_min) %>%
  ggplot(aes(x=max_storage_width, y=mem_min)) + default.theme.t() +
    geom_col(fill='gray') +
    geom_linerange(aes(ymin=mem_min,ymax=mem_max),  color=I('black'), size=.2) +
    scale_x_continuous(expand=c(0,0),
                       breaks=function (l) seq(0,ceiling(l[2]),5),
                       minor_breaks=function (l) seq(0,ceiling(l[2]),1)) +
    geom_point(aes(color = is_min)) +
    scale_color_manual(values = c(NA, 'red'), guide=FALSE) +
    coord_cartesian(ylim=c(0,1.7e6)) +
    facet_wrap(vars(benchmark),labeller=label_parsed,scales='free')
"

# hist(bench.minimals$max_shape_depth, breaks=diff(range(bench.minimals$max_shape_depth)))
#
# hist(bench.minimals$max_storage_width, breaks=diff(range(bench.minimals$max_storage_width)))
#
# with(bench.minimals %>% filter(!niladic),
#      hist(max_storage_width, breaks=diff(range(max_storage_width))))

##plot(locfit(cpu~lp(max_shape_depth,max_storage_width,substitution_threshold,scale=TRUE,nn=.1),data=dat),col.regions=hcl.colors(diff(round(range(cpu)))*2,rev=TRUE))
# plot(locfit(cpu~lp(max_shape_depth,max_storage_width,substitution_threshold,scale=TRUE,nn=.1),data=dat,family='qgamma',link='identity',kt='prod'),col.regions=hcl.colors(diff(round(range(dat$cpu)))*2,rev=TRUE))
##plot(locfit(cpu~lp(max_shape_depth,max_storage_width,substitution_threshold,scale=TRUE,nn=.1),data=dat,family='qrgamma'),col.regions=hcl.colors(diff(round(range(cpu)))*2,rev=TRUE))

# .l <-
# with(bench.dw, {
#   #.ll <- locfit(cpu~lp(max_shape_depth,max_storage_width,scale=TRUE,nn=.1),family='qgamma',link='identity')
#   .ll <- locfit(cpu~lp(max_shape_depth,max_storage_width,scale=TRUE,nn=.1))
#   .lp <- preplot(.ll, newdata=list(seq(2,25),seq(2,25)), get.data=TRUE)
#   plot(.lp,
#        type='image',
#        col=hcl.colors(diff(round(range(cpu)))*2,rev=TRUE))
#   list(locfit=.ll,preplot=.lp)})

dat <- bench.dw %>% filter(benchmark == "map[n]")
"
dat <- bench.dw %>% filter(benchmark == 'tree[E]')
dat <- bench.dw %>% filter(benchmark == 'tree[n]')
dat <- bench.dw %>% filter(benchmark == 'append[E]')
dat <- bench.dw %>% filter(benchmark == 'append[n]')
dat <- bench.dw %>% filter(benchmark == 'map[E]')
dat <- bench.dw %>% filter(benchmark == 'map[n]')
dat <- bench.dw %>% filter(benchmark == 'reverse[E]')
dat <- bench.dw %>% filter(benchmark == 'reverse[n]')
dat <- bench.dw %>% filter(benchmark == 'filter[E]')
dat <- bench.dw %>% filter(benchmark == 'filter[n]')
#dat <- bench.dw %>% filter(benchmark == 'arbitraty_precision_ints')
"

bench2lfpreplot <- function(.data, criterion='cpu') {
  #frml <- as.formula(paste0(criterion, '~lp(max_shape_depth,max_storage_width,scale=TRUE,nn=.1)'))
  #.ll <- locfit(frml, family='qgamma',link='identity', data=.data)
  .ll <- if(criterion=='mem')
    locfit(mem~lp(max_shape_depth,max_storage_width,scale=TRUE,nn=.1), data=.data, mint=50, maxit=50)
  else
    locfit(cpu~lp(max_shape_depth,max_storage_width,scale=TRUE,nn=.1), data=.data, mint=50, maxit=50)

  #.ll$call$formula <- frml
  # .ll$call$data <- match.call()$.data
  .lm <- locfit.matrix(.ll,data=.data)
  .lp <- preplot(.ll, newdata=list(seq(2,25),seq(2,25)), what='coeff',where='grid')
  .lp$data <- .lm
  .lp
}


lfpreplot2tibble <- function (.preplot) {
  original <- as_tibble(.preplot$data$x) %>%
    mutate_all(as.integer) %>%
    mutate(original=TRUE,value=.preplot$data$y)
  result <- expand_grid(
    !!.preplot$vnames[2] := .preplot$xev[[2]],
    !!.preplot$vnames[1] := .preplot$xev[[1]]
  ) %>%
    mutate(!!.preplot$yname := .preplot$trans(.preplot$fit))
  result %<>% left_join(original, by= c("max_shape_depth", "max_storage_width"))
  # retain the original values??
  #result %<>% mutate(!!.preplot$yname := coalesce(value,!!as.name(.preplot$yname)))
  result
}

"
base_family='Helvetica'
rm(base_family)
"

.explore.raster <- function(.data, criterion, aspect, frac, barwidth, bartitle=waiver(), text=FALSE, breaks=waiver(), labels=waiver(), color_option='D') {

  topk <- .data %>% top_frac(-frac,!!sym(criterion)) %>% mutate(topk=TRUE)
  .data %<>% left_join(topk,by=colnames(.data))

  .b <- function(limits) seq(ceiling(limits[1]),floor(limits[2]))
  .l <- function(x) {
    br <- scales::extended_breaks()(range(x))
    r <- rep("",length(x))
    r[c(1,length(r))] <- x[c(1,length(x))]
    r[x %in% br] <- x[x %in% br]
    r
  }
  p <- ggplot(data=.data,aes(x=max_shape_depth,y=max_storage_width)) +
    default.theme.t(fakeLegend = TRUE, axis.title.y =  element_blank(), plot.margin = unit(c(1,-0.1,0,-0.1),"mm"),) +
    # Thank you, Mac OS X, for being useless...
    # geom_raster(aes_string(fill=criterion)) +
    geom_tile(aes_string(fill=criterion)) +
    geom_point(aes(color=topk),
               size=1,shape=16,
               na.rm=TRUE,show.legend=FALSE,position=position_nudge(x = 0.25, y = 0.25)) +
    scale_color_manual(values=c(Set1Paired[1])) +
    scale_fill_viridis_c(direction = -1,option=color_option,breaks=breaks,labels=labels) +
    scale_x_continuous(expand=c(0,0),breaks=.b,labels=.l) +
    scale_y_continuous(expand=c(0,0),breaks=.b,labels=.l)+
    guides(fill = guide_colourbar(barwidth = unit(barwidth,'npc'), label.position = 'top',title = bartitle)) +
    ylab('') +
    theme(legend.position = 'top') +
    coord_fixed()+
    facet_null()
  if (text) {
    p <- p + geom_shadowtext(aes(label=Label),
                             lineheight=1,size=1.8, nudge_y=-.1,
                             na.rm = TRUE, color='grey95')
  }
  save.plot(basename=input.basename,aspect=aspect,plot=p,base_asp=0.9,base_height=5)

}
explore.raster <- function(.data, criterion='Time', tick.unit='s',aspect=c('cpu','NONE')) {

  b <- as.character(aspect[2]) %>% gsub('\\[(.+)\\]','-\\1', .)
  .aspect <- paste(aspect[1],b,sep='-')

  .data %>%
    .explore.raster(criterion, aspect=.aspect,
                    frac=.01,barwidth=0.8,text=TRUE,
                    labels=function (x) paste(x, tick.unit))
}

explore.raster.merge <- function(.data, criterion='cpu', aspect=c('cpu','NONE'),option='C') {
  .data %>%
    .explore.raster(criterion, aspect=paste(aspect,collapse='-'),
                    frac=.02, barwidth=0.6, bartitle=NULL,
                    breaks=identity, labels=c('favorable','unfavorable'),
                    color_option=option)
}

bench.dw %>%
  group_modify(~ lfpreplot2tibble(bench2lfpreplot(.x, criterion='cpu'))) %>%
  group_walk(function(.data, groups) {
    .data %>%
      mutate(Time=round(cpu),Label=round(value)) %>%  #ms
      explore.raster('Time', 'ms', aspect=c('cpu', as.character(groups$benchmark[1])))
  }) %>% invisible


bench.dw %>%
  group_modify(~ lfpreplot2tibble(bench2lfpreplot(.x, criterion='mem'))) %>%
  group_walk(function(.data, groups) {
    .data %>%
      mutate(Memory=round(mem/1024),Label=round(value/1024)) %>%  #kbyte->mbyte
      explore.raster('Memory', 'MB', aspect=c('mem', as.character(groups$benchmark[1])))
  }) %>% invisible


#handle reverse[n] extra.

bench.dw.rn <- bench.dw.orig %>% filter(benchmark=='reverse[n]')
bench.dw.rn %>%
  bench2lfpreplot(criterion='cpu') %>% lfpreplot2tibble %>%
  mutate(Time=round(cpu/1000),Label=round(value/1000,digits = 1)) %>% #s
  explore.raster('Time', 's', aspect=c('X-cpu', 'reverse[n]'))

bench.dw.cpu <- bench.dw %>%
  group_by(niladic,benchmark) %>%
    group_modify(~ lfpreplot2tibble(bench2lfpreplot(.x, criterion='cpu'))) %>%
    group_modify(function(.data,group) .data %>% mutate(cpu=scales::rescale(cpu))) %>%
  group_by(niladic,max_storage_width,max_shape_depth) %>%
    summarize(cpu=sum(cpu)) %>%
  group_by(niladic) %>%
    group_modify(function(.data,group) .data %>% mutate(cpu=scales::rescale(cpu))) %>%
    group_walk(function(.data,groups)
      .data %>%
        explore.raster.merge('cpu', aspect=c('cpu', if_else(groups$niladic, 'E', 'n')))
    ) %>% invisible


bench.dw.mem <- bench.dw %>%
  group_by(niladic,benchmark) %>%
  group_modify(~ lfpreplot2tibble(bench2lfpreplot(.x, criterion='mem'))) %>%
  group_modify(function(.data,group) .data %>% mutate(mem=scales::rescale(mem))) %>%
  group_by(niladic,max_storage_width,max_shape_depth) %>%
  summarize(mem=sum(mem)) %>%
  group_by(niladic) %>%
  group_modify(function(.data,group) .data %>% mutate(mem=scales::rescale(mem))) %>%
  group_walk(function(.data,groups)
    .data %>%
      explore.raster.merge('mem', aspect=c('mem', if_else(groups$niladic, 'E', 'n')))
  ) %>% invisible



bench.dw.nilladic <- full_join(
  bench.dw.cpu, bench.dw.mem, c("niladic", "max_storage_width", "max_shape_depth")
) %>%
  group_by(niladic,max_storage_width,max_shape_depth) %>%
  mutate(value=cpu+mem,cpu=NULL,mem=NULL) %>%
  group_by(niladic) %>%
  group_modify(function(.data,group) .data %>% mutate(value=scales::rescale(value))) %>%
  group_walk(function(.data,groups)
    .data %>%
      explore.raster.merge('value', aspect=c(if_else(groups$niladic, 'E', 'n')))
  ) %>% invisible


result <- bench.dw.nilladic %>%
  # pronounce non-niladic
  mutate(value=ifelse(niladic,value,value*3)) %>%
  group_by(max_storage_width,max_shape_depth) %>%
  summarize(value=sum(value)) %>% ungroup %>%
  mutate(value=scales::rescale(value)) %>%
  (function(.data) {
    explore.raster.merge(.data, 'value', aspect=c('result'),option='B')
    .data
  })

#
# EOF
