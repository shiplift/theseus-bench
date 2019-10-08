#! /usr/bin/env Rscript

input_name.default <- "output/explore.tsv"
"#
input_name.default <- 'output/20140307-cache-envs.tsv'
input_name.default <- 'output/20140524-explore.tsv'
input_name.default <- 'output/20140605-explore.tsv'
input_name.default <- 'output/20190921-explore.tsv'
input_name.default <- 'output/20190924-explore.tsv'
input_name.default <- 'output/20191002-explore.tsv'
input_name.default <- 'output/20191003-explore.tsv'
input_name.default <- 'output/20191007-explore.tsv'
input_name.default <- 'output/20191008-explore.tsv'
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


bench.s <- read_tsv(input_name, comment='#', col_names=rebench.col_names, col_types=rebench.col_types)

"
bench.s <- read_tsv(clipboard(), comment='#', col_names=rebench.col_names, col_types=rebench.col_types)
"

# --- shaping data
bench.s %<>% rename(max_shape_depth=cores, max_storage_width=variable_values, substitution_threshold=input_sizes)

if (nrow(bench.s %>% filter(invocation > 1))) {
  bench.s %<>%
    group_by(vm, benchmark, max_shape_depth, max_storage_width,substitution_threshold,criterion) %>%
    top_n(-1, invocation) %>%
    ungroup
}

bench <- bench.s %>%
  select(vm, benchmark, max_shape_depth, max_storage_width,substitution_threshold,criterion, value) %>%
  spread_measurements %>%
  filter(max_shape_depth<30&max_storage_width<34) %>% #old data
  factorize(.add_visuals = FALSE) %>%
  select(-vm,-gc) %>%
  group_by(benchmark)



# n.outliers <- rosnerTest(dat$cpu,k=min(10, floor(length(dat$cpu) * 0.1)),warn=FALSE)$n.outliers
# cap <- sort(dat$cpu,decreasing=TRUE)[n.outliers+1]


if (FALSE) {
dat <- bench %>% filter(benchmark=='reverse[E]')
"
dat <- bench %>% filter(benchmark=='reverse[n]')
dat <- bench %>% filter(benchmark=='map[E]')
dat <- bench %>% filter(benchmark=='map[n]')
dat <- bench %>% filter(benchmark=='append[E]')
dat <- bench %>% filter(benchmark=='append[n]')
dat <- bench %>% filter(benchmark=='filter[E]')
dat <- bench %>% filter(benchmark=='filter[n]')
dat <- bench %>% filter(benchmark=='tree[E]')
dat <- bench %>% filter(benchmark=='tree[n]')
"
"
dat %<>% filter(substitution_threshold < 100) # ¯\_(ツ)_/¯
dat %<>% filter(cpu < 15000)
dat %<>% filter(cpu < 8000)
"
with(dat, scatter3Drgl(x=max_shape_depth, y=max_storage_width, z=10*log10(substitution_threshold),
                    xlab='max shape depth',ylab='max storage width', zlab='threshold',
                    ticktype='detailed', nticks=30, cex=3,
                    colvar=cpu,col=viridis::viridis(colorrange(cpu), direction=-1)))
# add another point
scatter3Drgl(x = 0, y = 0, z = 0, add = TRUE, colkey = FALSE,
          pch = 18, cex = 3, col = "black")

with(dat, scatter3Drgl(max_shape_depth, max_storage_width, 10*log10(substitution_threshold),
                       xlab='max shape depth',ylab='max storage width', zlab='threshold',
                       ticktype='detailed', nticks=30, cex=3,
                       colvar=mem, col=viridis::viridis(colorrange(mem), direction=-1)))
# add another point
scatter3Drgl(x = 0, y = 0, z = 0, add = TRUE, colkey = FALSE,
             pch = 18, cex = 3, col = "black")

# .m <- dat %>% single_out(-substitution_threshold) %>% matrix_for_criterion('cpu','min')
.m <- dat %>% ungroup %>%  select(-substitution_threshold,-benchmark) %>% matrix_for_criterion('cpu','')
#.m <- dat %>% mutate(substitution_threshold=10*log10(substitution_threshold)) %>%  single_out(-max_storage_width) %>% matrix_for_criterion('cpu','max')
hist3Drgl(z=.m,col=viridis::viridis(colorrange(.m), direction=-1))

bench %>% filter(cpu < 15000) %$% hist(cpu)
}

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
"
base_family='Helvetica'
"
"
bench.optimals.c %>%
  pivot_longer(c('max_shape_depth','max_storage_width','substitution_threshold'), names_to='parameter') %>%
  ggplot(aes(
    x=parameter,y=value
  )) + default.theme.t(fakeLegend = FALSE) +
  geom_boxplot() +
  geom_dotplot(binaxis='y',
               stackdir='center',
               dotsize = .5,
               fill='red') +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,NA),
                     breaks=function(x) { seq(floor(x[[1]]),ceiling(x[[2]]),1)},
                     minor_breaks = NULL) +
  facet_wrap(criterion~.)
"
"=-=-=-="


#
# bench.models <- bench %>% group_modify(function (x,y) {
#   .mc <- lm(cpu~max_shape_depth + max_storage_width + substitution_threshold, data=x)
#   .mm <-lm(mem~max_shape_depth + max_storage_width + substitution_threshold, data=x)
#   tribble(
#     ~model_cpu, ~anova_cpu, ~model_mem, ~anova_mem,
#     .mc, anova(.mc), .mm, anova(.mm))
# })
#
# bench.models.p <- bench.models %>% group_modify(function (x, y) {
#   cd <- x$anova_cpu[[1]]$`Pr(>F)`[1]
#   cw <- x$anova_cpu[[1]]$`Pr(>F)`[2]
#   ct <- x$anova_cpu[[1]]$`Pr(>F)`[3]
#   md <- x$anova_mem[[1]]$`Pr(>F)`[1]
#   mw <- x$anova_mem[[1]]$`Pr(>F)`[2]
#   mt <- x$anova_mem[[1]]$`Pr(>F)`[3]
#   tribble(
#     ~p_cpu_max_shape_depth,
#     ~p_cpu_max_storage_width,
#     ~p_cpu_substitution_threshold,
#     ~p_mem_max_shape_depth,
#     ~p_mem_max_storage_width,
#     ~p_mem_substitution_threshold,
#     cd,cw,ct,md,mw,mt)})


# TODO: print model results
# forall bench: non-correlation hypothesis cannot be rejected for any substitution_threshold.


# bench.thres.best <- bench %>% group_by(benchmark,max_shape_depth,max_storage_width) %>% arrange(cpu) %>% top_n(-1,cpu) %>% ungroup()
# bench.thres.worst <- bench %>% group_by(benchmark,max_shape_depth,max_storage_width) %>% arrange(-cpu) %>% top_n(1,cpu) %>% ungroup()
#
# bench.thres.best <- bench %>% filter(substitution_threshold < 100) %>% group_by(benchmark,max_shape_depth,max_storage_width) %>% arrange(cpu) %>% top_n(-1,cpu) %>% ungroup()
# bench.thres.worst <- bench %>% filter(substitution_threshold < 100) %>% group_by(benchmark,max_shape_depth,max_storage_width) %>% arrange(-cpu) %>% top_n(1,cpu) %>% ungroup()

#--- >> -----
#bench.dw <- bench %>% single_out(-substitution_threshold, .keep_benchmark = TRUE, .make_min_default = TRUE)
#bench.dw <- bench %>% single_out(-substitution_threshold, .keep_benchmark = TRUE, .make_median_default = TRUE)
# bench.dw <- bench %>%  filter(substitution_threshold==13) %>% select(-substitution_threshold)
# bench.dw <- bench %>%  filter(substitution_threshold==5) %>% select(-substitution_threshold)
# bench.dw <- bench %>%  filter(substitution_threshold==21) %>% select(-substitution_threshold)
bench.dw <- bench %>%  select(-substitution_threshold)
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

bench.cpu <- bench.dw %>% arrange(cpu, .by_group=TRUE) %>% top_frac(-.05,cpu)
bench.mem <- bench.dw %>% arrange(mem, .by_group=TRUE) %>% top_frac(-.05,mem)

bench.minimals <- rbind(
  bench.cpu %>% mutate(criterion='cpu'),
  bench.mem %>% mutate(criterion='mem')) %>%
  mutate(criterion=as_factor(criterion))

"
base_family='Helvetica'
"
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
               fill='red') +
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

"
dat <- bench.dw %>% filter(benchmark == "map[n]")
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

# bench2lfpreplot <- function(.data, criterion='cpu') {
#   #frml <- as.formula(paste0(criterion, '~lp(max_shape_depth,max_storage_width,scale=TRUE,nn=.1)'))
#   #.ll <- locfit(frml, family='qgamma',link='identity', data=.data)
#   .ll <- if(criterion=='mem')
#     locfit(mem~lp(max_shape_depth,max_storage_width,scale=TRUE), data=.data, mint=50, maxit=50)
#   else
#     locfit(cpu~lp(max_shape_depth,max_storage_width,scale=TRUE), data=.data, mint=50, maxit=50)
#   # .ll <- if(criterion=='mem')
#   #   locfit(mem~lp(max_shape_depth,max_storage_width,scale=TRUE), family='qgamma',link='identity', data=.data, mint=50, maxit=50)
#   # else
#   #   locfit(cpu~lp(max_shape_depth,max_storage_width,scale=TRUE), family='qgamma',link='identity', data=.data, mint=50, maxit=50)
#
#   #.ll$call$formula <- frml
#   # .ll$call$data <- match.call()$.data
#   .lm <- locfit.matrix(.ll,data=.data)
#   .lp <- preplot(.ll, newdata=list(seq(2,25),seq(2,25)), what='coeff',where='grid')
#   .lp$data <- .lm
#   .lp
# }
#
#
# lfpreplot2tibble <- function (.preplot) {
#   original <- as_tibble(.preplot$data$x) %>%
#     mutate_all(as.integer) %>%
#     mutate(original=TRUE,
#            !!.preplot$yname := .preplot$data$y,
#            value=.preplot$data$y)
#   # result <- expand_grid(
#   #   !!.preplot$vnames[2] := .preplot$xev[[2]],
#   #   !!.preplot$vnames[1] := .preplot$xev[[1]]
#   # ) %>%
#   #   mutate(!!.preplot$yname := .preplot$trans(.preplot$fit))
#   # result %<>% left_join(original, by= c("max_shape_depth", "max_storage_width"))
#   # retain the original values??
#   # result %<>% mutate(!!.preplot$yname := coalesce(value,!!as.name(.preplot$yname)))
#   # result
#   original
# }

#bench2grid <- function(.data, criterion='cpu') .data %>% bench2lfpreplot(criterion) %>% lfpreplot2tibble

bench2grid <- function(.data, criterion='cpu') {
  result <- expand_grid(max_storage_width=2:23, max_shape_depth=2:23) %>% mutate(!!criterion := as.numeric(NA))
  original <- .data %>% transmute(
    max_shape_depth=max_shape_depth,max_storage_width=max_storage_width,
    original=TRUE,value=!!(as.name(criterion)))
  result %<>% left_join(original, by= c("max_shape_depth", "max_storage_width"))
  result %<>% mutate(!!criterion := coalesce(value,!!as.name(criterion)))
  result
}

"
base_family='Helvetica'
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
  .pos.point <- if (text) position_nudge(x = 0.25, y = 0.25) else position_identity()
  p <- ggplot(data=.data, aes(x=max_shape_depth,y=max_storage_width)) +
    default.theme.t(fakeLegend = TRUE, omit_x_title=TRUE, axis.title.y =  element_blank(), plot.margin = unit(c(1,-0.1,0,-0.1),"mm"),) +
    # Thank you, Mac OS X, for being useless...
    # geom_raster(aes_string(fill=criterion)) +
    geom_tile(aes(fill=!!(as.name(criterion)))) +
    geom_point(aes(color=topk), size=1.5, stroke=0,
               shape=16, na.rm=TRUE,show.legend=FALSE,position=.pos.point) +
    scale_color_manual(values=c(Set1Paired[1])) +
    scale_fill_viridis_c(direction = -1,option=color_option,breaks=breaks,labels=labels,na.value='grey95') +
    scale_x_continuous(expand=c(0,0),breaks=.b,labels=.l,minor_breaks=NULL) +
    scale_y_continuous(expand=c(0,0),breaks=.b,labels=.l,minor_breaks=NULL)+
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
                    frac=.02,barwidth=0.8,text=TRUE,
                    labels=function (x) paste(x, tick.unit))
}

explore.raster.merge <- function(.data, criterion='cpu', aspect=c('cpu','NONE'),option='C') {
  .data %>%
    .explore.raster(criterion, aspect=paste(aspect,collapse='-'),
                    frac=.01, barwidth=0.6, bartitle=NULL,
                    breaks=identity, labels=c('favorable','unfavorable'),
                    color_option=option)
}

# ----- print all the things! -----

bench.dw %>%
  group_modify(~ bench2grid(.x, criterion='cpu')) %>%
  group_walk(function(.data, groups) {
    .data %>%
      mutate(Time=round(cpu),Label=round(value)) %>%  #ms
      explore.raster('Time', 'ms', aspect=c('cpu', as.character(groups$benchmark[1])))
  }) %>% invisible


bench.dw %>%
  group_modify(~ bench2grid(.x, criterion='mem')) %>%
  group_walk(function(.data, groups) {
    .data %>%
      mutate(Memory=round(mem/1024),Label=round(value/1024)) %>%  #kbyte->mbyte
      explore.raster('Memory', 'MB', aspect=c('mem', as.character(groups$benchmark[1])))
  }) %>% invisible


#handle reverse[n] extra.

bench.dw.rn <- bench.dw.orig %>% filter(benchmark=='reverse[n]')
bench.dw.rn %>%
  bench2grid(criterion='cpu') %>%
  mutate(Time=round(cpu/1000),Label=round(value/1000,digits = 1)) %>% #s
  explore.raster('Time', 's', aspect=c('X-cpu', 'reverse[n]'))

bench.dw.cpu <- bench.dw %>%
  group_by(niladic,benchmark) %>%
    group_modify(~ bench2grid(.x, criterion='cpu')) %>%
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
  group_modify(~ bench2grid(.x, criterion='mem')) %>%
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


a <- bench.dw %>%
  group_by(niladic,benchmark) %>%
  group_modify(~ bench2grid(.x, criterion='cpu'))

b <- bench.dw %>%
  group_by(niladic,benchmark) %>%
  group_modify(~ bench2grid(.x, criterion='mem'))

View(
  b %>% group_modify(function(doet,g) {
  doet %<>% filter(!is.na(value))
  winner <- doet %>% arrange(value) %>% top_frac(-.01, value) %>% pull %>% mean
  looser <- doet %>% pull %>% max
  delt <- diff(c(winner,looser))
  m <- doet %>% pull %>% mean
  #,delt=diff(winner,looser)
  # catastrophe <- .data %>% filter(log10(value) > (ceiling(log10(winner)+1)))
  # catastrophe %>% mutate(winner=winner)
  tibble(winner=winner,looser=looser,m=m,v=sd(doet$value),delt=delt)
})
)
#
# EOF
