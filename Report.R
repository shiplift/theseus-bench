#! /usr/bin/env Rscript

# figure.width <- 3.5
# figure.height <- 2
figure.width <- 16/cm(1) # (28 picas +  (28*0.25) picas + (3*11) point)  in cm
figure.height <- 11.5/cm(1) # 28 pica in cm ~ 11.85 -> 11.5


tsv_name.default <- "output/current.tsv"
"#
tsv_name.default <- 'output/20190803-nanobenches-all.tsv'
tsv_name.default <- 'output/20190807-nanobenches-all.tsv'
tsv_name.default <- 'output/20190828-nanobenches-all.tsv'
tsv_name.default <- 'output/20190829c-nanobenches-all.tsv'
tsv_name.default <- 'output/20190902-nanobenches-all.tsv'
"#

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

if (FALSE) {
  setwd("/Users/tobias/dev/pypy/lamb-bench")
  cmd.line <- FALSE
} else {
  cmd.line <- TRUE
}

pkgs = c(
  "EnvStats",
  "tidyverse",
  "magrittr",
  "boot",
  "Hmisc",
  #"ggnewscale",
  "tools",
  "cowplot",
  "ggpubr"
)

source("./help.R")
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


print(paste0(">> ", input.basename))


bench <- read_benchmark(tsv_name)

bench.summary.s <- bench %>% benchmark_summarize


#--------------------------------------------------------------------------------------------------------
"
bench.summary <- bench.summary.s %>% as_tibble
bench.summary <- bench.summary.s %>% filter(vm %ni% c('SMLNJ', 'MLton', 'OCaml', 'Python'))
bench.summary <- bench.summary.s %>% filter(grepl('^RSqueak', vm))
bench.summary <- bench.summary.s %>% filter(vm %ni% c('SMLNJ', 'MLton', 'OCaml', 'Python','PyPy','Squeak','Racket','RSqueak (original)', 'RSqueak (optimized)'))
"

bench.summary <- bench.summary.s %>% filter(vm %ni% c('SMLNJ', 'MLton', 'OCaml', 'Python','PyPy','Squeak','Racket'))

dodge <- position_dodge(width=.8)
.palette <- "Set1"
.palette <- "Paired"
.direction=1
.direction=-1

process_executiontime <- function(dat, basename='plot', omitLegend=TRUE) {

  dat %<>% as_tibble
  "
  dat <- dat %>% filter(vm %ni% c('SMLNJ','Python'))
  dat <- dat %>% filter(benchmark %in% c('tree'))
  "

  dat.max <- dat %>% ungroup %>%
    summarise_at(vars(matches('(gc|cpu|total)_max')), ~max(.,na.rm=TRUE)) %>%
    max
  dat.mean <- dat %>% ungroup %>%
    summarise_at(vars(matches('(gc|cpu|total)_mean')), ~geomean(.,na.rm=TRUE)) %>%
    gather %>% deframe %>% geomean
  dat.maxima <- dat %>% ungroup %>%
    select(matches('(gc|cpu|total)_max')) %>%
    gather %>% pull

  n.outliers <- rosnerTest(dat.maxima,k=min(10, floor(length(dat.maxima) * 0.1)),warn=FALSE)$n.outliers

  CAPPING <- sort(dat.maxima,decreasing=TRUE)[n.outliers + 1]

  if (dat.mean > 1000) {
    .scale = 1/1000
    .ylab <- "Execution time (s)"
  } else {
    .scale = 1
    .ylab <- "Execution time (ms)"
  }
  .y.icon.max <- 100
  .y.max.steps <- 1000

  dat %<>% ungroup %>%
    mutate(cappingInfo=makeCappingInfo(., c('cpu', 'gc', 'total'), CAPPING)) %>%
    group_by(benchmark, vm)

  ymax <- ceiling_steps(dat.max, .y.max.steps) * .scale
  p <- ggplot(data = dat,
              aes(x=benchmark,y=cpu_mean*.scale,group=interaction(benchmark,vm),fill=vm,)
  ) + default.theme.t(fakeLegend=omitLegend) +
    geom_col(position=dodge, width=.75)+
    geom_errorbar(aes(ymin=(cpu_mean - cpu_err095) * .scale, ymax=(cpu_mean + cpu_err095) * .scale),  position=dodge, color=I("black"), size=.2, width=.6) +
    scale_y_continuous(
      labels=function(x) {paste0('  ', x)},
        breaks=seq(0, ymax, .y.max.steps * .scale),
        limits=c(0,ymax),
        expand=c(0,0)) +
    geom_col(position=dodge, width=.75, fill="white", alpha=0.25, aes(y=gc_mean*.scale))+
    geom_errorbar(aes(ymin=(gc_mean - gc_err095) * .scale, ymax=(gc_mean + gc_err095) * .scale),  position=dodge, color="black", alpha=.5, size=.2, width=.6) +
    scale_fill_brewer(name = "Implementation", type="qual", direction=.direction, palette=.palette) +
    # xlab("Benchmark") +
    geom_point(position=dodge,aes(y=max(.y.icon.max*.scale,min(cpu_mean*.scale)), shape=vm),size=2, color="grey90",stat="identity") +
    scale_shape(name = "Implementation", solid = FALSE) +
    ylab(.ylab) +
    coord_cartesian(ylim=c(0,ceiling(CAPPING*.scale))) +
    facet_null()
  if (.capped){
    p <- p +
      geom_text(position=position_dodge(width=.9), angle=90,aes(y=((CAPPING * .scale) *.9), label=cappingInfo), size=2)
  }

  if (omitLegend) {
    legend <- get_legend(p + theme(
      legend.position = c(0,1),
      legend.direction = "vertical",legend.justification=c(0,1) ,legend.box.just = "top",
      legend.box.margin = margin(0, 0, 0, 0)))
      #
    gg.file <- paste0(basename, "-cpu-legend.pdf")
    ggsave2(gg.file, plot=legend, colormodel='rgb')
    embed_fonts(gg.file, options=pdf.embed.options)
    p <- p + theme(legend.position="none")
  }
  p


  gg.file <- paste0(basename, "-cpu.pdf")
  ggsave(gg.file, width=figure.width, height=figure.height, units=c("in"), colormodel='rgb', useDingbats=FALSE)
  embed_fonts(gg.file, options=pdf.embed.options)

  #gg.file <- paste0(basename, "-cpu-pic.tex")
  #ggsave(gg.file, device=tikz, width=figure.width, height=figure.height, units=c("in"))
  p
}



process_memory <- function(dat, basename='plot') {

  dat %<>% as_tibble
  "
  dat %<>% filter(vm %ni% c('SMLNJ','Python'))
  "
  dat.max <- dat %>% ungroup %>% summarise_at(vars(matches('mem_max')), ~max(.,na.rm=TRUE)) %>% deframe
  dat.maxima <- dat %>% ungroup %>% .$mem_max

  n.outliers <- rosnerTest(dat.maxima,k=min(10, floor(length(dat.maxima) * 0.1)),warn=FALSE)$n.outliers
  CAPPING <- if (n.outliers <= 0) { Inf } else { sort(dat.maxima,decreasing=TRUE)[n.outliers + 1] }

  dat %<>% ungroup %>%
    mutate(cappingInfo=makeCappingInfo(., c('mem'), CAPPING)) %>%
    group_by(benchmark, vm)

  .viewscale = 1e-6
  .scale = 1
  .y.icon.max <- 750
  #.y.max.steps <- 2.5e5
  .y.max.steps <- 5e5

  ymax <- ceiling_steps(dat.max, .y.max.steps) *.scale
  p <- ggplot(data = dat,
              aes(x=benchmark,y=mem_mean*.scale,group=interaction(benchmark,vm),fill=vm)
  ) + default.theme.t(fakeLegend=TRUE) +
    geom_col(position=dodge, width=.75, aes(fill = vm))+
    #geom_errorbar(aes(ymin=(mem_mean - mem_err095) * .scale, ymax=(mem_mean + mem_err095) * .scale),  position=dodge, color=I("black"), size=.2, width=.6) +
    geom_point(position=dodge,aes(y=max(.y.icon.max,min(mem_mean)), shape=vm),size=2, color="grey90",stat="identity") +
    ylab("Memory consumption (GB)") +
    coord_cartesian() +
    scale_y_continuous(
       #labels=ylabsformat,
       labels=function (x) {x * .viewscale },
       breaks=seq(0,ymax, .y.max.steps),
       limits=c(0,ymax),
      expand=c(0,0)) +
    scale_fill_brewer(name = "Implementation", type="qual", palette=.palette, direction = .direction) +
    scale_shape(name = "Implementation", solid = FALSE) +
    facet_null()
  if (.capped) {
    p <- p +
      geom_text(position=position_dodge(width=.9), angle=90,aes(y=(CAPPING *.9), ymax=ymax*.scale,label=cappingInfo), size=2)
  }
  p <- p + theme(legend.position="none")
  p

  gg.file <- paste0(basename, "-mem.pdf")
  ggsave(gg.file, width=figure.width, height=figure.height, units=c("in"), colormodel='rgb', useDingbats=FALSE)
  embed_fonts(gg.file, options=pdf.embed.options)
  p
}

"

ltx <- function(t,ref,name, X) {
  len <- ncol(t)/2
  .just = rep(c('r',paste0('@{}>{\\smaller\\ensuremath{\\pm}}r@{\\,\\si{', X , '}}')), len)
  .just = c('@{}r', .just[2:length(.just)])
  out <- latex(t
              ,file=name
              ,rowlabel='Benchmark'
              ,rowlabel.just='@{}l'
              ,booktabs=TRUE
              ,table.env=FALSE, center='none'
              ,size='scriptsize'
               ,colheads=rep(c('mean','error'), len)
              ,col.just=.just
              ,cgroup=levels(as.factor(ref$vm))
              ,cdec=rep(0, len*2))
  }


cpu.ref <-data.frame(benchmark=bench.summary$benchmark,
                            vm=droplevels(bench.summary$vm),
                            mean=bench.summary$mean,
                            error=bench.summary$err095)
.c <- dcast(melt(cpu.ref,
                 id.vars = c('benchmark', 'vm')),
            benchmark ~ vm + variable)
cpu <- .c[2:length(.c)]
colnames(cpu) <-  sapply(colnames(cpu), function (x) {sedit(x, '_', ' ')})
rownames(cpu) <- .c$benchmark

ltx(cpu,cpu.ref,paste0(input.basename, '-cpu-numbers.tex'), '\\milli\\second')

mem.ref <-data.frame(benchmark=bench.summary.mem$benchmark,
                            vm=droplevels(bench.summary.mem$vm),
                            mean=bench.summary.mem$mean,
                            error=bench.summary.mem$err095)
.m <- dcast(melt(mem.ref,
                 id.vars = c('benchmark', 'vm')),
            benchmark ~ vm + variable)
mem <- .m[2:length(.m)]
colnames(mem) <-  sapply(colnames(mem), function (x) {sedit(x, '_', ' ')})
rownames(mem) <- .m$benchmark

ltx(mem,mem.ref,paste0(input.basename, '-mem-numbers.tex'), '\\kilo\\byte')


  #'@{\\,\\si{\\mega\\byte}}>{\\smaller\\ensuremath{\\pm}}r@{\\,\\si{\\kilo\\byte}}'
#   f <- (function() {
#     c <- bench.summary[bench.summary$vm != 'SMLNJ' & bench.summary$vm != 'LambUncached',]
#     m <- bench.summary.mem[bench.summary.mem$vm != 'SMLNJ' & bench.summary.mem$vm != 'LambUncached',]
#     m$mean = m$mean / 1024
#     data.frame(benchmark=c$benchmark, vm=droplevels(c$vm), time_mean=c$mean, time_error=c$err095,memory_mean=m$mean, memory_error=m$err095) })()
#
#   g <- dcast(melt(f, id.vars=c('benchmark','vm')), benchmark ~ vm + variable)
#   d <- g[2:length(g)]
#   rownames(d) <- g$benchmark
#   colnames(d) <- sapply(colnames(d), function(x) {sedit(x, '_', ' ')})
#
#
#   (function() {
#     len <- length(d)/2
#     out <- latex(d
#                 ,file=paste0(input.basename, '-numbers.tex')
#                 ,rowlabel='Benchmark'
#                 ,rowlabel.just='@{}l'
#                 ,booktabs=TRUE
#                 ,table.env=FALSE, center='none'
#                 #,size='footnotesize'
#                 ,size='scriptsize'
#                 ,colheads=rep(c('time', '', 'memory', ''), len/2)
#                 ,col.just=rep(c('@{}r','@{}>{\\smaller\\ensuremath{\\pm}}r','@{\\,\\si{\\milli\\second}}r','@{\\,\\si{\\mega\\byte}}>{\\smaller\\ensuremath{\\pm}}r@{\\,\\si{\\kilo\\byte}}'), len/2)
#                 ,cgroup=levels(f$vm)
#                 ,cdec=rep(0, len*2)
#     )})()

"

"make"
process_executiontime(bench.summary, basename=input.basename)
process_memory(bench.summary, basename=input.basename)

print(">> done")
#
# EOF
#
