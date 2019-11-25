#! /usr/bin/env Rscript

# figure.width <- 3.5
# figure.height <- 2
# figure.width <- 16/cm(1) # (28 picas +  (28*0.25) picas + (3*11) point)  in cm
# figure.width <- 14.5/cm(1) # ((27 * 1.2) picas + (2*11) point)  in cm
# figure.height <- 11.5/cm(1) # 28 pica in cm ~ 11.85 -> 11.5


input_name.default <- 'output/current.tsv'
input_name.squeak <- 'output/squeakparadigmtest.tsv'
"#
input_name.default <- 'output/20190803-nanobenches-all.tsv'
input_name.default <- 'output/20190807-nanobenches-all.tsv'
input_name.default <- 'output/20190828-nanobenches-all.tsv'
input_name.default <- 'output/20190829c-nanobenches-all.tsv'
input_name.default <- 'output/20190902-nanobenches-all.tsv'
input_name.default <- 'output/20191008-nanobenches-all.tsv'
input_name.squeak <- 'output/20190902-squeakparadigmtest.tsv'
input_name.default <- 'output/20191011-nanobenches-all.tsv'
input_name.squeak <- 'output/20191011-squeakparadigmtest.tsv'

input_name.default <- 'output/20191018-nanobenches-all.tsv'
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
  "boot",
  #"Hmisc",
  "tools",
  "cowplot",
  "ggpubr",
  NULL
)

source("./help.R")
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


print(paste0('>> ', input.basename))



bench <- read_benchmark(input_name)
"
bench <- read_benchmark(clipboard())
convert_rebench(clipboard(),'output/converted_to.tsv')
stratify_rebench(clipboard(),'output/strat.tsv')
"
bench.summary.s <- bench %>% benchmark_summarize
benches.summary.s <- bench.summary.s %>% benchmark_nest

#--------------------------------------------------------------------------------------------------------
"
bench.summary <- bench.summary.s %>% as_tibble
bench.summary <- bench.summary.s %>% filter(vm %ni% c('SML/NJ', 'MLton', 'OCaml', 'Python'))
bench.summary <- bench.summary.s %>% filter(grepl('^RSqueak', vm))
bench.summary <- bench.summary.s %>% filter(vm %ni% c('SML/NJ', 'MLton', 'OCaml', 'Python','PyPy','Squeak','Racket','RSqueak (original)', 'RSqueak (optimized)'))
"

bench.summary <- bench.summary.s %>% filter(vm %ni% c('SML/NJ', 'MLton', 'OCaml', 'Python','PyPy','Squeak','Racket'))

benches.summary <- benches.summary.s %>% filter(TRUE)

bench.summary.norm <- bench.summary %>% normalize_benches

bench.summary.gc <- bench.summary %>% add_gc_rate

dodge <- position_dodge(width=global_dodgewidth)

process_executiontime <- function(dat, basename='plot', partname=NULL, omitLegend=TRUE, capping=NULL, ...) {

  dat %<>% filter(TRUE)
  "
  dat <- dat %>% filter(vm %ni% c('SML/NJ','Python'))
  dat <- dat %>% filter(benchmark %in% c('tree'))
  "

  dat.max <- dat %>% ungroup %>%
    summarise_at(vars(matches('(gc|cpu|total)_max')), ~max(.,na.rm=TRUE)) %>%
    max
  dat.geomean <- dat %>% ungroup %>%
    summarise_at(vars(matches('(gc|cpu|total)_mean')), ~geomean(.,na.rm=TRUE)) %>%
    gather %>% deframe %>% geomean
  dat.maxima <- dat %>% ungroup %>%
    select(matches('(gc|cpu|total)_max')) %>%
    gather %>%
    filter(!is.na(value)) %>% pull

  CAPPING <- if (!is.null(capping)) capping else {
    n.outliers <- rosnerTest(dat.maxima,k=min(10, floor(length(dat.maxima) * 0.1)),warn=FALSE)$n.outliers
    CAPPING <- if (n.outliers <= 0) { Inf } else { sort(dat.maxima,decreasing=TRUE)[min(n.outliers+2,length(dat.maxima))]}
  }
  .capped <- CAPPING < Inf

  if (dat.geomean > 1000) {
    .scale <- 1/1000
    .ylab <- "Execution time (s)"
  } else {
    .scale = 1
    .ylab <- "Execution time (ms)"
  }
  .y.icon.max <- 200
  .ylim <- if(.capped) { ceiling(CAPPING*.scale) } else { Inf }
  .y.max.steps <- if (.ylim > 15) {2500} else {1000}


  .grouping <- sapply(group_vars(dat),as.name)
  dat %<>% ungroup %>%
    mutate(cappingInfo=makeCappingInfo(., c('cpu', 'gc', 'total'), capping = CAPPING,scale = .scale)) %>%
    group_by(!!!.grouping)

  ymax <- ceiling_steps(dat.max, .y.max.steps) * .scale
  p <- ggplot(data = dat,
              aes(x=benchmark,y=cpu_mean*.scale,group=interaction(benchmark,vm),fill=vm,shape=vm)
  ) + default.theme.t(fakeLegend=omitLegend, omit_x_title=TRUE) +
    geom_col(position=dodge, width=global_colwidth)+
    geom_errorbar(aes(ymin=(cpu_mean - cpu_err095) * .scale, ymax=(cpu_mean + cpu_err095) * .scale),  position=dodge, color=I("black"), size=.2, width=.6) +
    scale_y_continuous(
      trans=timing_trans(low=0,high=ymax,step=.y.max.steps*.scale,viewlimit=c(0,.ylim),scale=.scale),
      # labels=function(x) {paste0(' ', x)},
      #   breaks=seq(0, ymax, .y.max.steps * .scale),
        limits=c(0,ymax),
        expand=c(0,0)) +
    scale_x_discrete(labels = ggplot2:::parse_safe) +
    geom_col(position=dodge, width=global_colwidth, fill="white", alpha=0.25, aes(y=gc_mean*.scale))+
    geom_errorbar(aes(ymin=(gc_mean - gc_err095) * .scale, ymax=(gc_mean + gc_err095) * .scale),  position=dodge, color="black", alpha=.5, size=.2, width=.6) +

    geom_point(position=dodge,aes(y=max(.y.icon.max*.scale,min(cpu_mean*.scale))),size=global_iconsize, color="grey90",stat="identity") +
    scale_fill_manual(name = "Implementation",values=as.character(dat$color)) +
    scale_shape_manual(name = "Implementation",values=dat$shape) +
    coord_cartesian(ylim=(if (.capped) c(0,.ylim) else NULL))  +
    ylab(.ylab)
  if (.capped){
    p <- p +
     geom_text(position=position_dodge(width=.9), angle=90,aes(y=((CAPPING * .scale) *.9), label=cappingInfo, family=base_family), size=2)
  }

  if (omitLegend) {
    p <- p + theme(legend.position="none")
  }

  aspect <- if (!is.null(partname)) { paste0(partname, '-', 'cpu' )} else { 'cpu' }
  save.plot(basename=basename,aspect=aspect,plot=p, ...)
  #gg.file <- paste0(basename, "-cpu-pic.tex")
  #ggsave(gg.file, device=tikz, width=figure.width, height=figure.height, units=c("in"))
  p
}



process_memory <- function(dat, basename='plot', partname=NULL, omitLegend=TRUE, capping=NULL, ...) {

  dat %<>% filter(TRUE)
  "
  dat %<>% filter(vm %ni% c('SML/NJ','Python'))
  "
  dat.max <- dat %>% ungroup %>% summarise_at(vars(matches('mem_max')), ~max(.,na.rm=TRUE)) %>% deframe
  dat.maxima <- dat %>% ungroup %>% .$mem_max

  CAPPING <- if (!is.null(capping)) capping else {
    n.outliers <- rosnerTest(dat.maxima,k=min(10, floor(length(dat.maxima) * 0.1)),warn=FALSE)$n.outliers
    #CAPPING <- if (n.outliers <= 0) { Inf } else { geomean(sort(dat.maxima,decreasing=TRUE)[n.outliers:(n.outliers+1)]) }
    #.capped <- CAPPING < Inf
    if (n.outliers <= 0) { Inf }  else {
      #geomean(sort(dat.maxima,decreasing=TRUE)[n.outliers:(n.outliers+1)])
      sort(dat.maxima,decreasing=TRUE)[min(n.outliers+2,length(dat.maxima))]
    }
  }
  .capped <- (CAPPING < Inf)


  .viewscale = 1e-6
  .scale = 1
  .ylab <- "Memory consumption (GB)"
  .ylim <- if(!.capped) { Inf } else {
    .caporder <- floor(log10(CAPPING))
    10^.caporder * ceiling_steps((CAPPING / 10^.caporder) + 0.24,0.25)
    #.capmag <- (.ylim %/% 10^.caporder) + 0.24
    #ceiling(CAPPING)
  }

  .y.max.steps <- if (log10(.ylim) < 7) {
    2.5e5
  } else {
    5e5
  }
  .y.icon.max <- if (log10(.ylim) - log10(min(dat$mem_mean)) >= 2) {
    min(dat$mem_mean) * .9
  } else if (.ylim < 5e6 ) {
    10^floor(log10(.ylim)) * 0.25
  } else {
    10^floor(log10(.ylim)) * 0.75
  }

  .grouping <- group_vars(dat)
  dat %<>% ungroup %>%
    mutate(cappingInfo=makeCappingInfo(., c('mem'), capping=CAPPING,
                                       scale = .scale, viewscale = .viewscale)) %>%
    group_by(!!!.grouping)


  ymax <- ceiling_steps(dat.max, .y.max.steps) *.scale
  p <- ggplot(data = dat,
              aes(x=benchmark,y=mem_mean*.scale,group=interaction(benchmark,vm),fill=vm,shape=vm)
  ) + default.theme.t(fakeLegend=omitLegend, omit_x_title=TRUE) +
    geom_col(position=dodge, width=global_colwidth)+
    scale_y_continuous(
      trans=mem_trans(low=0,high=ymax,step=.y.max.steps,viewlimit=c(0,.ylim),scale=.scale,viewscale=.viewscale),
      limits=c(0,ymax),
      expand=c(0,0)) +
    scale_x_discrete(labels = ggplot2:::parse_safe) +
    geom_point(position=dodge,aes(y=.y.icon.max),size=global_iconsize, color="grey90",stat="identity") +
    scale_fill_manual(name = "Implementation",values=as.character(dat$color)) +
    scale_shape_manual(name = "Implementation",values=dat$shape) +
    coord_cartesian(ylim=(if(.capped) c(0,.ylim) else NULL)) +
    ylab(.ylab)

  if (mean(dat$mem_err095) * .viewscale > 0.0001) {
    p <- p +
      geom_errorbar(aes(ymin=(mem_mean - mem_err095) * .scale, ymax=(mem_mean + mem_err095) * .scale),  position=dodge, color=I("black"), size=.2, width=.6)
  }
  if (.capped) {
    p <- p +
      geom_text(position=position_dodge(width=.9), angle=90,aes(y=(CAPPING *.9),label=cappingInfo), size=2)
  }
  p <- p + theme(legend.position="none")
  p
  aspect <-if (!is.null(partname)) { paste0(partname, '-', 'mem' )} else { 'mem' }
  save.plot(basename=basename,aspect=aspect,plot=p, ...)
  p
}

plot_normalized <- function(dat, basename='plot', criterion='cpu', partname=NULL, omitLegend=TRUE, ...) {
  .ylab <- ifelse(criterion=='mem', 'Relative memory consumption', 'Relative execution time')
  p <- ggplot(data=dat,
              aes_string(x='benchmark',y=paste0(criterion, '_norm_mean'),group='interaction(benchmark,vm_group)',fill='vm_group',shape='vm_group')
  ) + default.theme.t(fakeLegend=omitLegend, omit_x_title=TRUE) +
    geom_col(position=dodge, width=global_colwidth)+
    geom_errorbar(aes_string(ymin=paste0(criterion, '_norm_lower'), ymax=paste0(criterion, '_norm_upper')),
                  position=dodge, color=I("black"), size=.2, width=.6,na.rm=TRUE) +
    geom_hline(yintercept=1) +
    scale_y_continuous(
      breaks=seq(0, 1, 0.2),
      limits=c(0,NA), expand=expand_scale(add=c(0,0.05))) +
    scale_x_discrete(labels=parse_label) +
    geom_point(position=dodge,aes(y=0.025),size=global_iconsize, color="grey90",stat="identity") +
    scale_fill_manual(name="Implementation",values=as.character(dat$color)) +
    scale_shape_manual(name="Implementation",values=dat$shape) +
    coord_cartesian() +
    ylab(.ylab) +
    facet_grid(cols=vars(overall), scales="free", space="free")
  p <- p + theme(legend.position="none")
  p
  aspect <-if (!is.null(partname)) { paste0(partname, '-', criterion )} else { criterion }
  save.plot(basename=basename,aspect=paste0(aspect,'-norm'),plot=p, ...)
  p
}

plot_gc <- function(dat, basename='plot', partname=NULL, omitLegend=TRUE, ...) {
  p <- ggplot(data=dat,
              aes(x=benchmark,y=gc_rate_mean,group=interaction(benchmark,vm),fill=vm,shape=vm)
  ) + default.theme.t(fakeLegend=omitLegend, omit_x_title=TRUE) +
    geom_col(position=dodge, width=global_colwidth)+
    geom_errorbar(aes(ymin=gc_rate_lower, ymax=gc_rate_upper),
                  position=dodge, color=I("black"), size=.2, width=.6,na.rm=TRUE) +
    geom_hline(yintercept=1) +
    scale_y_continuous(
      breaks=seq(0, 1, 0.2),
      limits=c(0,NA), expand=expand_scale(add=c(0,0.05))) +
    scale_x_discrete(labels=parse_label) +
    geom_point(position=dodge,aes(y=0.025),size=global_iconsize, color="grey90",stat="identity") +
    scale_fill_manual(name="Implementation",values=as.character(dat$color)) +
    scale_shape_manual(name="Implementation",values=dat$shape) +
    coord_cartesian() +
    ylab("GC's share of execution time") +
    facet_null()
  p <- p + theme(legend.position="none")
  p
  aspect <-if (!is.null(partname)) { paste0(partname, '-', 'gc' )} else { 'gc' }
  save.plot(basename=basename,aspect=aspect,plot=p, ...)
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
#     c <- bench.summary[bench.summary$vm != 'SML/NJ' & bench.summary$vm != 'LambUncached',]
#     m <- bench.summary.mem[bench.summary.mem$vm != 'SML/NJ' & bench.summary.mem$vm != 'LambUncached',]
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

"
set_datacontext(input_name)
"

report_vms(file = paste0(input.basename.pure, '-all-legend.pdf'))
report_dflt_vms(file = paste0(input.basename.pure, '-legend.pdf'))
iconize_vms(file = paste0(input.basename.pure, '-vm'))

"base_family <- 'Helvetica'"
process_executiontime(bench.summary, basename=input.basename,capping=1.5e4)
# process_executiontime(bench.summary, basename=input.basename)
process_memory(bench.summary, basename=input.basename,capping=5.2e6)
# process_memory(bench.summary, basename=input.basename)

pwalk(benches.summary, function(benchmark,data){
  dat <- data %>% mutate(benchmark=benchmark)
  process_executiontime(dat, basename=input.basename.parts,  partname=benchmark)
  process_memory(dat, basename=input.basename.parts,  partname=benchmark)
})



for (crit in c('cpu','mem')) plot_normalized(bench.summary.norm, criterion=crit,  basename=input.basename)

plot_gc(bench.summary.gc, basename=input.basename)

"'no good idea anymore'
if(FALSE){
print('>> squeak vs squeak')
set_datacontext(input_name.squeak)
bench.sq <- read_benchmark(input_name.squeak)
bench.sq.summary <- bench.sq %>% benchmark_summarize
process_executiontime(bench.sq.summary, basename=input.basename)
process_memory(bench.sq.summary, basename=input.basename)
}
"


print(">> done")
#
# EOF
#
