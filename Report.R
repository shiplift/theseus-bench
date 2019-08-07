#! /usr/bin/env Rscript

# figure.width <- 3.5
# figure.height <- 2
figure.width <- 7
figure.height <- 4


tsv_name.default <- "output/current.tsv"
"#
tsv_name.default <- 'output/20190803-nanobenches-all.tsv'
"#

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

if (FALSE) {
  setwd("/Users/tobias/dev/pypy/lamb-bench")
  cmd.line <- FALSE
} else {
  cmd.line <- TRUE
}

pkgs = c(
  "reshape2",
  "plyr",
  #"beanplot",
  "boot",
  "Hmisc",
  "ggplot2",
  "tools",
  "quantreg",
  "locfit",
  "dplyr"
)

source("./help.R")
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

if (length(commandArgs(trailingOnly=TRUE)) > 0) {
  if (length(commandArgs(trailingOnly=TRUE)) > 1) {
    CAPPING <- as.numeric(commandArgs(trailingOnly=TRUE)[2])
  } else {
    CAPPING <- Inf
  }
} else {
  CAPPING <- 10000
}

print(paste0(">> ", input.basename))

bench <- read.delim(tsv_name, comment.char = "#", header=FALSE,
                    col.names=c('timestamp', 'value', 'unit', 'criterion', 'benchmark', 'vm', 'suite', 'extra_args', 'warump', 'cores', 'input_sizes', 'variable_values'))

error_processing = (nrow(bench[bench$vm == 'Lamb' & bench$criterion=="total" & bench$benchmark == "map",]) != 1)


# --- shaping data

#bench <- droplevels(bench[c('criterion','vm','benchmark','value', 'unit', 'input_sizes')])
bench <- droplevels(bench[c('criterion','vm','benchmark','value', 'unit')])

# bench <- bench[
#   bench$vm != "SMLNJ" 
#   & bench$vm != "MLton"
#   & bench$vm != "LambUncached"
#   & bench$vm != "OCaml"
#   & bench$vm != "Python"
#   ,]



bench.tot <- droplevels(bench[bench$criterion == 'total',,drop=TRUE])
bench.cpu <- droplevels(bench[bench$criterion == "cpu",,drop=TRUE])
bench.mem <- droplevels(bench[bench$criterion == "mem",,drop=TRUE])
bench.gc  <- droplevels(bench[bench$criterion == "gc",,drop=TRUE])
for (vm in levels(bench$vm)) {
  for (benchmark in levels(bench.gc$benchmark)) {
    if (nrow(bench.gc[bench.gc$vm == vm & bench.gc$benchmark == benchmark,]) <= 0 ) {
      .x <- data.frame(criterion=levels(bench.cpu$criterion)[1],
                 vm=vm,benchmark=benchmark,value=NA,
                 unit=levels(bench.gc$unit)[1])
      bench.gc <- rbind(bench.gc, .x)
      if (error_processing) { for (. in 1:9) bench.gc <- rbind(bench.gc, .x) }
    } else {
      if (sum(bench.gc[bench.gc$vm == vm & bench.gc$benchmark == benchmark,]$value) == 0) {
        bench.gc[bench.gc$vm == vm & bench.gc$benchmark == benchmark,]$value <- NA
      }
    }
  }
}


bench.tree <- bench[(bench$criterion == "cpu" | bench$criterion == "mem") & bench$benchmark == "tree",,drop=TRUE]




#--------------------------------------------------------------------------------------------------------

if (error_processing) {
  print(">> with ep")
  bench.cpu$vm <- factor(bench.cpu$vm, levels = c("Lamb","LambUncached",
                                                  "PycketShapes", "PycketOrig", "Pycket",
                                                  "Racket", 
                                                  "MLton","SMLNJ","OCaml",
                                                  "Python", "Pypy"))
  levels(bench.cpu$vm)[levels(bench.cpu$vm) == "Lamb"] <- "Prototype"
  levels(bench.cpu$vm)[levels(bench.cpu$vm) == "PycketShapes"] <- "Pycket (optimized)"
  levels(bench.cpu$vm)[levels(bench.cpu$vm) == "PycketOrig"] <- "Pycket (original)"
  levels(bench.cpu$vm)[levels(bench.cpu$vm) == "Pypy"] <- "PyPy"
  bench.mem$vm <- factor(bench.mem$vm, levels = c("Lamb","LambUncached",
                                                  "PycketShapes", "PycketOrig", "Pycket",
                                                  "Racket",                                          
                                                  "MLton","SMLNJ","OCaml",
                                                  "Python", "Pypy"))
  levels(bench.mem$vm)[levels(bench.mem$vm) == "Lamb"] <- "Prototype"
  levels(bench.mem$vm)[levels(bench.mem$vm) == "PycketShapes"] <- "Pycket (optimized)"
  levels(bench.mem$vm)[levels(bench.mem$vm) == "PycketOrig"] <- "Pycket (original)"
  levels(bench.mem$vm)[levels(bench.mem$vm) == "Pypy"] <- "PyPy"
  bench.tree$vm <- factor(bench.tree$vm, levels = c("Lamb","LambUncached",
                                                    "PycketShapes", "PycketOrig", "Pycket",
                                                    "Racket",                          
                                                    "MLton","SMLNJ","OCaml",
                                                    "Python", "Pypy"))
  levels(bench.tree$vm)[levels(bench.tree$vm) == "Lamb"] <- "Prototype"
  levels(bench.tree$vm)[levels(bench.tree$vm) == "PycketShapes"] <- "Pycket (optimized)"
  levels(bench.tree$vm)[levels(bench.tree$vm) == "PycketOrig"] <- "Pycket (original)"
  levels(bench.tree$vm)[levels(bench.tree$vm) == "Pypy"] <- "PyPy"
  

  
  #bench.err <- bootstrapTo(bench.tot, 'benchmark', 'vm', 'Racket', 'value')
  bench.summary <- ddply(bench.cpu, .(benchmark,vm), summarise,
                         mean=mean(value),
                         median=median(value),
                         stdev=sd(value),
                         err095=confInterval095Error(value),
                         cnfIntHigh = mean(value) + (confInterval095Error(value)),
                         cnfIntLow = mean(value) - (confInterval095Error(value))
  )
  
  bench.summary.mem <- ddply(bench.mem, .(benchmark,vm), summarise,
                         mean=mean(value),
                         median=median(value),
                         stdev=sd(value),
                         err095=confInterval095Error(value),
                         cnfIntHigh = mean(value) + (confInterval095Error(value)),
                         cnfIntLow = mean(value) - (confInterval095Error(value))
  )
  
  bench.summary.tree <- ddply(bench.tree, .(criterion,vm), summarise,
                             mean=mean(value),
                             median=median(value),
                             stdev=sd(value),
                             err095=confInterval095Error(value),
                             cnfIntHigh = mean(value) + (confInterval095Error(value)),
                             cnfIntLow = mean(value) - (confInterval095Error(value))
  )
  
  yformat <- function(x, max.order, min.order) {
    if (x <= 0 | is.na(x))
      "0"
    else {
      start.vs.min <- x %/% 10^min.order
      start.vs.max <- x %/% 10^max.order
      if (
          (start.vs.min %% 2 == 1)
#           (start.vs.max < 1 & start.vs.min %% 2 == 1) |
#           (start.vs.min %% 5 == 0)
        ) {
        if (x < 1e6) {
          format(x/1e6, scientific=FALSE) 
        } else {
          paste0(as.integer(x/1e6))
        }
      } else {
        ""
      }
    }
  }
  
#   ylabsformat <- function(x) {
#     max.order <- floor(log10(max(x)))
#     min.order <- floor(log10(min(x[x != 0.0])))
#     labels <- sapply(x, function(y) yformat(y, max.order, min.order))
#     labels
#   }


  dat <- bench.summary[bench.summary$vm != "SMLNJ" & bench.summary$vm != "LambUncached",]
  if (nrow(dat) > 0) {
    if (mean(dat$mean) > 1000) {
      .scale = 1/1000
      .ylab <- "Execution time (s)"
    } else {
      .scale = 1
      .ylab <- "Execution time (ms)"
    }
    .y.icon.max <- 100
    .y.max.steps <- 1000
    dat$isCapped <- " "
    .needs.cap <- dat[!is.na(dat$mean) & dat$mean > CAPPING,]
    if (nrow(.needs.cap) > 0) {
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$median <- CAPPING
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$stdev <- 0
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$err095 <- 0
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$cnfIntHigh <- CAPPING
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$cnfIntLow <- CAPPING    
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$isCapped <- paste0("> ", format(.needs.cap$mean * .scale,digits=0))
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$mean <- CAPPING
    }
    
    dodge <- position_dodge(width=.8)
    ymax <- round_any(max(dat$mean), .y.max.steps, ceiling) *.scale
    p <- ggplot(data = dat, aes(x=benchmark,y=mean*.scale,group=interaction(benchmark,vm),fill=vm,)) + 
      geom_bar(stat="identity", position=dodge, width=.75, aes(fill = vm))+
      geom_errorbar(aes(ymin=cnfIntLow*.scale, ymax=cnfIntHigh*.scale),  position=dodge, color=I("black"), size=.2, width=.6) +  
      geom_point(position=dodge,aes(y=max(.y.icon.max*.scale,min(mean*.scale)), ymax=ymax, shape=vm),size=2, color="grey90",stat="identity") +
      # xlab("Benchmark") +
      ylab(.ylab) +
      coord_cartesian() +    
      theme_bw(base_size=8, base_family="Helvetica") +
      theme(
        rect = element_rect(),
        axis.title.x =  element_blank(),
        #   axis.text.x  = element_text(size=8, angle=45, hjust=1),
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(face="bold", size=8),
        axis.text.y  = element_text(size=8), #angle=45, hjust=0.2, vjust=0.5,
        legend.position=c(0.75, .65),
        plot.margin = unit(c(1,-0.1,-2,-1),"mm"),
        legend.text = element_text(size=7),
        legend.title = element_text(size=7, face="bold"),
        legend.background = element_rect(fill="gray90", size=0),
        legend.margin = unit(-.5, "cm"),
        legend.key=element_rect(fill="white"),
        legend.key.size=unit(3,"mm")
      ) +
      scale_y_continuous(
        labels=function(x) {paste0('  ', x)},
          breaks=seq(0, ymax, .y.max.steps * .scale), 
          limits=c(0,ymax),
          expand=c(0,0)) +
      scale_fill_brewer(name = "Implementation", type="qual", palette="Set1") +
      scale_shape(name = "Implementation", solid = FALSE) +
      facet_null()  
    if (nrow(.needs.cap) > 0){
      p <- p + 
        geom_text(position=position_dodge(width=.9), angle=90,aes(y=((CAPPING * .scale) *.9), ymax=ymax*.scale,label=isCapped), size=2) 
    }
    p

    gg.file <- paste0(input.basename, "-cpu.pdf")
    ggsave(gg.file, width=figure.width, height=figure.height, units=c("in"), colormodel='rgb', useDingbats=FALSE)
    embed_fonts(gg.file, options=pdf.embed.options)  
    
        
  }
  
  #dat <- subset(bench.cpu, vm == "ulamb-c" | vm == "ulambi-c")
  dat <- bench.summary.mem[bench.summary.mem$vm != "SMLNJ" & bench.summary.mem$vm != "LambUncached",]
  if (nrow(dat) > 0) {
    #CAPPING = 5e6
    .scale = 1e-6
    .y.icon.max <- 750
    .y.max.steps <- 2.5e5
    dat$isCapped <- " "
    .needs.cap <- dat[!is.na(dat$mean) & dat$mean > CAPPING,]
    if (nrow(.needs.cap) > 0) {
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$median <- CAPPING
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$stdev <- 0
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$err095 <- 0
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$cnfIntHigh <- CAPPING
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$cnfIntLow <- CAPPING    
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$isCapped <- paste0("> ", format(.needs.cap$mean * .scale,digits=2))
      dat[!is.na(dat$mean) & dat$mean > CAPPING,]$mean <- CAPPING
    }
    
    dodge <- position_dodge(width=.8)
    ymax <- round_any(max(dat$mean), .y.max.steps, ceiling)
    p <- ggplot(data = dat, aes(x=benchmark,y=mean,group=interaction(benchmark,vm),fill=vm,)) + 
      geom_bar(stat="identity", position=dodge, width=.75, aes(fill = vm))+
      #geom_errorbar(aes(ymin=cnfIntLow, ymax=cnfIntHigh),  position=dodge, color=I("black"), size=.33) +
      geom_point(position=dodge,aes(y=max(.y.icon.max,min(mean)), ymax=ymax, shape=vm),size=2, color="grey90",stat="identity") +
      ylab("Memory consumption (GB)") +
      coord_cartesian() +    
      theme_bw(base_size=8, base_family="Helvetica") +
      theme(
        rect = element_rect(),
        axis.title.x =  element_blank(),
        #   axis.text.x  = element_text(size=8, angle=45, hjust=1),
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(face="bold", size=8),
        axis.text.y  = element_text(size=8), #angle=45, hjust=0.2, vjust=0.5,
        legend.position=c(0.88, .65),
        plot.margin = unit(c(1,-0.1,-2,-1),"mm"),
        legend.text = element_text(size=7),
        legend.title = element_text(size=7, face="bold"),
        legend.background = element_rect(fill="gray90", size=0),
        legend.margin = unit(-.5, "cm"),
        legend.key=element_rect(fill="white"),
        legend.key.size=unit(3,"mm")
      ) +
      scale_y_continuous(
#         labels=ylabsformat,
        labels=function (x) {x * .scale },
         breaks=seq(0,ymax, .y.max.steps), 
         limits=c(0,ymax),
        expand=c(0,0)) +
      scale_fill_brewer(name = "Implementation", type="qual", palette="Set1") +
      scale_shape(name = "Implementation", solid = FALSE) +
      facet_null()  
    if (nrow(.needs.cap) > 0){
      p <- p + 
        geom_text(position=position_dodge(width=.9), angle=90,aes(y=(CAPPING *.9), ymax=ymax*.scale,label=isCapped), size=2) 
    }
    
    p
    
    gg.file <- paste0(input.basename, "-mem.pdf")
    ggsave(gg.file, width=figure.width, height=figure.height, units=c("in"), colormodel='rgb', useDingbats=FALSE)
    embed_fonts(gg.file, options=pdf.embed.options)
  }


  ltx <- function(t,ref,name, X) {
    len <- ncol(t)/2
    .just = rep(c('r',paste0('@{}>{\\smaller\\ensuremath{\\pm}}r@{\\,\\si{', X , '}}')), len)
    .just = c('@{}r', .just[2:length(.just)])
    out <- latex(t
                ,file=name
                ,rowlabel="Benchmark"
                ,rowlabel.just="@{}l"
                ,booktabs=TRUE
                ,table.env=FALSE, center="none"
                ,size="scriptsize"
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

  ltx(cpu,cpu.ref,paste0(input.basename, "-cpu-numbers.tex"), '\\milli\\second')

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

  ltx(mem,mem.ref,paste0(input.basename, "-mem-numbers.tex"), '\\kilo\\byte')

 
  #'@{\\,\\si{\\mega\\byte}}>{\\smaller\\ensuremath{\\pm}}r@{\\,\\si{\\kilo\\byte}}'
#   f <- (function() {
#     c <- bench.summary[bench.summary$vm != "SMLNJ" & bench.summary$vm != "LambUncached",]
#     m <- bench.summary.mem[bench.summary.mem$vm != "SMLNJ" & bench.summary.mem$vm != "LambUncached",]
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
#                 ,file=paste0(input.basename, "-numbers.tex")
#                 ,rowlabel="Benchmark"
#                 ,rowlabel.just="@{}l"
#                 ,booktabs=TRUE
#                 ,table.env=FALSE, center="none"
#                 #,size="footnotesize"
#                 ,size="scriptsize"
#                 ,colheads=rep(c('time', '', 'memory', ''), len/2)
#                 ,col.just=rep(c('@{}r','@{}>{\\smaller\\ensuremath{\\pm}}r','@{\\,\\si{\\milli\\second}}r','@{\\,\\si{\\mega\\byte}}>{\\smaller\\ensuremath{\\pm}}r@{\\,\\si{\\kilo\\byte}}'), len/2)
#                 ,cgroup=levels(f$vm)
#                 ,cdec=rep(0, len*2)
#     )})()
} else { # ERROR CORRECTION
  print(">> w/o ep")
  bench.summary <- ddply(bench.cpu, .(benchmark,vm))
  bench.summary.mem <- ddply(bench.mem, .(benchmark,vm))
  bench.summary.tree <- ddply(bench.tree, .(criterion,input_sizes,vm))

  dat <- bench.summary[
    bench.summary$vm != "SMLNJ" 
    & bench.summary$vm != "MLton"
    & bench.summary$vm != "LambUncached"
    & bench.summary$vm != "OCaml"
    & bench.summary$vm != "Python"
  ,]
  dat$vm <- factor(dat$vm, levels = c("Lamb", # "LambUncached",
                                      "PycketShapes",
                                      "PycketOrig",  
                                      "Racket", # "Pycket",
                                      "MLton","SMLNJ","OCaml",
                                      "Python", "Pypy"))
  levels(dat$vm)[levels(dat$vm) == "Lamb"] <- "Prototype"

  if (mean(dat$value) > 1000) {
    .scale = 1/1000
    .ylab <- "Execution time (s)"
  } else {
    .scale = 1
    .ylab <- "Execution time (ms)"
  }
  
  .y.icon.max <- 500
  .y.max.steps <- 1000
  dat$isCapped <- " "
  .needs.cap <- dat[!is.na(dat$value) & dat$value > CAPPING,]
  if (nrow(.needs.cap) > 0) {
    dat[!is.na(dat$value) & dat$value > CAPPING,]$isCapped <- paste0("> ", format(.needs.cap$value * .scale,digits=0))
    dat[!is.na(dat$value) & dat$value > CAPPING,]$value <- CAPPING
  }
  
  dodge <- position_dodge(width=.8)
  ymax <- (round_any(max(dat$value), .y.max.steps, ceiling) * .scale)
  p <-  ggplot(data=dat,
         aes(x=benchmark,y=value * .scale,group=interaction(benchmark,vm),fill=vm)
  ) +
    geom_bar(stat="identity", position=dodge, width=.75, aes(fill = vm))+
    geom_point(position=dodge,aes(y=max(.y.icon.max * .scale,min(value * .scale)), ymax=ymax, shape=vm),size=2, color="grey90",stat="identity") +
  #   xlab("Benchmark") +
    ylab(.ylab) +
    theme_bw(base_size=8, base_family="Helvetica") +
    theme(
      rect = element_rect(),
      axis.title.x =  element_blank(),
      #   axis.text.x  = element_text(size=8, angle=45, hjust=1),
      axis.text.x  = element_text(size=8),
      axis.title.y = element_text(face="bold", size=8),
      axis.text.y  = element_text(size=8), #angle=45, hjust=0.2, vjust=0.5,
      legend.position=c(0.70, .7),
      plot.margin = unit(c(-2,-0.1,-2,-1),"mm"),
      legend.text = element_text(size=7),
      legend.title = element_text(size=7, face="bold"),
      legend.background = element_rect(fill="gray90", size=0),
      legend.margin = unit(0, "cm"),
      legend.key=element_rect(fill="white"),
      legend.key.size=unit(5,"mm")
    ) +
    scale_y_continuous(
      breaks=seq(0, ymax, .y.max.steps * .scale), 
      limits=c(0,ymax),
      expand=c(0,0)) +
    scale_fill_brewer(name = "Implementation", type="qual", palette="Set1") +
    scale_shape(name = "Implementation", solid = FALSE) +
#     scale_fill_manual(name = "Implementation", values=c(
#       "#377eb8",
#       "#4daf4a",
#       "#f781bf",  "#e41a1c",
#       #"#ff7f00",
#       "#a65628"
#       )) +
    #scale_fill_grey(name = "Virtual Machine") +
    facet_null()
  if (nrow(.needs.cap) > 0){
    p <- p + 
      geom_text(position=position_dodge(width=.9), angle=90,aes(y=((CAPPING * .scale) *.9), ymax=ymax*.scale,label=isCapped), size=2) 
  }

  p

  gg.file <- paste0(input.basename, "-norm-col.pdf")
  ggsave(gg.file, width=figure.width, height=figure.height, units=c("in"), colormodel='rgb', useDingbats=FALSE)
  embed_fonts(gg.file, options=pdf.embed.options)

}

print(">> done");
#
# EOF
#
