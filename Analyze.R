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
  "locfit",
  "xlsx"
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
  tsv_name <- "output/current.tsv"
}

if (!file.exists(tsv_name)) {
  stop("Cannot open input file ", tsv_name)
}

input.basename = file_path_sans_ext(tsv_name)

bench <- read.delim(tsv_name, comment.char = "#", header=FALSE,
                    col.names=c('timestamp', 'value', 'unit', 'criterion', 'benchmark', 'vm', 'suite', 'extra_args', 'warump', 'cores', 'input_sizes', 'variable_values'))

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

bench <- bench[c('criterion','vm','benchmark','value', 'unit', 'input_sizes')]
bench.tot <- bench[bench$criterion == 'total',,drop=TRUE]
bench.cpu <- bench[bench$criterion == "cpu",,drop=TRUE]
bench.mem <- bench[bench$criterion == "mem",,drop=TRUE]

bench.tree <- bench[(bench$criterion == "cpu" | bench$criterion == "mem") & bench$benchmark == "tree",,drop=TRUE]


#bench.err <- bootstrapTo(bench.tot, 'benchmark', 'vm', 'Racket', 'value')
#--------------------------------------------------------------------------------------------------------

bench.summary <- ddply(bench.cpu, .(benchmark,input_sizes,vm), summarise,
                       mean=mean(value),
                       median=median(value),
                       stdev=sd(value),
                       err095=confInterval095Error(value),
                       cnfIntHigh = mean(value) + (confInterval095Error(value)),
                       cnfIntLow = mean(value) - (confInterval095Error(value))
)

bench.summary.mem <- ddply(bench.mem, .(benchmark,input_sizes,vm), summarise,
                       mean=mean(value),
                       median=median(value),
                       stdev=sd(value),
                       err095=confInterval095Error(value),
                       cnfIntHigh = mean(value) + (confInterval095Error(value)),
                       cnfIntLow = mean(value) - (confInterval095Error(value))
)

bench.summary.tree <- ddply(bench.tree, .(criterion,input_sizes,vm), summarise,
                           mean=mean(value),
                           median=median(value),
                           stdev=sd(value),
                           err095=confInterval095Error(value),
                           cnfIntHigh = mean(value) + (confInterval095Error(value)),
                           cnfIntLow = mean(value) - (confInterval095Error(value))
)


startnum <- function(x) { as.integer(x / 10^(floor(log10(x)))) }
yformat <- function(x) {
  if (x <= 0)
    "0"
  else
    start <- startnum(x)
  if (start == 9 | start == 8 | start == 6 | start == 4)
    ""
  else
    if (x < 1e3)
      format(x/1e3, scientific=FALSE) 
  else
    paste0(as.integer(x/1e3))
}


#dat <- subset(bench.cpu, vm == "ulamb-c" | vm == "ulambi-c")
dat <- bench.summary[bench.summary$vm != "SMLNJ" & bench.summary$vm != "Pycket" & bench.summary$vm != "LambUncached",]
upper <- ceiling(log10(max(dat$median)))
xl <- c(0,max(dat$input_sizes)+5e5)
yl <- c(2,upper)
yb <- seq(1,upper)
ybr <- sapply(sort(as.vector(c((seq(1,10,1) %o% 10^yb)))), as.integer)
#logbr <- sapply(sort(as.vector((c(2,5,7,10) %o% 10^yb))), as.integer)
xmax <- 2e7
xbr <- seq(0,xmax,1e7)
xlabs <- sapply(xbr, function(x) if (x > 0) paste0(format(x/1e6, scientific=FALSE), "\nmillion") else "0")
ylabs <- sapply(ybr, yformat)
#ratio=0.8*10^(upper)
ratio=1.1*10^(upper+2)
#ratio=0.8*10^(upper-2)
ggplot(data = dat, aes(x=input_sizes, y=mean, color=vm)) + 
  geom_point(aes(shape=factor(vm)), size=1.5) +  
#  geom_smooth(method="loess",
  geom_smooth(method="locfit",
              aes(fill=vm, ymin=mean - stdev, 
                  ymax = mean + stdev,
              ),
              size=0.5,
              alpha=2.5/10) + 
#   scale_y_sqrt(breaks = ybr, labels= ylabs )  +
  #scale_y_continuous(breaks = ybr, labels= ylabs, trans="sqrt") +
#   scale_y_continuous(breaks = ybr, labels= ylabs, trans="identity") +
#  scale_y_continuous(breaks = ybr, labels= ylabs, trans="log10") +
  #scale_y_continuous(trans="log10") +
  scale_x_continuous(limits=c(0,xmax),breaks = xbr,labels=xlabs,expand=c(0,0)) +
  #coord_fixed(ratio=ratio,xlim=xl,ylim=10^yl) +  
#   coord_fixed(ratio=ratio) +  
  #coord_cartesian(xlim=xl,ylim=5*10^(yl-1) ) +
  coord_cartesian() +
#   xlab("Problem size") + ylab("Runtime (s) log10") +
  theme_bw(base_size=9, base_family="Helvetica") +  
#   theme(
# #    rect = element_rect(),
# #         axis.title.x = element_text(face="bold", size=11),
# #         axis.text.x  = element_text(size=9), #angle=45, vjust=0.2, 
# #         axis.title.y = element_text(face="bold", size=11),
# #         axis.text.y  = element_text(size=9), #angle=45, hjust=0.2, vjust=0.5, 
#     legend.position=c(0.15, .75), 
#     legend.background = element_rect(fill="gray90", size=0), 
#     legend.margin = unit(0, "cm"),
#     legend.key=element_rect(fill="white"),
#     legend.key.size=unit(5,"mm"),
# #         legend.text = element_text(size=8),
# #         legend.title = element_text(size=8, face="bold")
#     plot.margin = unit(c(0,3,0,-1),"mm")
#   ) +
#   scale_shape_manual(name = "Optimization",
#                      labels=c("Recognition", "Inlining only", "None"), values=c(5,4,3)) +
#   scale_fill_discrete(guide=FALSE) +
#   scale_colour_discrete(name = "Optimization",
#                         labels=c("Recognition", "Inlining only", "None")) + 
  #facet_null()  
   facet_grid(. ~ benchmark)
#  facet_grid(benchmark ~ .)



#dat <- subset(bench.cpu, vm == "ulamb-c" | vm == "ulambi-c")
dat <- bench.summary.mem[bench.summary.mem$vm != "SMLNJ" & bench.summary.mem$vm != "Pycket" & bench.summary.mem$vm != "LambUncached",]
upper <- ceiling(log10(max(dat$median)))
xl <- c(0,max(dat$input_sizes)+5e5)
yl <- c(2,upper)
yb <- seq(1,upper)
ybr <- sapply(sort(as.vector(c((seq(1,10,1) %o% 10^yb)))), as.integer)
#logbr <- sapply(sort(as.vector((c(2,5,7,10) %o% 10^yb))), as.integer)
xmax <- 2e7
xbr <- seq(0,xmax,1e7)
xlabs <- sapply(xbr, function(x) if (x > 0) paste0(format(x/1e6, scientific=FALSE), "\nmillion") else "0")
ylabs <- sapply(ybr, yformat)
#ratio=0.8*10^(upper)
ratio=1.1*10^(upper+2)
#ratio=0.8*10^(upper-2)
ggplot(data = dat, aes(x=input_sizes, y=mean, color=vm)) + 
  geom_point(aes(shape=factor(vm)), size=1.5) +  
  #  geom_smooth(method="loess",
  geom_smooth(method="locfit",
              aes(fill=vm, ymin=mean - stdev, 
                  ymax = mean + stdev,
              ),
              size=0.5,
              alpha=2.5/10) + 
  #   scale_y_sqrt(breaks = ybr, labels= ylabs )  +
  #scale_y_continuous(breaks = ybr, labels= ylabs, trans="sqrt") +
  #   scale_y_continuous(breaks = ybr, labels= ylabs, trans="identity") +
  #  scale_y_continuous(breaks = ybr, labels= ylabs, trans="log10") +
  #scale_y_continuous(trans="log10") +
  scale_x_continuous(limits=c(0,xmax),breaks = xbr,labels=xlabs,expand=c(0,0)) +
  #coord_fixed(ratio=ratio,xlim=xl,ylim=10^yl) +  
  #   coord_fixed(ratio=ratio) +  
  #coord_cartesian(xlim=xl,ylim=5*10^(yl-1) ) +
  coord_cartesian() +
  #   xlab("Problem size") + ylab("Runtime (s) log10") +
  theme_bw(base_size=9, base_family="Helvetica") +  
  #   theme(
  # #    rect = element_rect(),
  # #         axis.title.x = element_text(face="bold", size=11),
  # #         axis.text.x  = element_text(size=9), #angle=45, vjust=0.2, 
  # #         axis.title.y = element_text(face="bold", size=11),
  # #         axis.text.y  = element_text(size=9), #angle=45, hjust=0.2, vjust=0.5, 
  #     legend.position=c(0.15, .75), 
  #     legend.background = element_rect(fill="gray90", size=0), 
  #     legend.margin = unit(0, "cm"),
  #     legend.key=element_rect(fill="white"),
  #     legend.key.size=unit(5,"mm"),
# #         legend.text = element_text(size=8),
# #         legend.title = element_text(size=8, face="bold")
#     plot.margin = unit(c(0,3,0,-1),"mm")
#   ) +
#   scale_shape_manual(name = "Optimization",
#                      labels=c("Recognition", "Inlining only", "None"), values=c(5,4,3)) +
#   scale_fill_discrete(guide=FALSE) +
#   scale_colour_discrete(name = "Optimization",
#                         labels=c("Recognition", "Inlining only", "None")) + 
#facet_null()  
facet_grid(. ~ benchmark)
#  facet_grid(benchmark ~ .)

f.name <- 'plot1.pdf'
ggsave(f.name, width=24, height=17, units=c("cm"), colormodel='cmyk', useDingbats=FALSE)
embed_fonts(f.name, format="pdfwrite", options="-dEmbedAllFonts=true -dPDFSETTINGS=/prepress -dCompatibilityLevel=1.4 -dSubsetFonts=true -dHaveTrueTypeFonts=true")



#dat <- subset(bench.cpu, vm == "ulamb-c" | vm == "ulambi-c")
dat <- bench.summary.tree[ bench.summary.tree$criterion == "cpu" & bench.summary.tree$vm != "Pycket" & bench.summary.tree$vm != "LambUncached",]
upper <- ceiling(log10(max(dat$median)))
xl <- c(0,max(dat$input_sizes)+5e5)
yl <- c(2,upper)
yb <- seq(1,upper)
ybr <- sapply(sort(as.vector(c((seq(1,10,1) %o% 10^yb)))), as.integer)
#logbr <- sapply(sort(as.vector((c(2,5,7,10) %o% 10^yb))), as.integer)
xmax <- 2e7
xbr <- seq(0,xmax,1e7)
xlabs <- sapply(xbr, function(x) if (x > 0) paste0(format(x/1e6, scientific=FALSE), "\nmillion") else "0")
ylabs <- sapply(ybr, yformat)
#ratio=0.8*10^(upper)
ratio=1.1*10^(upper+2)
#ratio=0.8*10^(upper-2)
ggplot(data = dat, aes(x=input_sizes, y=mean, color=vm)) + 
  geom_line(aes(shape=factor(vm)), size=1.5) +  
#  geom_smooth(method="loess",
#   geom_smooth(method="locfit",
#                aes(
#                     fill=vm, ymin=mean - stdev - 0.000001, 
#                    ymax = mean + stdev+0.000001,
#                    y=mean+0.0000001
#                ),
#               size=0.5,
#               alpha=2.5/10) + 
  #   scale_y_sqrt(breaks = ybr, labels= ylabs )  +
  #scale_y_continuous(breaks = ybr, labels= ylabs, trans="sqrt") +
  #   scale_y_continuous(breaks = ybr, labels= ylabs, trans="identity") +
  scale_y_continuous(trans="log10") +
  scale_x_continuous()+#limits=c(0,xmax),breaks = xbr,labels=xlabs,expand=c(0,0)) +
  #coord_fixed(ratio=ratio,xlim=xl,ylim=10^yl) +  
  #   coord_fixed(ratio=ratio) +  
  #coord_cartesian(xlim=xl,ylim=5*10^(yl-1) ) +
  coord_cartesian() +
  #   xlab("Problem size") + ylab("Runtime (s) log10") +
  theme_bw(base_size=9, base_family="Helvetica") +  
  #   theme(
  # #    rect = element_rect(),
  # #         axis.title.x = element_text(face="bold", size=11),
  # #         axis.text.x  = element_text(size=9), #angle=45, vjust=0.2, 
  # #         axis.title.y = element_text(face="bold", size=11),
  # #         axis.text.y  = element_text(size=9), #angle=45, hjust=0.2, vjust=0.5, 
  #     legend.position=c(0.15, .75), 
  #     legend.background = element_rect(fill="gray90", size=0), 
  #     legend.margin = unit(0, "cm"),
  #     legend.key=element_rect(fill="white"),
  #     legend.key.size=unit(5,"mm"),
# #         legend.text = element_text(size=8),
# #         legend.title = element_text(size=8, face="bold")
#     plot.margin = unit(c(0,3,0,-1),"mm")
#   ) +
#   scale_shape_manual(name = "Optimization",
#                      labels=c("Recognition", "Inlining only", "None"), values=c(5,4,3)) +
#   scale_fill_discrete(guide=FALSE) +
#   scale_colour_discrete(name = "Optimization",
#                         labels=c("Recognition", "Inlining only", "None")) + 
facet_null()  
#facet_grid(criterion ~ .)
#  facet_grid(benchmark ~ .)


dat <- bench.summary.tree[ bench.summary.tree$criterion == "mem" & bench.summary.tree$vm != "Pycket" & bench.summary.tree$vm != "LambUncached",]
upper <- ceiling(log10(max(dat$median)))
xl <- c(0,max(dat$input_sizes)+5e5)
yl <- c(2,upper)
yb <- seq(1,upper)
ybr <- sapply(sort(as.vector(c((seq(1,10,1) %o% 10^yb)))), as.integer)
#logbr <- sapply(sort(as.vector((c(2,5,7,10) %o% 10^yb))), as.integer)
xmax <- 2e7
xbr <- seq(0,xmax,1e7)
xlabs <- sapply(xbr, function(x) if (x > 0) paste0(format(x/1e6, scientific=FALSE), "\nmillion") else "0")
ylabs <- sapply(ybr, yformat)
#ratio=0.8*10^(upper)
ratio=1.1*10^(upper+2)
#ratio=0.8*10^(upper-2)
ggplot(data = dat, aes(x=input_sizes, y=mean, color=vm)) + 
  geom_point(aes(shape=factor(vm)), size=1.5) +  
  #  geom_smooth(method="loess",
  geom_smooth(method="locfit",
              aes(fill=vm, ymin=mean - stdev - 0.000001, 
                  ymax = mean + stdev+0.000001,
                  y=mean+0.0000001
              ),
              size=0.5,
              alpha=2.5/10) + 
  #   scale_y_sqrt(breaks = ybr, labels= ylabs )  +
  #scale_y_continuous(breaks = ybr, labels= ylabs, trans="sqrt") +
  #   scale_y_continuous(breaks = ybr, labels= ylabs, trans="identity") +
  #scale_y_continuous(trans="log10") +
  scale_x_continuous()+#limits=c(0,xmax),breaks = xbr,labels=xlabs,expand=c(0,0)) +
  #coord_fixed(ratio=ratio,xlim=xl,ylim=10^yl) +  
  #   coord_fixed(ratio=ratio) +  
  #coord_cartesian(xlim=xl,ylim=5*10^(yl-1) ) +
  coord_cartesian() +
  #   xlab("Problem size") + ylab("Runtime (s) log10") +
  theme_bw(base_size=9, base_family="Helvetica") +  
  #   theme(
  # #    rect = element_rect(),
  # #         axis.title.x = element_text(face="bold", size=11),
  # #         axis.text.x  = element_text(size=9), #angle=45, vjust=0.2, 
  # #         axis.title.y = element_text(face="bold", size=11),
  # #         axis.text.y  = element_text(size=9), #angle=45, hjust=0.2, vjust=0.5, 
  #     legend.position=c(0.15, .75), 
  #     legend.background = element_rect(fill="gray90", size=0), 
  #     legend.margin = unit(0, "cm"),
  #     legend.key=element_rect(fill="white"),
  #     legend.key.size=unit(5,"mm"),
# #         legend.text = element_text(size=8),
# #         legend.title = element_text(size=8, face="bold")
#     plot.margin = unit(c(0,3,0,-1),"mm")
#   ) +
#   scale_shape_manual(name = "Optimization",
#                      labels=c("Recognition", "Inlining only", "None"), values=c(5,4,3)) +
#   scale_fill_discrete(guide=FALSE) +
#   scale_colour_discrete(name = "Optimization",
#                         labels=c("Recognition", "Inlining only", "None")) + 
facet_null()  


if (FALSE) {
  filename <-"/Users/tobias/Documents/Uni/FK/2013/02.1 Fall Retreat/report/revplot.pdf"
  ggsave(filename , width=16, height=6.7, units=c("cm"), colormodel='rgb', useDingbats=FALSE)
  embed_fonts(filename, format="pdfwrite", options="-dEmbedAllFonts=true -dPDFSETTINGS=/prepress -dCompatibilityLevel=1.4 -dSubsetFonts=true -dHaveTrueTypeFonts=true")
}

ggplot(data = diff.cpu, aes(x=input_sizes, y=median, color=opt, fill=opt)) + 
  geom_point(aes(shape=factor(opt)), size=1.5) +  
  geom_smooth(method="loess",
              aes(fill=opt, ymin=arithmetic.mean - stdDev, 
                  ymax = arithmetic.mean + stdDev,
              )) +
  #geom_line() +
  facet_null()
  

#
# EOF
#