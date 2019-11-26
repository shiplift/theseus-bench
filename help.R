#! /usr/bin/env Rscript

use <- function(pkg) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg, repos="https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(require(pkg, character.only=TRUE))
}


.int.pkgs=c(
  'extrafont',
  'ggplot2',
  'magrittr',
  'tibble',
  'readr',
  'colorspace',
  'cowplot',
  'tools',
NULL)

invisible(sapply(.int.pkgs, use))

sapply(pkgs, use)



if (!require(devtools)) {
  install.packages("devtools", repos="https://cloud.r-project.org")
  library(devtools)
}

loadfonts(quiet=TRUE)
if (length(fonts()) == 0) {
  paths <- c("~/.local/share/fonts",
             "~/Library/Fonts/",
             "/Library/Fonts/", "/System/Library/Fonts")
  paths <- paths[file.exists(paths)]

 font_import(paths=paths,prompt=FALSE)
 loadfonts(quiet=TRUE)
}

if (!exists('base_family')) {
  base_family <- if ("Fira Sans" %in% fonts()) {"Fira Sans"} else {"Helvetica"}
}

options(ggplot2.continuous.fill='viridis')


pdf.embed.options <- "-dEmbedAllFonts=true -dPDFSETTINGS=/prepress -dCompatibilityLevel=1.4 -dSubsetFonts=true -dHaveTrueTypeFonts=true"

# --- DATA ---------


.rebench_0.16.col_names <- c('timestamp', 'value', 'unit', 'criterion', 'benchmark', 'vm', 'suite', 'extra_args', 'warmup', 'cores', 'input_sizes', 'variable_values')
.rebench_0.16_col_types <- cols(timestamp=col_skip(),value=col_number(),unit=col_factor(),criterion=col_factor(),benchmark=col_factor(), vm=col_factor(), suite=col_factor(), extra_args=col_guess(), warmup=col_logical(), cores=col_number(), input_sizes=col_number(), variable_values=col_number())

# vm -> executor, but we retain the old name
.rebench_1.0.col_names <- c('invocation', 'iteration', 'value', 'unit', 'criterion', 'benchmark', 'vm', 'suite', 'extra_args', 'cores', 'input_sizes', 'variable_values')
.rebench_1.0.col_types <- cols(invocation=col_integer(),iteration=col_integer(),value=col_number(),unit=col_factor(),criterion=col_factor(),benchmark=col_factor(), vm=col_factor(), suite=col_factor(), extra_args=col_guess(), cores=col_number(), input_sizes=col_number(), variable_values=col_number())

rebench.col_names <- .rebench_0.16.col_names
rebench.col_types <- .rebench_0.16_col_types
# ---- cmd line ----

if (cmd.line) {
  if (length(commandArgs(trailingOnly=TRUE)) > 0) {
    input_name = commandArgs(trailingOnly=TRUE)[1]
  } else {
    input_name <- input_name.default
  }

  if (!file.exists(input_name)) {
    stop("Cannot open input file ", input_name)
  }
} else {
  input_name <- input_name.default
}

set_datacontext <- function(file='current.tsv') {
  rebench_1.0.col_names  <- .rebench_1.0.col_names
  rebench_1.0.col_types  <- .rebench_1.0.col_types
  rebench_0.16_col_names <- .rebench_0.16.col_names
  rebench_0.16_col_types <- .rebench_0.16_col_types

  .basename <- file_path_sans_ext(file)

  .dir <- dirname(.basename)
  .indir <- function(what) {paste0(.dir, '/', paste(what,collapse = '-'))}
  .basename.parts <- .basename.pure <- .indir('current')

  .basecomponents <- unlist(strsplit(basename(.basename), '-',fixed=TRUE))
  if (length(.basecomponents) > 0) {
    .basename.parts <- .basename.pure <- .indir(.basecomponents[1])
    if (tail(.basecomponents,1) == 'all') {
      .basename.parts <- .indir(head(.basecomponents,-1))
    }

    if (!is.na(.data.date <- as.Date(.basecomponents[1],'%Y%m%d')) && .data.date > "2019-10-01") {
      assign('rebench.col_names', rebench_1.0.col_names, pos = .GlobalEnv)
      assign('rebench.col_types', rebench_1.0.col_types, pos = .GlobalEnv)
    } else {
      assign('rebench.col_names', rebench_0.16_col_names, pos = .GlobalEnv)
      assign('rebench.col_types', rebench_0.16_col_types, pos = .GlobalEnv)
    }
  }
  assign('input.basename', .basename, pos = .GlobalEnv)
  assign('input.basename.pure', .basename.pure, pos = .GlobalEnv)
  assign('input.basename.parts', .basename.parts, pos = .GlobalEnv)
}
set_datacontext(input_name)

# ------ functions -----
`%ni%` = Negate(`%in%`)


confInterval095Error <- function (samples) {
  if (length(sample) > 30)
    qnorm(0.975) * sd(samples) / sqrt(length(samples))
  else
    qt(0.975, df=length(samples)-1) * sd(samples) / sqrt(length(samples))
}

div_mean_x_y <- function(data, indices, extraid) {
    indexx = indices[extraid == "x"]
    indexy = indices[extraid == "y"]
    mean(data[indexx]) / mean(data[indexy])
}

normalize_value_bootstrap_confidence <- function(x, y, R=1000) {
    # x and y need to be *vectors* of all the values
    # the result is the confidence interval of mean(x) / mean(y)
    total <- c(x,y)
    id <- as.factor(c(rep("x",length(x)),rep("y",length(y))))

    b <- boot(total, div_mean_x_y, strata=id, R=R, extraid=id)
    norm <- boot.ci(b, type=c("norm"))$normal
    dimnames(norm) <- list(NULL,c("conf", "lower", "upper"))
    norm
}

geomean <- function(X, na.rm=FALSE) {
  value <- X[X > 0]
  if (na.rm) {
    value <- na.omit(value)
  }
  exp(mean(log(value)))
}

latexify <- function(df.sel, var){
  df.ltx <- df.sel[2:length(df.sel)]
  rownames(df.ltx) <- df.sel[[var]]
  colnames(df.ltx) <- sapply(colnames(df.ltx), function(x) {sedit(x, '_', ' ')})
  df.ltx
}

makeCappingInfo <- function(tbl, cols, capping=CAPPING, scale=1, viewscale=scale) {
  cappingInfo <- rep(' ',nrow(tbl))
  for (col in cols) {
    col_mean <- paste0(col, '_mean')
    cappingInfo[!is.na(tbl[[col_mean]]) & tbl[[col_mean]] > capping] <-
      paste0("~",format(tbl[!is.na(tbl[[col_mean]]) & tbl[[col_mean]] > capping,][[col_mean]] * viewscale,
                         digits=0))
    col_err095 <- paste0(col, '_err095')
    digits= if(scale==viewscale){ log10(1/scale)-1 } else { scale }
    cappingInfo[!is.na(tbl[[col_mean]]) &
                  tbl[[col_mean]] > capping &
                  (!is.na(tbl[[col_err095]]) & tbl[[col_err095]] * viewscale > scale)] <-
      paste0(
        cappingInfo[tbl[[col_mean]] > capping & (tbl[[col_err095]] * scale) > scale],
        "±",
        format(round(tbl[!is.na(tbl[[col_mean]]) &
                           tbl[[col_mean]] > capping &
                           (!is.na(tbl[[col_err095]]) & tbl[[col_err095]] * viewscale > scale), ][[col_err095]]*viewscale,
                     digits=digits)))
  }
  cappingInfo
}


#- Color --------------------

.make_Set1Paired <- function() {
  .Set1 <- c(RColorBrewer::brewer.pal(8, "Set1"), '#999999')
  # .paired <- c(unlist(mapply(c,lighten(.Set1,0.33),.Set1)))
  .paired <- c(unlist(mapply(c,lighten(.Set1,0.25),.Set1)))
  #names(.paired) <- as.character(seq(1,18))
  .paired
}
Set1Paired <- .make_Set1Paired()
Set1Paired_pal <- function(num) { Set1Paired[seq(1,num)] }

.make_Set1Triad <- function() {
  .Set1 <- c(RColorBrewer::brewer.pal(8, "Set1"), '#999999')
  .triad <- c(unlist(mapply(c,lighten(.Set1,0.25),.Set1,darken(.Set1,.15))))
  #names(.paired) <- as.character(seq(1,18))
  .triad
}
Set1Triad <- .make_Set1Triad()
Set1Triad_pal <- function(num) { Set1Triad[seq(1,num)] }



#- Read Helper -----------------------------------------------

read_rebench <- function(filename) {
  read_tsv(filename, comment = "#", col_names=rebench.col_names, col_types=rebench.col_types)
}

convert_rebench <- function(in_name, out_name) {
  result <-
    read_tsv(in_name,comment="#",col_names=.rebench_0.16.col_names,col_types=.rebench_0.16_col_types) %>%
    select(-warmup) %>%
    mutate(criterion=fct_relevel(criterion, 'cpu','gc','mem','total')) %>%
    group_by(criterion,benchmark,vm,suite) %>%
    mutate(invocation=1:n()) %>%
    ungroup %>%
    arrange(benchmark,vm,suite,invocation,criterion) %>%
    mutate(iteration=1) %>%
    select(invocation,iteration,value,unit,criterion,benchmark,vm,suite,extra_args,cores,input_sizes,variable_values)
  result %>% write_tsv(out_name,na='',col_names=FALSE,quote_escape=FALSE)
  invisible(result)
}
stratify_rebench <-  function(in_name, out_name) {
  result <-
    read_tsv(in_name, comment = "#", col_names=rebench.col_names, col_types=rebench.col_types) %>%     mutate(criterion=fct_relevel(criterion, 'cpu','gc','mem','total')) %>%
    mutate(criterion=fct_relevel(criterion, 'cpu','gc','mem','total')) %>%
    arrange(benchmark,vm,suite,invocation,criterion)
  result %>% write_tsv(out_name,na='',col_names=FALSE,quote_escape=FALSE)
  invisible(result)
}

#== Bench specs ============================

Spec.benchmarks.default <- tribble(
  ~level,     ~group,     ~name,
  'reverse',  'reverse',  'reverse[E]',
  'reversen', 'reverse',  'reverse[n]',
  'append',   'append',   'append[E]',
  'appendn',  'append',   'append[n]',
  'map',      'map',      'map[E]',
  'mapn',     'map',      'map[n]',
  'filter',   'filter',   'filter[E]',
  'filtern',  'filter',   'filter[n]',
  'tree',     'tree',     'tree[E]',
  'treen',    'tree',     'tree[n]',
  'geomean',  'geomean',  'geometic\nmean' #that one is artificial but ok.
)

Spec.vms.default <- tribble(
  ~level,                    ~group,       ~name,                               ~pch,  ~color,
  'Lamb',                    'Prototype',  'Prototype',                         5,     1,
  'LambUncached',            'Prototype',  'Prototype',                         5,     1,
  'LambUncachedMulti',       'Prototype',  'Prototype',                         5,     1,
  'LambNoopt',               'Prototype',  'Prototype (not optimized)',         9,     2,
  'PycketShapes',            'Pycket',     'Pycket (optimized)',                1,     3,
  'PycketShapesMulti',       'Pycket',     'Pycket (optimized)',                1,     3,
  'PycketOrig',              'Pycket',     'Pycket (original)',                10,     4,
  'Racket',                  'Pycket',     'Racket',                           13,     8,#8->
  'RSqueakShapes',           'RSqueak',    'RSqueak (optimized)',               0,     5,
  'RSqueakShapesMulti',      'RSqueak',    'RSqueak (optimized)',               0,     5,
  'RSqueakOrig',             'RSqueak',    'RSqueak (original)',               12,     6,
# 'RSqueakFunctionalShapes', 'RSqueak',    'RSqueak (optimized), functional',   4,    13,
# 'RSqueakFunctionalOrig',   'RSqueak',    'RSqueak (original), functional',    8,    14,
  'Squeak',                  'RSqueak',    'Squeak',                            7,     7,#9->
# 'SqueakFunctional',        'RSqueak',    'Squeak, functional',                3,    10,
  'Pypy',                    'Other',      'PyPy',                             15,     9,#11->9
  'Python',                  'Other',      'Python',                           18,    10,#12->10
  'MLton',                   'Other',      'MLton',                             2,    13,#7->
  'OCaml',                   'Other',      'OCaml',                             6,    11,#15->
  'SMLNJ',                   'Other',      'SML/NJ',                           11,    12,#16->
)

iconize_vms <- function(spec_vms=Spec.vms.default,color_set=Set1Paired, file="") {
  iconize <- function(vmnum, row){
    p <- ggplot(row,aes(x=Implementation,y=Implementation,fill=Implementation,shape=Implementation))+
      theme_void()+
      theme(plot.title=element_blank(),text=element_blank()
            ,plot.margin = unit(c(-1, -1,-1, -1), "pt")
            )+
      geom_tile(na.rm=TRUE)+
      geom_point(size=2, color="grey90")+
      labs(NULL) +
      coord_fixed()+
      scale_shape_manual(values=row$pch,guide=FALSE) +
      scale_fill_manual(values=row$color,guide=FALSE) +
      facet_null()
    ggsave(filename=paste0(file,'-',vmnum,'.pdf'), width=10/72,height=10/72, plot=p, useDingbats=FALSE)
    embed_fonts(file=paste0(file,'-',vmnum,'.pdf'), options=pdf.embed.options)
    invisible(p)
  }
  spec <- spec_vms %>% distinct(name, .keep_all=TRUE) %>%
    mutate(Implementation=fct_relevel(factor(name),name)) %>%
    mutate(color=sapply(color, function (x) {color_set[x]})) %>%
    select(Implementation,pch,color)
  for (i in seq_len(nrow(spec))) {
    iconize(i,spec[i,,drop=FALSE])
  }
}
report_vms <- function(spec_vms=Spec.vms.default,color_set=Set1Paired, file="",width=2.6,height=3.4) {

  .fnt <- base_family
  .szs <- 14
  spec <- spec_vms %>% distinct(name, .keep_all=TRUE) %>%
    mutate(Implementation=fct_relevel(factor(name),name)) %>%
    mutate(color=sapply(color, function (x) {color_set[x]})) %>%
    select(Implementation,pch,color)
  plot <- ggplot(data=spec,aes(x=Implementation,y=1,fill=Implementation,shape=Implementation)) +
    geom_col() + geom_point(size=3, color="grey90") +
    scale_fill_manual(values=spec$color) +
    scale_shape_manual(values=spec$pch) +
    theme(line = element_blank(),
          rect = element_blank(),
          text = element_text(family = .fnt,
                              face = "plain", colour = "black", size = .szs, lineheight = 0.9,
                              hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                              debug = FALSE),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks.length = unit(0, "pt"), axis.ticks.length.x = NULL,
          axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL,
          axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL,
          axis.ticks.length.y.right = NULL,
          legend.box = NULL,
          legend.key.size = unit(1.2, "lines"),
          legend.position='top',
          legend.justification='left',
          legend.direction='vertical',
          legend.text = element_text(),
          legend.title = element_text(hjust = 0),
          strip.text = element_blank(),
          strip.switch.pad.grid = unit(2, "pt"),
          strip.switch.pad.wrap = unit(2,"pt"),

          panel.ontop = TRUE,

          panel.spacing = unit(2,"pt"),
          plot.margin = unit(c(0, 0, 0, 0), "lines"),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          plot.caption = element_blank(),
          plot.tag = element_blank(),
          plot.tag.position = "topleft",
          complete = TRUE)

  plot

#  plot_g <- ggplot_gtable(ggplot_build(plot))
#  legend <- plot_g$grobs[[which(sapply(plot_g$grobs, function(x) x$name) == "guide-box")]]
#  plot_grid(legend)
  legend <- cowplot::get_legend(plot)
  ggsave(filename=file, width=width,height=height, plot=legend, useDingbats=FALSE)
  embed_fonts(file=file, options=pdf.embed.options)

  invisible(plot)
}
# report_squeak_vms <- function(spec_vms=Spec.vms.default,color_set=Set1Paired, file="",width=3.2,height=1.8) {
#   spec <- spec_vms %>% filter(str_detect(name, 'Squeak'))
#   report_vms(spec_vms=spec,file=file,width=width,height=height)
# }

report_dflt_vms <- function(spec_vms=Spec.vms.default,color_set=Set1Paired, file="",width=2.7,height=1.8) {
  v <- c(
    'Prototype',
    'Prototype (not optimized)',
    'Pycket (optimized)',
    'Pycket (original)',
    'RSqueak (optimized)',
    'RSqueak (original)',
    NULL)
  spec <- spec_vms %>% filter(name %in% v)
  report_vms(spec_vms=spec,file=file,width=width,height=height)
}

# Data transformer ====================================================

factorize_bench <- function(.data, spec_benchmarks=Spec.benchmarks.default) {
  #spec_benchmarks %<>% filter(level %in% !!.data$benchmark)
  .data %>%
    mutate(niladic=if_else(benchmark == 'geomean', NA, !endsWith(as.character(benchmark),'n'))) %>%
    mutate(benchmark = lvls_expand(benchmark, !!spec_benchmarks$level)) %>%
    mutate(benchmark = fct_relevel(benchmark, !!!spec_benchmarks$level)) %>%
    mutate(benchmark_group = fct_recode(benchmark, !!!(deframe(spec_benchmarks%>%select(group,level))))) %>%
    mutate(benchmark = fct_recode(benchmark, !!!(deframe(spec_benchmarks%>%select(name,level)))))
}
factorize_vm <- function(.data, spec_vms=Spec.vms.default) {
  .data %>%
    mutate(vm = lvls_expand(vm, !!spec_vms$level)) %>%
    mutate(vm = fct_relevel(vm, !!!spec_vms$level)) %>%
    mutate(vm = fct_recode(vm, !!!(deframe(spec_vms%>%select(name,level))))) %>%
    mutate(vm_group = recode(vm, !!!(deframe(spec_vms%>%select(name,group)))))
}

factorize <- function(.data,
                      spec_benchmarks=Spec.benchmarks.default,
                      spec_vms=Spec.vms.default,
                      color_set=Set1Paired,
                      .add_visuals=TRUE
) {
  spec_vms %<>% mutate(color=sapply(color, function (x) {color_set[x]}))
  #spec_vms %<>% filter(level %in% !!.data$vm)

  result <- .data %>%
    factorize_vm(spec_vms=spec_vms) %>%
    factorize_bench(spec_benchmarks=spec_benchmarks)

  result <- if (!.add_visuals) result else {
    result %>%
      mutate(color = recode(vm, !!!(deframe(spec_vms%>%select(name,color)))),
             shape = recode(vm, !!!(deframe(spec_vms%>%select(name,pch)))))
  }
  result
}

clean_gc <- function(.data) {
  if (nrow(.data[.data$criterion == 'gc' & .data$value == 0,]) > 0) {
    res <- .data %>% filter(TRUE)
    res[res$criterion == 'gc' & res$value == 0,]$value <- NA
    res
  } else {
    .data
  }
}

spread_measurements <- function(.data) {
  .data %>%
    group_by_at(vars(-value)) %>% mutate(row_id=1:n()) %>% ungroup() %>% #setup
    spread(key=criterion, value=value) %>% #spread
    select(-row_id) #order, lose row_id
}

read_benchmark <- function(filename, cols_to_keep=c('vm','benchmark','criterion','value')) {
  read_rebench(filename) %>%
    clean_gc %>%
    select(!!cols_to_keep) %>%
    spread_measurements %>%
    factorize
}

benchmark_summarize <- function(.data) {
  .data %>% group_by_at(vars(benchmark, benchmark_group, niladic, starts_with("vm"), color, shape)) %>%
    summarize_at(vars(cpu,total,gc,mem), list(
      ~mean(.),~median(.),stdev=sd,full=list,
      err095=confInterval095Error,
      max=~max(mean(.)+confInterval095Error(.),median(.)+confInterval095Error(.),
               mean(.)+sd(.),median(.)+sd(.)))) %>%
    select(benchmark, benchmark_group, niladic,
           starts_with("vm"), starts_with("total"), starts_with("cpu"), starts_with("gc"), starts_with("mem"),
           matches('color'), matches('shape'))
}


benchmark_nest <- function(.data) {
  .grouping <- .data %>%
    group_vars  %>%
    discard(function (x) x == 'benchmark') %>%
    sapply(as.name)

  .data %>%
    ungroup %>% group_by(benchmark) %>% nest %>%
    mutate(data=map(data, function (x) { x%>% group_by(!!!.grouping)}))
}


.paste_name <- function(...) { as.name(paste0(...))}
.un_full <- function(.df, crit) {
  .c <- as.name(paste0(crit, '_full'))
  .df %>% getElement(.c) %>% unlist
}


normalize_group <- function(.data,criteria=c('cpu','total','mem')) {

  .source <- .data %>% ungroup %>% filter(TRUE)

  cmp_vm <- tail(.source$vm,n=1)
  cmp <- .source %>% filter(vm==cmp_vm)  %>% as.list

  rest <- .source %>% filter(vm!=cmp_vm)
  result <- rest %>% filter(TRUE)

  for (criterion in lapply(criteria, as.name)) {
    .c <- .paste_name(criterion, '_mean')
    crit_norm <- rest %>% group_by(vm) %>% group_modify(function(.d, .g) {
      .boot <- .d %>%
        .un_full(criterion) %>%
        normalize_value_bootstrap_confidence(cmp %>% .un_full(criterion)) %>%
        as_tibble
      .d %>% transmute(
        !!(.paste_name(criterion, '_norm_mean')) := !!.c / (!!(cmp[[.c]])),
        !!(.paste_name(criterion, '_norm_lower')) := getElement(.boot, 'lower'),
        !!(.paste_name(criterion, '_norm_upper')) := getElement(.boot, 'upper'))})
    rest %<>% left_join(crit_norm, by='vm')
  }
  rest
}

add_gc_rate <- function(.data) {
  .data %>%  ungroup %>% group_by(benchmark, vm) %>%
    group_modify(function(.d, .g) {
      .boot <- .d %>%
        .un_full('gc') %>%
        normalize_value_bootstrap_confidence(.d %>% .un_full('cpu')) %>%
        as_tibble
      .d %>% mutate(
        gc_rate_mean=gc_mean / cpu_mean,
        gc_rate_lower=getElement(.boot, 'lower'),
        gc_rate_upper=getElement(.boot, 'upper'))})
}

normalize_benches <- function(.data) {
  # a tad expensive
  .data.norm <- .data %>%
    group_by(benchmark,vm_group) %>%
    group_modify(~normalize_group(.x,criteria=c('cpu','total','mem'))) %>%
    mutate(overall=FALSE)

  .data.norm.all <- .data.norm %>%
    group_by(color,shape,vm_group) %>%
    summarize_at(vars(ends_with('_norm_mean')), list(geomean)) %>%
    mutate(overall=TRUE, benchmark='geomean') %>%
    factorize_bench

  .data.norm %<>% bind_rows(.data.norm.all) %>%
    select(benchmark,vm_group,matches('_norm_'),color,shape,overall)
  .data.norm
}

#- Plot themes and visuals  ----------------------------------------------


default.theme <- function() {
    theme_bw(base_size=6, base_family=base_family) +
    theme(
      rect = element_rect(),
      axis.title.x =  element_blank(),
      axis.text.x  = element_text(size=6, angle=45, hjust=1),
      axis.title.y = element_text(face="bold", size=6),
      axis.text.y  = element_text(size=6), #angle=45, hjust=0.2, vjust=0.5,
      legend.position=c(0.15, .8),
      plot.margin = unit(c(-3.2,3,-3,-0.5),"mm"),
      legend.text = element_text(size=6),
      legend.title = element_text(size=6, face="bold"),
      legend.background = element_rect(fill="gray90", size=0),
      legend.margin = unit(0, "cm"),
      legend.key=element_rect(fill="white"),
      legend.key.size=unit(3,"mm")
    )
}

default.theme.t  <- function(fakeLegend=FALSE, base_family=NULL, omit_x_title=FALSE, ...) {
  if (is.null(base_family)) {
    base_family <- get('base_family',pos=1)
  }
  t <- theme_bw(base_size=8, base_family=base_family) +
    theme(
      rect = element_rect(),
      axis.title.x = if (omit_x_title) element_blank() else element_text(face="bold", size=8),
      #   axis.text.x  = element_text(size=8, angle=45, hjust=1),
      axis.text.x  = element_text(size=8),
      axis.title.y = element_text(face="bold", size=8),
      axis.text.y  = element_text(size=8), #angle=45, hjust=0.2, vjust=0.5,
      plot.margin = unit(c(1,0,0,0),"mm"),
      legend.text = element_text(size=7),
      legend.title = element_text(size=7, face="bold"),
      legend.background = element_rect(fill="gray90", size=0),
      legend.spacing = unit(-.5, "cm"),
      legend.key=element_rect(fill="white"),
      legend.key.size=unit(3,"mm"),
      strip.background = element_blank(),# facet
      strip.text.x = element_blank()#facet
      )
  if (!fakeLegend) {t <- t + theme(legend.position=c(0.75, .65))}
  t + theme(...)
}

trans.fmt <- function(x) { prettyNum(x, drop0trailing = TRUE, trim=TRUE, scientific=FALSE, width=4) }

timing_trans <- function(low=0, high, step, viewlimit, scale=1, viewscale=1) {
  if (is.null(viewlimit)) { viewlimit <- c(0,high) }
  vlow  <- viewlimit[1]
  vhigh <- viewlimit[2]
  timing_breaks <- function(y) {
    .breaks <- seq(low, min(vhigh,high), step)
    if (length(.breaks) > 20) {
      .offset <- 8
      .stepmag <- if (10^floor(log10(step)) == step) { 5 } else { 2 }
      # .first.coarse <- tail(which( head(.breaks/step,8) %% 2 == 0), 1)
      .first.coarse <- which(tail((.breaks/step) %% .stepmag, -.offset) == 0)[1] + .offset

      .breaks <- c(.breaks[1:.first.coarse - 1],
        seq(.breaks[.first.coarse], high, step*.stepmag))
    }
    .breaks
  }
  timing_format <- function(y) {
    min.order <- max(1, floor(log10(min(y[y != 0.0], na.rm=TRUE))) - 1)
    case_when(
      y * viewscale <= 0             ~ trans.fmt(0),
      y == vlow                      ~ trans.fmt(y * viewscale),
      y * viewscale < 1              ~ trans.fmt(y * viewscale),
      is.finite(y)                   ~ trans.fmt(y * viewscale),
      TRUE                           ~ "")
  }
  scales::trans_new("timing_trans",
                    transform=function(y) { y * scale },
                    inverse=function(y) { y / scale },
                    breaks=timing_breaks,
                    format=timing_format)
}


mem_trans <- function(low=0, high, step, viewlimit, scale=1, viewscale=1) {
  if (is.null(viewlimit)) { viewlimit <- c(0,high) }
  mem_breaks <- function(y) {
    .breaks <- seq(low, high, step)
    if (length(.breaks) > 20) {
      .first.coarse <- tail(which( head(.breaks/step,8) %% 2 == 0), 1)
      .breaks <- c(.breaks[1:.first.coarse - 1],
                   seq(.breaks[.first.coarse], high, step * 2))
    }
    .breaks
  }
  mem_format <- function(y) {
    .fmt <- function(x) { format(x, trim=TRUE, scientific=FALSE, width=4) }
    min.order <- max(1, floor(log10(min(y[y != 0.0], na.rm=TRUE))) - 1)
    vlow  <- viewlimit[1]
    vhigh <- viewlimit[2]
    case_when(
      y * viewscale <= 0             ~ trans.fmt(0),
      y == vlow                      ~ trans.fmt(y * viewscale),
      y * viewscale < 1              ~ trans.fmt(y * viewscale),
      (y %/% 10^min.order) %% 2 == 0 ~ paste0(y * viewscale),
      TRUE                           ~ "")
  }
  scales::trans_new("mem_trans",
                    transform=function(y) { y * scale },
                    inverse=function(y) { y / scale },
                    breaks=mem_breaks,
                    format=mem_format)
}


ceiling_steps <- function(x, accuracy) { ceiling(x/accuracy) * accuracy }

overall.labeller <- function(var, value) {
  if (var == 'overall') {
    label_bquote(default = "")(var, value)
  } else {
    label_value(var, value)
  }
}

parse_label <- function(x) {
  ifelse(grepl('\n',x), x, ggplot2:::parse_safe(x))
}

global_colwidth <- .78
global_dodgewidth <- .82
global_iconsize <- 1.5
#-- SAVE PLOT ------------------

save.plot <- function(basename,aspect,type='pdf',plot=ggplot2::last_plot(), ...) {
  .gg.file <- paste0(basename, '-', aspect, '.', type)
  save_plot(.gg.file, plot=plot, base_asp = 2, title=aspect, ...)
  embed_fonts(.gg.file, options=pdf.embed.options)
}

#-- Exploration ----------------
v2c <- function(value, ...) {
  cols <- viridis::viridis(ceiling(diff(range(dat$cpu))), ...)
  cols[findInterval(value, vec=seq(from=min(value), to=max(value), length.out=min(10000, length(cols))))]
}

colorrange <- function (x) min(2500,10^min(3, ceiling(diff(log10(range(x,na.rm=TRUE))))))



single_out <- function(.data, arg, .keep_benchmark=FALSE, .make_min_default=FALSE, .make_median_default=FALSE) {
  working <- if(.keep_benchmark) {
    .data
  } else {
    .data %>% ungroup %>% select(-benchmark)
  }
  grouped <- working %>%  group_by_at(vars({{ arg }}, -cpu,-total,-mem))
  groupvars <- sapply(group_vars(grouped),as.name)
  summed <- grouped %>%
    summarize_at(vars(cpu,total,mem), list(~max(.),~min(.),~mean(.),~median(.)))
  result <- summed %>% ungroup %>%
    select(!!!groupvars,starts_with('cpu'),starts_with('total'),starts_with('mem'))
  result <- if (!.keep_benchmark) result else {
    result %>% group_by(benchmark)
  }
  result <- if (.make_min_default) {
    result %>%
      mutate(cpu=cpu_min,mem=mem_min,total=total_min) %>%
      select(-matches("_(max|mean|min|median)"))
  } else if (.make_median_default) {
    result %>%
      mutate(cpu=cpu_median,mem=mem_median,total=total_median) %>%
      select(-matches("_(max|mean|min|median)"))
  } else result
  result
}

matrix_for_criterion <- function(.data, criterion='cpu', part='mean') {
  component <- if (is.null(part) || nchar(part) <= 0) { criterion } else { paste(criterion,part,sep='_') }
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

ytable2latex <- function(z,xdata) {
  ncount=ncol(z$x)
  nrow=nrow(z$x)
  cn=colnames(z$x)
  addrow=ifelse(z$include.rownames,1,0)

  NewAlign=getNewAlign(z)
  #NewAlign=z$align
  totalCol=ztable:::totalCol(z)
  colCount=colGroupCount(z)

  vlines=align2lines(z$align)

  rgroupcount=0
  printrgroup=1
  if(!is.null(z$n.rgroup)){
    if(length(z$n.rgroup)>1) {
      for(i in 2:length(z$n.rgroup)) {
        printrgroup=c(printrgroup,printrgroup[length(printrgroup)]+z$n.rgroup[i-1])
      }
    }
    rgroupcount=1
  }
  Fontsize=c("tiny","scriptsize","footnotesize","small","normalsize",
             "large","Large","LARGE","huge","Huge")


  if(z$tabular) sort="tabular"
  else if(z$sidewaystable) sort="sidewaystable"
  else if(z$wraptable) sort="wraptable"
  else if(z$rotate) sort="rotate"
  else if(z$turn) sort="turn"
  else sort="table"
  headingsize=ifelse(z$size>3,z$size-2,1)
  z$cellcolor=define_colors(z$cellcolor)
  start=attr(z$cellcolor,"no")
  z$frontcolor=define_colors(z$frontcolor,no=start)
  start=attr(z$frontcolor,"no")
  if(!is.null(z$cgroupcolor)) {
    for(i in 1:length(z$cgroupcolor)){
      z$cgroupcolor[[i]]=define_colors(z$cgroupcolor[[i]],no=start)
      start=attr(z$cgroupcolor[[i]],"no")
    }
  }
  if(!is.null(z$cgroupbg)) {
    for(i in 1:length(z$cgroupbg)){
      z$cgroupbg[[i]]=define_colors(z$cgroupbg[[i]],no=start)
      start=attr(z$cgroupbg[[i]],"no")
    }
  }

  if(!is.null(z$rgroupcolor)) z$rgroupcolor=define_colors(z$rgroupcolor,no=start)
  start=attr(z$rgroupcolor,"no")
  if(!is.null(z$rgroupbg)) z$rgroupbg=define_colors(z$rgroupbg,no=start)
  start=attr(z$rgroupbg,"no")

  align=alignCheck(z$align,ncount,addrow)
  if(z$longtable){
    cat(paste("\\color{",z$color,"}\n",sep=""))
    cat(paste("\\begin{",Fontsize[z$size],"}\n",sep=""))
    cat(paste("\\begin{longtable}{",NewAlign,"}\n",sep=""))

  } else {
    if(z$wraptable) {
      if(z$position=="flushright") wrapposition<-"r"
      else wrapposition<-"l"
      cat(paste("\\begin{wraptable}{",wrapposition,"}[10pt]{",
                z$wraptablewidth,"cm}\n",sep=""))

    } else if((sort=="rotate") | (sort=="turn")){
      cat(paste("\\begin{",sort,"}{",z$angle,"}\n",sep=""))
    } else if(sort!="tabular"){      # sidewaystable or table
      cat(paste("\\begin{",sort,"}[",z$placement,"]\n",sep=""))
      cat(paste("\\begin{",z$position,"}\n",sep=""))
    }
    if(!is.null(z$family)){
      if(z$family=="serif") cat("\\sffamily\n")
      else if(z$family=="times") cat("\\rmfamily\n")
      else if(z$family=="tt") cat("\\ttfamily\n")
      else {
        temp=paste0("\\",z$family,"\n")
        cat(temp)
      }
    }
    cat(paste("\\begin{",Fontsize[z$size],"}\n",sep=""))
    cat(paste("\\color{",z$color,"}\n",sep=""))
    cat(paste("\\begin{tabular}{",NewAlign,"}\n",sep=""))
  }

  if(!is.null(z$caption) & z$caption.placement=="top"){
    mycaption=caption2minipage(z,z$caption)
    cat(paste("\\multicolumn{",totalCol,"}{",
              z$caption.position,"}{",sep=""))
    if(z$caption.bold) cat(paste("\\textbf{",mycaption,"}",sep=""))
    else cat(mycaption)
    cat("}\\\\ \n")
  }
  if((z$show.heading==TRUE) & (!is.null(attr(z$x,"heading")))) {
    head=attr(z$x,"heading")
    for(i in 1:length(head)) {
      h1=gsub("~","$\\sim$",head[i],fixed=TRUE)
      if(nchar(head[i])<1) next
      cat(paste("\\multicolumn{",totalCol,"}{l}{\\",Fontsize[headingsize],
                "{",h1,"}}\\\\ \n",sep=""))
    }
  }
  if(is.null(z$hline.after)) cat(ifelse(z$booktabs,"\\toprule[1.2pt]\n","\\hline\n"))
  else if(-1 %in% z$hline.after) cat(ifelse(z$booktabs,"\\toprule[1.2pt]\n","\\hline\n"))
  if(!is.null(z$cgroup)) printLatexHead(z)
  subcolnames=ifelse(is.null(z$subcolnames),0,1)

  if(subcolnames) {
    if(is.na(z$subcolnames[1])) firstcn=paste("\\multirow{2}{*}{}",sep="")
    else firstcn=cn[1]
  }
  else firstcn=cn[1]
  if(z$colnames.bold) firstcn=paste("\\textbf{",firstcn,"}",sep="")

  if(z$frontcolor[1,2]!=z$color) firstcn=paste("\\color{",z$frontcolor[1,2],"}",firstcn,sep="")
  if(z$cellcolor[1,2]!="white") firstcn=paste("\\cellcolor{",z$cellcolor[1,2],"}",firstcn,sep="")
  if(z$include.rownames) {

    result=1
    if(!is.null(ztable:::isspanCol(z,1,1)))
      first=paste("\\multicolumn{",ztable:::isspanCol(z,1,1),"}{c}{}",sep="")
    else if(!is.null(ztable:::isspanRow(z,1,1))){
      result=ztable:::isspanRow(z,1,1)
      if(result>0) first=paste("\\multirow{",result,"}{*}{}",sep="")
    } else first=""
    if(z$cellcolor[1,1]!="white")
      first=paste("\\cellcolor{",z$cellcolor[1,1],first,"}",sep="")

    firstrow=paste(first,"&",firstcn,sep="")

  }
  else firstrow=firstcn

  if(ncount>1) {
    for(i in 2:ncount) {
      firstrow=paste(firstrow,"&",sep="")
      if((i==2)&(!is.null(colCount))){
        if(1 %in% colCount[-length(colCount)]) {
          if(vlines[1+2]==0) firstrow=paste(firstrow,"&",sep="")
        }
      }
      if(z$cellcolor[1,i+1]!="white")
        firstrow=paste(firstrow,"\\cellcolor{",z$cellcolor[1,i+1],"}",sep="")
      if(z$frontcolor[1,i+1]!=z$color)
        firstrow=paste(firstrow,"\\color{",z$frontcolor[1,i+1],"}",sep="")
      if(z$colnames.bold) boldcn=paste("\\textbf{",cn[i],"}",sep="")
      else boldcn=cn[i]
      result=1
      if(!is.null(ztable:::isspanCol(z,1,(i+1)))){
        result=ztable:::isspanCol(z,1,(i+1))
        if(result>0) boldcn=paste("\\multicolumn{",result,"}{c}{",boldcn,"}",sep="")
        else if(result==0) next
      } else if(!is.null(ztable:::isspanRow(z,1,(i+1)))){
        boldcn=paste("\\multirow{",ztable:::isspanRow(z,1,(i+1)),"}{*}{",boldcn,"}",sep="")
      }
      if((subcolnames==1)) {
        if(is.na(z$subcolnames[i])){
          # boldcn=paste("\\multirow{2}{*}{",boldcn,"}",sep="")
          boldcn=""
        }
      }
      firstrow=paste(firstrow,boldcn,sep="")
      if(!is.null(colCount)){
        if(i %in% colCount[-length(colCount)]) {
          if(vlines[i+2]==0) {
            #if(z$cellcolor[1,i+1]!="white")
            #    firstrow=paste(firstrow,"&\\cellcolor{",z$cellcolor[1,i+1],"}",sep="")
            #else firstrow=paste(firstrow,"&",sep="")
            firstrow=paste(firstrow,"&",sep="")
          }
        }
      }
    }
  }

  if((0 %in% z$prefix.rows) & !is.null(z$top.command)) cat(z$top.command)
  if(z$include.colnames) {
    cat(paste(firstrow,"\\\\ \n",sep=""))
    if(subcolnames){
      if(z$include.rownames) {
        if(z$cellcolor[1,1]!="white")
          cat(paste("\\cellcolor{",z$cellcolor[1,1],"} &",sep=""))
        else cat("&")

      }
      for(i in 1:length(z$subcolnames)){
        if(is.na(z$subcolnames[i])) {
          temp=paste("\\multirow{-2}{*}{",colnames(z$x)[i],"}",sep="")
          if(!is.null(z$colcolor)){
            if(z$frontcolor[1,i+1]!=z$color)
              temp=paste("\\color{",z$frontcolor[1,i+1],"}",temp,sep="")
            if(z$cellcolor[1,i+1]!="white")
              temp=paste("\\cellcolor{",z$cellcolor[1,i+1],"}",temp,sep="")
          }
          cat(temp)
          if(i!=length(z$subcolnames)) cat("&")
          if(i %in% colCount[-length(colCount)]) {
            if(vlines[i+2]==0){
              if((z$cellcolor[1,i+1]!="white") & (z$cellcolor[1,i+1]==z$cellcolor[1,i+2]))
                cat(paste("\\cellcolor{",z$cellcolor[1,i+1],"}&",sep=""))
              else cat("&")
            }
          }
          next
        }
        if(z$colnames.bold) boldcn=paste("\\textbf{",z$subcolnames[i],"}",sep="")
        else boldcn=z$subcolnames[i]

        if(z$cellcolor[1,i+1]!="white")
          cat(paste("\\cellcolor{",z$cellcolor[1,i+1],"}",boldcn,"&",sep=""))
        else cat(paste(boldcn,"&",sep=""))
        if(i %in% colCount[-length(colCount)]) {
          if(vlines[i+2]==0){
            if((z$cellcolor[1,i+1]!="white") & (z$cellcolor[1,i+1]==z$cellcolor[1,i+2]))
              cat(paste("\\cellcolor{",z$cellcolor[1,i+1],"}&",sep=""))
            else cat("&")
          }
        }
      }
      cat("\\\\ \n")
    }
    if(is.null(z$hline.after)) cat(ifelse(z$booktabs,"\\midrule\n","\\hline\n"))
    else if(0 %in% z$hline.after) cat(ifelse(z$booktabs,"\\midrule\n","\\hline\n"))
  }

  for(i in 1:nrow){
    printcline=0
    if(rgroupcount>0) {
      if(i %in% printrgroup) {
        for(k in 1:length(printrgroup)){
          if(i == printrgroup[k]){
            if(is.na(z$rgroup[k])) break
            if(z$rgroup[k]=="") break
            printRowGroup(z,i)
            break
          }
        }

      }
    }
    if(i %in% z$prefix.rows) {
      #if(is.numeric(z$zebra))
      #   cat(paste("\\rowcolor{",z$zebra.color[i],"}",sep=""))
      if(!is.null(z$commands[i])) cat(z$commands[i])
    }
    tempo=NULL
    if(z$include.rownames) {
      tempo=rownames(z$x)[i]
      if(z$frontcolor[i+1,1]!=z$color) {
        tempo=paste("\\color{",z$frontcolor[i+1,1],"}",
                    tempo,sep="")
      }
      if(z$cellcolor[i+1,1]!="white") {
        tempo=paste("\\cellcolor{",z$cellcolor[i+1,1],"}",
                    tempo,sep="")
      }
      if(!is.null(ztable:::isspanCol(z,(i+1),1)))
        tempo=paste("\\multicolumn{",ztable:::isspanCol(z,i+1,1),"}{c}{",tempo,"}",sep="")
      else if(!is.null(ztable:::isspanRow(z,(i+1),1))){
        result=ztable:::isspanRow(z,(i+1),1)
        if(result<0) tempo=paste("\\multirow{",result,"}{*}{",tempo,"}",sep="")
      }
      cat(tempo)
    }

    for(j in 1:ncount) {
      skip=0
      if(z$frontcolor[i+1,j+1]==z$color) temp1=xdata[i,j]
      else temp1=paste("\\color{",z$frontcolor[i+1,j+1],"}",
                       xdata[i,j],sep="")
      if(z$cellcolor[i+1,j+1]!="white") {
        temp1=paste("\\cellcolor{",z$cellcolor[i+1,j+1],"}",
                    temp1,sep="")
      }
      if(is.null(ztable:::isspanCol(z,(i+1),(j+1)))){
        if(is.null(ztable:::isspanRow(z,(i+1),(j+1)))){
          result=1

        } else {
          result=ztable:::isspanRow(z,(i+1),(j+1))
          if(result < 0) {
            k=ztable:::getspanRowData(z,i+1,j+1)
            if(z$cellcolor[i+1,j+1]=="white") temp2=xdata[k+1,j]
            else temp2=paste("\\cellcolor{",z$cellcolor[i+1,j+1],"}",
                             xdata[k-1,j],sep="")
            temp1=paste("\\multirow{",result,"}{*}{",temp2,"}",sep="")
          }
          else {
            skip=1
            result=0 #
            if(z$cellcolor[i+1,j+1]=="white") skipcolor=""
            else skipcolor=paste("\\cellcolor{",z$cellcolor[i+1,j+1],"}",sep="")
          }

        }

        if(j %in% colCount[-length(colCount)]) {
          if(vlines[j+2]==0) {
            backcolor=NULL
            if(!is.null(z$rowcolor)){
              if(z$rowcolor[i+1]!="white") backcolor=z$rowcolor[i+1]
            }
            if(is.null(backcolor)){
              if((z$cellcolor[i+1,j+1]!="white")&(z$cellcolor[i+1,j+1]==z$cellcolor[i+1,j+2]))
                backcolor=z$cellcolor[i+1,j+1]
            }
            if(is.null(backcolor)) temp1=paste(temp1,"&",sep="")
            else temp1=paste(temp1,"&\\cellcolor{",backcolor,"}",sep="")
            #temp1=paste(temp1,"&",sep="")
          }
        }

      } else {
        result=isspanCol(z,(i+1),(j+1))
        if(result>0) {
          width=spanColWidth(z,(i+1),(j+1))
          mcalign="c"
          mclinecount=vlines[j+width+1]
          if(mclinecount > 0) {
            for(k in 1:mclinecount)
              mcalign=paste(mcalign,"|",sep="")
          }
          temp1=paste("\\multicolumn{",result,"}{",mcalign,"}{",temp1,"}",sep="")
          if(isGroupCol(j,result,colCount))
            if(vlines[j+width+1]==0)
              #if((j+result)<ncol(z$x))
              temp1=paste(temp1,"&",sep="")
          #if((j+result-1) %in% colCount[-length(colCount)])
          #    if(vlines[j+result+1]==0) temp1=paste(temp1,"&",sep="")
        }
        else next
      }
      #browser()
      if(is.null(tempo)) {
        cat(temp1)
        tempo=temp1
      }
      else {
        if(result!=0) cat(paste("&",temp1,sep=""))
        else if(skip) cat(paste("&",skipcolor,sep=""))
      }

      if(!is.null(colCount)){
        count=j
        if(!is.null(ztable:::isspanCol(z,i+1,j+1))){
          result=ztable:::isspanCol(z,(i+1),(j+1))
          if(result>0) count=count+result
        }
        #if(count %in% colCount[-length(colCount)]) {
        #if(vlines[count+2]==0) cat("&")
        #if(z$cellcolor[i+1,j+1]=="white") cat("&")
        #else cat(paste("&\\cellcolor{",z$cellcolor[i+1,j+1],"}",sep=""))
        #}
      }
    }
    cat(paste("\\\\ \n",sep=""))
    if(i %in% z$hline.after)
      cat(ifelse(z$booktabs,ifelse(i==nrow,"\\bottomrule[1.2pt]\n","\\midrule"),"\\hline\n"))
  }
  if(is.null(z$hline.after)) cat(ifelse(z$booktabs,"\\bottomrule[1.2pt]\n","\\hline\n"))

  footer=attr(z$x,"footer")
  if(!is.null(footer) & (z$show.footer)){
    myfooter=caption2minipage(z,footer)
    myfooter=gsub("~","$\\sim$",myfooter,fixed=TRUE)
    cat(paste("\\multicolumn{",totalCol,"}{l}{\\",Fontsize[headingsize],
              "{",myfooter,"}}\\\\ \n",sep=""))
  }
  if(!is.null(z$caption) & z$caption.placement=="bottom"){
    mycaption=caption2minipage(z,z$caption)
    if(z$caption.bold) cat(paste("\\multicolumn{",totalCol,"}{",
                                 z$caption.position,"}{\\textbf{",mycaption,"}}\\\\ \n",sep=""))
    else cat(paste("\\multicolumn{",totalCol,"}{",
                   z$caption.position,"}{",mycaption,"}\\\\ \n",sep=""))
  }

  if(z$longtable) {
    if(!is.null(z$label)) cat(paste("\\label{",z$label,"}\n",sep=""))
    cat("\\end{longtable}\n")
    cat(paste("\\end{",Fontsize[z$size],"}\n",sep=""))
  } else {
    cat("\\end{tabular}\n")
    cat(paste("\\end{",Fontsize[z$size],"}\n",sep=""))
    if(!is.null(z$label)) cat(paste("\\label{",z$label,"}\n",sep=""))
    if(sort!="tabular") {
      if((sort=="table") | (sort=="sidewaystable"))
        cat(paste("\\end{",z$position,"}\n",sep=""))
      cat(paste("\\end{",sort,"}\n",sep=""))
    }
  }
  #cat("\\color{black}\n")
}

ytable <- function(.t, ...) {
  x <- ztable(.t)
  z=update_ztable(z=x, ...)
  xdata=data2table(z)
  # str(z)
  # str(xdata)
  ytable2latex(z,xdata)
}
