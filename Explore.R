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
input_name.default <- 'output/20191016-explore-all.tsv'
input_name.default <- 'output/20191017-explore.tsv'
input_name.default <- 'output/20191018-explore.tsv'
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
  #filter(max_shape_depth<30&max_storage_width<34) %>% #old data
  factorize(.add_visuals = FALSE) %>%
  select(-gc) %>%
  group_by(vm,benchmark)




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
dat %<>% filter(substitution_threshold < 100)
dat %<>% filter(cpu < 15000)
dat %<>% filter(cpu < 8000)
"
"
with(dat, scatter3Drgl(x=max_shape_depth, y=max_storage_width, z=10*log10(substitution_threshold),
                    xlab='max shape depth',ylab='max storage width', zlab='threshold',
                    ticktype='detailed', nticks=30, cex=3,
                    colvar=cpu,col=viridis::viridis(colorrange(cpu), direction=-1)))
# add another point
scatter3Drgl(x = 0, y = 0, z = 0, add = TRUE, colkey = FALSE,
          pch = 18, cex = 3, col = 'black')

with(dat, scatter3Drgl(max_shape_depth, max_storage_width, 10*log10(substitution_threshold),
                       xlab='max shape depth',ylab='max storage width', zlab='threshold',
                       ticktype='detailed', nticks=30, cex=3,
                       colvar=mem, col=viridis::viridis(colorrange(mem), direction=-1)))
# add another point
scatter3Drgl(x = 0, y = 0, z = 0, add = TRUE, colkey = FALSE,
             pch = 18, cex = 3, col = 'black')

# .m <- dat %>% single_out(-substitution_threshold) %>% matrix_for_criterion('cpu','min')
.m <- dat %>% ungroup %>%  select(-substitution_threshold,-benchmark) %>% matrix_for_criterion('cpu','')
#.m <- dat %>% mutate(substitution_threshold=10*log10(substitution_threshold)) %>%  single_out(-max_storage_width) %>% matrix_for_criterion('cpu','max')
hist3Drgl(z=.m,col=viridis::viridis(colorrange(.m), direction=-1))

bench %>% filter(cpu < 15000) %$% hist(cpu)
"

"=-=-=-="


#--- >> -----
#bench.dw <- bench %>% single_out(-substitution_threshold, .keep_benchmark = TRUE, .make_min_default = TRUE)

bench.dw <- bench %>%  select(-substitution_threshold)


"
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
"
#--- << -----
"
base_family='Helvetica'
"


"
dat <- bench.dw %>% filter(benchmark == 'map[n]')
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

disaster_filter <- function(var) {
  #if_else(var > 3*min(var), NA_real_, var)
  if_else(var > 5*min(var), NA_real_, var)
}

bench2grid <- function(.data, criterion='cpu') {
  result <- expand_grid(max_storage_width=2:23, max_shape_depth=2:23) %>% mutate(!!criterion := NA_real_)
  original <- .data %>% transmute(
    max_shape_depth=max_shape_depth,max_storage_width=max_storage_width,
    original=TRUE,value=disaster_filter(!!(as.name(criterion))),original_value=!!(as.name(criterion)))
  result %<>% left_join(original, by= c("max_shape_depth", "max_storage_width"))
  result %<>% mutate(!!criterion := coalesce(value,!!as.name(criterion)))
  result
}

"
base_family='Helvetica'
"

.explore.raster <- function(.data, criterion, aspect, frac, barwidth, bartitle=waiver(), text=FALSE, breaks=waiver(), labels=waiver(), color_option='D',guides_right=TRUE,force_asp=NULL) {

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
    scale_fill_viridis_c(direction = -1,option=color_option,breaks=breaks,labels=labels,na.value='grey80') +
    scale_x_continuous(expand=c(0,0),breaks=.b,labels=.l,minor_breaks=NULL) +
    scale_y_continuous(expand=c(0,0),breaks=.b,labels=.l,minor_breaks=NULL)+
    ylab('') +
    coord_fixed()+
    facet_null()
  if (guides_right) {
    asp <- 1.15
    p <- p +
      theme(legend.position='right') +
      guides(fill=guide_colourbar(barheight=unit(barwidth,'npc'), label.position='right', title=bartitle, reverse=!text))
  } else {
    asp <- 0.9
    p <- p +
      theme(legend.position='top') +
      guides(fill=guide_colourbar(barwidth=unit(barwidth,'npc'), label.position='top', title=bartitle))
  }
  if (text) {
    p <- p + geom_shadowtext(aes(label=Label),
                             # bg.colour='grey50',
                             bg.r=0.1,
                             lineheight=1,size=2, nudge_y=-.1,
                             na.rm = TRUE,
                             # color='grey95'
                             color='white'
                             )
  }
  if (!is.null(force_asp)) {
    asp <- force_asp
  }
  save.plot(basename=input.basename,aspect=aspect,plot=p,base_asp=asp,base_height=5)
}

explore.raster <- function(.data, criterion='Time', tick.unit='s',aspect=c('cpu','NONE'), guides_right=TRUE,...) {
  b <- as.character(aspect[2]) %>% gsub('\\[(.+)\\]','-\\1', .)
  .aspect <- paste(aspect[1],b,sep='-')
  if (length(aspect) >= 3) {
    .which <- as.character(aspect[3]) %>% gsub('\\ .+','',.) %>% tolower
    .aspect <- paste(.which, .aspect,sep='-')
  }
  .data %>%
    .explore.raster(criterion, aspect=.aspect,
                    frac=.02,barwidth=0.8,text=TRUE,
                    labels=function (x) paste(x, tick.unit),guides_right=guides_right,...)
}

explore.raster.merge <- function(.data, criterion='cpu', aspect=c('cpu','NONE'),option='C', guides_right=TRUE,verbose=FALSE,frac=.02,...) {
  .aspect <- paste(aspect[1:2],collapse='-')
  if (length(aspect) >= 3) {
    .which <- as.character(aspect[3]) %>% gsub('\\ .+','',.) %>% tolower
    .aspect <- paste(.which, .aspect,sep='-')
  }
  .lbl <- if(verbose) c('favorable', 'unfavorable') else c('+','-')
  .data %>%
    .explore.raster(criterion, aspect=.aspect,
                    frac=frac, barwidth=0.6, bartitle=NULL,
                    breaks=identity, labels=.lbl,
                    color_option=option,guides_right=guides_right,...)
}

# ----- print all the things! -----

bench.dw %>%
  group_modify(~ bench2grid(.x, criterion='cpu')) %>%
  group_walk(function(.data, groups) {
    .factor <-  if (.data %>% .$original_value %>% max(na.rm=TRUE) %>% log10 > 3.5) {
      .001
    } else { 1 }
    .unit <- if (.factor<1) 's' else 'ms'
    .r <- function(x) {if(.factor<1) {round(x*.factor,digits=case_when(
      x*.factor>10 ~ 0,
      x*.factor>5 ~ 1,
      TRUE ~ 2))} else {round(x)}}
    .data %>%
      mutate(Time=.r(cpu),Label=.r(original_value)) %>%  #ms
      explore.raster('Time', .unit, aspect=c('cpu', as.character(groups$benchmark[1]), as.character(groups$vm[1])))
  }) %>% invisible


bench.dw %>%
  group_modify(~ bench2grid(.x, criterion='mem')) %>%
  group_walk(function(.data, groups) {
    .data %>%
      mutate(Memory=round(mem/1024),Label=round(value/1024)) %>%  #kbyte->mbyte
      explore.raster('Memory', 'MB', aspect=c('mem', as.character(groups$benchmark[1]), as.character(groups$vm[1])))
  }) %>% invisible


#handle reverse[n] extra.

# bench.dw.rn <- bench.dw.orig %>% filter(benchmark=='reverse[n]')
# bench.dw.rn %>%
#   bench2grid(criterion='cpu') %>%
#   mutate(Time=round(cpu/1000),Label=round(value/1000,digits = 1)) %>% #s
#   explore.raster('Time', 's', aspect=c('X-cpu', 'reverse[n]'))

bench.dw.cpu <- bench.dw %>%
  group_by(niladic,add=TRUE) %>%
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
  group_by(niladic,add=TRUE) %>%
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
  summarize(value=sum(value)) %>% ungroup %>% #filter(!is.na(value))
  mutate(value=scales::rescale(value)) %>%
  (function(.data) {
    explore.raster.merge(.data, 'value', aspect=c('result'),option='B',guides_right=FALSE,verbose=TRUE)
    .data
  })

narrow <- result %>% filter(!is.na(value)) %>% rename(fav=value)
narrow_result <-  expand_grid(max_storage_width=do.call("seq",as.list(range(narrow$max_storage_width))),
            max_shape_depth=do.call("seq",as.list(range(narrow$max_shape_depth)))) %>%
            mutate(value=NA_real_) %>%
    left_join(narrow, by= c("max_shape_depth", "max_storage_width")) %>%
    mutate(value=coalesce(value,fav))
narrow_result %>% (function(.data) {
    explore.raster.merge(.data, 'value', aspect=c('result-slice'),option='B',guides_right=TRUE,verbose=FALSE,force_asp=0.4,frac=.075)
    .data
  }) %>% invisible

#
# EOF
