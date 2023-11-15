#' @importFrom magrittr %>%
NULL

# These are all the main Causal Map 3 functions except for the NLP functions which are in a separate file.



# constants -----------------------------------------------------------------

contrary_color <- "#f2ab73"
ordinary_color <- "#05aa99"

# helpers -----------------------------------------------------------------
## standard functions  -----------------------------------------------------------------

## from DT package

remove_lines_starting_with <- function(text,char="#"){
  text %>%
    str_split("\n") %>%
    pluck(1) %>%
    keep(~!str_detect(.,paste0("^",char))) %>%
    collap
}



## overwriting the tidyr one because it is so picky
replace_na <- function(vec,rep){
  # map(vec,~{if(is.na(.)) rep else .}) %>% unlist()
  vec[is.na(vec)] <- rep
  vec
}

replace_empty <- function(x,replacement=0){
  if(x=="") replacement else x
}

replace_null <- function(x,replacement=0){
  if(is.null(x)) replacement else x
}
replace_Inf <- function(x,replacement=0){
  # browser()
  ifelse(is.infinite(x),replacement , x)
}
replace_inf <- replace_Inf #alias

replace_zero <- function(x,replacement=0){
  if(length(x)==0) replacement else x
}



collap <- function(vec,sep="\n"){
  vec %>% paste0(collapse=sep)
}
collapc <- function(vec){
  vec %>% collap(",")
}
uncollap <- function(vec,sep="\n"){
  vec %>% map(function(x){
    str_split(x,pattern=sep) %>% pluck(1)

  })
}
uncollapc <- function(vec){
  vec %>% uncollap(",")
}

xc <- function(x, sep = " ") {
  str_split(x, sep)[[1]]
}

`%notin%` <- Negate(`%in%`)

time_stamp <- function(){
  Sys.time() %>% str_replace_all(":","-")
}

left_join_safe <- function (x, y, by = NULL, winner = "y", ...)
{
  if (is.null(by))
    by = intersect(colnames(x), colnames(y))
  if (winner == "y")
    x = x %>% select(-intersect(colnames(x), colnames(y)),
                     by)
  else y = y %>% select(-intersect(colnames(x), colnames(y)),
                        by)
  for (i in seq_along(by)) {
    y[, by[i]] <- coerceValue(unlist(y[, by[i]]), unlist(x[,
                                                           by[i]]))
  }
  left_join(x, y, by, ...)
}

# the deal with statement_id??
# source_id and statement_code which together define statement_id
# but there is little chance of them coming separated. they are never redefined.
# so we just save statement_id in the form source_id|statement_code.
# we also provide statement codes as a convenience
make_statement_id <- function(row){paste0(row$source_id," | ",row_statement_code)}
get_statement_code <- function(statement_ids){str_remove_all(statement_ids,"^.* \\| ")}
get_source_id <- function(statement_ids){str_remove_all(statement_ids," \\| .*$")}

# colours --------------------------------------------------------------

colorfun <- function(numvec,add_zero=T){
  # browser()
  ((scales::rescale(numvec,to=c(0,1),from=c(max(numvec),if(add_zero)0 else min(numvec))))^.8) %>%
    colorRamp(c(ordinary_color,"#FFFFFF"),bias=1)(.) %>% apply(1,function(x)rgb(x[1]/255,x[2]/255,x[3]/255))
  # colorRamp(xc("#2f78bc white"),bias=1)(.) %>% apply(1,function(x)rgb(x[1]/255,x[2]/255,x[3]/255))
  # map(~modCol("#0000ff",darken=-.,saturate=1-.)) %>% unlist

}


## from DT package
coerceValue <- function (val, old)
{
  # old=unlist(old)
  if (is.integer(old))
    return(as.integer(val))
  if (is.numeric(old))
    return(as.numeric(val))
  if (is.character(old))
    return(as.character(val))
  if (inherits(old, "Date"))
    return(as.Date(val))
  if (inherits(old, c("POSIXlt", "POSIXct"))) {
    val = strptime(val, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (inherits(old, "POSIXlt"))
      return(val)
    return(as.POSIXct(val))
  }
  if (is.factor(old)) {
    i = val %in% levels(old)
    if (all(i))
      return(val)
    warning("New value(s) \"", paste(val[!i], collapse = ", "),
            "\" not in the original factor levels: \"",
            paste(levels(old), collapse = ", "), "\"; will be coerced to NA.")
    val[!i] = NA
    return(val)
  }
  # warning("The data type is not supported: ", classes(old))
  val
}


fromJSONsafe <- function(vec){

# browser()
  vec <- vec %>% str_replace_all("\"\"","\"")
  map(vec,function(tx){
    # if(validate(tx))
      safely(~fromJSON(tx),otherwise=NA)() %>% .$result
    #else NA

  })
}

len_un <- function(vec){
  length(unique(vec))
}

clean_grv <- function(tx){
  tx %>% str_replace_all("'","&rsquo;") %>%
    str_replace_all("\"","&rsquo;") %>%
    str_replace_all("‘","&rsquo;") %>%
    str_replace_all("’","&rsquo;") %>%
    # strip_symbols() %>%
    str_replace_all("\"","'") %>%
    simplify_unicode
}



simplify_unicode <- function(texvec){
  texvec %>%
    str_replace_all("\u008d","'") %>%
    str_replace_all("\U008d","'") %>%
    str_replace_all("\u0085","-") %>%
    str_replace_all("\u0085","-") %>%
    str_replace_all("\u008e","'") %>%
    str_replace_all("\U008e","'") %>%
    str_replace_all("\u0092","`") %>%
    str_replace_all("\u008f","'") %>%
    str_replace_all("\u008g","'") %>%
    str_replace_all("\u2019","'") %>%
    str_replace_all("\u0090","'") %>%
    str_replace_all("\U0090","'") %>%
    str_replace_all("\UFFFD","") %>%    #that is the weird character
    str_replace_all("\xc9v","")
}


row_index <- function(df)1:nrow(df)



## special functions  -----------------------------------------------------------------

maxrn <-function(vec)max(vec,na.rm=T)
minrn <-function(vec)min(vec,na.rm=T)

keep_top_level <- function(vec)  vec %>% str_remove_all(";.*")
drop_top_level <- function(vec)  vec %>% map(~str_match(., ";.*") %>% replace_na(";") %>% str_remove("^;")) %>%
  unlist


make_factor_list <- function(links){

  main <-
    links %>%
    get_both_labels()

  tops <-
    main %>% keep_top_level() %>% unique

  c(tops,main) %>% unique
}
get_hashtags <- function(links){
  links$hashtags %>%  map(uncollapc) %>% unlist %>% unique %>% c("plain_coding")
}

get_both_labels <- function(link){c(link$from_label,link$to_label) %>% unique}




make_igraph_from_edgelist <- function(links){
  links %>% select(from_label,to_label) %>%
    filter(!is.na(from_label) & !is.na(to_label)) %>%
    as.matrix %>%
    igraph::graph_from_edgelist(directed = TRUE)
}

maxrn <-function(vec)max(vec,na.rm=T)
minrn <-function(vec)min(vec,na.rm=T)

make_factor_list <- function(links){

  main <-
    links %>%
    get_both_labels()

  tops <-
    main %>% keep_top_level() %>% unique

  c(tops,main) %>% unique
}
get_hashtags <- function(links){
  links$hashtags %>%  map(uncollapc) %>% unlist %>% unique %>% c("plain_coding")
}

get_both_labels <- function(link){c(link$from_label,link$to_label) %>% unique}

make_factors_from_links <- function(links){
  # browser()
  from_links <-
    links %>%
    select(from_label,source_id,any_of(c("found_from","tracing_source_found_from","tracing_target_found_from", "from_flipped"))) %>%
    group_by(from_label,across(any_of(c("found_from","tracing_source_found_from","tracing_target_found_from", "from_flipped")))) %>%
    summarise(from_source_count=len_un(source_id),
              from_sources=list(source_id %>% unique),
              from_frequency=n()) %>%
    ungroup

  to_links <-
    links %>%
    select(to_label,source_id,any_of(c("found_to","tracing_source_found_to","tracing_target_found_to","to_flipped"))) %>%
    group_by(to_label,across(any_of(c("found_to","tracing_source_found_to","tracing_target_found_to","to_flipped")))) %>%
    summarise(to_source_count=len_un(source_id),
              to_sources=list(source_id %>% unique),
              to_frequency=n()) %>%
    ungroup


  bound <-
    bind_rows(from_links %>% rename(label=from_label),to_links %>% rename(label=to_label))

  if("found_from" %notin% colnames(bound))bound$found_from <- NA
  if("tracing_source_found_from" %notin% colnames(bound)) bound$tracing_source_found_from <- NA
  if("tracing_target_found_from" %notin% colnames(bound)) bound$tracing_target_found_from <- NA
  if("found_to" %notin% colnames(bound))bound$found_to <- NA
  if("tracing_source_found_to" %notin% colnames(bound)) bound$tracing_source_found_to <- NA
  if("tracing_target_found_to" %notin% colnames(bound)) bound$tracing_target_found_to <- NA
  if("from_flipped" %notin% colnames(bound)) bound$from_flipped <- NA
  if("to_flipped" %notin% colnames(bound)) bound$to_flipped <- NA

  # browser()
  bound %>%
    group_by(label,across(any_of(c(
      "found_from",
      "tracing_source_found_from",
      "tracing_target_found_from",
      "found_to",
      "tracing_source_found_to",
      "tracing_target_found_to"
    )))) %>%
    mutate(
      found=replace_na(found_from,F)|replace_na(found_to,F),
    ) %>%
    mutate(
      found_source=replace_na(tracing_source_found_from,F)|replace_na(tracing_source_found_to,F),
      found_target=replace_na(tracing_target_found_from,F)|replace_na(tracing_target_found_to,F)
    ) %>%
    ungroup %>%
    select(-any_of("found_from"),-any_of("found_to")) %>%
    # extra step because when combining opposites, a label might be both found and not found in a tracing or focus step
    group_by(label) %>%
    mutate(found=any(replace_na(found,F))) %>%
    group_by(label,across(any_of(c("found", "found_source", "found_target")))) %>%
    summarise(source_count=c(unlist(from_sources),unlist(to_sources)) %>% len_un ,
              link_count=sum(from_frequency,to_frequency,na.rm=T),
              in_degree=sum(from_frequency,na.rm=T),
              out_degree=sum(to_frequency,na.rm=T),
              outcomeness=signif(100*out_degree/link_count) %>% replace_na(0),
              flipped_from=sum(from_flipped*from_frequency,na.rm=T),
              flipped_to=sum(to_flipped*to_frequency,na.rm=T)
    ) %>%
    ungroup
}

make_factors_from_transformed_linksWITHRETAINED <- function(links,sampled_links,original_links){  # this is the transformed version where we need to add original data

  from_links <-
    links %>%
    select(
      label=from_label,
      from_link_id=link_id,
      from_link_count=link_count,
      from_source_count=source_count,
      from_sources=sources
    )

  to_links <-
    links %>%
    # filter(retained) %>%
    select(
      label=to_label,
      to_link_id=link_id,
      to_link_count=link_count,
      to_source_count=source_count,
      to_sources=sources
    )


  bind_rows(from_links,to_links) %>%
    group_by(label,retained) %>%
    mutate(
      out_degree=sum(from_link_count,na.rm=T),
      in_degree=sum(to_link_count,na.rm=T),
      link_count=sum(from_link_count,to_link_count,na.rm=T),
      source_count=sum(from_source_count,to_source_count,na.rm=T),
      outcomeness=signif(100*from_link_count/link_count) %>% replace_na(0),
      original_out_degree=sum(from_link_count*retained,na.rm=T) %>% replace_na(0),
      original_in_degree=sum(to_link_count*retained,na.rm=T) %>% replace_na(0),
      original_link_count=sum(from_link_count*retained,to_link_count*retained,na.rm=T) %>% replace_na(0),
      original_source_count=sum(from_source_count*retained,to_source_count*retained,na.rm=T) %>% replace_na(0),
      original_outcomeness=signif(100*from_link_count*retained/link_count*retained) %>% replace_na(0)
    )%>%
    ungroup %>%
    filter(retained)
}

make_factors_from_transformed_links <- function(links){  # this is the transformed version where we need to add original data

  from_links <-
    links %>%
    select(
      label=from_label,
      from_link_id=link_id,
      from_link_count=link_count,
      from_source_count=source_count,
      from_sources=source_ID
    )

  to_links <-
    links %>%
    # filter(retained) %>%
    select(
      label=to_label,
      to_link_id=link_id,
      to_link_count=link_count,
      to_source_count=source_count,
      to_sources=sources
    )


  bind_rows(from_links,to_links) %>%
    group_by(label) %>%
    mutate(
      out_degree=sum(from_link_count,na.rm=T),
      in_degree=sum(to_link_count,na.rm=T),
      link_count=sum(from_link_count,to_link_count,na.rm=T),
      source_count=sum(from_source_count,to_source_count,na.rm=T),
      outcomeness=signif(100*from_link_count/link_count) %>% replace_na(0)
    )%>%
    ungroup
}

make_igraph_from_edgelist <- function(links){
  links %>% select(from_label,to_label) %>%
    filter(!is.na(from_label) & !is.na(to_label)) %>%
    as.matrix %>%
    igraph::graph_from_edgelist(directed = TRUE)
}




get_initials <- function(lis){
  # browser()
  oldlen <- length(unique(lis))
  nch <- 1
  new <- str_sub(lis,1,nch)

  # shorten so still unique
  while(
    length(unique(str_sub(lis,1,nch)))!=oldlen
  ){
    nch <- nch+1
    new <- str_sub(lis,1,nch)
  }

  oldlen <- length(unique(new))
  # now strip any non-unique leading chars
  # nch <- 1

  # if(length(unique(str_sub(new,5)))==oldlen)return(new)

  if(length(unique(new))==1) return(new)
  # browser()

  # strip identical leading chars
  while(length(unique(str_sub(new,2)))==oldlen
        &
        length(unique(str_sub(new,1,1)))==1 # only strip leading chars if they are the same
  ){

    # if(min(nchar(new))<4) return(new)


    new <- str_sub(new,2)
  }

  new
}

get_chi_surprises <- function(shown,not_shown,field,get_initials=F){

  dat <-
    data.frame(shown,not_shown,field) %>%
    filter(!is.na(field)) %>%
    filter("N/A"!=(field)) %>%
    distinct() %>%
    column_to_rownames("field")
  res <- chisq.test(dat,correct = T)
  # res <- chisq.test(dat,simulate.p.value = T)
  dat <- dat %>% rownames_to_column("field") %>% rename(shown_n=shown,not_shown_n=not_shown)

  #argh because of inconsistent output of chi

  if(nrow(dat)>1){
    stdres <- res$stdres
  } else {

    message("skipping chisq with no comparison")

    stdres <- tibble(not_shown=res$stdres[1],shown=res$stdres[2])
    res$p.value <- 1
    # 1 <- res$p.value

  }

  p <- res$p.value %>% replace_na(1)
  if(p<.1) {
    #browser()
    stdres %>%
      as_tibble %>%
      add_column(field=dat$field %>% keep(~!is.na(.)) %>% keep(.!="N/A") %>% as.character()) %>%
      left_join(dat,by="field") %>%
      arrange(desc(shown)) %>%
      filter(shown>0) %>%
      {if(get_initials)mutate(field=(get_initials(field))) else .} %>%
      mutate(new=paste0((field)," (",shown_n,"/",shown_n+not_shown_n,")")) %>%
      pull(new) %>%
      collap(", ")
  } else "notsig"#list("notsig","notsig")

}

get_top_words <- function(tx){
  # browser()
  ww2 <- tx %>% unlist%>% str_replace_all("  "," ") %>% str_split(" ") %>% unlist %>% table %>% sort
  ww2[setdiff(names(ww2) , stopwords(source="snowball"))] %>% tail(15) %>% rev %>% names %>% collap(", ")
}

get_surprises_factors <- function(factors,links,field,tots,type){
  message("Looking for surprises f")
 # browser()
  from_links <-
    links %>%
    select(from_label,source_id,field=UQ(sym(field))) %>%
    group_by(from_label,field) %>%
    summarise(from_source_count=len_un(source_id),
              from_sources=list(source_id %>% unique),
              from_frequency=n()) %>%
    ungroup

  to_links <-
    links %>%
    select(to_label,source_id,field=UQ(sym(field))) %>%
    group_by(to_label,field) %>%
    summarise(to_source_count=len_un(source_id),
              to_sources=list(source_id %>% unique),
              to_frequency=n()) %>%
    ungroup


  bound <-
    bind_rows(
      from_links %>% select(field,label=from_label,source_count=from_source_count,frequency=from_frequency,sources=from_sources),
      to_links %>% select(field,label=to_label,source_count=to_source_count,frequency=to_frequency,sources=to_sources)
      ) %>%
    group_by(label,field) %>%
    summarise(source_count=length(unique(unlist(sources))),citation_count=sum(frequency)) %>%
    ungroup()


  if(type=="surprise_links"){
  res <-
    bound %>%
      select(label,field,shown=citation_count) %>%
      left_join(tots %>% select(tot,field=UQ(sym(field))))
  } else {
  res <-
    bound %>%
      select(label,field,shown=source_count) %>%
      left_join(tots %>% select(tot,field=UQ(sym(field))))
  }
  res %>%
    mutate(not_shown=tot-shown) %>%
    group_by(label) %>%
    summarise(stat=get_chi_surprises(shown,not_shown,field),overall=sum(shown)) %>%
    ungroup %>%
    mutate(x=ifelse(stat=="notsig",overall,paste0(overall," \u2197 ",stat))) %>%
    mutate(label3=paste0(label,"\n(",x,")")) %>%
    left_join(factors,by="label")

}
get_surprises <- function(links,field,tots,type="surprise_links"){
  message("Looking for surprises")
  groups <- links[,field] %>% unique %>% na.omit()
  bundles <- links[,c("from_label","to_label")] %>% distinct
  complete <- cross_join(groups,bundles)

  # complete is all the combinations

  if(type=="surprise_links"){

    res1 <-
      links %>%
      ungroup %>%
      group_by(from_label,to_label,UQ(sym(field))) %>%
      summarise(shown=n())

  } else   if(type=="surprise_sources"){
    res1 <-
      links %>%
      ungroup %>%
      group_by(from_label,to_label,UQ(sym(field))) %>%
      summarise(shown=len_un(source_id))

  }
  # if(res$shown==1)
  # browser()
  res <-
    res1 %>%
    left_join(complete,.,by=c(field,"from_label","to_label")) %>%
    mutate(shown=ifelse(is.na(shown),0,shown)) %>%
    left_join(tots,by=field) %>%
    filter(!is.na(shown)) %>%
    mutate(not_shown=tot-shown) %>%
    select(-tot) %>%
    mutate(not_shown=replace_na(not_shown,0)) %>%
    group_by(from_label,to_label) %>%
    mutate(stat=get_chi_surprises(shown,not_shown,UQ(sym(field)))) %>%
    # filter(retained) %>%
    group_by(from_label,to_label) %>%
    mutate(overall=sum(shown)) %>%
    ungroup

  links %>%
    # filter(retained) %>%
    left_join(res) %>%
    mutate(label=ifelse(stat=="notsig",overall,paste0(overall," \u2197 ",stat))) %>%
    filter(shown!=0)

}

pipe_retain_current_statements <- function(links,current_statements){
  # browser()
  if(is.null(current_statements))current_statements <- Inf
  links %>%
    filter(statement_id %in% current_statements) #%>%
  # retain(x) %>%
  # select(-x)

}
pipe_discard <- function(links){
  if("retained" %notin% colnames(links))return(links)
  links %>%
    filter(retained)
}
# for combining opposites
clarify_opposites <- function(vec){
  str_replace_all(vec,"~","Worse / less / no / not -- ")
}
declarify_opposites <- function(vec){
  str_replace_all(vec,"Worse / less / no / not -- ","~")
}
flip_vector <- function(tex,flipchar="~",sepchar=";"){
  lapply(tex,function(x)flip_inner(x,flipchar=flipchar,sepchar=sepchar)) %>%
    unlist(recursive=F)
}
flip_fix_vector <- function(tex,flipchar="~",sepchar=";"){  # to get always one space between sep and flip
  tex %>%
    str_replace_all(paste0(sepchar," *",flipchar),paste0(sepchar,flipchar)) %>%
    str_replace_all(paste0(sepchar,flipchar," *"),paste0(sepchar,flipchar))
}

flip_inner_component <- function(tex,flipchar="~"){
  if_else(str_detect(tex,paste0("^ *",flipchar)),str_remove(tex,paste0("^ *",flipchar)),paste0("~",tex))
}
flip_inner <- function(tex,flipchar="~",sepchar=";"){
  tex %>%
    str_split(sepchar) %>%
    `[[`(1) %>%
    str_trim %>%
    flip_inner_component(flipchar=flipchar) %>%
    paste0(collapse="; ")
}
color_combined_links <- function(links){
  if("from_flipped" %notin% colnames(links)) return(links %>% mutate(color=ordinary_color))
  links %>% mutate(
    from_color = case_when(
      from_flipped  ~  contrary_color,
      T ~  ordinary_color
    )) %>%
    mutate(
      to_color = case_when(
        to_flipped  ~  contrary_color,
        T ~  ordinary_color
      )) %>%
    mutate(
      color=paste0(from_color,";0.5:",to_color)
    )


}

# main --------------------------------------------------------------------


make_print_map2 <- function(
    slinks,
    original,
    map_nodesep=.5,
    map_ranksep=.5,
    map_colour_opposites_red=F,
    map_color_factors_column="none",
    map_size_factors="source_count",
    map_size_links="source_count",
    map_label_factors="none",
    map_label_factors_type="normal",
    #    map_label_links="source_count",
    map_wrap_factor_labels=22,
    map_wrap_link_labels=22,
    legend=""
){


  # browser()
  if(map_label_factors_type=="surprise_links" & map_label_factors!="none"){

    message("going to look for surprises")
    tots <-
      slinks %>%
      group_by(UQ(sym(map_label_factors))) %>%
      summarise(tot=n())

  } else if(map_label_factors_type=="surprise_sources" & map_label_factors!="none"){

    message("going to look for surprises")
    tots <-
      slinks %>%
      group_by(UQ(sym(map_label_factors))) %>%
      summarise(tot=len_un(source_id))

  }

  original_nrow <- nrow(original)
  original_nsources <- original$source_id %>% len_un

  if("retained" %notin% colnames(slinks))slinks$retained=T


  if("label" %notin% colnames(slinks)) slinks$label <- "."

  labelled_links <-
    slinks %>%
    mutate(tooltip=paste0("Source: ",source_id, "\nStatement: ",statement_id,"\nHashtags: ",hashtags,"\nQuote: ",quote,"\n---")) %>%
    mutate(from_label= clean_grv(from_label)) %>%
    mutate(to_label= clean_grv(to_label)) %>%
    mutate(from_label=str_wrap(from_label,map_wrap_factor_labels)) %>%
    mutate(to_label=str_wrap(to_label,map_wrap_factor_labels)) %>%
    mutate(size_links=.data[[map_size_links]])

  links <-
    labelled_links %>%
    # group_by(sources,size_links,retained,from_label,to_label,label,source_count,link_count,original_sources,original_source_count,original_link_count) %>%
    group_by(size_links,from_label,to_label,source_count,link_count,across(any_of(xc("from_flipped to_flipped found found_from found_to")))) %>%
    summarise(
      link_id=link_id %>% collap(","),
      label=label %>% unique %>%   ifelse(.=="","-",.) %>% ifelse(.=="NA","-",.)  %>% ifelse(is.na(.),"-",.) %>% collap(", "),
      # label=label %>%   ifelse(.=="","-",.) %>% ifelse(.=="NA","-",.)  %>% ifelse(is.na(.),"-",.) %>% collap(", "),
      # label=label %>% unique %>% collap(", ") %>% ifelse(.=="","-",.),
      tooltip=clean_grv(collap(tooltip)),
      .groups="keep"
    ) %>%
    ungroup() %>%
    {if(map_wrap_link_labels!="Off") mutate(.,label=str_wrap(label,map_wrap_link_labels)) else .} %>%
    {if(map_size_links!="none") mutate(.,penwidth=size_links) else mutate(.,penwidth=1)} %>%
    color_combined_links



  if(nrow(links)==0)return()


  # or this could be links$label
  links$penwidth <- as.character(links$penwidth %>% as.numeric %>% scales::rescale(.,to=c(1,7)))
  if(min(as.numeric(links$penwidth)== max(as.numeric(links$penwidth))))links$penwidth <- "1"


  # if(is.null(recodes))
  recodes <- tibble(old=c(links$from_label,links$to_label) %>% unique) %>% mutate(new=old) %>% mutate(cluster=row_number())

  # links <- links[1:2,]
  tooltip_df <-
    recodes %>%
    ungroup %>%
    group_by(new,cluster) %>%
    #mutate(old=clean_grv(old)) %>%
    #mutate(new=clean_grv(new)) %>%
    mutate(new=str_wrap(new,map_wrap_factor_labels)) %>%
    summarise(.groups = "keep",n_factors=n(),tooltip=paste0('"',str_replace_all(old,'\n',' '),'"',collapse="\n") %>% clean_grv()) %>%
    rename(label2=new) %>%
    summarise_all(first) ## FIXME this shouldn't be necessary but i think we can get duplicates because of grv cleaning

  # %>%
  #   mutate(new=str_replace_all(new,"\n"," ")) %>%
  # browser()
  nodes_df <-
    labelled_links %>%
    make_factors_from_links() %>%
    mutate(label2=label) %>%
    # c(links$from_label,links$to_label) %>% clean_grv() %>% unique  %>%
    # tibble(label2=.) %>%
    left_join(.,tooltip_df) %>%
    ungroup %>%
    {if(map_color_factors_column!="none") mutate(.,fillcolor=.data[[map_color_factors_column]]%>% colorfun ) else .} %>%
    {if(map_size_factors!="none") mutate(.,fontsize=.data[[map_size_factors]]) else mutate(.,fontsize=2)} %>%
    mutate(fontsize=fontsize %>% as.numeric %>% scales::rescale(.,to=c(12,20))) %>%
    rename(cluster_number=cluster) %>%
    {
      if(str_detect(map_label_factors_type,"surprise")){
        # browser()
      get_surprises_factors(factors=.,links=labelled_links,field=map_label_factors,tots=tots,type=map_label_factors_type)
      }
      else if(str_detect(map_label_factors_type,"count")){ mutate(.,label3=paste0(label2," (",.data[[map_label_factors_type]],")"))}
      else mutate(.,label3=label2)}
  # browser()

  if("found" %in% colnames(nodes_df)){
    nodes_df <-
      nodes_df  %>%
      mutate(color=ifelse(found,"#22ccdd",ordinary_color)) %>%
      mutate(penwidth=ifelse(found,4,.5))
  }
  # browser()
  if("found_source" %in% colnames(nodes_df)){
    nodes_df <-
      nodes_df  %>%
      mutate(color=ifelse(found_source,"#cc22dd",color)) %>%
      mutate(penwidth=ifelse(found_source,4,penwidth))
  }
  if("found_target" %in% colnames(nodes_df)){
    nodes_df <-
      nodes_df  %>%
      mutate(color=ifelse(found_target,"#ccdd22",color)) %>%
      mutate(penwidth=ifelse(found_target,4,penwidth))
  }

  # browser()
  if(map_colour_opposites_red){

    nodes_df <-
      nodes_df  %>%
      mutate(fontcolor=ifelse(str_detect(label,"~"),"red","black"))

  } else {

    nodes_df <-
      nodes_df  %>%
      mutate(fontcolor="black")
  }

  # this does not yet work because make_factors_from_links does not reconstruct is_flipped
  if(F                   & "is_flipped" %in% colnames(nodes_df)){
    # browser()
    if(
      any(as.numeric(nodes_df$is_flipped)>0,na.rm=T) %>% replace_na(F)
      &
      "color.border" %notin% colnames(nodes_df)
    ){
      nodes_df$color= scales::div_gradient_pal(ordinary_color,"#eeeeee",contrary_color)(nodes_df$is_flipped)
    }

  } else {

  }

  #nodes_df$color= ordinary_color
  # browser()

  graph_title <- paste0(
    "\n---\nFilename: ",
    slinks$file %>% unique,
    ". Citation coverage ",
    signif(100*sum(links$link_count)/original_nrow,2),
    "%: "
    ,sum(links$link_count)
    ," of ",
    original_nrow,
    " total citations and ",
    len_un(labelled_links$source_id %>% unlist),
    " of ",
    original_nsources,
    " total sources are shown here.",

    "\n",
    if_else(map_label_factors_type!='normal',paste0('Numbers on factors show ',
           map_label_factors_type %>%
             str_replace_all('_',' ') %>%
             str_replace_all('factor','citation'),
           ifelse(map_label_factors_type %in% xc("count_all count_unique list_all list_unique surprise_factors surprise_sources"),paste0(": ",map_label_factors),""),
           "."),
           "")
    ,

    #if_else(map_label_factors!='none',paste0('\nNumbers on factors show ',map_label_factors %>% str_replace_all('_',' ') %>% str_replace_all('link','citation')),''),
    if_else(map_size_factors!='none',paste0('. Factor sizes show ',map_size_factors %>% str_replace_all('_',' ') %>% str_replace_all('link','citation')),''),
    if_else(map_color_factors_column!='none',paste0('. Darker factor colours show greater ',map_color_factors_column %>% str_replace_all('_',' ') %>% str_replace_all('link','citation')),''),
    ".\n",
    legend)
  grv_layout <- "dot"
  grv_splines <- "splines"
  grv_overlap <- F
  # nodesep <- 10
  # ranksep <- 10

  # links <-
  #   links %>%
  #   select(from_label,to_label,label,tooltip,link_id,penwidth)
# browser()
  graf <-
    create_graph() %>%
    add_nodes_from_table(
      table = nodes_df ,
      label_col = label
    ) %>%
    add_edges_from_table(
      table = links %>% select(from_label,to_label,label,penwidth,tooltip,color),
      from_col = from_label,
      to_col = to_label,
      from_to_map = label
    ) %>%
    set_node_attrs(label,nodes_df$label3)# %>% clean_grv()  )



  # browser()
  tmp <-
    graf %>%
    add_global_graph_attrs("label", graph_title, "graph") %>%
    add_global_graph_attrs("layout", grv_layout, "graph") %>%
    add_global_graph_attrs("splines", grv_splines, "graph") %>%
    add_global_graph_attrs("overlap", grv_overlap, "graph") %>%
    add_global_graph_attrs("labelloc", "bottom", "graph") %>%
    add_global_graph_attrs("labeljust", "c", "graph") %>%
    add_global_graph_attrs("outputorder", "nodesfirst","graph") %>%
    add_global_graph_attrs("tooltip", " ", "graph") %>%
    add_global_graph_attrs("rankdir", "LR", "graph") %>%
    add_global_graph_attrs("fontname", "Arial","graph")%>%
    add_global_graph_attrs("forcelabels", T, "graph") %>%
    add_global_graph_attrs("nodesep", map_nodesep,"graph") %>%
    add_global_graph_attrs("ranksep", map_ranksep,"graph") %>%

    add_global_graph_attrs("width", "0", "node") %>%
    add_global_graph_attrs("height", "0", "node") %>%
    add_global_graph_attrs("style", "rounded, filled","node") %>%
    # add_global_graph_attrs("penwidth", "0.5","node") %>%
    add_global_graph_attrs("fixedsize", "false","node") %>%
    add_global_graph_attrs("margin", "0.19","node") %>%
    add_global_graph_attrs("shape", "box","node") %>%

    add_global_graph_attrs("arrowtail","none", "edge") %>%
    add_global_graph_attrs("dir", "both","edge") %>%
    add_global_graph_attrs("style", "solid","edge") %>%
    add_global_graph_attrs("fontsize", 12, "edge") %>%
    render_graph()
  attr(tmp,"factors") <- nodes_df
  attr(tmp,"links") <- links
  tmp
}
get_from_excel <- function(path){
  preloaded <-
    readxl::excel_sheets(path %>% str_replace_all("\\\\", "/")) %>%
    set_names %>% map(~readxl::read_excel(path,sheet = .))

  names(preloaded) <- tolower(names(preloaded))
  # browser()

  preloaded$statements <-
    preloaded$statements %>%
    {if(!is.null(preloaded$sources))left_join_safe(.,preloaded$sources,by="source_id",winner="x") else . }%>%
    {if(!is.null(preloaded$questions))left_join_safe(.,preloaded$questions,by="question_id",winner="x") else .}


  #if(nrow(links)>0)
  if("link_id" %notin% colnames(preloaded$links)){
    preloaded$links <-
    preloaded$links %>%
      mutate(link_id=row_number())
  }

  preloaded$links <-
    preloaded$links %>%
    filter(!is.na(link_id)) %>%
    filter(!is.na(from_label)) %>%
    filter(!is.na(to_label)) %>%
    filter(!is.na(statement_id)) %>%
    distinct(link_id,.keep_all = T) %>%
    add_link_sources(preloaded$statements) %>%
    add_link_counts_simple()


  preloaded <-
    preloaded %>% "["(xc("factors links statements sources")) %>% compact

}
#
# # text <- "this is a larger text"
# # link <- list()
# # link$quote <- "larger"
# # link$id <- "larger"
# # link$color <- "red"
# # tooltip <- "asdf"
#
# statement_chooser_replace <- function(text,link,link_id,tooltip="toolt") {
#   # return(text)
#   # browser()
#   if(link$quote_end==0 | link$quote_start==0) return(text)
#   # text_stripped <- str_remove_all(text,"<.*?>")
#   # res <- afind(text,link$quote)
#   # loc <- res$location %>% as.vector()
#   # len <- res$match %>% as.vector() %>% nchar
#   # dis <- res$distance %>% as.vector()
#   #start_point <- link$quote_start
#
#   # how to find the right place when link$quote_start etc are anticipating a text without extra chips
#
#   startr <- "➡️"
#   endr <- "⬅️"
#   startr <-   paste0("<span id='link_edit_",
#                      link_id,
#                      "' class='linky quote' title='",
#                      tooltip,
#                      "' style='background-color:",
#                      link$color,
#                      "66;'>",
#                      " ",
#                      "</span>")
#   endr <-   paste0("<span id='link_edit_",
#                      link_id,
#                      "' class='linky quote_end' title='",
#                      tooltip,
#                      "' style='background-color:",
#                      link$color,
#                      "66;'>",
#                      " ",
#                      "</span>")
#   endr <-   paste0("<span id='link_edit_",
#                      link_id,
#                      "' class='linky quote_end' title='",
#                      tooltip,
#                      "' style='background-color:",
#                      link$color,
#                      "66;'>",
#                      " ",
#                      "</span>")
#
#   startr <-   paste0("",link_id,">>>")
#   endr <-   paste0("<<<",link_id,"")
#
#   paste0(
#   str_sub(text,1,link$quote_start-1)
#   ,
#   startr
#
#   ,
#   str_sub(text,link$quote_start,link$quote_end-1)
#   ,
#   endr
#   ,
#   str_sub(text,link$quote_end,-1)
#   )
#
# }
convert_to_cm2 <- function(table_list) {
  links <- table_list$links%>% rename(old_id=statement_id)
  statements <- table_list$statements %>% rename(old_id=statement_id) %>% mutate(statement_id=row_number())
  sources <- table_list$sources

  factors <- tibble(label=get_both_labels(links)) %>%
    mutate(factor_id=row_number())
  recodes <- (factors$factor_id %>% set_names(factors$label))
  links$from <- links$from_label %>% recode(!!!recodes)
  links$to <- links$to_label %>% recode(!!!recodes)
  srecodes <- (statements$statement_id %>% set_names(statements$old_id))
  links$statement_id <- links$old_id %>% recode(!!!srecodes)
  # links$hashtags <-
  #   links$hashtags %>% map(~{fromJSON(replace_na(.,"[]")) %>% unlist %>% collap(",")}) %>% unlist

  # browser()
  list(
    factors=factors,
    links=links,
    statements=statements,
    sources=sources
  )


}


#' Title
#'
#' @param table_list
#' @param file_name
#'
#' @return
#' @export
#' @examples
convert_from_cm2 <- function(table_list,file_name) {

  links <- NULL
  statements <- NULL
  sources <- NULL
  questions <- NULL


  if(is.null(table_list$statements)) {
    notify("Your table has no statements, not importing, sorry",4)
    return()
  }
  # browser()

  if(is.null(table_list$sources)) table_list$sources <- tibble(source_id=table_list$statements$source_id %>% unique)
  if(is.null(table_list$questions)) table_list$questions <- tibble(question_id=table_list$statements$question_id %>% unique,question_text="-")

  statements <-
    table_list$statements %>%
    select(text, source_id, question_id) %>%
    mutate(statement_code=row_number()) %>%
    mutate(statement_id=paste0(source_id," | ",statement_code)) %>%
    left_join_safe(table_list$sources,by="source_id",winner="x") %>%
    left_join_safe(table_list$questions,by="question_id",winner="x")

  if(!is.null(table_list$links)){

  links <-
    table_list$links %>%
    select(statement_id, from, to, quote,hashtags) %>%
    left_join(table_list$factors %>% select(from_label=label,from=factor_id)) %>%
    left_join(table_list$factors %>% select(to_label=label,to=factor_id)) %>%
    select(-from,-to,statement_code=statement_id) %>%
    left_join(statements,by="statement_code") %>%
    mutate(link_id=row_number()) %>%
    # mutate(hashtags = (hashtags %>% str_split(",") %>% map(toJSON)) %>% unlist) %>%
    select(-statement_code,-any_of("text"),-any_of("question_id")) %>%
    mutate(created = time_stamp()) %>%
    mutate(modified = time_stamp())
  }
  # %>%
  #   add_link_counts()

  # %>%
  #   mutate(retained=T)
  # %>%
  #   add_link_counts()


  if(!is.null(table_list$sources)){

  sources <-
    table_list$sources
  }
  # %>%
  #   mutate_all(as.character) %>%
  #   pivot_longer(cols=-(source_id))

  if(!is.null(table_list$statements))statements <- statements %>%
    select(-statement_code)%>%
    mutate(created = time_stamp()) %>%
    mutate(modified = time_stamp())

  files <-
    row <- tibble(
      file=file_name,
      modified=time_stamp()
      # edit=input$file_access_edit %>% c(sess$user) %>% unique %>% toJSON(), # you can't delete yourself. You have to add someone else and get them to remove you.
      # copy=input$file_access_copy %>% toJSON(),
      # view=input$file_access_view %>% toJSON(),
      # description=input$file_access_description,
      # archived=input$file_access_archived,
      # locked=input$file_access_locked

    )

  # browser()
  links <-
    links %>%
    add_link_sources(statements)
  res <-
    list(
      files=files,
      links=links,
      statements=statements,
      sources=sources,
      settings=tibble(setting="")
    )
  res %>%
    map(~mutate(.,file=file_name))
  # browser()
}


keep_level <- function(vec,level){
  vec %>%
    str_split(";") %>% map(~head(.,level) %>%
                             paste0(collapse=";")) %>% unlist
}
quip_recode <- function(tex) {
  # browser()
  recs <- quip_recodes[,2] %>% unlist
  names(recs) <- quip_recodes[,1] %>% map(~tolower(.)) %>% unlist
  recs["n/a"] <- NA


  tex <- tolower(tex) %>% str_trim
  recode(tex,!!!recs) %>% #this is how to provide the recode argument as a list
    replace_zero("")
}

make_closed_tabl <- function(sources){
  # if(is.null(graf$sources$source_id)) return()
  if(nrow(sources)<2) return()
  # browser()
  tmp <- sources %>%
    group_by(source_id) %>%
    select(matches("^Overall|_Overall|\\*"))

  # browser()
  if(ncol(tmp)<2) return(NULL)
  tmp %>%
    mutate_all(quip_recode) %>%
    ungroup %>%
    pivot_longer(-1) %>%
    rename(question=name)
  # %>%
  #   left_join_safe(sources,by="source_id",winner = "x")
}

make_mentions_tabl <- function(links){

  # %>%   browser()
  # graf$factors <- graf$factors[,colnames(graf$factors)!=""]
  # graf$links <- add_labels_to_links(graf$links,factors=graf$factors)
  influence <- links %>% mutate(label=from_label,direction="influence")
  consequence <- links %>% mutate(label=to_label,direction="consequence")

  either_from <- influence %>% mutate(direction="either")
  either_to <- consequence %>% mutate(direction="either")
  both <- bind_rows(consequence,influence,either_from,either_to)
  both %>% select(-from_label,-from_label) %>%
    mutate(label=str_replace_all(label,"\n"," ")) %>%
    mutate(mentions="any") %>%  ## this is actually just a hack so we can use this field in the Mentions table
    select(label,direction,mentions,link_id,everything())
}



retain <- function(links,condition){
  # browser()
  links <-
    links %>%
    mutate(xretained={{condition}})

  if("retained" %notin% colnames(links))links$retained <- T
  links$retained <-links$retained & links$xretained
  links$xretained <-NULL
  links
}


# links <- sess$file$links
# statements <- sess$file$statements assumes it already has source and question info
add_link_sources <- function(links,statements){
# browser()
  links %>%
    select(-any_of("source_id")) %>%
    left_join_safe(statements ,by="statement_id") %>%
    # left_join_safe(sources,by="source_id",winner="x") %>%
    # left_join_safe(questions,by="question_id",winner="x") %>%
    mutate(statement_code=get_statement_code(statement_id))

}

# important that this has to enforce treating any flipped citations as separate links
add_link_counts_simple <- function(links){
  links %>%
    group_by(from_label,to_label,across(any_of(xc("from_flipped to_flipped")))) %>%
    mutate(source_count=len_un(source_id),link_count=n()) %>%
    mutate(bundle=paste0(from_label," / ",to_label)) %>%
    ungroup

}
add_link_counts <- function(links,original_links){

  if(nrow(links)==0)return(links)

  links <-
    links %>%
    add_link_counts_simple()


  original_links <-
    original_links %>%
    rename(original_link_count=original_link_count) %>%
    rename(original_source_count=original_source_count) %>%
    select(link_id,asdfadsfasdf)

  links %>%
    left_join(original_links,by="link_id")


}

links <- function(lis){
  lis$links
}
# pipe_link_count_limit <- function(links,link_count_limit,type="Sources"){
#   # browser()
#   if("retained" %notin% colnames(links))links$retained <- T
#   counter <- ifelse(type=="Sources","source_count","link_count")
#
#   links <-
#     links %>%
#     add_link_counts()
#
#   # maxcount <- links[links$retained,counter] %>% max
#
#   # browser()
#   links %>%
#     mutate(x=.data[[counter]]>=as.numeric(link_count_limit)) %>%
#     retain(x)%>%
#     select(-x)
# }
# pipe_factor_count_limit <- function(links,factor_count_limit,type="Sources"){
#   if("retained" %notin% colnames(links))links$retained <- T
#   # browser()
#   counter <- ifelse(type=="Sources","source_count","link_count")
#   slinks <-
#    links %>% add_link_counts()
#   factors <-
#     slinks %>%
#     make_factors_from_transformed_links() %>%
#     filter(.data[[counter]]>=as.numeric(factor_count_limit))
#
#   links %>%
#     mutate(x=from_label %in% factors$label & to_label %in% factors$label) %>%
#     retain(x) %>%
#     select(-x)
#
# }

# formatting funs -------------------------------------------------------

pipe_label <- function(slinks,map_label_links="link_id",type="none"){

  #xc("None Count_all Count_unique List_all List_unique surprise_links surprise_sources")

  # slinks <-
  #   slinks %>%
  # browser()
  #   add_link_counts()
  if((map_label_links==""))return(slinks)
  if(is.null(map_label_links))return(slinks)
  if(map_label_links %notin% colnames(slinks)){
    message("map_label_links not in table")
    map_label_links <- "link_id"
  }
  # we want the background for the surprise to be everything not just this
  # do we need to calculate surprise
  is_surprise <-str_detect(type,"surprise")
  # browser()

  if(type=="surprise_links"){
    message("going to look for surprises")
    tots <-
      slinks %>%
      group_by(UQ(sym(map_label_links))) %>%
      summarise(tot=n())

  } else if(type=="surprise_sources"){
    message("going to look for surprises")
    tots <-
      slinks %>%
      group_by(UQ(sym(map_label_links))) %>%
      summarise(tot=len_un(source_id))

  }
  message("map label links is " %>% paste0(map_label_links,"\n"))
  slinks %>%
    {if(is_surprise) get_surprises(.,map_label_links,tots,type=type) else mutate(.,label="") } %>%
    {
      if(type=="none") mutate(.,label="") else
      if(type=="source_count") mutate(.,label=source_count) else
      if(type=="link_count") mutate(.,label=link_count) else
      if(type=="list_unique") mutate(.,label=.data[[map_label_links]]%>% unlist %>% unique %>% collap(", ")) else
      if(type=="list_all") mutate(.,label=.data[[map_label_links]]%>% unlist  %>% collap(", ")) else
      if(type=="count_unique") mutate(.,label=.data[[map_label_links]]%>% unlist %>% unique %>% length) else
      if(type=="count_all") mutate(.,label=.data[[map_label_links]]%>% unlist  %>% length) else
      # if(map_label_links=="sources") mutate(.,label=sources %>% unlist %>% unique %>% collap(", ")) else
        # if(map_label_links=="original_sources") mutate(.,label=original_sources %>% unlist %>% unique %>% collap(", ")) else
            if(!is_surprise) mutate(.,label= .data[[map_label_links]]) else
              .
      # if(map_label_links %in% colnames(sess$file$sources)) mutate(.,label=get_surprises(link_count,original_link_count))else
      # mutate(.,label= .data[[map_label_links]])
    }

}

# transform filters -------------------------------------------------------


#' Top factors
#'
#' @param links
#' @param top
#' @param type
#' @param which
#'
#' @return
#' @export
#'
#' @examples
pipe_top_factors <- function(links,top=10,type="Sources",which="Top"){
  # browser()
  counter <- ifelse(type=="Sources","source_count","link_count")
  factors <-
    links %>%
    add_link_counts_simple() %>%
    make_factors_from_links() %>%
    arrange(desc(.data[[counter]]))

  if(which=="Top"){

    factors <-
      factors %>%
      slice(1:top)
  }
  else {

    #  browser()
    factors <-
      factors %>%
      filter(.data[[counter]]>=as.numeric(top))
  }

  links %>%
    group_by(from_label,to_label) %>%
    filter(all(from_label %in% factors$label) & all(to_label %in% factors$label))

}
pipe_top_links <- function(links,top=10,type="Sources",which="Top"){
  counter <- ifelse(type=="Sources","source_count","link_count")

  links <-
    links %>%
    add_link_counts_simple() # note add link counts provides numbers for retained and nonretained separately

  if(which=="Top"){


    indx <-
      links %>%
      ungroup %>%
      group_by(.data[[counter]],from_label,to_label) %>%
      arrange(.data[[counter]]) %>%
      summarise(group=max(.data[[counter]]),.groups="keep") %>%
      ungroup %>%
      arrange(desc(.data[[counter]])) %>%
      filter(row_number()<=top) %>%
      select(from_label,to_label)

    indx %>%
      left_join(links,by=xc("from_label to_label")) %>%
      add_link_counts_simple()

  }
  else {
    links %>%
      filter(.data[[counter]]>=as.numeric(top)) %>%    # some of these are already nonretained but it doesn't matter
      add_link_counts_simple()

  }

}

pipe_zoom <- function(links,level=1){
  if(nrow(links)==0) return(links)
# browser()
  links %>%
    mutate(.,from_label=keep_level(from_label,level),to_label=keep_level(to_label,level)) %>%
    add_link_counts_simple()
}
pipe_combine_opposites <- function(links){

  # browser()

  factors <-
    links %>%
    make_factors_from_links() %>%
    mutate(
      unflipped_label=label,
      is_flipped=str_detect(label,paste0("^ *",flipchar)),
      try_label=if_else(is_flipped,flip_vector(label,flipchar = flipchar) %>% replace_null(""),label),
      label=flip_fix_vector(try_label)
    )

  # browser()

  if(nrow(factors)>0) links <-
      links %>%
      mutate(from_flipped=(recode(from_label,!!!(factors$is_flipped %>% set_names(factors$unflipped_label)))) %>% as.logical) %>%
      mutate(to_flipped=(recode(to_label,!!!(factors$is_flipped %>% set_names(factors$unflipped_label)))) %>% as.logical) %>%
      mutate(from_label=(recode(from_label,!!!(factors$label %>% set_names(factors$unflipped_label))))) %>%
      mutate(to_label=(recode(to_label,!!!(factors$label %>% set_names(factors$unflipped_label))))) %>%
      unite("flipped_bundle",from_flipped,to_flipped,sep = "|",remove=F)
  # %>%
  #   {if(add_colors)color_combined_links(.) else .}


  links %>%
    add_link_counts_simple()
}

# pipe_compact <- function(links,
#                        sess_links,
#                        from_labels=NULL,
#                        to_labels=NULL
# ){
#   browser()
#   slinks <-
#     links %>%
#     add_link_counts_simple()
#
#   slinks_left <-
#     slinks %>%
#     select(left_label=from_label,from_label=to_label,source_id,left_link_id=link_id,left_source_count=source_count) %>%
#     group_by(left_label) %>%
#     mutate(left_source_factor_count=len_un(source_id)) %>%
#     ungroup
#
#   slinks_right <-
#     slinks %>%
#     select(right_label=to_label,to_label=from_label,source_id,right_link_id=link_id,right_source_count=source_count)
#
#   number_of_mergeable_framents <-
#     slinks %>%
#     group_by(from_label) %>%
#     mutate(from_source_factor_count=len_un(source_id)) %>%
#     ungroup %>%
#     left_join(slinks_left,by=xc("from_label source_id"),relationship="many-to-many") %>%
#     select(left_source_factor_count,from_source_factor_count,source_id,left_label,left_link_id,from_label,link_id,to_label) %>%
#     # select(left_source_count,from_source_factor_count,left_source_factor_count,source_count,source_id,left_label,left_link_id,from_label,link_id,to_label) %>%
#     filter(!is.na(left_label)) %>%
#     group_by(source_id,from_label) %>%
#     mutate(left_n=len_un(left_link_id),to_n=len_un(to_label)) %>%
#     filter((left_n+to_n)<3) %>%
#     select(link_id) %>%
#     distinct %>%
#     nrow
#
#
#
#     arrange(from_source_factor_count,from_label,to_label) %>%
#     mutate(
#       merge=
#         from_source_factor_count<=left_source_factor_count
#       &
#         from_source_factor_count<=left_source_factor_count ### should have sth like only if not in top 10
#       &
#         from_label %notin% from_labels
#
#            )
#
#   # the left links get the to_label and the original link is deleted
#   relabels <-
#     new %>%
#     filter(merge) %>%
#     select(left_link_id,to_label)
#
#   delete_ids <-
#     new %>%
#     filter(merge) %>%
#     select(link_id) %>%
#     unique %>%
#     unlist
#
#   links %>%
#     filter(link_id %notin% delete_ids) %>%
#     select(-to_label) %>%
#     left_join(relabels %>% rename(link_id=left_link_id),by="link_id")
#
#
#
#   new %>%
#     View
#   #  mutate(new_from_label=left_label) %>%
#
#   # slinks %>%
#   #   left_join(slinks_left,by=xc("from_label source_id"),relationship="many-to-many") %>%
#   #   left_join(slinks_right,by=xc("to_label source_id"),relationship="many-to-many") %>%
#   #   select(source_id,left_label,left_link_id,from_label,link_id,link_count,to_label,right_link_id,right_label) %>%
#   #   group_by(from_label,to_label) %>%
#   #   arrange(desc(link_count)) %>%
#   #   View
#
# # we need to get a df with just link ids to delete
# # and a df with just link ids to modify (replace left or right label, later add other stuff)
#
# }

pipe_trace <- function(links,
                       sess_links,
                       from_labels=NULL,
                       to_labels=NULL,
                       steps=4,
                       transforms_tracing_strict=F,
                       transforms_tracing_threads=F,
                       any=F
){
  if(is.null(from_labels) & is.null(to_labels))return(links)

  fromids <- list()
  toids <- list()

  # browser()
  if(is.null(transforms_tracing_strict))transforms_tracing_strict <- F
  # work out what are the starting labels in order to arrive at fromids[[stage]] for each stage

  if(transforms_tracing_strict){

    dlinks <-
      sess_links %>%
      add_link_counts_simple() %>%
      filter(!is.na(from_label) & !is.na(to_label)) %>%
      select(link_id,from_label,to_label,source_id) %>%
      filter(link_id %in% links$link_id)

  } else {


    dlinks <-
      links %>%
      add_link_counts_simple() %>%
      filter(!is.na(from_label) & !is.na(to_label)) %>%
      select(link_id,from_label,to_label,source_id)
  }

  if(!is.null(from_labels)) {

    tolinks <- dlinks %>% rename(common=from_label)# just all the links, with the receiving slots renamed as common
    stage0 <-
      dlinks %>%
      #  %>%
      {
        if(any)filter(.,map(.$from_label,~{any(str_detect(.,coll(from_labels)))}) %>% unlist) else
          filter(.,from_label %in% from_labels)
        } %>%

      rename(common=from_label,common_source_id=source_id)

    tmps <- list()
    tmp <- stage0
    # browser()

    for(stage in 1:steps){
      message(stage %>% paste0("step: ",.))
      fromids[[stage]] <- tmp$link_id

      # tmps[[stage]] <- tmp
      tmp <-
        tmp %>%
        select(common=to_label,common_source_id) %>% # note how we flip it round
        distinct %>%
        left_join(tolinks,by="common",relationship="many-to-many") %>% # this is where we join it to the next stage
        filter(!is.na(to_label)) %>%
        {if(transforms_tracing_threads) filter(.,common_source_id==source_id) else .}

    }
  }

  if(!is.null(to_labels)) {
    fromlinks <- dlinks %>% rename(common=to_label)# just all the links, with the receiving slots renamed as common

    stage0 <-
      dlinks %>%
      {
        if(any)filter(.,map(.$to_label,~{any(str_detect(.,coll(to_labels)))}) %>% unlist) else
          filter(.,to_label %in% to_labels)
      } %>%
      rename(common=to_label,common_source_id=source_id)

    tmps <- list()
    tmp <- stage0
    # browser()
    for(stage in 1:steps){
      message(stage %>% paste0("step: ",.))
      toids[[stage]] <- tmp$link_id
      tmps[[stage]] <- tmp
      tmp <-
        tmp %>%
        select(common=from_label,common_source_id) %>%
        distinct %>%
        left_join(fromlinks,by="common",relationship="many-to-many") %>%
        filter(!is.na(from_label)) %>%
        {if(transforms_tracing_threads) filter(.,common_source_id==source_id) else .}
    }
  }

  if(!is.null(from_labels) & !is.null(to_labels)) {
    froms <- imap(fromids, ~ tibble(step=.y, link_id=.x)) %>% bind_rows %>% group_by(link_id) %>% summarise(step=min(step))
    tos <- imap(toids, ~ tibble(step=.y, link_id=.x)) %>% bind_rows %>% group_by(link_id) %>% summarise(step=min(step))
    common_ids <-
      full_join(froms,tos,by="link_id") %>%
      filter(`+`(step.x,step.y)<=(steps+1)) %>%
      pull(link_id)

  } else {
    # browser()
    if(is.null(to_labels)) common_ids <- fromids %>% unlist %>% unique
    if(is.null(from_labels)) common_ids <- toids %>% unlist %>% unique

  }
  # browser()

  if(any){
  links %>%
    filter(link_id %in% common_ids) %>%
    add_link_counts_simple()  %>%
    mutate(tracing_source_found_from=map(.$from_label,~{any(str_detect(.,coll(from_labels)))}) %>% unlist) %>%
    mutate(tracing_source_found_to=map(.$to_label,~{any(str_detect(.,coll(from_labels)))}) %>% unlist) %>%
    mutate(tracing_target_found_from=map(.$from_label,~{any(str_detect(.,coll(to_labels)))}) %>% unlist) %>%
    mutate(tracing_target_found_to=map(.$to_label,~{any(str_detect(.,coll(to_labels)))}) %>% unlist)

  } else {

  links %>%
    filter(link_id %in% common_ids) %>%
    add_link_counts_simple()  %>%
    mutate(tracing_source_found_from=from_label %in% from_labels) %>%
    mutate(tracing_source_found_to=to_label %in% from_labels) %>%
    mutate(tracing_target_found_from=from_label %in% to_labels) %>%
    mutate(tracing_target_found_to=to_label %in% to_labels)
  }

}

pipe_remove_brackets <- function(links,square=F,round=F){
  if(!square & !round)return(links)
  # browser()
  if(square) links <-
      links %>%
      mutate(from_label=str_remove_all(from_label,"\\[[^\\]]+\\]") %>% str_trim)
  if(round) links <-
      links %>%
      mutate(from_label=str_remove_all(from_label,"\\([^\\)]+\\)") %>% str_trim)
  if(square) links <-
      links %>%
      mutate(to_label=str_remove_all(to_label,"\\[[^\\]]+\\]") %>% str_trim)
  if(round) links <-
      links %>%
      mutate(to_label=str_remove_all(to_label,"\\([^\\)]+\\)") %>% str_trim)
  links %>% add_link_counts_simple()
}
pipe_retain_hashtags <- function(links,hashtags,keep=T){
  # browser()
  targets=hashtags
  if(keep){
    links %>%
      filter(map(hashtags,function(x){any(targets %in% uncollapc(x)) })%>% unlist) %>%
      add_link_counts_simple()
  } else {
    links %>%
      filter(map(hashtags,function(x){all(targets %notin% uncollapc(x)) })%>% unlist) %>%
      add_link_counts_simple()
  }
}
pipe_focus <- function(links,focus,any=F){
  # browser()
  links <-
    links %>% ungroup
  if(any){
    links %>%
      mutate(found_from=map(.$from_label,~{any(str_detect(.,coll(focus,ignore_case=T)))}) %>% unlist) %>%
      mutate(found_to=map(.$to_label,~{any(str_detect(.,coll(focus,ignore_case=T)))}) %>% unlist) %>%
      filter(found_from | found_to) %>%
      add_link_counts_simple()

  } else {

    links %>%
      mutate(found_from=map(.$from_label,~{any(. %in% focus)}) %>% unlist) %>%
      mutate(found_to=map(.$to_label,~{any(. %in% focus)}) %>% unlist) %>%
      filter(found_from | found_to) %>%
      add_link_counts_simple()

  }

}
pipe_exclude <- function(links,exclude,any=F){
  links <-
    links %>% ungroup
  # browser()
  if(any){
    links <-
      links %>%
      filter(!(map(.$from_label,~{any(str_detect(.,exclude))}) %>% unlist | map(.$to_label,~{any(str_detect(.,exclude))}) %>% unlist))

  } else {
    links <-
      links %>%
      filter(!(map(.$from_label,~{any(. %in% exclude)}) %>% unlist | map(.$to_label,~{any(. %in% exclude)}) %>% unlist))
  }
  links %>% add_link_counts_simple()

}

