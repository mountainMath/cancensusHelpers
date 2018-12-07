
#' Returns dataset with age data for the specified region
#' @param dataset Cancensus Datatset
#' @param regions Cancensus Regions
#' @export
get_age_data <- function(dataset,regions,should_sum=FALSE) {
  long_labels <- function(data){
    labels=cancensus::label_vectors(data)
    name_change=setNames(as.character(lapply(labels$Vector,function(x){return(labels %>% dplyr::filter(Vector==x) %>% dplyr::pull("Detail"))})),
                         labels$Vector)
    new_names=as.character(lapply(names(data),function(x){
      n=as.character(name_change[x])
      return(ifelse(is.na(n),x,n))
    }))
    names(data)=new_names
    return(data)
  }

  male <-cancensus::search_census_vectors('Total - Age',dataset,"Male", quiet=TRUE) %>%
    cancensus::child_census_vectors(TRUE) %>%
    dplyr::filter(!grepl("Average",label),!grepl(" to ",label),!grepl("5 years and over",label),!grepl("Under 5 years",label))
  female <-cancensus::search_census_vectors('Total - Age',dataset,"Female", quiet=TRUE) %>%
    cancensus::child_census_vectors(TRUE) %>%
    dplyr::filter(!grepl("Average",label),!grepl(" to ",label),!grepl("5 years and over",label),!grepl("Under 5 years",label))
  vectors <- rbind(male,female) %>% dplyr::pull("vector")
  male_data <- cancensus::get_census(dataset = dataset, regions=regions,level="Regions",labels="short", vectors=male$vector, quiet=TRUE)
  female_data <- cancensus::get_census(dataset = dataset, regions=regions,level="Regions",labels="short", vectors=female$vector, quiet=TRUE)

  labels=male_data %>% cancensus::label_vectors() %>% dplyr::pull("Detail")
  labels[labels=="100 years and over"]="100+"

  label_levels=c("Under 1 year",seq(1,99),"100+")

  data <- rbind(male_data %>% long_labels %>% dplyr::mutate(Gender="Male"), female_data %>%
                  long_labels %>%
                  dplyr::mutate(Gender="Female")) %>%
    dplyr::mutate(`100+`=`100 years and over`) %>% dplyr::select(c("Region Name","Gender",labels))
  if (should_sum) {
    selects <- setdiff(names(data),"Region Name")
    data <- data %>% dplyr::select(selects) %>% dplyr::group_by(Gender) %>% dplyr::summarize_all(sum,na.rm=TRUE)
  }
  plot_data <- data %>% tidyr::gather(key="Age", value="Population",labels) %>%
    dplyr::mutate(Age=factor(Age,levels=label_levels,ordered=TRUE))
  plot_data[plot_data$Gender=="Male","Population"]=-plot_data[plot_data$Gender=="Male","Population"]

  return (plot_data)
}

#' Assigns long labels
#' @export
detail_labels <- function(data){
  ns=names(data)
  labels=cancensus::label_vectors(data)
  for (i in 1:nrow(labels)) {
    ns <- gsub(labels[i,"Vector"],labels[i,"Detail"],ns)
  }
  names(data) <- ns
  return(data)
}

#' @export
format_currency <- function(x){paste0("$",format(x,big.mark = ","))}

#' @export
format_number <- function(x){format(x,big.mark = ",")}

#' @export
format_percent <- function(x,digits=1){paste0(round(x*100,digits),"%")}

#' @export
format_ratio <- function(x,digits=2){round(x,digits)}

#' Aggregate variables to common cts, returns data2 on new tiling matching data1 geography
#' @param data1 Cancensus CT level datatset for year1 < year2 to serve as base for common geography
#' @param data2 Cancensus CT level datatset for year2 to be aggregated to common geography
#' @param data2_sum_vars vector of variable names to by summed up when aggregating geographies
#' @export
common_cts <- function(data1,data2,data2_sum_vars) {
  cts_1 <- data1$GeoUID
  cts_2 <- data2$GeoUID
  cts_diff_1 <- setdiff(cts_1,cts_2) %>% sort
  cts_diff_2 <- setdiff(cts_2,cts_1) %>% sort

  d<-st_intersection(
    data2 %>% dplyr::filter(GeoUID %in% cts_diff_2) %>%
      rename(GeoUID2=GeoUID) %>%
      dplyr::select(GeoUID2) %>% dplyr::mutate(area2=st_area(geometry)),
    data1 %>% dplyr::filter(GeoUID %in% cts_diff_1) %>%
      dplyr::select(GeoUID) %>% dplyr::mutate(area=st_area(geometry))
  )

  d <- d %>% dplyr::mutate(area3=st_area(geometry)) %>% dplyr::mutate(ratio=as.numeric(area3/area2)) %>% dplyr::filter(ratio>0.1) %>% arrange(ratio)

  dd<- d %>% as.data.frame %>% dplyr::group_by(GeoUID) %>%summarize(ratio=sum(ratio)/n(),n=n())
  if(dd %>% dplyr::filter(n<=1) %>% nrow >0) {base::stop("problem with computing common ct data")}

  ct_translation <- lapply(split(d, d$GeoUID), function(x) x$GeoUID2)
  ct_translation2 <- lapply(split(d, d$GeoUID2), function(x) x$GeoUID)

  new2 <- data2 %>%
    dplyr::filter(GeoUID %in% cts_diff_2) %>%
    dplyr::mutate(GeoUID2=GeoUID) %>%
    dplyr::mutate(GeoUID=as.character(ct_translation2[GeoUID2])) %>%
    dplyr::group_by(GeoUID)

  nnew <- summarize_at(new2,data2_sum_vars,sum)

  data_2 <- rbind(data2 %>% dplyr::filter(!(GeoUID %in% cts_diff_2)) %>% dplyr::select("GeoUID",data2_sum_vars), nnew)
  return(data_2)
}

#' convert vector to string to be pasted into code
#' @export
paste_vector <- function(v){
  paste0('c(',purrr::map(v,function(d)paste0('"',d,'"')) %>% unlist %>% paste0(collapse=",\n"),')') %>% cat
}

#' swap names and values in named vector
#' @export
name_flip <- function(v){
  set_names(names(v),as.character(v))
}

#' turn two columns of a tibble into a named vector
#'
#' @param data a tibble
#' @param name_column string with the name of the column to be used as names in the vector
#' @param value_column string with the name of the column to be used as values in the vector
#' @return a named vector
#' @export
to_named_vector <- function(data,name_column,value_column){
  set_names(data[[value_column]],data[[name_column]])
}

#' helper for clean map theme for sf
#' @export
map_theme <- list(
  ggplot2::theme_void(),
  ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "transparent"))
)

#' Get variables for CTs making up Old Toronto
#' @export
get_old_toronto_data<-function(dataset,vectors=c(),labels="short",geo_format=NA,also_new_toronto=FALSE,aggregate=FALSE){
  old_toronto_cts <- list(CT=c("5350002.00","5350001.00","5350008.02","5350011.00","5350012.03","5350012.01","5350013.02","5350012.04","5350014.00","5350016.00","5350013.01","5350015.00","5350017.00","5350029.00","5350068.00","5350034.01","5350037.00","5350041.00","5350040.00","5350039.00","5350010.02","5350035.00","5350036.00","5350038.00","5350032.00","5350034.02","5350033.00","5350030.00","5350019.00","5350018.00","5350031.00","5350069.00","5350028.02","5350028.01","5350027.00","5350020.00","5350026.00","5350021.00","5350022.00","5350023.00","5350078.00","5350024.00","5350079.00","5350080.02","5350080.01","5350076.00","5350082.00","5350075.00","5350077.00","5350074.00","5350081.00","5350007.01","5350004.00","5350047.04","5350047.02","5350005.00","5350007.02","5350008.01","5350009.00","5350010.01","5350044.00","5350043.00","5350048.00","5350049.00","5350050.03","5350050.01","5350050.04","5350104.00","5350103.00","5350152.00","5350105.00","5350101.00","5350106.00","5350107.00","5350111.00","5350112.00","5350113.00","5350114.00","5350116.00","5350118.00","5350119.00","5350131.00","5350167.01","5350130.00","5350132.00","5350133.00","5350277.00","5350141.01","5350141.02","5350142.00","5350138.00","5350139.01","5350140.00","5350139.02","5350137.00","5350127.00","5350126.00","5350125.00","5350086.00","5350067.00","5350136.01","5350135.00","5350134.00","5350136.02","5350128.02","5350129.00","5350122.00","5350128.05","5350128.04","5350121.00","5350124.00","5350110.00","5350108.00","5350100.00","5350102.05","5350102.04","5350102.02","5350102.03","5350099.00","5350098.00","5350051.00","5350052.00","5350053.00","5350046.00","5350047.03","5350042.00","5350045.00","5350054.00","5350096.02","5350097.04","5350096.01","5350109.00","5350097.01","5350097.03","5350056.00","5350055.00","5350065.02","5350064.00","5350063.05","5350061.00","5350060.00","5350057.00","5350058.00","5350059.00","5350063.06","5350066.00","5350063.04","5350063.03","5350062.02","5350062.01","5350087.00","5350088.00","5350089.00","5350091.01","5350092.00","5350093.00","5350094.00","5350095.00","5350115.00","5350091.02","5350090.00","5350120.00","5350117.00","5350128.06","5350025.00","5350065.01","5350123.00","5350006.00","5350003.00"))
  short_cts <- sub("\\.\\d{2}$","",old_toronto_cts$CT) %>% unique

  old_toronto <- get_census("CA16",regions=list(CSD="3520005"),vectors=vectors,level="CT",labels=labels,geo_format=geo_format) %>%
    mutate(short_ct=sub("\\.\\d{2}$","",GeoUID)) %>%
    mutate(old=short_ct %in% short_cts & GeoUID != "5350167.02") %>%
    select(-short_ct)

  if (aggregate){
    summary_vars=c("Area (sq km)","Population", "Dwellings", "Households")
    if(length(vectors)>0) {
      variable_names=names(old_toronto)[grepl(paste(vectors,collapse="|"),names(old_toronto))]
      summary_vars=c(summary_vars,variable_names)
    } else {
      old_toronto <- old_toronto %>% rename(`Area (sq km)`=`Shape Area`)
    }
    old_toronto <- old_toronto %>%
      group_by(old) %>%
      summarize_at(summary_vars,sum,na.rm=TRUE) %>%
      mutate(`Region Name`="Old Toronto",
             type="",
             GeoUID="xxx")
  }

  if (!also_new_toronto){
    old_toronto <- old_toronto %>%
      filter(old==TRUE) %>%
      select(-old)
  }
  old_toronto
}

#' Add lat long coordiantes from geometry
#' @export
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


#' Simple key-value cache function accepting closures
#' @param object closure with return expression to be cached
#' @param key cache key
#' @param path path to cache the data
#' @param refresh bool option to force refresh of cache, default FALSE
#' @export
simpleCache <- function(object,key,path=getOption("custom_data_path"),refresh=FALSE){
  cache_path=file.path(path,key)
  if(!refresh & file.exists(cache_path)) {
    readRDS(cache_path)
  } else {
    data=object
    saveRDS(data,file=cache_path)
    data
  }
}

#' Get vector tile data, expects option variable nextzen_API_key to be set
#' @param bbox bounding box for which to get vector tile data
#' @export
get_vector_tiles <- function(bbox){
  rmapzen::mz_set_tile_host_nextzen(getOption("nextzen_API_key"))
  mx_box=rmapzen::mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
  rmapzen::mz_vector_tiles(mx_box)
}



#' load and parse census data for a given year
#' @export
get_cov_census_data <- function(year,use_cache=TRUE){
  base_name="CensusLocalAreaProfiles"
  year_name=paste0(base_name,year,".csv")
  path=paste0(getOption("custom_data_path"),year_name)
  if (!use_cache | !file.exists(path)) {
    base_data_url="ftp://webftp.vancouver.ca/opendata/csv/"
    destfile=tempfile()
    download.file(paste0(base_data_url,year_name),destfile=destfile)
    data <- read_csv(destfile,skip=4,locale=locale(encoding = "windows-1252"),na=c(NA,"..","F")) %>%
      mutate(IDX = 1:n())
    if (!("ID" %in% names(data))) {
      data <- data %>% mutate(ID=IDX)
    }
    if (!("Variable" %in% names(data))) {
      data <- data %>% rename(Variable=X1)
    }
    n<- names(data)[!grepl("^X",names(data))]
    data <- data %>%
      select(n) %>%
      mutate(Variable=ifelse(is.na(ID),paste0("filler_",IDX),paste0("v_",year,"_",ID,": ",Variable))) %>%
      select(-IDX,-ID)
    unlink(destfile)
    dd <- data %>% as.data.frame()
    row.names(dd)=dd$Variable
    d <- t(dd %>% select(-Variable))
    region_names <- rownames(d)
    transposed_data <- tibble::as.tibble(d) %>%
      dplyr::mutate_all(dplyr::funs(parse_number)) %>%
      mutate(NAME=case_when(
        grepl("CSD",region_names) ~ "City of Vancouver",
        grepl("CMA",region_names) ~ "Metro Vancouver",
        TRUE ~ region_names), Year=year)
    write_csv(transposed_data,path=path)
  }
  data <- read_csv(path)
  # %>% inner_join(get_neighbourhood_geos(),by="NAME")
}

#' Convenience function to serach for census variable
#' @export
find_cov_variables <- function(data,search_string){
  names(data)[grepl(search_string,names(data),ignore.case = TRUE)]
}

#' Get 2016 population ecumene data
#' @export
get_ecumene_2016 <- function(refresh=FALSE){
  path=file.path(getOption("custom_data_path"),"ecomene_2016")
  if (!dir.exists(path)){
    tmp <- tempfile()
    download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lecu000e16a_e.zip",tmp)
    unzip(tmp,exdir=path)
    dir(path)
    unlink(tmp)
  }
  read_sf(file.path(path,"lecu000e16a_e.shp"))
}

#' download zipped shapefile and read shapefile.
#' @param path URL string to zipped shape file
#' @param file_mask optional grep string in case there are several shape files in the package
#' @return an sf object with the data from the shape file
#' @export
get_shapefile <- function(path,file_mask=NA){
  tmp <- tempfile()
  download.file(path,tmp)
  tmpdir <- tempdir()
  utils::unzip(tmp,exdir=tmpdir)
  file_names <- dir(tmpdir,"*.shp$")
  if (is.na(file_mask)) {
    file_name=file_names[1]
  } else {
    file_name <- file_names[grepl(file_mask,file_names)][0]
  }
  message_string <- paste0("Reading ",file_name,".")
  if (length(file_names)>0) {
    message_string <- paste0(message_string,"\nIgnoring ",
                             paste0(setdiff(file_names,file_name),collapse = ", "),".")
  }
  message(message_string)
  data <- sf::read_sf(file.path(tmpdir,file_name))
  unlink(tmp)
  #unlink(tmpdir,recursive = TRUE)
  data
}


#' generate data for waffle plots`
#' @param data tibble with data
#' @param grouping_variables variables used for grouping
#' @param category column name for coloring
#' @param vakue column name that contains the counts
#' @param nrow number of rows in waffle
#' @param nrow number of columns in waffle
#' @export
waffle_tile <- function(data,grouping_variables,category="Type",value="Value",nrow=10,ncol=10){
  fix_remainders <- function(data,total){
    while (sum(data$diff)!=0) {
      dd <- sum(data$diff)
      s <- sign(dd)
      t <- top_n(data,1,-1 * s * rem)
      data <- data %>%
        mutate(waffleValue=ifelse(!!as.name(category)==t[[category]],waffleValue-1 * s,waffleValue),
               rem=ifelse(!!as.name(category)==t[[category]],rem+1*s,rem)) %>%
        mutate(diff=sum(waffleValue)-total)
    }
    data
  }

  total=nrow * ncol

  data %>%
    group_by_at(vars(grouping_variables)) %>%
    mutate(Total=sum(!!as.name(value),na.rm=TRUE)) %>%
    mutate(val=!!as.name(value)/Total*total) %>%
    mutate(waffleValue=round(val),
           rem=val-waffleValue) %>%
    mutate(diff=sum(waffleValue)-total) %>%
    do(fix_remainders(.,total)) %>%
    mutate(Value=waffleValue) %>%
    select(-waffleValue,-rem,-val,-diff) %>%
    filter(Value>0) %>%
    group_by_at(vars(c(grouping_variables,category))) %>%
    expand(counter=seq(1:Value)) %>%
    group_by_at(vars(grouping_variables)) %>%
    mutate(n=row_number()) %>%
    mutate(col=(n-1) %% ncol+1,
           row=floor((n-1)/ncol)+1)
}

#' waffle geometry for ggplot
#' @param color background color
#' @param size size of grid
#' @export
geom_waffle <- function(color = "white", size = 0.4,...){
  list(geom_tile(aes(x = row, y = col),color=color,size=size,...),
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank()))
}

#' @import xml2
#' @importFrom dplyr %>%
#' @importFrom rlang .data
NULL



