
#' Returns dataset with age data for the specified region
#' @param dataset Cancensus Datatset
#' @param regions Cancensus Regions
#' @export
get_age_data <- function(dataset,regions,sum=FALSE) {
  long_labels <- function(data){
    labels=label_vectors(data)
    name_change=setNames(as.character(lapply(labels$Vector,function(x){return(labels %>% filter(Vector==x) %>% pull("Detail"))})),
                         labels$Vector)
    new_names=as.character(lapply(names(data),function(x){
      n=as.character(name_change[x])
      return(ifelse(is.na(n),x,n))
    }))
    names(data)=new_names
    return(data)
  }

  male <-search_census_vectors('Total - Age',dataset,"Male", quiet=TRUE) %>%
    child_census_vectors(TRUE) %>%
    filter(!grepl("Average",label),!grepl(" to ",label),!grepl("5 years and over",label),!grepl("Under 5 years",label))
  female <-search_census_vectors('Total - Age',dataset,"Female", quiet=TRUE) %>%
    child_census_vectors(TRUE) %>%
    filter(!grepl("Average",label),!grepl(" to ",label),!grepl("5 years and over",label),!grepl("Under 5 years",label))
  vectors <- rbind(male,female) %>% pull("vector")
  male_data <- get_census(dataset = dataset, regions=regions,level="Regions",labels="short", vectors=male %>% pull("vector"), quiet=TRUE)
  female_data <- get_census(dataset = dataset, regions=regions,level="Regions",labels="short", vectors=female %>% pull("vector"), quiet=TRUE)

  labels=male_data %>% label_vectors %>% pull("Detail")
  labels[labels=="100 years and over"]="100+"

  label_levels=c("Under 1 year",seq(1,99),"100+")

  data <- rbind(male_data %>% long_labels %>% mutate(Gender="Male"), female_data %>%
                  long_labels %>%
                  mutate(Gender="Female")) %>%
    mutate(`100+`=`100 years and over`) %>% select(c("Region Name","Gender",labels))
  if (sum) {
    data <- data %>% select(-`Region Name`) %>% group_by(Gender) %>% summarize_all(sum)
  }
  plot_data <- data %>% gather(key="Age", value="Population",labels) %>%
    mutate(Age=factor(Age,levels=label_levels,ordered=TRUE))
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
    data2 %>% filter(GeoUID %in% cts_diff_2) %>%
      rename(GeoUID2=GeoUID) %>%
      select(GeoUID2) %>% mutate(area2=st_area(geometry)),
    data1 %>% filter(GeoUID %in% cts_diff_1) %>%
      select(GeoUID) %>% mutate(area=st_area(geometry))
  )

  d <- d %>% mutate(area3=st_area(geometry)) %>% mutate(ratio=as.numeric(area3/area2)) %>% filter(ratio>0.1) %>% arrange(ratio)

  dd<- d %>% as.data.frame %>% group_by(GeoUID) %>%summarize(ratio=sum(ratio)/n(),n=n())
  if(dd %>% filter(n<=1) %>% nrow >0) {base::stop("problem with computing common ct data")}

  ct_translation <- lapply(split(d, d$GeoUID), function(x) x$GeoUID2)
  ct_translation2 <- lapply(split(d, d$GeoUID2), function(x) x$GeoUID)

  new2 <- data2 %>%
    filter(GeoUID %in% cts_diff_2) %>%
    mutate(GeoUID2=GeoUID) %>%
    mutate(GeoUID=as.character(ct_translation2[GeoUID2])) %>%
    group_by(GeoUID)

  nnew <- summarize_at(new2,data2_sum_vars,sum)

  data_2 <- rbind(data2 %>% filter(!(GeoUID %in% cts_diff_2)) %>% select("GeoUID",data2_sum_vars), nnew)
  return(data_2)
}

#' helper for clean map theme for sf
#' @export
map_theme <- list(
  ggplot2::theme_void(),
  ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "transparent"))
)

