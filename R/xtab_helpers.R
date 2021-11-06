#' parse geography name column from beyond20/20 export˚
#' @export
parse_geography_from <- function(data,geo_column="Geography"){
  data %>%
    dplyr::mutate(GeoUID=gsub("^.+ \\((\\d+\\.*\\d*)\\) .+$","\\1",!!as.name(geo_column))) %>%
    dplyr::mutate(Name=gsub("^(.+) \\((\\d+\\.*\\d*)\\).+$","\\1",!!as.name(geo_column)))
}

#' read in beyond20/20 export of statcan xtab into tidy long-format data frame
#' @export
get_multi_header_xtab <- function(path,parse_geography_column = function(data){data}){
  have_uid=FALSE
  header_rows=list()
  skip=0
  first_twenty <- readr::read_csv(path,col_types = readr::cols(.default = "c"),skip=0,n_max=20,guess_max=0,progress = FALSE,col_names = FALSE)

  while(!have_uid){
    header <- first_twenty[skip + 1,] %>% as.character %>% sub("_\\d+$","",.)
    header_rows[skip+1] <- list(header)
    skip=skip+1
    have_uid <- !is.na(header[1])
  }
  header <- seq(1,length(header_rows[[1]])) %>% purrr::map(function(i) lapply(header_rows, function(r)r[[i]]) %>% unlist %>% paste0(collapse = ";&;")) %>% unlist
  header[1]=header[1] %>% strsplit(";&;") %>% unlist %>% last
  categories <- header[2:length(header)]
  data <- readr::read_csv(path,col_types = readr::cols(.default = "c"),skip=skip,col_names=FALSE,guess_max=0,progress = TRUE) %>%
    purrr::set_names(header) %>%
    parse_geography_column %>%
    tidyr::gather(key="Type",value="Value",categories) %>%
    dplyr::mutate(Value=as.numeric(Value))

  labels <- data$Type %>% strsplit(";&;")
  label_length <- skip
  var_label <- 1
  new_labels <- seq(1,label_length) %>% purrr::map(function(i) labels %>% purrr::map(function(a)a[i]) %>% unlist)
  new_names <- new_labels %>% purrr::map(function(l){
    n <- l %>% unique
    nn=n[grepl("Total - ",n)]
    if (length(nn)==1) {
      nn=gsub("^Total - ","",nn)
    } else {
      # nn=n[grepl("Total ",n)]
      # if (length(nn)==1) {
      #   nn=gsub("^Total ","",nn)
      # } else {
      nn=paste0("Var_",var_label)
      var_label <<- var_label + 1
      #     }
    }
    nn
  })
  for (i in seq(1,label_length)){
    n=new_names[[i]] %>% as.character
    l=new_labels[[i]] %>% unlist
    data <- data %>%
      dplyr::mutate(!!n:=l)
  }
  data %>% dplyr::select(-Type)
}

#' get xtab in long form
#' @export
standardize_xtab <- function(data){
  ns <- names(data)[grepl("^Dim: ",names(data))]
  grep_string <- paste0(gsub(" \\(\\d+\\): .+$","",ns[1])," \\(\\d+\\): Member ID: \\[\\d+\\]: ")
  key_string <- paste0(gsub("^Dim: ","",gsub(" \\(\\d+\\): .+$","",ns[1])))

  data %>%
    cancensusHelpers::gather_for_grep_string(key_string,grep_string) %>%
    rlang::set_names(gsub("^DIM: | \\(\\d+\\)$","",names(.))) %>%
    cancensusHelpers::strip_columns_for_grep_string("^Notes: ") %>%
    dplyr::mutate(Value=as.numeric(Value,na=c("x", "F", "...", "..",NA)))
}



#' cache path for statcan data with given code
#' @export
xtab_path_for_code <- function(code){
  file.path(sub("/$","",getOption('custom_data_path')),paste0(code,"_ENG_CSV"),paste0(code,"_English_CSV_data.csv"))
}

#' @export
xtab_dir_for_code <- function(code){
  file.path(sub("/$","",getOption('custom_data_path')),paste0(code,"_ENG_CSV"))
}

#' @export
remove_xtab_for_code <- function(code){
  if (dir.exists(xtab_dir_for_code(code))) {
    unlink(xtab_dir_for_code(code),recursive=TRUE)
    print("Removed xtab")
  } else {
    print("Xtab not found")
  }
}

#' Download csv xtab data from url
#' @export
ensure_local_statcan_xtab<-function(code,url,refresh=FALSE,system_unzip=TRUE){
  path=xtab_path_for_code(code)
  if (refresh | !file.exists(path)) {
    if (is.na(url)) {
      number=109619+as.integer(substr(code,nchar(code)-2,nchar(code)))
      year=as.integer(substr(code,nchar(code)-6,nchar(code)-3))
      url = paste0("http://www12.statcan.gc.ca/census-recensement/",year,"/dp-pd/dt-td/CompDataDownload.cfm?LANG=E&PID=",number,"&OFT=CSV")
    }
    base_path=dirname(path)
    temp <- tempfile()
    download.file(url,temp)
    if (system_unzip) {
      decompression <-
        system2("unzip",
                args = c(paste0("-o -d ",base_path), # exdidecrr
                         temp),
                stdout = TRUE)
    } else {
      utils::unzip(temp,exdir = base_path)
    }
    unlink(temp)
  }
  path
}

#' Download csv xtab data from url, tag with code for caching
#' @export
xtab_for <- function(code,url=NA,refresh=FALSE,system_unzip=FALSE){
  path=ensure_local_statcan_xtab(code,url,refresh,system_unzip)
  data <- read_csv(path,col_types = cols(.default = "c")) %>%
    rename(GeoUID = `GEO_CODE (POR)`)
  ns <- names(data)[grepl("^Dim: ",names(data))]
  data %>% mutate_at(ns,as.numeric)
}

unzip_file <- function(file,exdir,system_unzip=FALSE) {
  if (system_unzip) {
    decompression <-
      system2("unzip",
              args = c(paste0("-o -d ",exdir), # exdidecrr
                       file),
              stdout = TRUE)
  } else {
    utils::unzip(file,exdir = exdir)
  }
}

#' Download xml xtab data from url, tag with code for caching
#' useful for older (2006) data that does not come as csv option
#'
#' @param code The statcan table code, needed for caching and for locating the table to extract
#' @param url The url to the xml table download. Only needed if the table is not cached, but
#' reommended to always include for reproducibility
#' @param refresh Will refresh cached data if set to TRUE, default is FALSE
#' @param time_value If set, will use this as the time variable, otherwise extract the time variable from the data.
#' Setting this will slightly speed up the parsing.
#' @param temp A path to the downloaded xml zip file. Useful if file has already been downloaded and should not be
#' downloaded again.
#'
#' @export
xml_xtab_for <- function(code,url,refresh=FALSE,time_value=NA,temp=NA,system_unzip=FALSE){
  data <- NA
  path <- file.path(getOption("custom_data_path"),paste0(code,".rda"))
  if (!file.exists(path) | refresh) {
    exdir <- tempdir()
    if (is.na(temp)) {
      temp <- tempfile()
      download.file(url,temp)
      unzip_file(temp,exdir=exdir,system_unzip = TRUE)
      unlink(temp)
    } else {
      unzip_file(temp,exdir=exdir,system_unzip = TRUE)
    }
    # read xml xtab for 2006 data.
    message("Parsing XML")
    xml_structure <- read_xml(file.path(exdir,paste0("Structure_",code,".xml")))
    xml_data <- read_xml(file.path(exdir,paste0("Generic_",code,".xml")))
    unlink(exdir,recursive = TRUE)

    str_concepts <- xml_structure %>% xml_find_all("//structure:ConceptScheme")
    var_concepts <- str_concepts %>%
      xml_find_all("structure:Concept") %>%
      xml_attr("id") %>% setdiff(c("TIME", "GEO", "OBS_VALUE", "OBS_STATUS" ))
    concepts <- c("GEO",var_concepts)

    concept_names <- lapply(concepts,function(c){
      xml_structure %>% xml_find_all(paste0("//structure:ConceptScheme/structure:Concept[@id='",c,"']/structure:Name[@xml:lang='en']")) %>%
        xml_text}) %>% unlist

    concept_lookup <- rlang::set_names(concept_names,concepts)

    descriptions_for_code <- function(c){
      c <- gsub("0$","",c)
      base <- xml_structure %>% xml_find_all(paste0("//structure:CodeList[@id='CL_",toupper(c),"']/structure:Code"))
      desc_text = ifelse(length(xml_find_all(base[1] ,".//structure:Description[@xml:lang='en']"))==0,".//structure:Description",".//structure:Description[@xml:lang='en']")
      rlang::set_names(
        base %>% purrr::map(function(e){e %>% xml_find_all(desc_text) %>% xml_text}) %>% unlist %>% trimws(),
        base %>% purrr::map(function(e){xml_attr(e,"value")}) %>% unlist
      )
    }

    series <- xml_find_all(xml_data,"//generic:Series")

    #series <- series[1:10]

    #l <- lapply(concepts,function(c){series %>% xml_find_all(paste0(".//generic:Value[@concept='",c,"']")) %>% xml_attr("value")})

    time_data <- function(series){
      if (!is.na(time_value)) {
        result=rep(time_value,length(series))
      } else {
        message("Extracting year data")
        series %>% xml_find_all(".//generic:Time") %>% xml_text
      }
    }
    value_data <- function(series){
      message("Extracting values data")
      #series %>% xml_find_all(".//generic:ObsValue") %>% xml_attr("value")
      series %>% xml_find_all("./generic:Obs") %>% xml_find_first("./generic:ObsValue") %>% xml_attr("value")
    }
    code_data <- function(series,c){
      message(paste0("Extracting data for ",concept_lookup[c]))
      series %>% xml_find_all(paste0(".//generic:Value[@concept='",c,"']")) %>% xml_attr("value")
    }

    df=concepts %>%
      purrr::map(function(c){code_data(series,c)}) %>%
      rlang::set_names(concept_names) %>%
      tibble::as.tibble() %>%
      dplyr::bind_cols(tibble::tibble(Year = time_data(series), Value = value_data(series)))

    for (i in seq(1,length(concepts),1)) {
      c=concepts[i]
      n=concept_names[i] %>% as.name
      lookup <- descriptions_for_code(c)
      nid=paste0(n," ID") %>% as.name
      df <- df %>%
        mutate(!!nid := !!n) %>%
        mutate(!!n := lookup[!!nid])
    }

    fix_ct_geo_format <- function(geo){
      ifelse(nchar(geo)==9,paste0(substr(geo,1,7),".",substr(geo,8,9)),geo)
    }

    data <- df  %>%
      rename(GeoUID=`Geography ID`) %>%
      mutate(GeoUID = fix_ct_geo_format(GeoUID),
             Value=as.numeric(Value))

    saveRDS(data,path)
  } else {
    data <- readRDS(path)
  }
  data
}

#' Download xml xtab data from url, tag with code for caching
#' useful for (2001) census profile data
#' @export
xml_census_2001_profile <- function(code,url,refresh=FALSE,time_value=NA,temp=NA){
  data <- NA
  path <- file.path(getOption("custom_data_path"),paste0(code,".rda"))
  if (!file.exists(path) | refresh) {
    exdir <- tempdir()
    if (is.na(temp)) {
      temp <- tempfile()
      download.file(url,temp)
      utils::unzip(temp,exdir=exdir)
      unlink(temp)
    } else {
      utils::unzip(temp,exdir=exdir)
    }
    # read xml xtab for 2006 data.
    message("Parsing XML")
    xml_structure <- read_xml(file.path(exdir,paste0("Structure_",code,".xml")))
    xml_data <- read_xml(file.path(exdir,paste0("Generic_",code,".xml")))

    str_concepts <- xml_structure %>% xml_find_all("//structure:ConceptScheme")
    var_concepts <- str_concepts %>%
      xml_find_all("structure:Concept") %>%
      xml_attr("id") %>% setdiff(c("TIME", "GEO", "OBS_VALUE", "OBS_STATUS" ))
    concepts <- c("GEO",var_concepts)

    concept_names <- lapply(concepts,function(c){
      xml_structure %>% xml_find_all(paste0("//structure:ConceptScheme/structure:Concept[@id='",c,"']/structure:Name[@xml:lang='en']")) %>%
        xml_text}) %>% unlist

    concept_lookup <- rlang::set_names(concept_names,concepts)

    descriptions_for_code <- function(c){
      c <- gsub("0$","",c)
      base <- xml_structure %>% xml_find_all(paste0("//structure:CodeList[@id='CL_",toupper(c),"']/structure:Code"))
      desc_text = ifelse(length(xml_find_all(base[1] ,".//structure:Description[@xml:lang='en']"))==0,".//structure:Description",".//structure:Description[@xml:lang='en']")
      rlang::set_names(
        base %>% purrr::map(function(e){e %>% xml_find_all(desc_text) %>% xml_text}) %>% unlist %>% trimws(),
        base %>% purrr::map(function(e){xml_attr(e,"value")}) %>% unlist
      )
    }

    series <- xml_find_all(xml_data,"//generic:Series")

    #series <- series[1:10]

    #l <- lapply(concepts,function(c){series %>% xml_find_all(paste0(".//generic:Value[@concept='",c,"']")) %>% xml_attr("value")})

    time_data <- function(series){
      if (time_value) {
        result=rep(time_value,length(series))
      } else {
        message("Extracting year data")
        series %>% xml_find_all(".//generic:Time") %>% xml_text
      }
    }
    value_data <- function(series){
      message("Extracting values data")
      #series %>% xml_find_all(".//generic:ObsValue") %>% xml_attr("value")
      series %>% xml_find_all("./generic:Obs") %>% xml_find_first("./generic:ObsValue") %>% xml_attr("value")
    }
    code_data <- function(series,c){
      message(paste0("Extracting data for ",concept_lookup[c]))
      series %>% xml_find_all(paste0(".//generic:Value[@concept='",c,"']")) %>% xml_attr("value")
    }

    df=concepts %>%
      purrr::map(function(c){code_data(series,c)}) %>%
      rlang::set_names(concept_names) %>%
      tibble::as.tibble()  %>%
      dplyr::bind_cols(tibble::tibble(Year = time_data(series), Value = value_data(series)))

    for (i in seq(1,length(concepts),1)) {
      c=concepts[i]
      n=concept_names[i] %>% as.name
      lookup <- descriptions_for_code(c)
      nid=paste0(n," ID") %>% as.name
      df <- df %>%
        mutate(!!nid := !!n) %>%
        mutate(!!n := lookup[!!nid])
    }

    fix_ct_geo_format <- function(geo){
      ifelse(nchar(geo)==9,paste0(substr(geo,1,7),".",substr(geo,8,9)),geo)
    }

    data <- df  %>%
      rename(GeoUID=`Geography ID`) %>%
      mutate(GeoUID = fix_ct_geo_format(GeoUID),
             Value=as.numeric(Value))

    unlink(exdir,recursive = TRUE)
    saveRDS(data,path)
  } else {
    data <- readRDS(path)
  }
  data
}

#' Set up virtual conda env
#' @export
install_python_environment <- function(python_path="/anaconda3/bin/python3"){
  reticulate::use_python(python_path)
  reticulate::conda_binary(conda = python_path)
  reticulate::conda_remove("r-reticulate")
  reticulate::conda_create("r-reticulate",packages="lxml")
}

#' Use python to parse xml
#' @export
xml_to_csv <- function(code,url,python_path="/anaconda3/bin/python3",refresh=FALSE,time_value=NA,temp=NA){
  path <- file.path(getOption("custom_data_path"),paste0(code,".csv"))
  if (refresh | !file.exists(path)) {
    remove_temp=FALSE
    if (is.na(temp)) {
      message("Downloading file")
      remove_temp=TRUE
      temp <- tempfile()
      download.file(url,temp)
    }
    message("Converting xml to csv")
    reticulate::use_python(python_path)
    reticulate::conda_binary(conda = python_path)
    reticulate::use_condaenv("r-reticulate")
    statcan = reticulate::import_from_path("statcan",file.path(system.file(package="cancensusHelpers")))
    statcan$convert_statcan_xml_to_csv(temp,path)
    if (remove_temp) unlink(temp)
    message("Done converting xml to csv")
  }
  path
}


#' Download xml xtab data from url, tag with code for caching
#' useful for older (2006) data that does not come as csv option
#'
#' @param code The statcan table code, needed for caching and for locating the table to extract
#' @param url The url to the xml table download. Only needed if the table is not cached, but
#' reommended to always include for reproducibility
#' @param python_path path to python executable, expects a conda environment 'r-reticulate' to exist.
#' Can be set up with `install_python_environment`.
#' @param refresh Will refresh cached data if set to TRUE, default is FALSE
#' @param temp A path to the downloaded xml zip file. Useful if file has already been downloaded and should not be
#' downloaded again.
#'
#' @export
xml_via_python <- function(code,url,python_path="/anaconda3/bin/python3",refresh=FALSE,temp=NA){
  readr::read_csv(xml_to_csv(code,url,python_path=python_path,refresh=refresh,temp=temp))
}


#' @export
gather_for_grep_string <- function(data,gather_key,grep_string){
  vars <- names(data)[grepl(grep_string,names(data))]
  short_vars <- gsub(grep_string,"",vars)
  names(data) <- gsub(grep_string,"",names(data))
  data %>% tidyr::gather(key=!!gather_key,value="Value",short_vars)
}

#' @export
strip_columns_for_grep_string <- function(data,grep_string){
  data %>% dplyr::select(names(data)[!grepl(grep_string,names(data))])
}


#' convert csv to sqlite
#' from https://rdrr.io/github/coolbutuseless/csv2sqlite/src/R/csv2sqlite.R
#'
#' @param csv_file input csv path
#' @param sqlite_file output sql database path
#' @param table_name sql table name
#' @param transform optional function that transforms each chunk
#' @param chunk_size optional chunk size to read/write data, default=1,000,000
#' @param append optional parameter, append to database or overwrite, defaul=`FALSE`
#' @export
csv2sqlite <- function(csv_file, sqlite_file, table_name, transform=NULL,chunk_size=1000000, append=FALSE,col_types=NULL,na=c(NA,"..","F")) {
  # Read first 1000 rows just to determine col_types and col_names
  df <- readr::read_csv(csv_file, n_max=100,col_types = col_types,na=na)
  if (nrow(readr::problems(df)) > 0) print(readr::problems(df))

  # Convert column classes to character string for read_csv()
  # i.e. character, integer, integer => "cii"
  # This is done to ensure that read_csv doesn't try to re-determine
  # the column types for every chunk.
  col_names  <- colnames(df)
  if (is.null(col_types)) {
    type_trans <- c(character='c', numeric='d', integer='i', logical='l', Date='c')
    col_types  <- type_trans[sapply(df, FUN=class)]
    col_types  <- paste0(col_types, collapse="")
    cat("col_types:", col_types, "\n")
  }

  if (!is.null(transform)) df <- df %>% transform
  df <- as.data.frame(df)

  # Connect to database.
  if (!append) file.remove(sqlite_file)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=sqlite_file)

  # Read the data from the beginning of the CSV
  # Remember to skip the header
  chunk    <- 0
  rowsread <- 0
  while(TRUE) {
    cat("Chunk:", chunk, "\n")
    # skip = iter*chunk_size + 1.  the "+1" is because we can now skip the header
    # The "%>% as.data.frame" is because dbWriteTable is a bit fussy on data structure
    df <- readr::read_csv(csv_file, col_names, col_types, skip=chunk*chunk_size+1, n_max=chunk_size)
    if (nrow(readr::problems(df)) > 0) print(readr::problems(df))
    if (!is.null(transform)) df <- df %>% transform
    df <- as.data.frame(df)

    if (nrow(df) == 0) {
      cat("Out of rows...\n")
      break
    }
    if ((nrow(df) == 1) & (sum(!(is.na(df))) == 0)) {
      # this is a workaround for a bug in readr::read_csv when col_types is specified
      # and skip > #rows in file.  readr incorrectly returns a data.frame with a single row
      # of all NA values.
      cat("Out of rows (bugged)...\n")
      break
    }
    rowsread <- rowsread + nrow(df)
    cat("Rows read:", nrow(df), "  total so far:", rowsread, "\n")
    DBI::dbWriteTable(con, table_name, df, append=TRUE)
    chunk <- chunk + 1
  }
  cat("Total Read:", rowsread, "\n")

  DBI::dbDisconnect(con)
}


#' Get unique values for field
#'
#' @param connection Database connection
#' @param table_name Name of the table
#' @param field name of the field
#' @where_string optional where condition
#' @export
get_unique_values_for_field <- function(connection,table_name,field,where_string=NA){
  if (!is.na(where_string)) {
    where_string=paste0(" WHERE ",where_string,";")
  } else {
    where_string=";"
  }

  DBI::dbGetQuery(connection,paste0("SELECT DISTINCT `",field,"` FROM ",table_name, where_string))[[field]]
}

