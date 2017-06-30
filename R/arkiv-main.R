#' Allows to overwrite the path to the config file
#' @export
rkiv.init <- function(path_to_config = "./rkiv.conf",default_location="./") {
  cfg = rkiv.new(default_location)
  cfg$path_to_config = path_to_config
  write_json(cfg, path_to_config,simplifyVector=TRUE)
}

#' Create a new configuration
rkiv.new <- function(default_location) {
  first_res=list(type="note",date="",info="empty object",dep=list(),fsize=0,loc="default")
  cfg = list(resources = list(first_res=first_res))
  cfg = rkiv.addloc(cfg,"default",default_location)
  return(cfg)
}

#' add a location
#' @export
rkiv.addloc <- function(rk,name,path,global=T) {
  rk$locations[[name]] = list(name=name,global=global,path=path)
  return(rk)
}

#' Internal function which recovers the description file
rkiv <- function(path_to_config = "./rkiv.conf") {

  # check if config exists, otherwise create it
  if (file.exists(path_to_config)) {
    cfg = read_json(path_to_config,simplifyVector=TRUE)
  } else {
    flog.warn("config file does not exists, creating it at %s",path_to_config)
  }

  class(cfg)="rkiv"

  return(cfg)
}

#' lists the resources
#' @export
print.rkiv <- function(rk) {
  for (nn in names(rk$resources)) {
    cat(sprintf("%s [%ik @ %s] - %s - %2i deps\n",nn,round(rk$resources[[nn]]$fsize[[1]]/1000),rk$resources[[nn]]$location,rk$resources[[nn]]$info,length(rk$resources[[nn]]$dep)))
  }
}


#' get handle to a resource by name
rkiv.getres <- function(rk,name) {
  if (name %in% names(rk$resources)) {
    rs = rk$resources[[name]]
    rs$rkiv = rk
    rs$name = name
    return(rs)
  } else {
    return(list(name=name,type="",date="",info="empty object",dep=list(),fsize=0,loc="default"))
  }
}


#' load the value of a given resource from an archive
#' @export
rkiv.load <- function(rk,name) {

  # check that dependent resource exists
  if (!(name %in% names(rk$resources))) {
    error("this resource does not exists")
  }

  rs = rk$resources[[name]]

  res_filename = sprintf("%s/%s.rkiv",rk$locations[[rs$location]]$path,tolower(rs$name))
  load(res_filename)

  return(value)
}

#' add dependency to a resource
rkiv.adddep <-function(rs,dname) {

  # check that dependent resource exists
  if (!(dname %in% rs$tmp$dep_available)) {
    error("this dependency is not known")
  }

  # add dependencies
  if (!(dname %in% rs$dependencies)) {
    rs$dep = c(rs$dep,dname)
  }

  return(rs)
}

# returns a resource
rkiv.start <-function(rk,res_name) {

  # get the resource
  res = rkiv.getres(rk,res_name)

  # this should reload the main config?
  res$start_time = Sys.time()
  res$location = "default"

  # append available dep @fixme, it should be only the resources available here
  res$tmp = list( dep_available = names(rk$resources))

  return(res)
}

#' store a resources into the archive
rkiv.put <- function(rk,rs,value) {

  # construct the file name and path
  res_file_name = sprintf("%s/%s.rkiv",rk$locations[[rs$location]]$path,tolower(rs$name))

  # save the variable to the matching file at the location
  save(value,file=res_file_name)

  # get the file size
  rs2 = copy(rs)
  rs2$fsize = file.size(res_file_name)
  rs2$time_end = Sys.time()
  rs2$checksum = md5sum(res_file_name)
  rs2$tmp = NULL # removing the link to parent
  rs2$rkiv = NULL
  rk$resources[[rs$name]]= rs2

  rk2 = copy(rk)
  class(rk2) = "list"
  write_json(rk2, rk$path_to_config[[1]])
  return(rk)
}



rkiv.test <- function() {

  # initialize and list
  rkiv.init()
  rk = rkiv()

  # create some random variables
  rs = rkiv.start(rk,"norm_draws")
  rs$info = "random draws from a normal distribution"
  #set.seed(rkiv.getseed(rk,"norm_draws"))
  X = rnorm(1000)
  rk = rkiv.put(rk,rs,X)

  # load them, and take the mean
  rs = rkiv.start(rk,"norm_mean")
  rs$info = "mean of the norm_mean resource"
  X  = rkiv.load(rk,"norm_draws")
  rs = rkiv.adddep(rs,"norm_draws")
  m = mean(X)
  rk = rkiv.put(rk,rs,m)


  # ---- syntax with implicit path! ---- #
  rs = rkiv0.start("norm_draws")
  rs$info = "random draws from a normal distribution"
  X = rnorm(1000)
  rkiv0.put(rs,X)

  rs = rkiv0.start("norm_draws2")
  rs$info = "random draws from a normal distribution"
  X = rnorm(1000)
  rkiv0.put(rs,X)

}
