
read_obs= function(dirpath=getwd(), obs_filenames=NULL, usms=NULL, usms_filename="usms.xml"){
  return(get_obs(dirpath = dirpath,
                 obs_filenames = obs_filenames,
                 usms = usms,
                 usms_filename = usms_filename))
}
