## crude approach to deal with AWS instances
#' @importFrom sys exec_wait
#' @importFrom jsonlite read_json
#' @importFrom purrr map map_df
#' @importFrom dplyr filter pull


## WARNING: before running these scripts, you need to make sure
## you have configured `aws` correctly. The script here assumes
## a "datacarpentry" profile.
##
## $ aws configure --profile datacarpentry

get_aws_instances <- function() {

  fout <- tempfile()
  res <- sys::exec_wait("aws",
                        c("--profile", "datacarpentry",
                          "ec2", "describe-instances"),
                        std_out = fout)
  if (!identical(res, 0L))
    stop("error")

  jsonlite::read_json(fout)[[1]]

}

update_aws_security_group <- function(instance_id,
                                      security_group_id = "sg-a5dba9ef") {
  sys::exec_wait("aws",
                 c("--profile", "datacarpentry",
                   "ec2", "modify-instance-attribute",
                   "--instance-id", instance_id,
                   "--groups", security_group_id)
                 )
}

extract_aws_instances_slot <- function(aws) {
  aws %>%
    purrr::map("Instances") %>%
    unlist(recursive = FALSE)
}


extract_aws_info <- function(aws) {
  aws %>%
    extract_aws_instances_slot() %>%
    purrr::map_df(function(x) {
      list(instance_id = x[["InstanceId"]],
           instance_tag = x[["Tags"]][[1]][["Value"]])
    })

}
