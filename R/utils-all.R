get_os <- function(){

    ## Return operation system of the system
    ##
    ## Args:
    ##     None
    ##
    ## Returns:
    ##     os (str): Operating system of the hosting machine
    ##
    ## Example:
    ##     os = get_os()

    os = Sys.info()["sysname"]
    return(os)
}
