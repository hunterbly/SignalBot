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

get_calendar <- function(){

    ## Return some calendar object which handles weekends
    ##
    ## Args:
    ##     None
    ##
    ## Returns:
    ##     calendar

    calendar <- bizdays::create.calendar(name = "calendarday", weekdays=c('saturday', 'sunday'))
    return(calendar)
}
