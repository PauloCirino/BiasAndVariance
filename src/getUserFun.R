getUserFun <- function(funInput){
    fun <- eval( parse( text = paste( 'function(x){ ',
                                      funInput,
                                      ' } ',
                                      sep = '')))
    fun
}