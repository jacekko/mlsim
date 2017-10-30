find_shapes <- function (rex = "", ignorecase = T) 
{
    dirlist <- ls(globalenv())
    for (i in dirlist) {
        obj <- get(i)
        if (is.function(obj)) {
            tmp <- deparse(obj)
            if (max(grepl(rex, tmp, ignore.case = ignorecase))) 
                print(i)
        }
    }
}
