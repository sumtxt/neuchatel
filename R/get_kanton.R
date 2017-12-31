#' Get the name, label or number of the canton from the Gemeindenummer 
#'
#'
#' @param x a \code{vector} of codes to be converted 
#' @param render Return \code{label} (default), the \code{name} or the \code{number}?  
#' @param bfs \code{x} is the Gemeindenummer (default) or the canton number? 
#' 
#' @details Each canton uses certain ranges of the Gemeindenummer (GEOSTAT-Nr. or BFS-Nr.)
#' which runs from 1 to 6806. This function converts Gemeindenummern either to the 
#' name of the canton (if \code{render="name"}), the two-character label 
#' (\code{render="label"}) or the canton number (\code{render="number"}). 
#' 
#' \code{x} can either be the 1-4 digit Gemeindenummer (set \code{bfs=TRUE})
#' or the 1-2 digits canton number (\code{bfs=FALSE}). If \code{x} is not 
#' a numeric vector, it is coerced to one.
#' 
#'  
#' @return a character vector of names/labels or a numeric vector.  
#'
#'
#' @examples 
#'  \dontrun{
#'  
#' 	 get_kanton( c("4951") )
#'   get_kanton( c("02", "3"), bfs=TRUE )
#' 
#'  }
#' 
#' 
#' 
#' 
#' @export
get_kanton <- function(x,render="label", bfs=TRUE){

	nam <- c("Zürich", "Bern", "Luzern", "Uri", "Schwyz", 
			"Obwalden", "Nidwalden", "Glarus", "Zug", 
			"Fribourg", "Solothurn", "Basel-Stadt", 
			"Basel-Landschaft", "Schaffhausen", 
			"Appenzell Ausserrhoden", "Appenzell Innerrhoden", 
			"St. Gallen", "Graubünden", "Aargau", 
			"Thurgau", "Ticino", "Vaud", "Valais", 
			"Neuchâtel", "Geneva", "Jura")

	lab <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", 
		"ZG", "FR", "SO", "BS", "BL", "SH", "AR", 
		"AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU")

	if ( !is.numeric(x) ) x <- as.numeric(x)

	if ( bfs==TRUE ){ 
		y = NA
		y = ifelse(1 <= x & x <= 299, 1, y) 
		y = ifelse(301 <= x & x <= 999, 2, y) 
		y = ifelse(1001 <= x & x <= 1199, 3, y) 
		y = ifelse(1201 <= x & x <= 1299, 4, y) 
		y = ifelse(1301 <= x & x <= 1399, 5, y) 
		y = ifelse(1401 <= x & x <= 1499, 6, y) 
		y = ifelse(1501 <= x & x <= 1599, 7, y) 
		y = ifelse(1601 <= x & x <= 1699, 8, y) 
		y = ifelse(1701 <= x & x <= 1799, 9, y) 
		y = ifelse(2001 <= x & x <= 2399, 10, y) 
		y = ifelse(2401 <= x & x <= 2699, 11, y) 
		y = ifelse(2701 <= x & x <= 2703, 12, y) 
		y = ifelse(2761 <= x & x <= 2899, 13, y) 
		y = ifelse(2901 <= x & x <= 2999, 14, y) 
		y = ifelse(3001 <= x & x <= 3099, 15, y) 
		y = ifelse(3101 <= x & x <= 3199, 16, y) 
		y = ifelse(3201 <= x & x <= 3499, 17, y) 
		y = ifelse(3501 <= x & x <= 3999, 18, y) 
		y = ifelse(4001 <= x & x <= 4399, 19, y) 
		y = ifelse(4401 <= x & x <= 4999, 20, y) 
		y = ifelse(5001 <= x & x <= 5399, 21, y) 
		y = ifelse(5401 <= x & x <= 5999, 22, y) 
		y = ifelse(6001 <= x & x <= 6300, 23, y) 
		y = ifelse(6401 <= x & x <= 6599, 24, y) 
		y = ifelse(6601 <= x & x <= 6699, 25, y) 
		y = ifelse(6701 <= x & x <= 6899, 26, y) 
	} else {
 		y <- x 
	}

	if(render=="label")	return(lab[y])
	if(render=="name")	return(nam[y])
	if(render=="number") return(y)	
	
	}
