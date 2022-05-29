.onAttach <- function(libname, pkgname) {

	msg <- paste0(
		"--------------------------------------------\n",
		"The dispositionEffect is the first R package to perform behavioural analysis on financial data.\n",
		"Use citation('dispositionEffect') to cite the package.\n",
		"Learn more at: https://marcozanotti.github.io/dispositionEffect/index.html\n",
		"--------------------------------------------"
	)
	packageStartupMessage(msg)

	return(invisible(NULL))

}
