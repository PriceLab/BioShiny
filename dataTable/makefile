default:
	@echo "targets:"
	@echo "   roxy: build roxygen documentation and NAMESPACE"
	@echo "   install"
	@echo "   demo"

roxy:
	R -e "devtools::document()"

install:
	R CMD INSTALL .

demo:
	(cd inst/demos; R -f dataTableWidgetDemo.R)

demo2:
	(cd inst/demos; R -f dtwDemo2.R)

studio.2:
	open -na RStudio inst/demos/dtwDemo2.R


