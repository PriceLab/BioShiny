default:
	@echo "targets:"
	@echo "   roxy: build roxygen documentation and NAMESPACE"
	@echo "   install"
	@echo "   demo1 demo2 demo3 studio.1 studio.2"

roxy:
	R -e "devtools::document()"

install:
	R CMD INSTALL .

demo1:
	(cd inst/demos; R -f dataTableWidgetDemo.R)

demo2:
	(cd inst/demos; R -f dtwDemo2.R)

demo3:
	(cd inst/demos; R -f dtwCollapseExpandRows.R)

misalignedColumnsDemo:
	(cd inst/demos; R -f dtwMisalignedColumnTitles.R)

studio.1:
	open -na RStudio inst/demos/dataTableWidgetDemo.R

studio.2:
	open -na RStudio inst/demos/dtwDemo2.R


