default:
	@echo "targets:"
	@echo "   roxy: build roxygen documentation and NAMESPACE"
	@echo "   install"
	@echo "   demo1 - with mtcars"
	@echo "   demo2"
	@echo "   all: all of the above"

all: roxy install demo1

roxy:
	R -e "devtools::document()"

install:
	R CMD INSTALL .

demo1:
	(cd inst/demos; R -f HeatmapWidgetDemo1.R)

demo2:
	(cd inst/demos; R -f HeatmapWidgetDemo2.R)

