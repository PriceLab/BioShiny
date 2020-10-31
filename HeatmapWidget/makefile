default:
	@echo "targets:"
	@echo "   roxy: build roxygen documentation and NAMESPACE"
	@echo "   install"
	@echo "   demo"
	@echo "   all: all of the above"

all: roxy install demo

roxy:
	R -e "devtools::document()"

install:
	R CMD INSTALL .

demo:
	(cd inst/demos; R -f HeatmapWidgetDemo.R)

