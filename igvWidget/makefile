default:
	@echo "targets:"
	@echo "   roxy: build roxygen documentation and NAMESPACE"
	@echo "   install"
	@echo "   demo"

roxy:
	R -e "devtools::document()"

install:
	R CMD INSTALL . --no-test-load

demo:
	(cd inst/demos; R -f igvWidgetDemo.R)

