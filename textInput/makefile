default:
	@echo "targets:"
	@echo "   roxy: build roxygen documentation and NAMESPACE"
	@echo "   install"
	@echo "   demo demoMulti"

roxy:
	R -e "devtools::document()"

install:
	R CMD INSTALL .

demo:
	(cd inst/demos; R -f TextInputWidgetDemo.R)

demoMulti:
	(cd inst/demos; R -f DynamicMultipleTextInputWidgetDemo.R)

