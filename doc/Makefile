all: doc

api:
	cd ..
	lein doc

doc:
	mkdir -p dist/latest/
	cp ../logo/logo.png dist/latest/
	asciidoctor -a docinfo -a stylesheet! -o dist/latest/index.html content.adoc

github: doc api
	ghp-import -m "Generate documentation" -b gh-pages dist/
	git push origin gh-pages
