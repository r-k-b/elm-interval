all: tests.js

tests.js: FORCE $(shell find src -type f -name '*.elm' -o -name '*.js')
	npx elm-make --yes --warn

FORCE: