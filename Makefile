default: build

build: | node_modules
	node_modules/.bin/esbuild --bundle main.ts --outfile=main-bundle.js --servedir=. --watch --global-name=Compiler

node_modules:
	npm ci
