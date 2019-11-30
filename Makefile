default: build

build: | node_modules
	node_modules/.bin/tsc && node_modules/.bin/rollup -c && mocha test-bundle.js

node_modules:
	npm ci
