default: build

build:
	tsc && rollup -c && mocha test-bundle.js
