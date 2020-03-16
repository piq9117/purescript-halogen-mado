build-all:
	spago build && npm install

build:
	spago build

bundle:
	spago bundle-app --main Mado --to dist/app.js

bundle-watch:
	spago bundle-app --main Mado --to dist/app.js --watch

clean:
	rm -rf .cache .spago node_modules .psci_modules output dist

