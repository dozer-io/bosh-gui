# The GUI for [BOSH](https://bosh.io/) by [dozer.io](https://dozer.io/)

## Usage

The best way to use this project is by using the [bosh release](https://github.com/dozer-io/bosh-gui-boshrelease).

## Development

This project is written in [Elm](http://elm-lang.org/) and is applying [The Elm Architecture](https://guide.elm-lang.org/architecture/) pattern.
The visual design language follows the [Material Design spec](https://material.io/), which is implemented in [elm-mdl](https://github.com/debois/elm-mdl).
To ease development there is a partial [BOSH director API stub](https://github.com/dozer-io/bosh-gui/tree/master/api_stub) implemented with [stubby](https://github.com/mrak/stubby4node).

### Setting up your environment

To setup the development environment first clone the repo:

```
git clone git@github.com:dozer-io/bosh-gui.git
cd bosh-gui
```

Next install `node.js` and `npm` by following the instructions [here](https://docs.npmjs.com/getting-started/installing-node).

With `npm` installed, install the remaining dependencies:

```
npm install
```

Once the dependencies are installed we can start [webpack](https://webpack.github.io/) and our BOSH [stubby](https://github.com/mrak/stubby4node) server:

```
npm start
```
