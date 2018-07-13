# witan [![Build Status](https://travis-ci.org/witan-org/witan.svg?branch=master)](https://travis-ci.org/witan-org/witan)
A prototype implementation of McSat solver

## Installation ##

Using [opam](http://opam.ocaml.org/):

```shell
opam pin add witan https://github.com/witan-org/witan.git
```

## Development ##

```shell
git clone https://github.com/witan-org/witan.git
opam pin add dolmen https://github.com/Gbury/dolmen.git#1cabe6b20b3b5f32c4dd9704fdb105cd941cd572
opam pin add --no-action witan .
opam install --deps-only witan
opam install ounit
make build-dev
make test-dev
```
