# This file has been generated by node2nix 1.11.1. Do not edit!

{nodeEnv, fetchurl, fetchgit, nix-gitignore, stdenv, lib, globalBuildInputs ? []}:

let
  sources = {};
in
{
  prettier = nodeEnv.buildNodePackage {
    name = "prettier";
    packageName = "prettier";
    version = "2.7.1";
    src = fetchurl {
      url = "https://registry.npmjs.org/prettier/-/prettier-2.7.1.tgz";
      sha512 = "ujppO+MkdPqoVINuDFDRLClm7D78qbDt0/NR+wp5FqEZOoTNAjPHWj17QRhu7geIHJfcNhRk1XVQmF8Bp3ye+g==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      description = "Prettier is an opinionated code formatter";
      homepage = "https://prettier.io";
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
}