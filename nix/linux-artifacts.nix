{ pkgs, version, project, ... }:
let
  csmt-utxo = project.musl64.csmt-utxo.components.exes.csmt-utxo;
  tarball-derivation = pkgs.stdenv.mkDerivation rec {
    pname = "csmt-utxo";
    inherit version;
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp ${csmt-utxo}/bin/csmt-utxo $out/unpacked
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-linux64.tar.gz .
      rm -rf $out/unpacked
    '';
  };
in { packages.linux64.tarball = tarball-derivation; }
