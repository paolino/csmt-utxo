{ pkgs, project, version, ... }:

pkgs.dockerTools.buildImage {
  name = "ghcr.io/paolino/csmt-utxo/csmt-utxo";
  tag = version;
  config = { EntryPoint = [ "csmt-utxo" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ project.packages.csmt-utxo.package.components.exes.csmt-utxo ];
  };
}
