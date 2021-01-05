{ pkgs ? import <nixpkgs> { }}:
with pkgs;

let
  caramia-src = fetchFromGitHub {
    owner = "abbradar";
    repo = "caramia";
    rev = "e5dd5d5596a75a9ff628cfd553e9ccabd896558f";
    sha256 = "0dxc6wy8lf8vcv2i1bac0592k273ds7g09xm4hfzi7rn38nqzjgj";
  };
  my-caramia = haskellPackages.callPackage caramia-src { };
  my-ghc = haskellPackages.ghcWithPackages (h: [ my-caramia h.SDL h.sdl2 ]);
in
mkShell {
  buildInputs = [ my-ghc ];
}
