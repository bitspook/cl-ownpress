{
  pkgs ? import (<nixpkgs>) {}
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    # sbcl
    sqlite
  ];

  shellHook = ''
    export CLOWN_LIBRARY_PATH=${pkgs.sqlite.out}/lib:${pkgs.openssl.out}/lib;
  '';
}
