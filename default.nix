{
  pkgs ? import (<nixpkgs>) {}
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    sbcl
    sqlite
    sqlitebrowser
  ];

  shellHook = ''
    export CASK_DIR=${pkgs.cask.out}/share/emacs/site-lisp/cask/;
    export CLOWN_LIBRARY_PATH=${pkgs.sqlite.out}/lib;
  '';
}
