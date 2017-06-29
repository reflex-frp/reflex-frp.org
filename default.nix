{}: (import ./focus {}).mkDerivation {
  name = "theProjectName";
  version = "0.1";
  commonDepends = p: with p; [
    data-default
    file-embed
  ];
  frontendDepends = p: with p; [
    data-default
    file-embed
    obelisk-http-th
    focus-js
    ghcjs-dom
    reflex
    reflex-dom
    safe
    these
    jsaddle
    font-awesome-type
  ];
  backendDepends = p: with p; [
    data-default
    resource-pool
    snap
    snap-core
    snap-loader-static
    snap-server
    obelisk-serve
  ];
}
