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
    focus-http-th
    focus-js
    ghcjs-dom
    reflex
    reflex-dom
    safe
    these
  ];
  backendDepends = p: with p; [
    data-default
    resource-pool
    snap
    snap-core
    snap-loader-static
    snap-server
  ];
}
