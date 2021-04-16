self: super: {
  haskell-nix = super.haskell-nix // {
    hackageQuirks = { name, version }:
      if name == "haskell-language-server"
         && (builtins.compareVersions version "1.0.0.0") >= 0
      then
        {}
      else
        super.haskell-nix.hackageQuirks { inherit name version; }
    ;
  };
}
