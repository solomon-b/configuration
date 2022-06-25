{s}: rec
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:hkd-options-parsing' --allow-eval --warnings";
  allScripts = [ghcidScript];
}
