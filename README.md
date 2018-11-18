# glossy-haskell-game (GHG)

## Installation

To install Stack and get a general idea of workflow, follow [this guide](https://docs.haskellstack.org/en/stable/README/).

Then navigate to the `ghg` directory and run `stack build`. It should do everything for you.

## Adding new packages

Edit `package.yaml` and add the desired package to `dependencies:`.

If that doesn't work, check [Stackage](https://www.stackage.org) to see if it's available there. It may be in one resolver (eg. LTS 12.18), but not another (eg. Nightly 2018-11-17, the one we're using). If this is the case, change the resolver in `stack.yaml`.

If it's not available anywhere on Stackage, add it to `extra-deps:` in `stack.yaml` (you may have to uncomment the line it's on).

If that doesn't work, it's probably not worth the effort.