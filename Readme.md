Setup instructions Macintosh Computer:
```bash
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" &&
brew install ghc cabal-install wget &&
cabal update &&
export LIBRARY_PATH=/usr/local/lib:/usr/X11/lib &&
cabal install xmonad &&
cabal its-raining-letters.cabal 
```
