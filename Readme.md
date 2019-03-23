## Training for dvorak keyboard:
Letters rain down on the screen and you must input their dvorak equivalent to gain points. After you reach a certain number the amount of characters increases and the letters change. Have fun!

first level translation n=j t=f

To get out of the program early use `<ESC>`


Setup instructions Macintosh Computer:
```bash
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" &&
brew install ghc cabal-install wget &&
cabal update &&
export LIBRARY_PATH=/usr/local/lib:/usr/X11/lib &&
cabal install xmonad &&
cabal its-raining-letters.cabal 
```
