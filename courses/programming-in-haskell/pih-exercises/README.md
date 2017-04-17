# Tests for the exercises of the book Programming in Haskell 

This project contains tests for the exercises of the
book
[Functional Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html).
The contents of this project are by no means associated with the author of the
book, and it is only intended to provide additional support for the study
group.

## Testing the exercises
    
Install [stack](https://docs.haskellstack.org/en/stable/README/), then run:

```sh
stack setup
```

Then run the tests:
```sh
stack test
```

If you're using [Visual Studio Code](https://code.visualstudio.com/download)
with
[Haskero](https://marketplace.visualstudio.com/items?itemName=Vans.haskero)
install [intero](http://commercialhaskell.github.io/intero/):

```sh
cd pih-exercises
stack install intero
```

Note that you have to run the installation command above from the
`phi-exercises` folder. 

Running the test for a single chapter:

```sh
stack test --test-arguments "-m "ChapterXX""
```

replace `XX` above for the chapter number you want to run the tests for.
