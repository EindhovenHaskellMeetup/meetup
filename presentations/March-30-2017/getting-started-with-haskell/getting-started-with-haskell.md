% Getting started with Haskell
% Damian Nadales
% March 30, 2017

# Introduction

## Objectives

- Introduce Haskell's main features.
- Get you started with Haskell tooling.
- Write some Haskell!

## Prerequisites

- Stack is installed in your system.
- You created and setup a `hello-haskell` project.
- Optional: you downloaded and installed exercism.

# Our first Haskell project

## Stack

- Package manager.
- Build tool.
- Scaffolding tool.
- "One-stop shop for all the Haskell tooling you need".
- Aims at achieving reproducible builds.

## Creating our first project
```sh
stack new hello-haskell
```

## Installing GHC through Stack

```sh
stack setup
```

## Building the project

```sh
stack build
```

## Executing the program we just build
```sh
stack exec hello-haskell-exe
```

## Anatomy of a Haskell application

```sh
$ tree hello-haskell
hello-haskell
├── LICENSE
├── Setup.hs
├── app
│   └── Main.hs
├── hello-haskell.cabal
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs

3 directories, 7 files
```

## Adding additional dependencies

Let's add `containers` (required by `Data.Map`)

# A first taste of Haskell

## Let's fire up the REPL

```sh
cd hello-haskell
stack ghci
```

## Simple expressions

- Arithmetic operations.
- Strings.
- Lists.

## Declaring a function

- The `sayHello` function!
- In the REPL.
- In the `Lib.hs` file.
- We can use the REPL for quick feedback and testing.
- Reload with `:r`.

## No types?

- Let's see the type of `sayHello`.
- Use `:t` to see the types.
- We can declare types explicitly.
  - Serve more as a documentation tool.
  - Although in occasions we need to help GHC.
  
## Associativity: function declaration

- Function declaration associates right.
```haskell
a -> b -> c = a -> (b -> c)
```

- Or if you prefer a mnemonic.
```haskell
Avoid -> Success -> AtAllCosts
```
  should be read as
```haskell
Avoid -> (Success -> AtAllCosts)
```

## Associativity: function application

- Function application associates left.
```haskell
f x y = (f x) y
```
- Or if you prefer a mnemonic.
```haskell
heGivesHer cat food
```
  should be read as
```haskell
(heGivesHer cat) food
```

## Infix notation

Ready to use!

```haskell
like a b = a ++ " like " ++ b
like "dogs" "meat"
"pandas" `like` "bamboo"
```

## Lists

```haskell
xs = [0, 1, 2]
ys = "hello"
oddNumbers = [ 2 * i + 1 | i <- [0..]]
pairs = [('a', 1), ('z', 10)]
pairs2 = [(i,j) | i <- [1,2], j <- [1..4]]
```
## Operations on lists

- `map`
```haskell
map (*2) [0..10]
```

- `filter`
```haskell
filter (\x -> x `mod` 2 == 0) [3, 8, 41, 1, 52]
```

- `fold`
```haskell
foldl (+) 0 [0..10] -- = sum [0..10]
```

## Zip

```haskell
zip "foo" "bar"
zip "hello" "bar"
zip [0, 1, 2] "abc"
zip [0 ..] "a very long string"
```

## More functions on lists

- See `Data.List`

## Collections

Module `Data.Map` requires `collections` package.

```haskell
import qualified Data.Map.Strict as Map
```

- The `qualified` keyword means that you can access all functions in `Map` as
  `Map.functionName`.
  
- Useful for preventing name clashes.

## Map examples

- Construct a dictionary from a list of pairs using `fromList`
```haskell
Map.fromList [("foo", 0), ("bar", 1)]
```

- Look up elements using `lookup` or `findWithdefault`:
```haskell
findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
```

## Function composition is the dot

```haskell
import Data.Char

capitalize = map toUpper
screamHello name = sayHello (capitalize name)
screamHello1 = sayHello . capitalize
```

## Partial application

```haskell 
map (*2) xs
xs = ["functional programming", "Haskell"]
map (++ " rocks") xs
map ("we love " ++ ) xs
```

## Lambda abstractions

```haskell
filter (\x -> x == reverse x) ["foo", "bar", "ana"]
```

## Flattening lists 

```haskell
concat :: [[a]] -> [a]
```

## Intersperse

```haskell
intersperse :: a -> [a] -> [a]
```

## Flattening and mapping

- Sometimes it is useful to map a function to a list that returns a list, and
concatenate all the elements together.
```haskell
>>= :: [a] -> (a -> [b]) -> [b]
```
- Example:
```haskell
[0, 1, 2, 3] >>= \i -> take i (repeat i)
```

## Flattening and mapping

- Another example:
```haskell
[(0, "foo"), (2, "bar")] >>= \(i, str) -> zip (repeat i) str
```
- The type of `>>=` is more general here (I used list to avoid scaring people
  with the *m* word).

# First exercise: Scrabble score

## About dependencies

`exercism.io` uses `package.yaml` for extra dependencies, so add them there:

```haskell
name: scrabble-score

dependencies:
  - base

library:
  exposed-modules: Scrabble
  source-dirs: src
  dependencies:
    containers >= 0.5
```

## Fetch the exercise

Source: http://exercism.io/exercises/haskell/scrabble-score/readme

```sh
exercism fetch haskell scrabble-score
```
... and build it.

## Tip: Hoogle

Example: search for functions with type `a -> [a]`.

## Go!
- Hint: you can use functions in `Data.Char`.
- Submitting: 
```sh
exercism submit src/Scrabble.hs package.yaml
```

## A possible solution

[http://exercism.io/submissions/2df2f9affdf14a9eba8dc988b8714eff](source)

```haskell
module Scrabble (scoreLetter, scoreWord) where

import           Data.Char       (toUpper)
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map

scores = Map.fromList $ [ ("AEIOULNRST", 1)
                        , ("DG", 2)
                        , ("BCMP", 3)
                        , ("FHVWY", 4)
                        , ("K", 5)
                        , ("JX", 8)
                        , ("QZ", 10)
                        ] >>= (\(cs, score) -> zip cs (repeat score))

scoreLetter letter = Map.findWithDefault 0 (toUpper letter) scores

scoreWord = sum . map scoreLetter
```

## How do people solve this in Java?
[http://exercism.io/submissions/ce7dc1c8ef864726b1b2483da3638258](source)

```java
import java.util.HashMap;

public class Scrabble {

    private String scrabbleWord;
    private int totalScore = 0;
    private HashMap<Character, Integer> scrabbleScore = new HashMap();

    public Scrabble(String word) {

        scrabbleWord = word == null ? "" : word.toUpperCase();

        scrabbleScore.put('A', 1);
        scrabbleScore.put('E', 1);
        scrabbleScore.put('I', 1);
        scrabbleScore.put('O', 1);
        scrabbleScore.put('U', 1);
        scrabbleScore.put('L', 1);
        scrabbleScore.put('N', 1);
        scrabbleScore.put('R', 1);
        scrabbleScore.put('S', 1);
        scrabbleScore.put('T', 1);
        scrabbleScore.put('D', 2);
        scrabbleScore.put('G', 2);
        scrabbleScore.put('B', 3);
        scrabbleScore.put('C', 3);
        scrabbleScore.put('M', 3);
        scrabbleScore.put('P', 3);
        scrabbleScore.put('F', 4);
        scrabbleScore.put('H', 4);
        scrabbleScore.put('V', 4);
        scrabbleScore.put('W', 4);
        scrabbleScore.put('Y', 4);
        scrabbleScore.put('K', 5);
        scrabbleScore.put('J', 8);
        scrabbleScore.put('X', 8);
        scrabbleScore.put('Q', 10);
        scrabbleScore.put('Z', 10);
    }

    public int getScore() {

            for (int i = 0; i < scrabbleWord.length(); i++) {
                if (scrabbleScore.containsKey(scrabbleWord.charAt(i))) {
                    totalScore = totalScore + scrabbleScore.get(scrabbleWord.charAt(i));
                }
            }
        return totalScore;
    }
}
```

# More material

## Declaring new data-types

```haskell
data Color = Red | Green | Blue
data Maybe a = Just a | Nothing
data List a = Cons a (List a) | Nil
```

## Pattern matching on colors

```haskell
toRGB :: Color -> (Int, Int, Int)
toRGB Red = (255, 0, 0)
toRGB Green = (0, 255, 0)
toRGB Blue = (0, 0, 255)
```

## Pattern matching on Maybe

```haskell
f (Just x) = x 
f Nothing = 0
```

## Pattern matching on lists

```haskell
f (x:y:z:xs) = [x, z]
f _  = []

show Red = (255)
```

`_` is a /wildcard/ pattern. Matches everything.

## Matching by cases

```haskell
f  (Just x)
 | x == 0 = "zero"
 | x == 1 = "One"
 | otherwise = "More"
f Nothing = "Nothing!" 
```

# Second exercise: Ocr Numbers

## Fetch it!

Source: http://exercism.io/exercises/haskell/ocr-numbers/readme

```sh
exercism fetch haskell ocr-numbers
```

## A short solution

Source: http://exercism.io/submissions/3c3799bc94d3446baf490acff982647b

```haskell
module OCR (convert) where

import Data.List (transpose, intercalate)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

convert :: String -> String
convert = intercalate "," . map recognizeRow . chunksOf 4 . lines
  where
    recognizeRow = map recognize . fingerprint
    recognize x = Map.findWithDefault '?' x digits
    fingerprint = transpose . map (chunksOf 3)
    digits = Map.fromList (fingerprint reference `zip` ['0'..])
    reference =
      [ " _     _  _     _  _  _  _  _ "
      , "| |  | _| _||_||_ |_   ||_||_|"
      , "|_|  ||_  _|  | _||_|  ||_| _|"
      , "                              "
      ]
```

## Java solution number 0

Source: http://exercism.io/submissions/176d27f6113d46d3a96194bdad92b86c

```java
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

final class OpticalCharacterReader {
    private static Map<String, Character> characterMap = new HashMap<String, Character>() {{
        put(" _ | ||_|   ", '0');
        put("     |  |   ", '1');
        put(" _  _||_    ", '2');
        put(" _  _| _|   ", '3');
        put("   |_|  |   ", '4');
        put(" _ |_  _|   ", '5');
        put(" _ |_ |_|   ", '6');
        put(" _   |  |   ", '7');
        put(" _ |_||_|   ", '8');
        put(" _ |_| _|   ", '9');
    }};

    public static String parse(List<String> strings) {
        String string = String.join(",", parseLines(strings));
        return string;
    }

    private static List<String> parseLines(List<String> strings) {
        if(strings.size() % 4 != 0) {
            throw new IllegalArgumentException("Number of input rows must be a positive multiple of 4");
        }
        ArrayList<String> lines = new ArrayList<>();
        for(int i = 0; i < strings.size(); i += 4) {
            lines.add(parseLine(strings.subList(i, i + 4)));
        }
        return lines;
    }

    private static String parseLine(List<String> line) {
        int length = line.get(0).length();
        if(length % 3 != 0) {
            throw new IllegalArgumentException("Number of input columns must be a positive multiple of 3");
        }
        String string = "";
        for(int j = 0; j < length; j += 3) {
            string += parseCharacter(line, j);
        }
        return string;
    }

    private static Character parseCharacter(List<String> line, int j) {
        String characterString = getCharacterString(line, j);
        return parseCharacterString(characterString);
    }

    private static Character parseCharacterString(String characterString) {
        if(characterMap.keySet().contains(characterString)) {
            return characterMap.get(characterString);
        }
        return '?';
    }

    private static String getCharacterString(List<String> line, int j) {
        String string = "";
        for(int i = 0; i < 4; i++) {
            string += line.get(i).substring(j, j + 3);
        }
        return string;
    }
}
```

## Java solution number 1

Source: http://exercism.io/submissions/08b891cf57544d4087b4587c403305f8

```java
import java.awt.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

final class OpticalCharacterReader {
    private final int CharacterWidth = 3;
    private final int CharacterHeight = 4;

    private final Map<String, String> CharactersMap = getCharactersMap();
    private List<String> lines;

    public String parse(List<String> lines) {
        this.lines = lines;

        if (lines.size() % CharacterHeight != 0) {
            throw new IllegalArgumentException("Number of input rows must be a positive multiple of 4");
        }

        if (lines.get(0).length() % CharacterWidth != 0) {
            throw new IllegalArgumentException("Number of input columns must be a positive multiple of 3");
        }

        int x = this.horizontalCharacters();
        int y = this.verticalCharacters();

        return IntStream.range(0, verticalCharacters())
            .mapToObj(this::rowCharacters)
            .collect(Collectors.joining(","));
    }

    private String rowCharacters(int row) {
        return IntStream.range(0, horizontalCharacters())
                .mapToObj(x -> new Point(x, row))
                .map(this::convertCharacter)
                .collect(Collectors.joining(""));
    }

    private int horizontalCharacters() {
        return lines.get(0).length() / CharacterWidth;
    }

    private int verticalCharacters() {
        return lines.size() / CharacterHeight;
    }

    private String convertCharacter(Point point) {
      return matchCharacter(character(point));
    }

    private String character(Point point) {
        return IntStream.range(0, CharacterHeight)
                .mapToObj(offset -> characterOnLine(point, offset))
                .collect(Collectors.joining(""));
    }

    private String characterOnLine(Point point, Integer offset) {
        String completeLine = lines.get(point.y * CharacterHeight + offset);
        return completeLine.substring(point.x * CharacterWidth, point.x * CharacterWidth + CharacterWidth);
    }

    private String matchCharacter(String character) {
        return CharactersMap.getOrDefault(character, "?");
    }

    private static Map<String, String> getCharactersMap() {

        Map<String, String> map = new HashMap<>();
        map.put(" _ " +
                "| |" +
                "|_|" +
                "   ",
                "0");
        map.put("   " +
                "  |" +
                "  |" +
                "   ",
                "1");
        map.put(" _ " +
                " _|" +
                "|_ " +
                "   ",
                "2");
        map.put(" _ " +
                " _|" +
                " _|" +
                "   ",
                "3");
        map.put("   " +
                "|_|" +
                "  |" +
                "   ",
                "4");
        map.put(" _ " +
                "|_ " +
                " _|" +
                "   ",
                "5");
        map.put(" _ " +
                "|_ " +
                "|_|" +
                "   ",
                "6");
        map.put(" _ " +
                "  |" +
                "  |" +
                "   ",
                "7");
        map.put(" _ " +
                "|_|" +
                "|_|" +
                "   ",
                "8");
        map.put(" _ " +
                "|_|" +
                " _|" +
                "   ",
                "9");
        return map;
    }
}
```

## Java solution number 2

Source: http://exercism.io/submissions/4768cb6be8244201bceb45653ed7f5cf

```java
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

final class OpticalCharacterReader {
    private static final int LETTER_WIDTH = 3;
    private static final int LETTER_HEIGHT = 4;

    private static Map<Letter, String> NUMBERS = new HashMap<>();

    static {
        List<String> in = Arrays.asList(
                " _     _  _     _  _  _  _  _ ",
                "| |  | _| _||_||_ |_   ||_||_|",
                "|_|  ||_  _|  | _||_|  ||_| _|",
                "                              "
        );

        for (int i = 0; i < 10; i++) {
            NUMBERS.put(OpticalCharacterReader.getRawCharAt(in, 0, i), String.valueOf(i));
        }
    }

    private static Letter getRawCharAt(List<String> input, long line, long pos) {
        String[] result = new String[LETTER_HEIGHT];

        for (int y = 0; y < LETTER_HEIGHT; y++) {
            result[y] = input.get((int)line * LETTER_HEIGHT + y)
                    .substring((int)pos * LETTER_WIDTH, (int)(pos + 1) * LETTER_WIDTH);
        }

        return new Letter(result);
    }

    public String parse(List<String> input) {
        if (input.size() == 0 || input.size() % LETTER_HEIGHT != 0) {
            throw new IllegalArgumentException("Number of input rows must be a positive multiple of " +
                    LETTER_HEIGHT);
        }

        if (input.get(0).length() == 0 || input.get(0).length() % LETTER_WIDTH != 0) {
            throw new IllegalArgumentException("Number of input columns must be a positive multiple of " +
                    LETTER_WIDTH);
        }

        return LongStream.range(0, input.size() / LETTER_HEIGHT).boxed()
                .map(y -> LongStream.range(0, input.get(0).length() / LETTER_WIDTH).boxed()
                        .map(x -> NUMBERS.getOrDefault(getRawCharAt(input, y, x), "?"))
                        .collect(Collectors.joining()))
                .collect(Collectors.joining(","));
    }
}

class Letter {
    private final String[] input;

    Letter(String[] input) {
        this.input = input;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Letter letter = (Letter) o;

        return Arrays.equals(input, letter.input);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(input);
    }
}
```
