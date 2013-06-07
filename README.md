Sentiment lexicons from WordNet
===============================

To build:

```
cabal configure
cabal build
```

To run:

```
./dist/build/sentlex/sentlex WordNet-3.0/dict/ examples/friend.seeds.txt
```

Note that [WordNet](http://wordnet.princeton.edu/wordnet/download/current-version/) is not included.

