---
title: 'Type safety in practice'
author: 'Nick Van den Broeck'
patat:
    incrementalLists: true
    slideLevel: 1
...

# Why should I care?

* Coding
* API design
* Maintenance

# Outline

* What is a neural network?

* The naive approach
* Fully connected layers
* Sample code: backprop
* Problems

* Alternative approach
* Shapes
* Layers
* Fully connected layers
* Backprop revisited
* Recap

* Q&A

# What is a network?

. . .

```haskell
runNetwork :: Network -> Input -> Output
```

. . .

```haskell
createNetwork :: MonadRandom m => m Network
```

. . .

```haskell
updateNetwork :: Network -> Gradient -> HyperParameters -> Network
```

. . .

```haskell
runBackward :: Network -> Input -> Output -> Gradient
```
# The naive approach: Fully connected layers

. . .

```haskell
data FCL = FCL
  { biases  :: Vector Double
  , weights :: Matrix Double
  }
```

. . .

```haskell
type Network = [FCL]
```

# Sample code: Backprop

. . .

```haskell
getGradientFCL :: FCL
               -> Vector Double
               -> Vector Double
               -> (FCL, Vector Double)
getGradientFCL (FCL b w) inpt dCdz =
  let gradB = dCdz
      gradW = dCdz `outerProd` inpt
      dCdz' = transpose weights <#> dCdz
  in (Gradient $ FCL gradB gradW, dCdz')
```

# Problems

* Partial functions
* Invariants are not checked by the compiler
* Many mistakes are possible
* ..

# The alternative approach: Shapes

. . .
```haskell
data Shape = D1 Nat
           | D2 Nat Nat
           | D3 Nat Nat Nat
```

. . .
```haskell
data S (n :: Shape) where
    S1D :: V n -> S ('D1 n)
    S2D :: M i j -> S ('D2 i j)
    S3D :: M i (j * k) -> S ('D3 i j k)
```

# Layers

. . .
```haskell
class UpdateLayer x where
  createRandom :: MonadRandom m => m x
  applyGradient :: x -> Gradient x -> HyperParams -> x

newtype Gradient x = Gradient x
```

. . .
```haskell
class UpdateLayer x => Layer x i o where
  type Tape i o :: *
  runForwards :: x -> S i -> (Tape i o, S o)
  runBackwards :: x -> Tape i o -> S o -> (Gradient x, S i)
```

# FullyConnected layer

. . .
```haskell
data FullyConnected i o = FullyConnected
  { weights :: M i o
  , biases :: V o
  }
```

. . .
```haskell
runForwards FullyConnected {..} (S1D v) =
    (v, S1D $ weights <#> v <+> biases)
```

# runBackwards revisited

```haskell
runBackwards :: FullyConnected i o
             -> S i
             -> S o
             -> (Gradient FullyConnected, S i)
runBackwards (FullyConnected bias weight) vIn (S1D dCdz) =
    let gradBias = dCdz
        gradWeight = dCdz `outerProd` vIn
        dCdz' = transpose weight <#> dCdz
     in (Gradient $ FullyConnected gradBias gradWeight, S1D dCdz')
```

# Benefits of type safety

* Help the author

* Help the maintainer

* Help the user

. . .
```haskell
createNetwork :: MonadRandom m => m (Network layers shapes)
```

# Q&A

. . .

```
___________                      
\__    ___/__.__.______   ____   
  |    | <   |  |\____ \_/ __ \  
  |    |  \___  ||  |_> >  ___/  
  |____|  / ____||   __/ \___  > 
          \/     |__|        \/  
                _____       __          
  ___________ _/ ____\_____/  |_ ___.__.
 /  ___/\__  \\   __\/ __ \   __<   |  |
 \___ \  / __ \|  | \  ___/|  |  \___  |
/____  >(____  /__|  \___  >__|  / ____|
     \/      \/          \/      \/     
```
