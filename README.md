# smb2-cmmod

## Wat?

Converts SMB1 stagedefs to SMB2 stagedefs

## Compiling

This program is written in Haskell. If you don't already have Haskell, I'd recommend getting Haskell Platform.

I use the lens and mtl libraries, so install them using cabal, which comes with Haskell Platform

Then just run
```
ghc SMB1To2
```

## Usage

```
./SMB1To2 [in raw] [out raw]
```

## Functionality

I don't want to figure out bg stuff, and we already have bgtool, so I ain't copying bg stuff, period. 
Also, of the time of writing, we don't know where conveyor speed and texture scroll is in the SMB1 stagedef, so I ain't copying conveyor stuff for now. 

HAPPY CANADA DAY
