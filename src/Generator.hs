module Generator where

import System.Random

import L4mbd4Val

generateString :: StdGen -> Int -> [Char] -> String -> (String, StdGen)
generateString stdGen 0 dict acc = (acc, stdGen)
generateString stdGen l dict acc =
  let
    (i,nextStdGen) = randomR (0,(length dict)-1) stdGen
  in
    generateString nextStdGen (l-1) dict (dict!!i : acc)

generateVariable :: StdGen -> Int -> Int -> (L4mbd4Val,StdGen)
generateVariable stdGen minLength maxLength =
  let
    (l,nextStdGen) = randomR (minLength,maxLength) stdGen
    (name, nextNextStdGen) = generateString nextStdGen l "abcdefghijklmnopqrtuvwxyz" ""
  in
    (Variable name,nextNextStdGen)

generateId :: StdGen -> Int -> Int -> (L4mbd4Val,StdGen)
generateId stdGen minLength maxLength =
  let
    (l,nextStdGen) = randomR (minLength,maxLength) stdGen
    (name, nextNextStdGen) = generateString nextStdGen l "ABCDEFGHIJKLMNOPQRTUVWXYZ" ""
  in
    (Id name,nextNextStdGen)


generateDef :: StdGen -> Int -> Int -> Int -> (L4mbd4Val,StdGen)
generateDef stdGen depth minLength maxLength =
  let
    ((Id name),nextStdGen) = generateId stdGen minLength maxLength
    (val, nextNextStrGen) = generate nextStdGen (depth-1)
  in
    (Def name val,nextNextStrGen)

generateLambda :: StdGen -> Int -> Int -> Int -> (L4mbd4Val,StdGen)
generateLambda stdGen depth minLength maxLength =
  let
    ((Variable name),nextStdGen) = generateVariable stdGen minLength maxLength
    (val, nextNextStrGen) = generate nextStdGen (depth-1)
  in
    (Lambda name val,nextNextStrGen)


generateList :: StdGen -> Int -> Int -> Int -> (L4mbd4Val,StdGen)
generateList stdGen depth minLength maxLength =
  let
    (l,nextStdGen) = randomR (minLength,maxLength) stdGen
    f i acc =
      let
        innerStdGen = snd acc
        (val, nextInnerStrGen) = generate innerStdGen (depth-1)
        accVal = fst acc
      in
        (val:accVal,nextInnerStrGen)
    vals = foldr f ([],nextStdGen) [0..l-1]
  in
    (List (fst vals),snd vals)


data GeneratorContext = GeneratorContext {
  getStdGen::StdGen,
  getVariables::[String],
  getIds::[String]
}

data GeneratorConfig = GeneratorConfig{
  minVariableLength::Int,
  maxVariableLength::Int,
  minIdLength::Int,
  maxIdLength::Int,
  minListLength::Int,
  maxListLength::Int
}

generate :: StdGen -> Int -> (L4mbd4Val,StdGen)
generate stdGen 0 =
  let
    (r,nextStdGen) = randomR (0::Int,1) stdGen
  in case r of
    0 -> generateVariable nextStdGen 1 1
    1 -> generateId nextStdGen 1 1

generate stdGen depth =
  let
    (r,nextStdGen) = randomR (0::Int,4) stdGen
  in case r of
    0 -> generateVariable nextStdGen 1 1
    1 -> generateId nextStdGen 1 1
    2 -> generateList nextStdGen depth 1 10
    3 -> generateDef nextStdGen depth 1 1
    4 -> generateLambda nextStdGen depth 1 1
