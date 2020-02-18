module TestsClass
where

import ClassState
import TestPP
import Data.List (sort, inits)

formGetValues = map ((++) "Get Values -> ")

testSimpleVar :: TestPP ()
testSimpleVar = tests 0 5.0 [
        testVal (getValues (initEmptyClass) Var) vars0V vars0StrV 1,
        testVal (getValues class1Var Var) vars1V vars1StrV 1,
        testVal (sort $ getValues class2Var Var) vars2V vars2StrV 1,
        testVal (sort $ getValues class3Var Var) vars3V vars3StrV 1,
        testVal (sort $ getValues class4Var Var) vars4V vars4StrV 1
      ]
      where

        vars = [["Marvel", "Int"], ["DC", "String"], ["PP", "NotARandomString"], ["Haskell", "Awesome"]]
        classesVars = foldl (\acc x -> (insertIntoClass (head acc) Var x):acc) [initEmptyClass] vars
        [class4Var, class3Var, class2Var, class1Var, _] = classesVars

        varsK@[_, vars1K, vars2K, vars3K, vars4K] = map sort $ inits $ map head vars
        varsV@[vars0V, vars1V, vars2V, vars3V, vars4V] = map sort $ inits $ vars

        [vars0StrV, vars1StrV, vars2StrV, vars3StrV, vars4StrV] = formGetValues (map (((++) "Test Var ").show) [1..5])


testSimpleFunc :: TestPP ()
testSimpleFunc = tests 1 5.0 [
        testVal (getValues (initEmptyClass) Func) [] funcs0StrV 1,
        testVal (getValues class1Func Func) funcs1V funcs1StrV 1,
        testVal (sort $ getValues class2Func Func) funcs2V funcs2StrV 1,
        testVal (sort $ getValues class3Func Func) funcs3V funcs3StrV 1,
        testVal (sort $ getValues class4Func Func) funcs4V funcs4StrV 1
      ]
      where
        funcs = [["Rick", "Int", "Season1", "Season2"],
             ["Sanchez", "String", "Cool"],
             ["Morty", "Void", "PP", "PA", "PC"],
             ["Smith", "PP"]]

        classesFuncs = foldl (\acc x -> (insertIntoClass (head acc) Func x):acc) [initEmptyClass] funcs
        [class4Func, class3Func, class2Func, class1Func, _] = classesFuncs
        funcsK@[_, funcs1K, funcs2K, funcs3K, funcs4K] = map sort $ inits $ map head funcs
        funcsV@[funcs0V, funcs1V, funcs2V, funcs3V, funcs4V] = map sort $ inits $ funcs

        [funcs0StrV, funcs1StrV, funcs2StrV, funcs3StrV, funcs4StrV] = formGetValues (map (((++) "Test Funcs ").show) [1..5])

testComplex :: TestPP ()
testComplex = tests 2 10 [
        testVal (sort $ getValues classesVarsFuncs Var) vars varsStrV 1,
        testVal (sort $ getValues classesVarsFuncs Func) funcs funcsStrV 1
      ]
      where
        vars = sort [["Marvel","Int"], ["DC","String"],
               ["PP","Paradigms"], ["Haskell","Awesome"],
               ["Prolog","Logic"], ["PC","Protocols"],
               ["PA","Algorithms"], ["NPC","Problem"],
               ["Typing","Strong"], ["Pattern","Matching"]]

        funcs = sort [["Avengers","String","Int","Int"], ["End Game","Int","Float","Dobule","Int"],
                ["Captain Marvel","Float"], ["Batman","Void"],
                ["Superman","Int","String"], ["Ironman","String","String","String"],
                ["Hulk","Smash"], ["Thor","Int","Float"],
                ["Wonderwoman","Int","String","Dobule"], ["Aquaman","Int","String"]]

        addVar acc val = insertIntoClass acc Var val
        addFunc acc val = insertIntoClass acc Func val

        classesVarsFuncs = foldl addFunc (foldl addVar initEmptyClass vars) funcs

        varsK = map head vars
        funcsK = map head funcs
        allKeys = sort $ [varsK, funcsK]

        l@[keys1, keys2, keys3] = [sort (varsK ++ funcsK)]
        testsKeys = (map unwords allKeys):allKeys

        [varsStrV] = formGetValues ["TestVarC 1"]
        [funcsStrV] = formGetValues ["TestFuncC 1"]


classTests = [ testSimpleVar, testSimpleFunc, testComplex ]
