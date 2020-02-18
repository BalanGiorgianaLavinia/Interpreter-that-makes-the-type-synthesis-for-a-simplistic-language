module ClassState
where


-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq)

-- TODO - Trebuie definit ClassState
data ClassState = Empty | Container ([[String]], [[String]])

initEmptyClass :: ClassState
initEmptyClass = Empty

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass Empty Var str = Container (str:[], [])
insertIntoClass Empty Func str = Container ([], str:[])
insertIntoClass (Container (varList, funcList)) Var str = Container (str:varList, funcList)
insertIntoClass (Container (varList, funcList)) Func str = Container (varList, str:funcList)

getValues :: ClassState -> InstrType -> [[String]]
getValues Empty _ = []
getValues (Container (varList, funcList)) Var = varList
getValues (Container (varList, funcList)) Func = funcList
