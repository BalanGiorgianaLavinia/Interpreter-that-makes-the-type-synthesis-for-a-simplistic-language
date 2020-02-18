module InputInference
where

import InferenceDataType

expr0 = FCall "b" "foo" [Va "c", Va "c"]
expr1 = FCall "d" "foo" [Va "c", Va "c"]
expr2 = FCall "a" "foo" [Va "c", Va "d"]
expr3 = FCall "a" "foo" [FCall "a" "doo" [Va "e", Va "g"], Va "d"]
expr4 = FCall "a" "foo" [Va "b", Va "c"]
expr5 = FCall "a" "boo" [Va "c", Va "b"]

expr60 = FCall "a" "apply" [Va "nr", Va "nr", Va "nrD", Va "str"]
expr61 = FCall "a" "apply" [Va "nr", Va "nrD"]
expr62 = FCall "aaa" "apply" [Va "nr", Va "nr", Va "nrD", Va "str"]
expr63 = FCall "bb" "func" [Va "aa", Va "aa"]
expr64 = FCall "bb" "func" [Va "aa", Va "aa", Va "cc"]
expr65 = FCall "bb" "func" [Va "aa", Va "bb"]
expr66 = FCall "b" "func" [Va "aa", Va "bb"]
expr67 = FCall "b" "func" [Va "nr", FCall "aaa" "anotherF" [FCall "aaa" "boo" [Va "aa", Va "aa"], FCall "a" "apply" [Va "nr", Va "nr", Va "nrD", Va "str"], Va "f"]]
expr68 = FCall "c" "func" [Va "nr", FCall "a" "apply" [Va "nr", Va "nr", Va "nrD", Va "str"]]
expr69 = FCall "aaa" "boo" [FCall "aaa" "boo" [Va "aa", Va "aa"], FCall "aa" "boo" [Va "aa", FCall "aaa" "boo" [Va "aa", FCall "aaa" "boo" [Va "aa", Va "aa"]]]]
