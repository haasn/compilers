module Core.Examples where

import Core.Common

example1 :: CoreProgram
example1 = CoreProgram
  [ ScDef "id" ["x"] (Free "x")
  , ScDef "main" [] (App (Free "id") (Constr 3 0))
  ]


example2 :: CoreProgram
example2 = CoreProgram [ScDef "main" []
  (App (App (App (Free "S") (Free "K")) (Free "K")) (Constr 3 0))]


example3 :: CoreProgram
example3 = CoreProgram
  [ ScDef "twice" ["f", "x"] (App (Free "f") (App (Free "f") (Free "x")))
  , ScDef "id" ["x"] (Free "x")
  , ScDef "main" [] (App (App (App (Free "twice")(Free "twice"))
                         (Free "id")) (Constr 3 0))
  ]

example4 :: CoreProgram
example4 = CoreProgram
  [ ScDef "twice" ["f", "x"] (App (Free "f") (App (Free "f") (Free "x")))
  , ScDef "main" [] (LetRec [("f", (App (Free "twice") (Free "I")))]
                      (App (App (Free "twice") (Free "f")) (Constr 3 0)))
  ]

example5 :: CoreProgram
example5 = CoreProgram
  [ ScDef "y" ["f"] (LetRec [("x", App (Free "f") (Free "x"))] (Free "x"))
  , ScDef "main" [] (App (Free "y") (App (Free "K") (Constr 3 0)))
  ]

example6 :: CoreProgram
example6 = CoreProgram
  [ ScDef "main" [] (Case (Constr 1 0)
    [ Match 0 [] (Constr 4 0)
    , Match 1 [] (Constr 5 0)
    ])
  ]

examplePair :: CoreProgram
examplePair = CoreProgram
  [ ScDef "pair" ["x", "y"] (App (App (Constr 0 2) (Free "x")) (Free "y"))
  , ScDef "main" [] (App (App (Free "pair") (Constr 2 0)) (Constr 4 0))
  ]

example7 :: CoreProgram
example7 = CoreProgram
  [ ScDef "true"  [] (Constr 1 0)
  , ScDef "false" [] (Constr 0 0)
  , ScDef "and" ["x", "y"] (Case (Free "x")
    [ Match 1 [] (Free "y")
    , Match 0 [] (Free "false")
    ])
  , ScDef "main" [] (App (App (Free "and") (Free "true")) (Free "false"))
  ]

example8 :: CoreProgram
example8 = CoreProgram
  [ ScDef "zero" [] (Constr 0 0)
  , ScDef "succ" ["n"] (App (Constr 1 1) (Free "n"))
  , ScDef "length" ["l"] (Case (Free "l")
    [ Match 0 [] (Free "zero")
    , Match 1 ["x", "xs"] (App (Free "succ") (App (Free "length") (Free "xs")))
    ])
  , ScDef "main" [] (Constr 0 0)
  ]
