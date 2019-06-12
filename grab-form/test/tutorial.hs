{-# LANGUAGE

    OverloadedLists, OverloadedStrings,
    ScopedTypeVariables, TemplateHaskell, TypeApplications

#-}

import Data.GrabForm
import Data.Text (Text)
import Prelude hiding ((/))

-- Imports for testing the tutorial
import Control.Monad (when)
import System.IO (hSetEncoding, stdout, stderr, utf8)
import System.Exit (exitFailure)
import Hedgehog

main :: IO ()
main =
  do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    ok <- checkParallel $$(discover)
    when (not ok) exitFailure

example =
    withTests 1 . property

x ~> y =
    example (x === y)


--- Tutorial ---

{-

We are concerned here with data submitted by web browsers in a normal form
submission. Ignoring the encoding details, we can think of a form as looking
something like this:

    name:               Alonzo
    state:              Montana
    security_question:  What is your favorite hobby?
    security_answer:    watching cars

This example has four parameters. Each parameter has a name and a value. We
might represent this in Haskell as:

    [ ("name", "Alonzo")
    , ("state", "Montana")
    , ("security_question", "What is your favorite hobby?")
    , ("security_answer", "watching cars")
    ]

Suppose we're only interested in two parts of this form: The name and the state.

-}

nameAndState :: Grab EnglishSentence (Text, Text)
nameAndState =
    (,)
        <$> (at "name" / only text)
        <*> (at "state" / only text)

{-

If we apply the `nameAndState` grab to the form parameters above, we get the
following result:

    ("Alonzo", "Montana")

-}

prop_1 =
    readTextParams (etAlia nameAndState)
        [ ("name", "Alonzo")
        , ("state", "Montana")
        , ("security_question", "What is your favorite hobby?")
        , ("security_answer", "watching cars")
        ]
    ~>
    (Log [], Just ("Alonzo", "Montana"))

{-

When receiving information submitted from an external source, there is usually
some possibility that the input is invalid. Consider the following form that is
missing the "state" field. In this case, the result we get is `Nothing`,
accompanied by an error message indicating that a required parameter is missing.

-}

prop_2 =
    readTextParams (etAlia nameAndState)
        [ ("name", "Alonzo")
        , ("security_question", "What is your favorite hobby?")
        , ("security_answer", "watching cars")
        ]
    ~>
    (Log [("state", "Required parameter is missing.")], Nothing)

{-

The `etAlia` function we've been using signifies that the input is allowed to
contain parameters other than the ones that `nameAndState` grabs. If we use
`only` instead, we can specify that there should be no additional parameters.

-}

prop_3 =
    readTextParams (only nameAndState)
        [ ("name", "Alonzo")
        , ("state", "Montana")
        , ("security_question", "What is your favorite hobby?")
        , ("security_answer", "watching cars")
        ]
    ~>
    ( Log [ ("security_question", "Unexpected parameter.")
          , ("security_answer", "Unexpected parameter.")
          ]
    , Just ("Alonzo", "Montana")
    )

{-

However, we still get the result: ("Alonzo", "Montana"). Unexpected parameters
do not prevent us from being able to read the form. Whether you choose `only` or
`etAlia` only determines whether these warnings end up in the log; it does not
affect whether reading the form succeeds or fails.

Duplicate parameters are not permitted, since we cannot know which of the values
to accept as the real one. Alonzo must make up his mind whether he lives in
Georgia or Montana:

-}

prop_4 =
    readTextParams (only nameAndState)
        [ ("name", "Alonzo")
        , ("state", "Georgia")
        , ("state", "Montana")
        ]
    ~>
    (Log [("state", "Parameter may not appear more than once.")], Nothing)


{-

Duplicated parameters are only allowed if they have the same value, because in
that case the problem of deciding which value to accept does not arise.

-}

prop_5 =
    readTextParams (only nameAndState)
        [ ("name", "Alonzo")
        , ("state", "Montana")
        , ("state", "Montana")
        ]
    ~>
    (Log [], Just ("Alonzo", "Montana"))
