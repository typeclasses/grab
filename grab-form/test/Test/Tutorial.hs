{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE

    OverloadedLists, OverloadedStrings,
    ScopedTypeVariables, TemplateHaskell, TypeApplications

#-}

module Test.Tutorial where

import Data.GrabForm
import Data.Text (Text)

import Hedgehog (Group, discover, withTests, property, (===))

testGroup :: Group
testGroup = $$(discover)

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
        <$> at "name" (only text)
        <*> at "state" (only text)

{-

If we apply `nameAndState` to the form parameters above, we get the following
result:

    ("Alonzo", "Montana")

-}

prop_nameAndState_success =
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
accompanied by an error message indicating that something is missing.

-}

prop_missingState =
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

prop_unexpectedParameters =
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
to accept as the real one. Alonzo cannot live in both Georgia and Montana:

-}

prop_duplicateParameterWithDifferentValue =
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

prop_duplicateParameterWithSameValue =
    readTextParams (only nameAndState)
        [ ("name", "Alonzo")
        , ("state", "Montana")
        , ("state", "Montana")
        ]
    ~>
    (Log [], Just ("Alonzo", "Montana"))

{-

Sometimes a form has a tree structure. Suppose there are multiple security
questions. If we were using a data format like YAML, it might look like this:

    name: Alonzo
    state: Montana
    security:
      - Q: What is your favorite hobby?
        A: watching cars
      - Q: What is your oldest sibling's name?
        A: melman
      - Q: What was the make and model of your first car?
        A: bmw x5

To cajole this data into our concept of a form as a list of parameters, we need
to flatten it somehow. We adopt the following convention:

    name:           Alonzo
    state:          Montana
    security[1].Q:  What is your favorite hobby?
    security[1].A:  watching cars
    security[2].Q:  What is your oldest sibling's name?
    security[2].A:  melman
    security[3].Q:  What was the make and model of your first car?
    security[3].A:  bmw x5

Let's define a data type to represent a question and answer:

-}

data QA = QA { qa_question :: Text, qa_answer :: Text } deriving (Eq, Show)

nameStateAndQAs :: Grab EnglishSentence (Text, Text, [QA])
nameStateAndQAs =
    (,,)
        <$> at "name" (only text)
        <*> at "state" (only text)
        <*> at "security" (only (natList (only qa)))

qa :: Grab EnglishSentence QA
qa =
    QA
        <$> at "Q" (only text)
        <*> at "A" (only text)

prop_multipleQuestions =
    readTextParams (only nameStateAndQAs)
        [ ("name", "Alonzo")
        , ("state", "Montana")
        , ("security[0].Q", "What is your favorite hobby?")
        , ("security[0].A", "watching cars")
        , ("security[1].Q", "What is your oldest sibling's name?")
        , ("security[1].A", "melman")
        , ("security[2].Q", "What was the make and model of your first car?")
        , ("security[2].A", "bmw x5")
        ]
    ~>
    ( Log []
    , Just
          ( "Alonzo"
          , "Montana"
          , [ QA
                { qa_question = "What is your favorite hobby?"
                , qa_answer = "watching cars"
                }
            , QA
                { qa_question = "What is your oldest sibling's name?"
                , qa_answer = "melman"
                }
            , QA
                { qa_question = "What was the make and model of your first car?"
                , qa_answer = "bmw x5"
                }
            ]
          )
    )

{-

The parameters of the list may appear in any order. The order of the result is
determined by the numbers in the parameter names.

-}

prop_listOrder =
    readTextParams (only (at "security" (only (natList (only qa)))))
        [ ("security[2].Q", "What was the make and model of your first car?")
        , ("security[1].A", "melman")
        , ("security[0].Q", "What is your favorite hobby?")
        , ("security[1].Q", "What is your oldest sibling's name?")
        , ("security[0].A", "watching cars")
        , ("security[2].A", "bmw x5")
        ]
    ~>
    ( Log []
    , Just
          [ QA
              { qa_question = "What is your favorite hobby?"
              , qa_answer = "watching cars"
              }
          , QA
              { qa_question = "What is your oldest sibling's name?"
              , qa_answer = "melman"
              }
          , QA
              { qa_question = "What was the make and model of your first car?"
              , qa_answer = "bmw x5"
              }
          ]
    )

{-

Error messages work the same within nested grabs. The result is a complete list
of every error encountered.

-}

prop_manyErrors =
    readTextParams (only nameStateAndQAs)
        [ ("state", "Montana")
        , ("itchy face", "yes")
        , ("security[0].Q", "What is your favorite hobby?")
        , ("security[0].A", "watching cars")
        , ("security[1].Q", "What is your oldest sibling's name?")
        , ("security[1].A", "melman")
        , ("security[1].A", "iowa")
        , ("security[2].Q", "What was the make and model of your first car?")
        , ("security[2].A", "bmw x5")
        , ("security[2].A2", "xyz")
        ]
    ~>
    ( Log [ ("name", "Required parameter is missing.")
          , ("itchy face", "Unexpected parameter.")
          , ("security[1].A", "Parameter may not appear more than once.")
          , ("security[2].A2", "Unexpected parameter.")
          ]
    , Nothing
    )
