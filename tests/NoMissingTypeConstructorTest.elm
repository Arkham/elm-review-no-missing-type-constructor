module NoMissingTypeConstructorTest exposing (all)

import NoMissingTypeConstructor exposing (rule)
import Review.Test
import Test exposing (..)


all : Test
all =
    describe "NoMissingTypeConstructor"
        [ test "should report when a declaration named `all...` that is of type `List <CustomTypeName>` does not have all the type constructors in its value (1)" <|
            \_ ->
                """module A exposing (..)
type Thing = A | B | C | D | E
allThings : List Thing
allThings = [ A, C, B ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`allThings` does not contain all the type constructors for `Thing`"
                            , details =
                                [ "We expect `allThings` to contain all the type constructors for `Thing`."
                                , """In this case, you are missing the following constructors:
    , D
    , E"""
                                ]
                            , under = "allThings"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 1 }, end = { row = 4, column = 10 } }
                        ]
        , test "should report when a declaration named `all...` that is of type `List <CustomTypeName>` does not have all the type constructors in its value (2)" <|
            \_ ->
                """module A exposing (..)
type Shenanigan = FirstThing | SecondThing | ThirdThing
allShenanigans : List Shenanigan
allShenanigans = [ FirstThing, ThirdThing ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`allShenanigans` does not contain all the type constructors for `Shenanigan`"
                            , details =
                                [ "We expect `allShenanigans` to contain all the type constructors for `Shenanigan`."
                                , """In this case, you are missing the following constructors:
    , SecondThing"""
                                ]
                            , under = "allShenanigans"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 1 }, end = { row = 4, column = 15 } }
                        ]
        , test "should report when a declaration named `all...` that is of type `List <CustomTypeName>` does not have all the type constructors in its value, where type is defined in a different module" <|
            \_ ->
                [ """module A exposing (..)
import CustomTypeHolder exposing (..)
allShenanigans : List Shenanigan
allShenanigans = [ FirstThing, ThirdThing ]
""", """module CustomTypeHolder exposing (..)
type Shenanigan = FirstThing | SecondThing | ThirdThing
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "`allShenanigans` does not contain all the type constructors for `Shenanigan`"
                                , details =
                                    [ "We expect `allShenanigans` to contain all the type constructors for `Shenanigan`."
                                    , """In this case, you are missing the following constructors:
    , SecondThing"""
                                    ]
                                , under = "allShenanigans"
                                }
                                |> Review.Test.atExactly { start = { row = 4, column = 1 }, end = { row = 4, column = 15 } }
                            ]
                          )
                        ]
        , test "should nt report when a declaration named `all...` that is of type `List <CustomTypeName>` has all the type constructors in its value, where type is defined in a different module (unqualified import)" <|
            \_ ->
                [ """module A exposing (..)
import CustomTypeHolder exposing (..)
allShenanigans : List Shenanigan
allShenanigans = [ FirstThing, SecondThing, ThirdThing ]
""", """module CustomTypeHolder exposing (..)
type Shenanigan = FirstThing | SecondThing | ThirdThing
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should nt report when a declaration named `all...` that is of type `List <CustomTypeName>` has all the type constructors in its value, where type is defined in a different module (qualified import)" <|
            \_ ->
                [ """module A exposing (..)
import CustomTypeHolder exposing (..)
allShenanigans : List Shenanigan
allShenanigans = [ CustomTypeHolder.FirstThing, CustomTypeHolder.SecondThing, CustomTypeHolder.ThirdThing ]
""", """module CustomTypeHolder exposing (..)
type Shenanigan = FirstThing | SecondThing | ThirdThing
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should report when a declaration named `all...` that is of type `List <CustomTypeName>` has all the type constructors in its value" <|
            \_ ->
                """module A exposing (..)
type Thing = A | B | C | D | E
allThings : List Thing
allThings = [ A, C, B, D, E ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report when a declaration named `all...` is a list of an unknown custom type" <|
            \_ ->
                """module A exposing (..)
type Thing = A | B | C | D | E
allThings : List OtherThing
allThings = [ A, C, B ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report when a declaration is not named `all...`" <|
            \_ ->
                """module A exposing (..)
type Thing = A | B | C | D | E
someOfTheThings : List Thing
someOfTheThings = [ A, C, B ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
