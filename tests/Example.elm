module Example exposing (suite)

import Expect exposing (Expectation)
import Queue
import Test exposing (..)
import Tuple


suite : Test
suite =
    describe "exposed functions"
        [ test "an empty Queue can be made" <|
            \() ->
                Queue.count Queue.empty
                    |> Expect.equal 0
        , test "count for Queue increases by 1" <|
            \() ->
                Queue.enqueue "Lebowski" Queue.empty
                    |> Queue.count
                    |> Expect.equal 1
        , test "add value returned on peek" <|
            \() ->
                Queue.enqueue "Lebowski" Queue.empty
                    |> Queue.peek
                    |> Expect.equal (Just "Lebowski")
        , test "peek returns the front of Queue" <|
            \() ->
                Queue.empty
                    |> Queue.enqueue "Lebowski"
                    |> Queue.enqueue "Donny"
                    |> Queue.enqueue "Walter"
                    |> Queue.peek
                    |> Expect.equal (Just "Lebowski")
        , test "dequeue returns the only element present" <|
            \() ->
                Queue.empty
                    |> Queue.enqueue "Lebowski"
                    |> Queue.dequeue
                    |> Tuple.first
                    |> Expect.equal (Just "Lebowski")
        , test "dequeue on single element queue results in an empty Queue" <|
            \() ->
                Queue.enqueue "Lebowski" Queue.empty
                    |> Queue.dequeue
                    |> Tuple.second
                    |> Queue.peek
                    |> Expect.equal Nothing
        , test "dequeue returns the first in element" <|
            \() ->
                Queue.empty
                    |> Queue.enqueue "Lebowski"
                    |> Queue.enqueue "Donny"
                    |> Queue.enqueue "Walter"
                    |> Queue.dequeue
                    |> Tuple.first
                    |> Expect.equal (Just "Lebowski")
        , test "dequeue returns the expected element" <|
            \() ->
                Queue.empty
                    |> Queue.enqueue "Lebowski"
                    |> Queue.enqueue "Donny"
                    |> Queue.enqueue "Walter"
                    |> Queue.dequeue
                    |> Tuple.second
                    |> Queue.peek
                    |> Expect.equal (Just "Donny")
        , test "conversion to list, in First-In, First-Out order" <|
            \() ->
                Queue.empty
                    |> Queue.enqueue "Lebowski"
                    |> Queue.enqueue "Donny"
                    |> Queue.enqueue "Walter"
                    |> Queue.toList
                    |> Expect.equal [ "Lebowski", "Donny", "Walter" ]
        , test "initialize from list" <|
            \() ->
                Queue.fromList [ "Lebowski", "Donny", "Walter" ]
                    |> Queue.toList
                    |> Expect.equal [ "Lebowski", "Donny", "Walter" ]
        , test "initialize from list, count matches" <|
            \() ->
                Queue.fromList [ "Lebowski", "Donny", "Walter" ]
                    |> Queue.count
                    |> Expect.equal (List.length [ "Lebowski", "Donny", "Walter" ])
        , test "initialize from list, peek returns first element" <|
            \() ->
                Queue.fromList [ "Lebowski", "Donny", "Walter" ]
                    |> Queue.peek
                    |> Expect.equal (Just "Lebowski")
        ]
