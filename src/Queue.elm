module Queue exposing (Queue, count, dequeue, empty, enqueue, peek, toList, fromList)


type Node a
    = NodeImpl a (Maybe (Link a))


type Link a
    = LinkImpl (Node a)


type alias Impl a =
    { head : Maybe (Node a)
    , count : Int
    }


type Queue a
    = Queue (Impl a)


empty : Queue a
empty =
    Queue { head = Nothing, count = 0 }


count : Queue a -> Int
count (Queue impl) =
    impl.count


peek : Queue a -> Maybe a
peek (Queue impl) =
    case impl.head of
        Just (NodeImpl val _) ->
            Just val

        _ ->
            Nothing


enqueue : a -> Queue a -> Queue a
enqueue val (Queue impl) =
    if impl.head == Nothing then
        let
            el =
                NodeImpl val Nothing
        in
            Queue { head = Just el, count = 1 }

    else
        let
            newImpl =
                { impl
                    | head = impl.head |> traverseAndAppend_ val
                    , count = impl.count + 1
                }
        in
        Queue newImpl


dequeue : Queue a -> ( Maybe a, Queue a )
dequeue (Queue impl) =
    if impl.head == Nothing then
        ( Nothing, empty )

    else
        updateHead_ impl


toList : Queue a -> List a
toList (Queue impl) =
    List.reverse (traverse_ [] impl.head)


fromList : List a -> Queue a
fromList source =
    source
        |> List.foldl (\val -> enqueue val) empty


updateHead_ : Impl a -> ( Maybe a, Queue a )
updateHead_ impl =
    let
        val =
            unbox_ impl.head

        nextHead =
            next_ impl.head
    in
        ( val, Queue { impl | head = nextHead, count = impl.count - 1 } )


updateNode_ : a -> Maybe (Node a) -> Maybe (Node a)
updateNode_ val node =
    case node of
        Just node ->
            Just (NodeImpl val (Just (LinkImpl node)))

        Nothing ->
            Just (NodeImpl val Nothing)


traverse_ : List a -> Maybe (Node a) -> List a
traverse_ vals curr =
    case curr of
        Just (NodeImpl val (Just (LinkImpl next))) ->
            traverse_ (val :: vals) (Just next)
        Just (NodeImpl val Nothing) ->
            traverse_ (val :: vals) Nothing
        Nothing ->
            vals


traverseAndAppend_ : a -> Maybe (Node a) -> Maybe (Node a)
traverseAndAppend_ newVal curr =
    case curr of
        Just (NodeImpl val (Just (LinkImpl next))) ->
            (traverseAndAppend_ newVal <| Just next)
                |> updateNode_ val

        Just (NodeImpl val Nothing) ->
            --Just (NodeImpl val (Just (LinkImpl (NodeImpl newVal Nothing))))
            Just (NodeImpl newVal Nothing)
                |> updateNode_ val

        Nothing ->
            Nothing


unbox_ : Maybe (Node a) -> Maybe a
unbox_ node =
    case node of
        Just (NodeImpl data _) ->
            Just data

        Nothing ->
            Nothing


next_ : Maybe (Node a) -> Maybe (Node a)
next_ node =
    case node of
        Just (NodeImpl _ (Just (LinkImpl next))) ->
            Just next

        Just (NodeImpl _ Nothing) ->
            Nothing

        Nothing ->
            Nothing
