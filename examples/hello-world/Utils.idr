module Utils

%default total


||| Cycle given bounded index through the list, returning back to the
||| first element after reaching the end
export
listCycle : (xs : List a) -> (n : Nat ** InBounds n xs) -> (n : Nat ** InBounds n xs)
listCycle (x :: xs) dpair = case findNext (x :: xs) dpair of
  Just next => next
  Nothing => (Z ** InFirst)
where
  findNext : (xs : List a) -> (n : Nat ** InBounds n xs) -> Maybe (n : Nat ** InBounds n xs)
  findNext (x :: Nil) (Z ** InFirst) = Nothing
  findNext (x :: x' :: xs) (Z ** InFirst) = Just (S Z ** InLater InFirst)
  findNext (x :: xs) (S k ** InLater pf) with (findNext xs (k ** pf))
    findNext (x :: xs) (S k ** InLater pf) | (Just (n ** pf2)) = Just (S n ** InLater pf2)
    findNext (x :: xs) (S k ** InLater pf) | Nothing = Nothing
  findNext [] (n ** pf) impossible
listCycle [] (n ** pf) impossible

