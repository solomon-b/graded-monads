Graded Monads
=============

🚨 **WORK IN PROGRESS** 🚨

[![graded-monads::CI](https://github.com/solomon-b/graded-monads/actions/workflows/nix.yaml/badge.svg)](https://github.com/solomon-b/graded-monads/actions/workflows/nix.yaml)

A Graded Monad `𝐺` maps from a Monoidal Category `(𝔼, ⊗, Iₑ)` to the
Category of Endofunctors: `𝐺 : 𝔼 → [ℂ, ℂ]`.

Graded Monads give you two monad-like operations:

```
η : Id → G Iₑ
μ : 𝐺 x ∘ 𝐺 y → 𝐺 (x ⊗ y)
```

Which capture, or 'grades', the monad with objects of `𝔼`.

This library encodes graded monads in Haskell for managing effects
using a `QualifiedDo` interface:

```haskell
type GradedMonad ::
  ([Type] -> Type -> Type) ->
  Type ->
  (Type -> Type -> Type) ->
  Constraint
class (FunctorF m) => GradedMonad m i t | m -> i t where
  {-# MINIMAL greturn, (gjoin | gbind) #-}
  greturn :: Identity ~> m '[]

  gjoin :: (AppendTensored xs) => (m xs `Compose` m ys) ~> m (xs ++ ys)
  gjoin (Compose m) = m `gbind` id

  gbind :: (AppendTensored xs) => m xs a -> (a -> m ys b) -> m (xs ++ ys) b
  gbind mxa f = gjoin $ Compose $ fmap f mxa
```

We currently have graded version of `ExceptT` and `WriterT`. 

The graded form of `ExceptT` allows you to track specify specific
error types per subroutine and tensor those error types together when
performaing a graded bind:

```haskell
mkRequest :: (GradedMonadError m) => String -> m '[ParseError] Request
mkRequest _ = G.return Request

transformRequest :: (GradedMonadError m) => Request -> m '[TransformError] Request
transformRequest _ = gthrowError TransformError


invokeRequest _ = G.return Response

main :: IO ()
main = do
  result <- runExceptT' $ G.do
    req <- mkRequest "hoogle.hackage.com"
    req' <- transformRequest req
    invokeRequest req'

  case result of
   Left (Left ParseError) -> print ParseError
   Left (Right (Left TransformError)) -> print TransformError
   Left (Right (Right HttpError)) -> print HttpError
   Right Response -> print Response
```

The graded form of `WriterT` allows you to specifiy unique log types per subroutine in a similar manner:
```haskell
mkRequest :: (GradedMonadWriter m) => String -> m '[ParseLog] Request
mkRequest _ = G.do
  gtell ParseLog
  G.return Request

transformRequest :: (GradedMonadWriter m) => Request -> m '[TransformLog] Request
transformRequest _ = G.do
  gtell TransformLog
  G.return Request

invokeRequest :: (GradedMonadWriter m) => Request -> m '[HttpLog] Response
invokeRequest _ = G.do
  gtell HttpLog
  G.return Response

main :: IO ()
main = do
  result <- runWriterT' $ G.do
    req <- mkRequest "hoogle.hackage.com"
    req' <- transformRequest req
    invokeRequest req'

  -- result :: (Response, Tensored (,) () '[ParseLog, TransformLog, HttpLog])
  print result
```
