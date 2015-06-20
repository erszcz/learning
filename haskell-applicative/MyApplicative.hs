-- class Functor f => Applicative f where
--     -- | Lift a value.
--     pure :: a -> f a

--     -- | Sequential application.
--     (<*>) :: f (a -> b) -> f a -> f b

--     -- | Sequence actions, discarding the value of the first argument.
--     (*>) :: f a -> f b -> f b
--     a1 *> a2 = (id <$ a1) <*> a2
--     -- This is essentially the same as liftA2 (const id), but if the
--     -- Functor instance has an optimized (<$), we want to use that instead.

--     -- | Sequence actions, discarding the value of the second argument.
--     (<*) :: f a -> f b -> f a
--     (<*) = liftA2 const

-- class Applicative m => Monad m where
--     -- | Sequentially compose two actions, passing any value produced
--     -- by the first as an argument to the second.
--     (>>=)       :: forall a b. m a -> (a -> m b) -> m b

--     -- | Sequentially compose two actions, discarding any value produced
--     -- by the first, like sequencing operators (such as the semicolon)
--     -- in imperative languages.
--     (>>)        :: forall a b. m a -> m b -> m b
--     m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]
--     [># INLINE (>>) #<]

--     -- | Inject a value into the monadic type.
--     return      :: a -> m a
--     return      = pure

--     -- | Fail with a message.  This operation is not part of the
--     -- mathematical definition of a monad, but is invoked on pattern-match
--     -- failure in a @do@ expression.
--     fail        :: String -> m a
--     fail s      = error s

class Monad f => MyApplicative f where
    myPure :: a -> f a
    myApp :: f (a -> b) -> f a -> f b

instance MyApplicative Maybe where
    myPure = return
    -- works
    -- myApp fop fa = do
    --     op <- fop
    --     a <- fa
    --     return $ op a
    -- also works
    -- myApp fop fa = fa >>= \a -> fop >>= \op -> return $ op a
    -- and this works too
    -- myApp fop = flip (>>=) (\a -> fop >>= \op -> return $ op a)
    myApp fop = (=<<) $ \a -> fop >>= \op -> return $ op a

main = putStrLn . show $ myPure (*3) `myApp` Just 2
