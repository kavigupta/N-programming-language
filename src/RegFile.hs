{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables, FlexibleContexts #-}
module RegFile(RegFile(..), RegisterIndex(..), initialRegFile, interpretRegAct) where

import Control.Monad.State
import Control.Lens

data Register o = Pull | Push o
    deriving Show

data RegFile o = RegFile {_alpha :: Register o, _beta :: Register o, _gamma :: Register o, _delta :: Register o}
    deriving Show

initialRegFile :: RegFile o
initialRegFile = RegFile Pull Pull Pull Pull

makeLenses ''RegFile

type RegisterIndexer o = Lens' (RegFile o) (Register o)

data RegisterIndex = Alpha | Beta | Gamma | Delta
    deriving (Show, Eq)

getReg :: RegisterIndex -> RegisterIndexer o
getReg Alpha = alpha
getReg Beta = beta
getReg Gamma = gamma
getReg Delta = delta

interpretRegAct :: (Monad m, MonadState (e, RegFile o) m) => m o -> (o -> m a) -> RegisterIndex -> m ()
interpretRegAct pop push loc = do
    file <- get
    let reg = file ^. _2 . getReg loc
    case reg of
        Push o -> do
            push o
            _2 . getReg loc .= Pull
        Pull -> case loc of
            Alpha -> do
                top <- pop
                _2 . getReg loc .= Push top
            Beta -> do
                top <- pop
                push top
                _2 . getReg loc .= Push top
            Gamma -> do
                top <- pop
                second <- pop
                push top
                _2 . getReg loc .= Push second
            Delta -> do
                top <- pop
                second <- pop
                push second
                push top
                _2 . getReg loc .= Push second
