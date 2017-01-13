{-# LANGUAGE FlexibleContexts #-}
module Error (
    InterpreterError,
    underflow, multipleAssign, lookupNonString, bindNonString, builtInError,
    libraryError, unboundVariable, indexWithNoNumber, executeNonCode) where

import Object

import Control.Monad.Except
import Text.Parsec

data InterpreterError e a
    = UnboundVariable String
    | CriticalError String
    | StackUnderflow
    | MultipleAssignmentError String
    | BindingToNonStringError (Object e a)
    | LookingUpNonStringError (Object e a)
    | IndexingWithNonNumberError (Object e a)
    | ExecutedNonCodeError (Object e a)
    | BuiltinTypeError String
    | CannotCompareCodeError
    | LibraryError ParseError
        deriving Show

underflow :: (MonadError (InterpreterError e a) m) => m x
underflow = throwError StackUnderflow

multipleAssign :: (MonadError (InterpreterError e a) m) => String -> m x
multipleAssign = throwError . MultipleAssignmentError

bindNonString :: (MonadError (InterpreterError e a) m) => Object e a -> m x
bindNonString = throwError . BindingToNonStringError

lookupNonString :: (MonadError (InterpreterError e a) m) => Object e a -> m x
lookupNonString = throwError . LookingUpNonStringError

builtInError :: (MonadError (InterpreterError e a) m) => String -> m x
builtInError = throwError . BuiltinTypeError

libraryError :: (MonadError (InterpreterError e a) m) => ParseError -> m x
libraryError = throwError . LibraryError

unboundVariable :: (MonadError (InterpreterError e a) m) => String -> m x
unboundVariable = throwError . UnboundVariable

indexWithNoNumber :: (MonadError (InterpreterError e a) m) => Object e a -> m x
indexWithNoNumber = throwError . IndexingWithNonNumberError

executeNonCode :: (MonadError (InterpreterError e a) m) => Object e a -> m x
executeNonCode = throwError . ExecutedNonCodeError
