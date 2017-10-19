{-# LANGUAGE LambdaCase #-}

module InterState
  ( InterState(..)
  , fromScriptState  -- Exit from a script state.
  , fromTransState   -- Exit from a transformation state.
  , isScriptState    -- Check if current state is a script state.
  , isTransState     -- Check if current state is a transformation state.
  , toScriptState    -- Enter a script state.
  , toTransState     -- Enter a transformation state.
  ) where 

{-
  Information:
  -----------------------------------------------------------------------------
  - InterState enumerates the possible states the interpreter can be in;
  - The interpreter's current state dictates how the it responds to user input 
    and its output to the command line (e.g., it will display the term under 
    transformation in the TRANS/TRANS_SCRIPT state, but not in the 
    INITIAL/SCRIPT state. This is because a term /should/ never be under 
    transformation in these states);
  - We implement necessary fail-safes to return the interpreter to its initial
    state if somehow an invalid state change occurs. In this situation, a 
    critical error is reported to the user;
  - We also restrict state changes to the functions defined in this file, and
    the state is /never/ modified directly.
-}

data InterState 
  = INITIAL        -- Initial state: the system's "base" state.
  | TRANS          -- Transformation state: a term is being transformed.
  | SCRIPT         -- Script state: one or more active scripts.
  | TRANS_SCRIPT   -- Transformation + script state.
    deriving (Show, Eq)

-- State checks: --------------------------------------------------------------

isTransState :: InterState -> Bool 
isTransState  = (`elem` [TRANS, TRANS_SCRIPT])

isScriptState :: InterState -> Bool 
isScriptState  = (`elem` [SCRIPT, TRANS_SCRIPT])

-- State changes: -------------------------------------------------------------

toTransState :: InterState -> InterState
toTransState INITIAL = TRANS
toTransState SCRIPT  = TRANS_SCRIPT
toTransState st      = st

fromTransState :: InterState -> InterState
fromTransState TRANS        = INITIAL
fromTransState TRANS_SCRIPT = SCRIPT
fromTransState st           = st 
          
toScriptState  :: InterState -> InterState
toScriptState INITIAL = SCRIPT
toScriptState TRANS   = TRANS_SCRIPT
toScriptState st      = st

fromScriptState :: InterState -> InterState
fromScriptState SCRIPT       = INITIAL
fromScriptState TRANS_SCRIPT = TRANS
fromScriptState st           = st                      