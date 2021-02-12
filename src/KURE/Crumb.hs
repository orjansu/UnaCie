
module Crumb (Crumb(..)) where

{-
  Information:
  -----------------------------------------------------------------------------
  - A list of crumbs defines a path through the working AST;
  - Paths are used for navigation purposes and for applying transformations at
    specific locations.

  Working notes:
  -----------------------------------------------------------------------------
  - Let/Case/Bind/Alt crumbs have indices for a specific navigation purpose.
    This allows us to jump to the RHS of a binding and then back into the
    correct previous location:

           focus
             |
             V
           x = <some term def.>     (Let_Bind i)

      unie> rhs

                   focus
                     |
                     V
           x = <some term def.>     (Bind_Body i)

      unie> up

           focus
             |
             V
           x = <some term def.>     (Let_Bind i)

  - This would not be possible without storing an index /somewhere/, and here
    seems the most appropriate place.
-}

data Crumb = CBind_Kind    | CBind_Name    | CBind_Body
           | TBind_Name    | TBind_Body
           | Abs_Name      | Abs_Body
           | App_Fun       | App_Arg
           | Tick_Body
           | Let_Bind  Int | Let_Body
           | Bind_Name Int | Bind_Body Int | Bind_Idx Int
           | Case_Scrut    | Case_Alt  Int
           | Alt_Con       | Alt_Name  Int | Alt_Body Int | Alt_Idx Int
           | AppD_Con      | AppD_Body Int
           | CVar_Kind     | CVar_Name     | CVar_Body
             deriving (Show, Eq)
