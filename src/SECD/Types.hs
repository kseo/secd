module SECD.Types where

data MValue = MInt Int
            | MClosure [Command] [MValue]
            | MUndefined
              deriving (Eq)

instance Show MValue where
  show (MInt i) = show i
  show (MClosure _ _) = "<<closure>>"
  show MUndefined = "<<undefined>>"

data Command = IInt Int
             | IAdd
             | ISub
             | IAccess Int
             | IClosure [Command]
             | IApply
             | IReturn
               deriving (Eq, Show)

type SECD = ([Command], [MValue], [MValue])
