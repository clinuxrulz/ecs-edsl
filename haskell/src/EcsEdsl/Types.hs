module EcsEdsl.Types where

newtype Entity = Entity Int

newtype Component = Component Int

newtype ComponentType = ComponentType Int

newtype System = System Int

newtype List a = List Int

newtype TInt = TInt Int

newtype TDouble = TDouble Int

newtype TString = TString Int
