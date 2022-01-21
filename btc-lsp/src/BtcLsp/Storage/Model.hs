{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BtcLsp.Storage.Model where

import BtcLsp.Class.Storage
import BtcLsp.Data.Kind
import BtcLsp.Data.Type
import BtcLsp.Import.External
import qualified BtcLsp.Import.Psql as Psql
import Database.Persist.Quasi
import Database.Persist.TH

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
  [mkPersist sqlSettings, mkMigrate "migrateAuto"]
  $(persistFileWith lowerCaseSettings "config/model")

instance HasTableName User where
  getTableName = const UserTable

instance HasTableName LnChan where
  getTableName = const LnChanTable

instance Out (Psql.BackendKey Psql.SqlBackend)

deriving stock instance Generic (Psql.Key User)

instance Out (Psql.Key User)

instance Out User

deriving stock instance Generic (Psql.Key SwapIntoLn)

instance Out (Psql.Key SwapIntoLn)

instance Out SwapIntoLn

deriving stock instance Generic (Psql.Key LnChan)

instance Out (Psql.Key LnChan)

instance Out LnChan