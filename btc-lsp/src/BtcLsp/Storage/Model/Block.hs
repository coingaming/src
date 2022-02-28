module BtcLsp.Storage.Model.Block
  ( createUpdate,
    getLatest,
  )
where

import BtcLsp.Import
import qualified BtcLsp.Import.Psql as Psql

createUpdate ::
  ( Storage m
  ) =>
  BlkHeight ->
  BlkHash ->
  BlkPrevHash ->
  m (Entity Block)
createUpdate height hash prev = do
  ct <- getCurrentTime
  --
  -- TODO : investigate how this will behave
  -- in case where multiple instances of scanner
  -- are running (as lsp cluster).
  -- Any possible race conditions?
  --
  runSql $ do
    Psql.update $ \row -> do
      Psql.set
        row
        [ BlockStatus Psql.=. Psql.val BlkOrphanNew,
          BlockUpdatedAt Psql.=. Psql.val ct
        ]
      Psql.where_ $
        ( row Psql.^. BlockHeight
            Psql.==. Psql.val height
        )
          Psql.&&. ( row Psql.^. BlockStatus
                       Psql.==. Psql.val BlkConfirmed
                   )
          Psql.&&. ( row Psql.^. BlockHash
                       Psql.!=. Psql.val hash
                   )
    Psql.upsertBy
      (UniqueBlock hash)
      Block
        { blockHeight = height,
          blockHash = hash,
          blockPrev = prev,
          blockStatus = BlkConfirmed,
          blockInsertedAt = ct,
          blockUpdatedAt = ct
        }
      [ BlockStatus Psql.=. Psql.val BlkConfirmed,
        BlockUpdatedAt Psql.=. Psql.val ct
      ]

getLatest :: (Storage m) => m (Maybe (Entity Block))
getLatest =
  runSql $
    listToMaybe
      <$> Psql.selectList
        [ BlockStatus `Psql.persistEq` BlkConfirmed
        ]
        [ Psql.Desc BlockHeight,
          Psql.LimitTo 1
        ]