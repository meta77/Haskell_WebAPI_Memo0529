{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-} -- エラーメッセージ内の型注釈のため

-- このモジュールは、外部からアクセスできる関数（ハンドラー）をまとめる
module MemoApp0529.Handler (
  createMemoHandler,
  getMemosHandler,
  getMemoHandler,
  updateMemoHandler,
  deleteMemoHandler
) where

import Web.Scotty
import Database.SQLite.Simple
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=), eitherDecode) -- eitherDecode を追加
import Network.HTTP.Types.Status
    ( status200, status201, status204, status400, status404, status500 )

import MemoApp0529.Core ( Memo(..), NewMemo(..), dbFile, MemoId )

-- DB接続を取得するヘルパー (リクエストごとに開閉)。毎回この関数でデータベースに接続。
getConn :: ActionM Connection
getConn = liftIO $ open dbFile

-- POST /memos : 新しいメモを作成
createMemoHandler :: ActionM ()
createMemoHandler = do
  b <- body -- body は、HTTPリクエストの「中身」（JSONなど）を取得する関数。
  case eitherDecode b :: Either String NewMemo of
    Left err -> do
      status status400
      json $ object ["error" .= ("Invalid JSON: " ++ err)]
    Right newMemo -> do
      conn <- getConn
      liftIO $ execute conn "INSERT INTO memos (title, content) VALUES (?, ?)"
                        (newMemoTitle newMemo, newMemoContent newMemo)
      newId <- liftIO $ lastInsertRowId conn
      -- 作成されたメモを取得して返す
      createdMemos <- liftIO $ query conn "SELECT id, title, content FROM memos WHERE id = ?" (Only newId) :: ActionM [Memo]
      liftIO $ close conn
      case createdMemos of
        [memo] -> do
          status status201
          json memo
        _ -> do
          status status500
          json $ object ["error" .= ("Failed to retrieve created memo" :: String)]

-- GET /memos : 全てのメモを取得
getMemosHandler :: ActionM ()
getMemosHandler = do
  conn <- getConn
  memos <- liftIO $ query_ conn "SELECT id, title, content FROM memos" :: ActionM [Memo]
  liftIO $ close conn
  json memos

-- GET /memos/:id : 特定のメモを取得
getMemoHandler :: ActionM () -- ActionM () は「Webリクエストに対するアクション」で、() は「返り値が特にない（無視していい）」という意味。
getMemoHandler = do
  memoIdParam <- param "id" :: ActionM MemoId
  conn <- getConn
  memos <- liftIO $ query conn "SELECT id, title, content FROM memos WHERE id = ?" (Only memoIdParam) :: ActionM [Memo]
  liftIO $ close conn
  case memos of
    [memo] -> json memo
    []     -> do
      status status404
      json $ object ["error" .= ("Memo not found" :: String)]
    _      -> do -- 通常、主キー検索では発生しない
      status status500
      json $ object ["error" .= ("Multiple memos found with the same ID" :: String)]

-- PUT /memos/:id : 特定のメモを更新
updateMemoHandler :: ActionM ()
updateMemoHandler = do
  memoIdParam <- param "id" :: ActionM MemoId
  b <- body
  case eitherDecode b :: Either String NewMemo of -- 更新時もタイトルと本文を受け取る
    Left err -> do
      status status400
      json $ object ["error" .= ("Invalid JSON: " ++ err)]
    Right memoToUpdate -> do
      conn <- getConn
      -- 先に存在確認
      existingMemos <- liftIO $ query conn "SELECT id FROM memos WHERE id = ?" (Only memoIdParam) :: ActionM [Only MemoId]
      if null existingMemos
      then do
        liftIO $ close conn
        status status404
        json $ object ["error" .= ("Memo not found, cannot update" :: String)]
      else do
        liftIO $ execute conn "UPDATE memos SET title = ?, content = ? WHERE id = ?"
                          (newMemoTitle memoToUpdate, newMemoContent memoToUpdate, memoIdParam)
        -- 更新されたメモを取得して返す
        updatedMemos <- liftIO $ query conn "SELECT id, title, content FROM memos WHERE id = ?" (Only memoIdParam) :: ActionM [Memo]
        liftIO $ close conn
        case updatedMemos of
          [updatedMemo] -> json updatedMemo
          _ -> do
            status status500 -- 更新は成功したが取得に失敗した場合
            json $ object ["error" .= ("Failed to retrieve updated memo" :: String)]

-- DELETE /memos/:id : 特定のメモを削除
deleteMemoHandler :: ActionM ()
deleteMemoHandler = do
  memoIdParam <- param "id" :: ActionM MemoId
  conn <- getConn
  -- 先に存在確認
  existingMemos <- liftIO $ query conn "SELECT id FROM memos WHERE id = ?" (Only memoIdParam) :: ActionM [Only MemoId]
  if null existingMemos
  then do
    liftIO $ close conn
    status status404
    json $ object ["error" .= ("Memo not found, cannot delete" :: String)]
  else do
    liftIO $ execute conn "DELETE FROM memos WHERE id = ?" (Only memoIdParam)
    liftIO $ close conn
    status status204 -- No Content
    -- text "" -- Scottyではjsonなど何かしら返すかfinishすることが推奨されるが、204の場合はボディなしが一般的