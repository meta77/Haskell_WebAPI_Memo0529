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
getConn :: ActionM Connection -- 「Webリクエスト文脈でデータベース接続を返す」型
getConn = liftIO $ open dbFile -- liftIO により、IO Connection を ActionM Connection に変換


-- POST /memos : 新しいメモを作成
createMemoHandler :: ActionM () -- Webリクエストに対して何らかの処理を行うが、処理の結果（値）は外に返さない。副作用だけを行う。
createMemoHandler = do
  b <- body -- body は、HTTPリクエストの「中身」（JSONなど）を取得する関数。b :: ByteString
  case eitherDecode b :: Either String NewMemo of -- eitherDecode は JSON の文字列（ByteString）を Haskell の型に変換する関数。
  -- :: Either String NewMemo は「変換に成功すれば Right NewMemo、失敗すれば Left String（エラー文字列）」を返す、という "型注釈 :: "です。
  -- Either 型は Left エラー または Right 成功データ のどちらか。それに応じて、case ... ofで分岐する。
  -- eitherDecode :: FromJSON a => ByteString -> Either String a 　ゆえに、成功したら Right a を返す。失敗したら Left String を返す。返されるのは、まさに Either 型の「値」。


    Left err -> do -- eitherDecode が Left err を返した場合（＝パース失敗）。err は失敗の理由が入っている String 型
      status status400 -- HTTPレスポンスのステータスを 400 に設定する
      -- status： Scotty（Webフレームワーク）で HTTP ステータスコードを設定する関数
      -- status400： Haskellの Network.HTTP.Types.Status モジュールから来ている定数で、HTTP 400 Bad Request を表す

      json $ object ["error" .= ("Invalid JSON: " ++ err)]
      -- json は Scotty の関数で、Haskellの値を JSON に変換して返す
      -- object []　はAesonライブラリの関数。キーと値のペアで JSON オブジェクトを作る. 例：object ["key" .= value] → { "key": value } という JSON に変換される
      -- err に入るのは、Aesonライブラリ（HaskellのJSON処理ライブラリ）が生成するエラーメッセージです。


    Right newMemo -> do -- JSONパースに成功した場合。newMemo という変数に、パースされたメモ（NewMemo型）が入る
      conn <- getConn -- getConn は SQLite の接続を取得する自作の関数。conn に接続オブジェクトを入れる。

      -- liftIO は「IO処理（データベース操作など）」を ActionM の中で使うための関数。execute は SQL を実行する関数（副作用だけで返り値はない）。
      liftIO $ execute conn "INSERT INTO memos (title, content) VALUES (?, ?)"
                        (newMemoTitle newMemo, newMemoContent newMemo) -- newMemoTitleは、newMemoTitle型のフィールドアクセサ（ゲッター）です。


      -- 登録が成功したか・内容が正しいかを再確認するため
      newId <- liftIO $ lastInsertRowId conn -- lastInsertRowId は直前に挿入されたデータのID（自動採番）を取得する関数。
      -- 作成されたメモを取得して返す
      createdMemos <- liftIO $ query conn "SELECT id, title, content FROM memos WHERE id = ?" (Only newId) :: ActionM [Memo]
      {-
      query :: (FromRow r, ToRow q) => Connection -> Query -> q -> IO [r]
      データベースに SELECT を送る関数
      ・第一引数：conn（接続情報）
      ・第二引数：SQL 文（"SELECT id, title, content FROM memos WHERE id = ?"）
      ・第三引数：パラメータ（Only newId）で、? を置き換えるために使う
      -}
      -- Only は、1個だけのパラメータを包むときに使う
      -- :: ActionM [Memo]　　　ActionM 型の中に [Memo] が入っている。
      -- [Memo]の例　　[Memo 1 "title1" "content1", Memo 2 "title2" "content2"]

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
  -- liftIO 通常の IO 処理を、ActionM モナドの中で使えるように「持ち上げる」関数。
  -- IO [Memo] を ActionM [Memo] に変換
  liftIO $ close conn
  json memos -- memos は [Memo] 型のリスト（データベースから取得した全メモ）。json 関数は、それを JSON形式でHTTPレスポンスとして返す関数。

{-
liftIO :: IO a -> m a

「通常の IO 処理を、他のモナド（たとえば ActionM）の中で使うために「持ち上げる（lift）」」のが liftIO
SQLiteの操作は全部 IO 型の処理ですが、Scotty の中（ActionM モナド）で使うには liftIO が必要です。
-}





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