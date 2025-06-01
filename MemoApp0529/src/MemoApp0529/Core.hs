{-
「メモ」とは何か、というデータ構造を定義する。（データ型定義）
その「メモ」を、JSON形式やデータベース形式に変換するための「翻訳ルール」を定義する。（型クラスインスタンス）
メモを保存するためのデータベースとテーブルを準備する。（DB初期化）
-}

{-# LANGUAGE OverloadedStrings #-} -- "ただの文字列" を、より高機能な Text 型として扱えるようにしてくれます。日本語などを扱う際に便利です。
{-# LANGUAGE DeriveGeneric     #-} -- 後で出てくる deriving (Generic) を使えるようにするためのスイッチです。これを使うと、JSONへの変換ルールなどを自動で作れるようになります。

module MemoApp0529.Core (
  Memo(..),
  MemoId,
  NewMemo(..),
  initDB,
  dbFile
) where

import Data.Aeson (FromJSON, ToJSON, object, (.=)) -- (.=) を追加
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import GHC.Generics (Generic)
import Data.Text (Text)

-- | メモの一意なID
type MemoId = Int

-- | メモのデータ型
data Memo = Memo {
  memoId        :: MemoId,
  memoTitle     :: Text,
  memoContent   :: Text
} deriving (Show, Generic, Eq) -- Eqを追加 (テストや比較に便利)

-- JSONへの変換/JSONからの変換を自動導出
instance ToJSON Memo
instance FromJSON Memo -- IDを含めて受信することは少ないが、念のため定義

-- SQLiteからの行データをMemo型に変換
instance FromRow Memo where
  fromRow = Memo <$> field <*> field <*> field

-- Memo型をSQLiteの行データに変換 (IDは自動インクリメントなので挿入時は不要だが更新時に使用)
instance ToRow Memo where
  toRow (Memo mId mTitle mContent) = toRow (mId, mTitle, mContent)

-- | 新規作成時にIDを含まないメモのデータ型
data NewMemo = NewMemo {
  newMemoTitle   :: Text,
  newMemoContent :: Text
} deriving (Show, Generic, Eq)

{-
これは、ユーザーが新しくメモを作成するときに送ってくるデータを表します。
Memoとの違いは memoId がないことです。なぜなら、新しいメモのIDはユーザーが決めるのではなく、データベースが自動で割り振るからです。
-}

instance ToJSON NewMemo
instance FromJSON NewMemo

-- | データベースファイル名
dbFile :: String
dbFile = "memoapp0529.db"

-- | データベースの初期化 (テーブル作成)
initDB :: IO ()
initDB = withConnection dbFile $ \conn -> do
  execute_ conn "CREATE TABLE IF NOT EXISTS memos (\
                \id INTEGER PRIMARY KEY AUTOINCREMENT, \
                \title TEXT NOT NULL, \
                \content TEXT NOT NULL)"
  putStrLn $ "Database initialized (" ++ dbFile ++ ")"