{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty
import Network.Wai.Middleware.Cors (simpleCors) -- CORSミドルウェア
import Control.Monad.IO.Class (liftIO)          -- initDBの実行のため
import Network.HTTP.Types.Status (status404) -- 忘れずに追加

import qualified MemoApp0529.Core as Core
import qualified MemoApp0529.Handler as H -- ハンドラ関数を修飾名で呼び出す

main :: IO ()
main = do
  liftIO Core.initDB -- サーバー起動前にDBを初期化
  putStrLn "Starting server on http://localhost:3000"
  scotty 3000 $ do
    middleware simpleCors -- CORSを許可するミドルウェアを追加

    -- APIエンドポイントのルーティング
    post "/memos" H.createMemoHandler
    get "/memos" H.getMemosHandler -- get :: RoutePattern -> ActionM () -> ScottyM ()　　　H.getMemosHandlerの型はActionM ()
    get "/memos/:id" H.getMemoHandler
    put "/memos/:id" H.updateMemoHandler
    delete "/memos/:id" H.deleteMemoHandler

    -- マッチしなかったルートに対するデフォルトのレスポンス (任意)
    notFound $ do
      status status404
      json ("Route not found." :: String)