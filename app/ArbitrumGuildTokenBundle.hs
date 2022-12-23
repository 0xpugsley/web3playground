{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ArbitrumGuildTokenBundle where

import Network.Ethereum.Contract.TH

[abiFrom|app/ArbitrumGuildTokenBundle.json|]
-- [abiFrom|app/ERC20.json|]