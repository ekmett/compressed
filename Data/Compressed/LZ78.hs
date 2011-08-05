-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Compressed.LZ78
-- Copyright   :  (c) Edward Kmett 2009-2011
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- Compression algorithms are all about exploiting redundancy. When applying
-- an expensive 'Reducer' to a redundant source, it may be better to 
-- extract the structural redundancy that is present. 'LZ78' is a compression
-- algorithm that does so, without requiring the dictionary to be populated
-- with all of the possible values of a data type unlike its later 
-- refinement LZW, and which has fewer comparison reqirements during encoding
-- than its earlier counterpart LZ77. 
-----------------------------------------------------------------------------

module Data.Compressed.LZ78 
    ( 
    -- * Lempel-Ziv 78 
      LZ78
    -- * Encoding
    , encode    -- /O(n)/
    , encodeOrd -- /O(n log n)/
    , encodeEq  -- /O(n^2)/
    -- * Decoding (reduce)
    , decode
    -- * Recoding
    , recode    -- /O(n)/
    , recodeOrd -- /O(n log n)/
    , recodeEq  -- /O(n^2)/
    ) where

import Data.Compressed.Internal.LZ78
