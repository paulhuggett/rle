{-# LANGUAGE ViewPatterns #-}

import Data.Char (ord, chr)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)

import Data.Maybe
import Data.Int
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Bits (shiftR, (.&.))


import Test.HUnit -- for HUnit
import qualified Test.QuickCheck as QC -- for QuickCheck

-- toHex
-- ~~~~~
-- Covert a ByteString to a string of space-separated hex characters.
toHex :: BL.ByteString -> String
toHex x = BL.foldr addByte "" x
    where
        addByte :: Word8 -> [Char] -> [Char]
        addByte w xs  = concat [
                [ num2HexChar $ shiftR w 4 ],
                [ num2HexChar $ w .&. (15::Word8) ],
                separator xs,
                xs
            ]

        separator :: [Char] -> [Char]
        separator "" = ""
        separator _ = " "

        num2HexChar :: Word8 -> Char
        num2HexChar x
            | x < 10 = chr (xi + ord '0')
            | otherwise = chr (xi - 10 + ord 'a')
            where xi = fromIntegral x


-- packStr
-- ~~~~~~~
packStr :: String -> BL.ByteString
packStr = encodeUtf8 . T.pack


-- runLengthDecode
-- ~~~~~~~~~~~~~~~
-- TODO: rewrite using one of the higher-order list processing functions.
runLengthDecode :: BL.ByteString -> Maybe BL.ByteString
runLengthDecode bytes
    | bytes == BL.empty = Nothing
    -- end-of-data
    | h == 128 = if t == BL.empty then Just BL.empty else Nothing

    -- if h < 128, the following h + 1 bytes are copied literally
    | h < 128 = do
        rhs <- runLengthDecode $ BL.drop (h+1) t
        return $ BL.append (BL.take (h+1) t) rhs

    -- h > 128, the following byte is copied 257−h (2 to 128) times
    | otherwise = do
        rhs <- runLengthDecode $ BL.drop 1 t
        return $ BL.append (BL.replicate (257-h) (BL.head t)) rhs

    where
        h = fromIntegral $ BL.head bytes
        t = BL.tail bytes





bl_foldl :: Monad m => (BL.ByteString -> Word8 -> m BL.ByteString) -> BL.ByteString -> BL.ByteString -> m BL.ByteString
bl_foldl step initial (BL.uncons -> Nothing) = return initial
bl_foldl step initial bytes = do
        next <- step initial (BL.head bytes)
        res <- bl_foldl step next (BL.tail bytes)
        return res

-- TODO:
--runLengthDecode2 :: BL.ByteString -> Maybe BL.ByteString
--runLengthDecode2 bytes = bl_foldl f (Just BL.empty) bytes
--    where
--        f :: Word8 -> BL.ByteString -> Maybe BL.ByteString
--        f = Nothing


-- runLengthEncode
-- ~~~~~~~~~~~~~~~
runLengthEncode :: BL.ByteString -> BL.ByteString
runLengthEncode bs = BL.foldr step (BL.pack [128]) bs
    where
        step :: Word8 -> BL.ByteString -> BL.ByteString
        step a bytes
            | h == 128 = BL.pack [0, a, 128]
            -- We're building a literal sequence.
            | h < 128 =
                let literal = prefix_literal a h t
                in BL.append (BL.pack $ fst literal) (snd literal)
            -- We're in a run.
            | otherwise = BL.append (prefix_run a h $ BL.head t) t
            where
                h = BL.head bytes
                t = BL.tail bytes

        -- Add a character to an existing literal unless the literal has reached the
        -- maximum allowed length in which event we start a new literal.
        -- c = The character to prefix.
        -- rc = The RLE code: the number of characters in the literal.
        -- xs = The encoded ByteString tail.
        prefix_literal :: Word8 -> Word8 -> BL.ByteString -> ([Word8],BL.ByteString)
        prefix_literal c rc xs
            -- Add this character to the head of the literal group.
            | c /= (BL.head xs) = (if rc < 127 then [rc+1,c] else [0,c,rc], xs)
            -- This is a run of 2,
            | otherwise = (if rc /= 0 then [255,c,rc-1] else [255,c], BL.tail xs)

        -- Add this character to the existing run unless the run is already at
        -- maximum length in which event make a new literal of length 1.
        -- c = The character to prefix.
        -- rc = The RLE code. Defines the length of the repeat run.
        -- h = The head of the encoded ByteString.
        prefix_run :: Word8 -> Word8 -> Word8 -> BL.ByteString
        prefix_run c rc h = BL.pack $ if c == h && rc > 129 then [rc-1] else [0,c,rc]



runLengthEncode2 :: BL.ByteString -> BL.ByteString
runLengthEncode2 bs = BL.foldl step BL.empty bs
    where
        step :: BL.ByteString -> Word8 -> BL.ByteString
        step a b = a


runLengthEncodeList :: [Word8] -> BL.ByteString
runLengthEncodeList = runLengthEncode . BL.pack


unitTests :: IO Counts
unitTests = runTestTT $ TestList [
  "RLE encode empty"     ~: (BL.pack [128]) ~=? runLengthEncode BL.empty
 ,"RLE encode 65"        ~: (BL.pack [0,65,128]) ~=? runLengthEncodeList [65]
 ,"RLE encode 65,66"     ~: (BL.pack [1,65,66,128]) ~=? runLengthEncodeList [65,66]
 ,"RLE encode 65,65"     ~: (BL.pack [255,65,128]) ~=? runLengthEncodeList [65,65]
 ,"RLE encode 3x65"      ~: (BL.pack [254,65,128]) ~=? (runLengthEncodeList [65,65,65])
 ,"RLE encode 2x65,66"   ~: (BL.pack [255,65,0,66,128]) ~=? (runLengthEncodeList [65,65,66])
 ,"RLE encode 2x65,2x66" ~: (BL.pack [255,65,255,66,128]) ~=? (runLengthEncodeList [65,65,66,66])
 ,"RLE encode 128x65"    ~: (BL.pack [129,65,128]) ~=? (runLengthEncode $ BL.replicate 128 65)
 ,"RLE encode 129x65"    ~: (BL.pack [0,65,129,65,128]) ~=? (runLengthEncode $ BL.replicate 129 65)
 ,"RLE encode 0..127"    ~: (BL.pack ([127] ++ [0..127] ++ [128])) ~=? (runLengthEncodeList [0..127])
 ,"RLE encode 0..128"    ~: (BL.pack ([0,0,127] ++ [1..128] ++ [128])) ~=? (runLengthEncodeList [0..128])

 ,"RLE decode empty" ~: Nothing ~=? runLengthDecode BL.empty
 ,"RLE decode EOD" ~: (Just BL.empty) ~=? (runLengthDecode $ BL.pack [128])
 ,"RLE decode early EOD" ~: Nothing ~=? (runLengthDecode $ BL.pack [128, 0])
 ,"RLE decode 65" ~: (Just $ BL.pack [65]) ~=? (runLengthDecode $ BL.pack [0,65,128])
 ,"RLE decode 65,66" ~: (Just $ BL.pack [65,66]) ~=? (runLengthDecode $ BL.pack [1,65,66,128])
 ,"RLE decode 2x65" ~: (Just $ BL.pack [65,65]) ~=? (runLengthDecode $ BL.pack [255,65,128])
 ,"RLE decode 128x65" ~: (Just $ BL.replicate 128 65) ~=? (runLengthDecode $ BL.pack [129,65,128])
 ,"RLE decode 66,2x65" ~: (Just $ BL.pack [66,65,65]) ~=? (runLengthDecode $ BL.pack [0,66,255,65,128])
 ,"RLE no EOD" ~: Nothing ~=? (runLengthDecode $ BL.pack [0,66,255,65])
 ]


propRLE :: [Word8] -> Bool
propRLE x = (Just $ BL.pack x) == (runLengthDecode $ runLengthEncodeList x)

qc :: IO ()
qc = QC.quickCheck propRLE

qcVerbose :: IO ()
qcVerbose = QC.verboseCheck propRLE

main :: IO ()
main = let res = runLengthDecode $ BL.pack [1,65,66,128]
    in print $ if res == Nothing then "error" else toHex $ Data.Maybe.fromJust res

--eof
