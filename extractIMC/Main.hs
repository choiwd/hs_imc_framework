{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Main (main)
where

import qualified Text.XML.Light as XML
import qualified Data.Map       as Map
import Data.Maybe                       ( fromMaybe, isNothing )
import Text.Read                        ( readMaybe )
import qualified Data.Text      as T
import Data.List                        ( transpose )

import System.Console.CmdArgs

data ProtoMessage = ProtoMessage { iid       :: Int
                                 , abbrev     :: String
                                 , fields   :: [(String, String)]}  -- (abbrev, type)
                  | Incomplete deriving (Eq, Show)

imctype2type :: Map.Map String String
imctype2type = Map.fromList [
    ("int8_t", "Int8")
  , ("uint8_t", "Word8")
  , ("int16_t", "Int16")
  , ("uint16_t", "Word16")
  , ("int32_t", "Int32")
  , ("uint32_t", "Word32")
  , ("int64_t", "Int64")
  , ("fp32_t", "Float")
  , ("fp64_t", "Double")
  , ("rawdata", "BS.ByteString")
  , ("plaintext", "String")
  , ("message", "MessageF")
  , ("message-list", "[MessageF]")
  ]

getId :: XML.Element -> Maybe Int
getId x = XML.lookupAttr (XML.QName "id" Nothing Nothing) (XML.elAttribs x) >>= readMaybe :: Maybe Int
getAbbrev :: XML.Element -> Maybe String
getAbbrev x = XML.lookupAttr (XML.QName "abbrev" Nothing Nothing) $ XML.elAttribs x
getFields :: XML.Element -> [[Maybe String]]
getFields x = map (\ x -> [(XML.lookupAttr (XML.QName "abbrev" Nothing Nothing) . XML.elAttribs) x,
                          (XML.lookupAttr (XML.QName "type" Nothing Nothing) . XML.elAttribs) x]) fields_elements
            where fields_elements = XML.findElements (XML.QName "field" Nothing Nothing) x

extractMessages :: XML.Element -> ProtoMessage
extractMessages x
    | isNothing id                      = Incomplete
    | isNothing abbrev                  = Incomplete
    | isNothing $ mapM sequence fields  = Incomplete
    | otherwise                         = ProtoMessage (fromMaybe 0 id) (fromMaybe "" abbrev) (map (\[x,y] -> (fromMaybe "" x, fromMaybe "" y)) fields)
      where id = getId x
            abbrev = getAbbrev x
            fields = getFields x

templateDataString1 :: T.Text
templateDataString1 = "newtype %NAME%Fields = %NAME%Fields { %ATT% } deriving (Show, Eq, Generic)\ninstance IMCSerialize %NAME%Fields"

templateDataString2 :: T.Text
templateDataString2 = "data %NAME%Fields = %NAME%Fields { %ATT% } deriving (Show, Eq, Generic)\ninstance IMCSerialize %NAME%Fields"

templatetoFields :: T.Text
templatetoFields = "%NAME% f -> putIMC be (%ID% :: Word16) >> putIMC be f"
templatefromFields :: T.Text
templatefromFields = "%ID% -> %NAME% <$> (getIMC be :: Get %NAME%Fields)"

tuple2text :: (String, String) -> T.Text
tuple2text (v, _type) = T.pack $ "_" ++ v ++ " :: "++ fromMaybe "NOTHING" (Map.lookup _type imctype2type)

makeMessageData :: ProtoMessage -> [T.Text]
makeMessageData pm = [constructor, toFields, fromFields, dataDeclaration]
  where _abbrev = T.pack $ abbrev pm
        _id = iid pm
        constructor = T.replace "%NAME%" _abbrev "%NAME% %NAME%Fields"
        toFields = T.replace "%NAME%" _abbrev $ T.replace "%ID%" (T.pack $ show _id) templatetoFields
        fromFields = T.replace "%NAME%" _abbrev $ T.replace "%ID%" (T.pack $ show _id) templatefromFields
        dataDeclaration = T.replace "%NAME%" _abbrev aux

        aux = let att = T.intercalate ", " . map tuple2text $ fields pm -- data with 1 and 2 or more fields have
              in if length (fields pm) == 1                             -- different templates
                    then T.replace "%ATT%" att templateDataString1
                    else T.replace "%ATT%" att templateDataString2

-- Do not use newtype, bc it breaks.
data ExtractIMC = ExtractIMC  {
                        whitelist :: String
                      --, blacklist :: String
                      } deriving (Show, Data, Typeable)

opts :: ExtractIMC
opts = ExtractIMC {
                whitelist = def
                &= help "List of messages"
                &= typ "FILE"
                }
                  &= summary "\nGenerates data types for selected IMC messages and a Map that contains their specification"
                  &= details [ "extractIMC", ""
                  ,"The file must contain the abbreviation of the message, one per line."
                  , "Since the type checker cannot handle all messages as Haskell data types"
                  , "(it blows up the memory), it is recommended you choose those you want."
                  , "Also, keep in mind that it does not check messages dependencies."
                  , "If you want to send or receive messages that contain inline messages,"
                  , "you should add both to the extraction list."]

main :: IO ()
main = do
  opts <- cmdArgs opts
  
  if whitelist opts == ""
    then putStrLn "\nA whitelist is a required argument! See the help using -? flag, for example:\n\n   $ cabal run extractIMC -- -?\n\nExiting.\nNo file has been changed."
    else do 
      putStrLn $ "Trying to open: \"" ++ whitelist opts ++ "\"..."
      f <- readFile $ whitelist opts
      let desiredMessages = lines f

      imcFile <- putStrLn "Reading IMC.xml..." >> readFile "IMC.xml"
      messagesFile <- putStrLn "Reading Messages_template.txt..." >> T.pack <$> readFile "Messages_template.txt"

      let tree = XML.onlyElems $ XML.parseXML imcFile
          root = filter (\x -> XML.QName "messages" Nothing Nothing == XML.elName x) tree

          messages = filter (\x -> "message" == XML.qName (XML.elName x)) $ XML.elChildren $ head root

          protoMessages = map extractMessages messages
          selectedMessages = filter (\m -> abbrev m `elem` desiredMessages) protoMessages

          [c, t, f, d] = transpose (map makeMessageData selectedMessages) -- Did it really accept this?
          constructors = T.intercalate "\n  | " c
          toFieldsInstances = T.intercalate "\n    " t
          fromFieldsInstances = T.intercalate "\n      " f
          dataDecls = T.intercalate "\n\n" d

          messageDict = Map.fromList [ (iid x, x) | x <- protoMessages]

      writeFile "Messages.hs" (T.unpack $ T.replace "%MESSAGES%" dataDecls (T.replace "%CONSTRUCTORS%" constructors (T.replace "%IMCPUTS%" toFieldsInstances (T.replace "%IMCGETS%" fromFieldsInstances messagesFile))))
      putStrLn "Finished writing Messages.hs..."
      putStrLn "\n\nMove these files to the source directory of the IMC module, replacing existing files."