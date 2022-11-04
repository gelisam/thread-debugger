module History where


type Key
  = Maybe String

type Entry
  = (Key, String)

type History
  = [Entry]

parseEntry
  :: String
  -> Entry
parseEntry s
  = case words s of
      x1:x2:xs
        -> (Just x1, unwords (x2:xs))
      _
        -> (Nothing, s)

parseHistory
  :: [String]
  -> History
parseHistory
  = map parseEntry
