{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Either
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text as DT
import qualified Data.Text.IO as T

type Password = DT.Text

data PasswordPolicy = PasswordPolicy {
                        passwordPolicyMin :: Int,
                        passwordPolicyMax :: Int,
                        char :: Char } deriving (Show, Eq)

data PasswordRecord = PasswordRecord {
                        passwordPolicy :: PasswordPolicy,
                        password :: Password } deriving (Show, Eq)

main = T.interact
        (DT.pack .
        (++ "\n") .
        show .
        length .
        (Prelude.filter passwordP) .
        (Prelude.map parsePasswordRecord) .
        DT.lines)

-- 2a
-- checkPassword :: PasswordRecord -> Bool
-- checkPassword (PasswordRecord PasswordPolicy {passwordPolicyMin = mn, passwordPolicyMax = mx, char = c} password) = 
--   let found = DT.length $ DT.filter (== c) password
--   in mn <= found && found <= mx

checkPassword :: PasswordRecord -> Bool
checkPassword (PasswordRecord PasswordPolicy {passwordPolicyMin = mn, passwordPolicyMax = mx, char = c} password) = 
  case ((DT.index password (mn-1)) == c, (DT.index password (mx-1)) == c) of
    (True,  False) -> True
    (False, True)  -> True
    (_    , _   )  -> False

passwordP :: Either String PasswordRecord -> Bool
passwordP (Right pr) = checkPassword pr
passwordP _ = False

parsePasswordRecord :: DT.Text -> Either String PasswordRecord
parsePasswordRecord = AT.parseOnly (
  (\mn mx char password -> PasswordRecord (PasswordPolicy mn mx char) password) <$>
  AT.decimal <*
  AT.char '-' <*>
  AT.decimal <*
  AT.char ' ' <*>
  AT.anyChar <*
  AT.string ": " <*>
  AT.takeText)

