{-# LANGUAGE QuasiQuotes #-}

module Domain.Validation ( validate
                         , passwordValidations
                         , emailValidations
                         , PasswordValidationError
                         , EmailValidationError 
                         ) where

import qualified Data.Text as Txt
import Text.Regex.PCRE.Heavy
import Data.Maybe

type Validation a e = a -> Maybe e

data PasswordValidationError = PasswordLengthError Int Int |
                               PasswordMissingUppercaseError |
                               PasswordMissingLowercaseError |
                               PasswordMissingNumberError
                               deriving (Eq)

instance Show PasswordValidationError where
    show (PasswordLengthError lowerBound upperBound) = "Password Must Be Between " ++ show lowerBound ++ " & " ++ show upperBound ++ " Characters!"
    show PasswordMissingLowercaseError = "Password Must Contain At Least One Lowercase Character!"
    show PasswordMissingUppercaseError = "Password Must Contain At Least One Uppercase Character!"
    show PasswordMissingNumberError = "Password Must Contain At Least One Number!"
    
data EmailValidationError = EmailValidationError deriving (Eq)

instance Show EmailValidationError where
    show _ = "Email Format Is Invalid!"


-- |A List Of Validation Functions For Ensuring That A Password Conforms To The Desired Format. A Password 
-- Should Be Between 5 & 35 Characters, & Contain A Lowercase Letter, An Uppercase Letter, & A Digit.
passwordValidations :: [Validation Txt.Text PasswordValidationError]
passwordValidations = [validatePasswordLength, passwordContainsLowerCase, passwordContainsUpperCase, passwordContainsNumber]

-- |A List Of Validation Functions For Ensuring That A Password Conforms To The Desired Format. A Password 
-- Should Be Between 5 & 35 Characters, & Contain A Lowercase Letter, An Uppercase Letter, & A Digit.
emailValidations :: [Validation Txt.Text EmailValidationError]
emailValidations = [validateEmail]


-- |Given Some Value, Checks A List Of Validation Functions And If All Should Pass,
-- Applies A Constructor To Said Value And Returns The Result. Otherwise, Returns
-- A List Of Error Messages.
validate :: (a -> b) -> [Validation a e] -> a -> Either [e] b
validate constructor validations val =
    case catMaybes $ validations <*> [val] of
        [] -> Right $ constructor val
        errors -> Left errors


-- |Validates That A Password Is The Correct Length
validatePasswordLength :: Validation Txt.Text PasswordValidationError
validatePasswordLength = lengthInRange 5 35 (PasswordLengthError 5 35)


-- |Validates That A Password Contains A Number
passwordContainsNumber :: Validation Txt.Text PasswordValidationError
passwordContainsNumber = matchRegex [re|\d|] PasswordMissingNumberError


-- |Validates That A Password Contains A Lowercase Letter
passwordContainsLowerCase :: Validation Txt.Text PasswordValidationError
passwordContainsLowerCase = matchRegex [re|[a-z]|] PasswordMissingLowercaseError


-- |Validates That A Password Contains An Uppercase Letter
passwordContainsUpperCase :: Validation Txt.Text PasswordValidationError
passwordContainsUpperCase = matchRegex [re|[A-Z]|] PasswordMissingUppercaseError


-- |Validates That An Email Is Of The Correct Format
validateEmail :: Validation Txt.Text EmailValidationError
validateEmail = matchRegex [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|] EmailValidationError


-- |Constructs A Validator Which Confirms That The Length Of Some Text Falls Within A Given Range
lengthInRange :: Int -> Int -> e -> Validation Txt.Text e
lengthInRange minLen maxLen errorMessage value =
    if Txt.length value >= minLen && Txt.length value <= maxLen then Nothing else Just errorMessage


-- |Constructs A Validator Which Confirms That Some Text Matches A Given Regex
matchRegex :: Regex -> e -> Validation Txt.Text e
matchRegex regex errorMessage value =
    if value =~ regex then Nothing else Just errorMessage
