{-# LANGUAGE OverloadedStrings #-}
module SendEmail where

import Network.Mail.SMTP

emailAddressDest = Address Nothing "8574078840@txt.att.net"
emailFrom = Address Nothing "smsdirections08@gmail.com"

sendInstructions body = sendMailWithLogin
                        "smtp.gmail.com"
                        "smsdirections08@gmail.com"
                        "directme"
                        (createMail body)

createMail body = simpleMail emailFrom [emailAddressDest] [] [] "" [(plainTextPart body)]

