module CliArgs
       ( CliArgs (..)
       , CliCmd (..)
       , CliJsonBatchArgs (..)
       , runWithArgs
       ) where

import Control.Lens
import Data.Default.Class
import Data.List.NonEmpty (nonEmpty)
import Data.Monoid ((<>))
import FCMClient.Types
import Options.Applicative
import System.Environment

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import qualified FCMClient.JSON.Types as J

data CliArgs = CliArgs { cliAuthKey :: String
                       , cliCmd     :: CliCmd
                       }

data CliJsonBatchArgs = CliJsonBatchArgs { cliBatchInput  :: Maybe FilePath
                                         , cliBatchOutput :: Maybe FilePath
                                         , cliBatchConc   :: Int
                                         }

data CliCmd = CliCmdSendJsonBatch CliJsonBatchArgs
            | CliCmdSendMessage FCMMessage


parseArgs :: Maybe String -- ^ default auth key from shell env
          -> Parser CliArgs
parseArgs maybeAuthKey = CliArgs
  <$> strOption
      ( long "auth-key"
     <> short 'k'
     <> ( case maybeAuthKey
            of Nothing -> mempty
               Just ak -> value ak
        )
     <> help "Auth key, defaults to FCM_AUTH_KEY environmental variable." )
  <*> subparser ( command "message" (info (helper <*> parseCliCmdSendMessage)
                                          (progDesc "Send test message."))

               <> command "batch" (info  (helper <*> (CliCmdSendJsonBatch <$> parseCliJsonBatchArgs))
                                         (progDesc "Send message batch."))
                )


parseCliJsonBatchArgs :: Parser CliJsonBatchArgs
parseCliJsonBatchArgs = CliJsonBatchArgs
  <$> optional ( strOption
        ( long "input"
       <> short 'i'
       <> help "Batch input file, one JSON object per line (or STDIN)."))
  <*> optional ( strOption
        ( long "output"
       <> short 'o'
       <> help "Batch input file (or STDOUT)."))
  <*> option (auto >>= (\c -> if c < 1 then error "Concurrency must be >= 1" else return c))
        ( long "concurrency"
       <> short 'c'
       <> value 1
       <> showDefaultWith show
       <> help "How many HTTP requests to run in concurrently.")


parseCliCmdSendMessage :: Parser CliCmd
parseCliCmdSendMessage = CliCmdSendMessage <$>
    (
        ( fmap (set fcmTo)
                (optionalText
                  ( long "to"
                 <> short 't'
                 <> help "message to (reg token, notification key or topic)"))
        <|> fmap (set fcmRegistrationIds)
                (textList
                  ( long "registration-id"
                 <> short 'r'
                 <> help "Registration token or ID, up to 1000, for multicast messaging."))
        <|> fmap (set fcmCondition)
                (optionalText
                  ( long "condition"
                 <> short 'c'
                 <> help "The message target. Supported condition: Topic, formatted as \"'yourTopic' in topics\". This value is case-insensitive.  Supported operators: &&, ||. Maximum two operators per topic message supported."))
         )

      <..> fmap (set fcmCollapseKey)
                (optionalText
                  ( long "collapse-key"
                 <> help "Identifies a group of messages that can be collapsed, so that only the last message gets sent when delivery can be resumed."))

      <..> fmap (set J.fcmPriority)
                (optionalText
                  ( long "priority"
                 <> help "Sets the priority of the message. Valid values are 'normal' and 'high'. On iOS, these correspond to APNs priorities 5 and 10."))

      <..> fmap (set fcmContentAvailable)
                (switch
                  ( long "content-available"
                 <> help "On iOS, use this field to represent content-available in the APNS payload. When a notification or message is sent and this is set to true, an inactive client app is awoken. On Android, data messages wake the app by default. On Chrome, currently not supported."))

      <..> fmap (set fcmDelayWhileIdle)
                (switch
                  ( long "delay-while-idle"
                 <> help "When this parameter is set to true, it indicates that the message should not be sent until the device becomes active. The default value is false."))

      <..> fmap (set fcmTimeToLive)
                (optional $ option auto
                  ( long "time-to-live"
                 <> help "This parameter specifies how long (in seconds) the message should be kept in FCM storage if the device is offline. The maximum time to live supported is 4 weeks, and the default value is 4 weeks. For more information, see Setting the lifespan of a message."))

      <..> fmap (set fcmRestrictedPackageName)
                (optionalText
                  ( long "restricted-package-name"
                 <> help "This parameter specifies the package name of the application where the registration tokens must match in order to receive the message."))

      <..> fmap (set fcmDryRun)
                (switch
                  ( long "dry-run"
                 <> help "This parameter, when set to true, allows developers to test a request without actually sending a message. The default value is false."))

      <..> fmap (set fcmData . J.decode . LB.pack)
                (strOption
                  ( long "data"
                 <> short 'd'
                 <> help "This parameter specifies the custom key-value pairs of the message's payload.  For example, with data:{\"score\":\"3x1\"}: On iOS, if the message is sent via APNS, it represents the custom data fields. If it is sent via FCM connection server, it would be represented as key value dictionary in AppDelegate application:didReceiveRemoteNotification:.  On Android, this would result in an intent extra named score with the string value 3x1.  The key should not be a reserved word (\"from\" or any word starting with \"google\" or \"gcm\"). Do not use any of the words defined in this table (such as collapse_key). ONLY values in string types are supported. You have to convert values in objects or other non-string data types (e.g., integers or booleans) to string."))

      <..> fmap (set $ fcmWithNotification . fcmTitle)
                (optionalText
                  ( long "title"
                 <> short 'T'
                 <> help "notification title text"))

      <..> fmap (set $ fcmWithNotification . fcmBody)
                (optionalText
                  ( long "body"
                 <> short 'b'
                 <> help "notification body text"))

      <..> fmap (set $ fcmWithNotification . fcmAndroidChannelId)
                (optionalText
                  ( long "android-channel-id"
                 <> help "Android: The notification's channel id (new in Android O).  The app must create a channel with this channel ID before any notification with this channel ID is received.  If you don't send this channel ID in the request, or if the channel ID provided has not yet been created by the app, FCM uses the channel ID specified in the app manifest."))

      <..> fmap (set $ fcmWithNotification . fcmIcon)
                (optionalText
                  ( long "icon"
                 <> help "Indicates notification icon."))

      <..> fmap (set $ fcmWithNotification . fcmSound)
                (optionalText
                  ( long "sound"
                 <> help "IOS: Indicates a sound to play when the device receives a notification. Sound files can be in the main bundle of the client app or in the Library/Sounds folder of the app's data container. See the iOS Developer Library for more information.  Android: Indicates a sound to play when the device receives a notification. Supports default or the filename of a sound resource bundled in the app. Sound files must reside in /res/raw/.  ."))

      <..> fmap (set $ fcmWithNotification . fcmTag)
                (optionalText
                  ( long "tag"
                 <> help "Android: Indicates whether each notification results in a new entry in the notification drawer on Android.  If not set, each request creates a new notification. If set, and a notification with the same tag is already being shown, the new notification replaces the existing one in the notification drawer."))

      <..> fmap (set $ fcmWithNotification . J.fcmColor)
                (optionalText
                  ( long "color"
                 <> help "Android: Indicates color of the icon, expressed in #rrggbb format"))

      <..> fmap (set $ fcmWithNotification . fcmBadge)
                (optionalText
                  ( long "badge"
                 <> help "Indicates the badge on the client app home icon."))

      <..> fmap (set $ fcmWithNotification . fcmClickAction)
                (optionalText
                  ( long "click-action"
                 <> help "IOS: Indicates the action associated with a user click on the notification. Corresponds to category in the APNs payload.  Android: Indicates the action associated with a user click on the notification. When this is set, an activity with a matching intent filter is launched when user clicks the notification."))

      <..> fmap (set $ fcmWithNotification . fcmBodyLocKey)
                (optionalText
                  ( long "body-loc-key"
                 <> help "IOS: Indicates the key to the body string for localization. Corresponds to \"loc-key\" in the APNs payload.  Android: Indicates the key to the body string for localization. Use the key in the app's string resources when populating this value."))

      <..> fmap (set $ fcmWithNotification . J.fcmBodyLocArgs)
                (optionalText
                  ( long "body-loc-args"
                 <> help "IOS: Indicates the string value to replace format specifiers in the body string for localization. Corresponds to \"loc-args\" in the APNs payload.  Android: Indicates the string value to replace format specifiers in the body string for localization. For more information, see Formatting and Styling."))

      <..> fmap (set $ fcmWithNotification . J.fcmTitleLocKey)
                (optionalText
                  ( long "title-loc-key"
                 <> help "IOS: Indicates the key to the title string for localization. Corresponds to \"title-loc-key\" in the APNs payload.  Android: Indicates the key to the title string for localization. Use the key in the app's string resources when populating this value."))

      <..> fmap (set $ fcmWithNotification . J.fcmTitleLocArgs)
                (optionalText
                  ( long "title-loc-args"
                 <> help "IOS: Indicates the string value to replace format specifiers in the title string for localization.Corresponds to \"title-loc-args\" in the APNs payload.  Android: Indicates the string value to replace format specifiers in the title string for localization. For more information, see Formatting strings."))
       <*> pure def
    )

  where optionalText opt = fmap (fmap T.pack) $ optional $ strOption opt
        textList opt = fmap nonEmpty $ many $ T.pack <$> strOption opt


-- | Chains functions through Applicative
(<..>) :: (Applicative f)
       => f (b -> c)
       -> f (a -> b)
       -> f (a -> c)
(<..>) bc ab = pure (.) <*> bc <*> ab


runWithArgs:: (CliArgs -> IO ())
           -> IO ()
runWithArgs rwa = do
  maybeAuthKey <- lookupEnv "FCM_AUTH_KEY"

  let opts = info (helper <*> parseArgs maybeAuthKey) ( fullDesc
               <> header "Simple FCM CLI client, send a test message or a JSON batch from a file."
               <> progDesc ("E.g. " <> " fcm-client -k AUTH_KEY message --title Test --to /topics/mytopic --color '#FF0000' -d test")
               )

  execParser opts >>= rwa
