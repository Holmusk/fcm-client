{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}


-- | Google Firebase Cloud Messaging model / JSON conversions.
--   https://firebase.google.com/docs/cloud-messaging/http-server-ref
--
--   Data types in this module map field for field in the google docs.
--   See FCMClient.Types module for some wrapper functions on top of raw JSON types.
--
module FCMClient.JSON.Types (
  FCMData
, FCMNotification
, fcmTitle
, fcmBody
, fcmIcon
, fcmSound
, fcmTag
, fcmColor
, fcmBadge
, fcmClickAction
, fcmBodyLocKey
, fcmBodyLocArgs
, fcmTitleLocKey
, fcmTitleLocArgs
, FCMMessage
, fcmTo
, fcmRegistrationIds
, fcmCondition
, fcmCollapseKey
, fcmPriority
, fcmContentAvailable
, fcmDelayWhileIdle
, fcmTimeToLive
, fcmRestrictedPackageName
, fcmDryRun
, fcmData
, fcmNotification
, FCMResult ( FCMResultSuccess
            , FCMResultError
            )
, _FCMResultSuccess
, _FCMResultError
, FCMClientError ( FCMErrorResponseInvalidJSON
                 , FCMErrorResponseInvalidAuth
                 , FCMServerError
                 , FCMClientJSONError
                 , FCMClientHTTPError
                 )
, fcmErrorMessage
, fcmErrorHttpStatus
, _FCMErrorResponseInvalidJSON
, _FCMErrorResponseInvalidAuth
, _FCMServerError
, _FCMClientJSONError
, _FCMClientHTTPError
, FCMResponseBody(..)
, FCMMessageResponse
, _FCMMessageResponse
, _FCMTopicResponse
, fcmCanonicalIds
, fcmFailure
, fcmMulticastId
, fcmResults
, fcmSuccess
, FCMMessageResponseResult(..)
, _FCMMessageResponseResultOk
, _FCMMessageResponseResultError
, FCMMessageResponseResultOk
, fcmMessageId
, fcmRegistrationId
, FCMTopicResponse(..)
, FCMTopicResponseOk
, _FCMTopicResponseOk
, _FCMTopicResponseError
, fcmTopicMessageId
, FCMError(..)
) where


import           Control.Applicative
import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map (Map)
import           Data.Text (Text)
import           Network.HTTP.Types (Status)


type FCMData = Map Text Text


-- | FCM Notification as defined in https://firebase.google.com/docs/cloud-messaging/http-server-ref#notification-payload-support
-- Abstract type, use lens API to access fields. Record fields are kept private and used for JSON conversion.
data FCMNotification =
  FCMNotification {

    -- | title  Optional, string
    -- Indicates notification title. This field is not visible on iOS phones and tablets.
    _fcmTitle :: !(Maybe Text)

    -- | body  Optional, string
    -- Indicates notification body text.
  , _fcmBody :: !(Maybe Text)

    -- | icon   Optional, string
    -- Android: Indicates notification icon. Sets value to myicon for drawable resource myicon.
  , _fcmIcon :: !(Maybe Text)

    -- | sound  Optional, string
    -- IOS: Indicates a sound to play when the device receives a notification. Sound
    -- files can be in the main bundle of the client app or in the
    -- Library/Sounds folder of the app's data container. See the iOS Developer
    -- Library for more information.
    -- Android: Indicates a sound to play when the device receives a
    -- notification. Supports default or the filename of a sound resource
    -- bundled in the app. Sound files must reside in /res/raw/.
  , _fcmSound :: !(Maybe Text)

    -- | tag   Optional, string
    -- Android: Indicates whether each notification results in a new entry in the
    -- notification drawer on Android.  If not set, each request creates a new
    -- notification. If set, and a notification with the same tag is already being
    -- shown, the new notification replaces the existing one in the notification
    -- drawer.
  , _fcmTag :: !(Maybe Text)

    -- | color   Optional, string
    -- Android: Indicates color of the icon, expressed in #rrggbb format
  , _fcmColor:: !(Maybe Text)

    -- | badge  Optional, string
    -- Indicates the badge on the client app home icon.
  , _fcmBadge :: !(Maybe Text)

    -- | click_action  Optional, string
    -- IOS: Indicates the action associated with a user click on the notification.
    -- Corresponds to category in the APNs payload.
    -- Android: Indicates the action associated with a user click on the
    -- notification. When this is set, an activity with a matching intent
    -- filter is launched when user clicks the notification.
  , _fcmClickAction :: !(Maybe Text)

    -- | body_loc_key  Optional, string
    -- IOS: Indicates the key to the body string for localization. Corresponds to
    -- "loc-key" in the APNs payload.
    -- Android: Indicates the key to the body string for localization. Use the
    -- key in the app's string resources when populating this value.
  , _fcmBodyLocKey :: !(Maybe Text)

    -- | body_loc_args  Optional, JSON array as string
    -- IOS: Indicates the string value to replace format specifiers in the body string
    -- for localization. Corresponds to "loc-args" in the APNs payload.
    -- Android: Indicates the string value to replace format specifiers in the
    -- body string for localization. For more information, see Formatting and
    -- Styling.
  , _fcmBodyLocArgs :: !(Maybe Text)

    -- | title_loc_key  Optional, string
    -- IOS: Indicates the key to the title string for localization. Corresponds to
    -- "title-loc-key" in the APNs payload.
    -- Android: Indicates the key to the title string for localization. Use the
    -- key in the app's string resources when populating this value.
  , _fcmTitleLocKey :: !(Maybe Text)


    -- | title_loc_args  Optional, JSON array as string
    -- IOS: Indicates the string value to replace format specifiers in the title
    -- string for localization.Corresponds to "title-loc-args" in the APNs
    -- payload.
    -- Android: Indicates the string value to replace format specifiers in the
    -- title string for localization. For more information, see Formatting
    -- strings.
  , _fcmTitleLocArgs:: !(Maybe Text)
  } deriving (Eq, Show)

$(makeLenses ''FCMNotification)
$(deriveJSON (aesonPrefix snakeCase) { omitNothingFields = True } ''FCMNotification)


instance Default FCMNotification where
  def = FCMNotification Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


-- | FCM Message as defined in https://firebase.google.com/docs/cloud-messaging/http-server-ref#send-downstream
-- Abstract type, use lens API to access fields. Record fields are kept private and used for JSON conversion.
data FCMMessage =
  FCMMessage {
    -- | to  Optional, string
    -- This parameter specifies the recipient of a message.
    -- The value must be a registration token, notification key, or topic.
    -- Do not set this field when sending to multiple topics. See condition.
    _fcmTo :: !(Maybe Text)

    -- | registration_ids String array
    -- This parameter specifies a list of devices (registration tokens, or IDs)
    -- receiving a multicast message. It must contain at least 1 and at most 1000
    -- registration tokens.
    -- Use this parameter only for multicast messaging, not for single
    -- recipients. Multicast messages (sending to more than 1 registration tokens) are
    -- allowed using HTTP JSON format only.
  , _fcmRegistrationIds :: !(Maybe (NonEmpty Text))

    -- | condition  Optional, string
    -- This parameter specifies a logical expression of conditions that determine
    -- the message target.  Supported condition: Topic, formatted as "'yourTopic' in
    -- topics". This value is case-insensitive.  Supported operators: &&, ||. Maximum
    -- two operators per topic message supported.
  , _fcmCondition :: !(Maybe Text)

    -- | collapse_key  Optional, string
    -- This parameter identifies a group of messages (e.g., with collapse_key:
    -- "Updates Available") that can be collapsed, so that only the last message gets
    -- sent when delivery can be resumed. This is intended to avoid sending too many
    -- of the same messages when the device comes back online or becomes active (see
    -- delay_while_idle).
    -- Note that there is no guarantee of the order in which messages get sent.
    -- Note: A maximum of 4 different collapse keys is allowed at any given time.
    -- This means a FCM connection server can simultaneously store 4 different
    -- send-to-sync messages per client app. If you exceed this number, there is no
    -- guarantee which 4 collapse keys the FCM connection server will keep.
  , _fcmCollapseKey :: !(Maybe Text)

    -- | priority  Optional, string
    -- Sets the priority of the message. Valid values are "normal" and "high." On
    -- iOS, these correspond to APNs priorities 5 and 10.  By default, messages are
    -- sent with normal priority. Normal priority optimizes the client app's
    -- battery consumption and should be used unless immediate delivery is
    -- required. For messages with normal priority, the app may receive the message
    -- with unspecified delay.  When a message is sent with high priority, it is
    -- sent immediately, and the app can wake a sleeping device and open a network
    -- connection to your server.  For more information, see Setting the priority
    -- of a message.
  , _fcmPriority :: !(Maybe Text)

    -- | content_available  Optional, JSON boolean
    -- On iOS, use this field to represent content-available in the APNS
    -- payload. When a notification or message is sent and this is set to true,
    -- an inactive client app is awoken. On Android, data messages wake the app
    -- by default. On Chrome, currently not supported.
  , _fcmContentAvailable :: !(Maybe Bool)

    -- | delay_while_idle  Optional, JSON boolean
    -- When this parameter is set to true, it indicates that the message should
    -- not be sent until the device becomes active. The default value is false.
  , _fcmDelayWhileIdle :: !(Maybe Bool)

    -- | time_to_live  Optional, JSON number
    -- This parameter specifies how long (in seconds) the message should be kept in
    -- FCM storage if the device is offline. The maximum time to live supported is
    -- 4 weeks, and the default value is 4 weeks. For more information, see Setting
    -- the lifespan of a message.
  , _fcmTimeToLive :: !(Maybe Word)

    -- | restricted_package_name  Optional, string
    -- This parameter specifies the package name of the application where the
    -- registration tokens must match in order to receive the message.
  , _fcmRestrictedPackageName :: !(Maybe Text)

    -- | dry_run  Optional, JSON boolean
    -- This parameter, when set to true, allows developers to test a request
    -- without actually sending a message. The default value is false.
  , _fcmDryRun :: !(Maybe Bool)

  -- Payload

    -- | data  Optional, JSON object
    --  This parameter specifies the custom key-value pairs of the message's
    --  payload.  For example, with data:{"score":"3x1"}: On iOS, if the message is
    --  sent via APNS, it represents the custom data fields. If it is sent via FCM
    --  connection server, it would be represented as key value dictionary in
    --  AppDelegate application:didReceiveRemoteNotification:.  On Android, this
    --  would result in an intent extra named score with the string value 3x1.  The
    --  key should not be a reserved word ("from" or any word starting with
    --  "google" or "gcm"). Do not use any of the words defined in this table (such
    --  as collapse_key).  Values in string types are recommended. You have to
    --  convert values in objects or other non-string data types (e.g., integers or
    --  booleans) to string.
  , _fcmData :: !(Maybe FCMData)

    -- | notification  Optional, JSON object
    -- This parameter specifies the predefined, user-visible key-value pairs of
    -- the notification payload. See Notification payload support for detail.
    -- For more information about notification message and data message
    -- options, see Payload.
  , _fcmNotification :: !(Maybe FCMNotification)
  } deriving (Eq, Show)


$(makeLenses ''FCMMessage)
$(deriveJSON (aesonPrefix snakeCase) { omitNothingFields = True } ''FCMMessage)


instance Default FCMMessage where
  def = FCMMessage Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


-- | String specifying the error that occurred when processing the message
-- for the recipient. The possible values can be found in table 9.
-- https://firebase.google.com/docs/cloud-messaging/http-server-ref#table9
data FCMError =
    FCMErrorDeviceMessageRate
  | FCMErrorInternalServerError
  | FCMErrorInvalidDataKey
  | FCMErrorInvalidPackageName
  | FCMErrorInvalidRegistration
  | FCMErrorInvalidTtl
  | FCMErrorMessageTooBig
  | FCMErrorMismatchSenderId
  | FCMErrorMissingRegistration
  | FCMErrorNotRegistered
  | FCMErrorTopicsMessageRate
  | FCMErrorUnavailable
  | FCMErrorOther Text -- ^ used if we can't parse any of the above
  deriving (Eq, Show)

instance ToJSON FCMError where
  toJSON FCMErrorDeviceMessageRate   = object [ ("error", "DeviceMessageRate") ]
  toJSON FCMErrorInternalServerError = object [ ("error", "InternalServerError") ]
  toJSON FCMErrorInvalidDataKey      = object [ ("error", "InvalidDataKey") ]
  toJSON FCMErrorInvalidPackageName  = object [ ("error", "InvalidPackageName") ]
  toJSON FCMErrorInvalidRegistration = object [ ("error", "InvalidRegistration") ]
  toJSON FCMErrorInvalidTtl          = object [ ("error", "InvalidTtl") ]
  toJSON FCMErrorMessageTooBig       = object [ ("error", "MessageTooBig") ]
  toJSON FCMErrorMismatchSenderId    = object [ ("error", "MismatchSenderId") ]
  toJSON FCMErrorMissingRegistration = object [ ("error", "MissingRegistration") ]
  toJSON FCMErrorNotRegistered       = object [ ("error", "NotRegistered") ]
  toJSON FCMErrorTopicsMessageRate   = object [ ("error", "TopicsMessageRate") ]
  toJSON FCMErrorUnavailable         = object [ ("error", "Unavailable") ]
  toJSON (FCMErrorOther e)           = object [ ("error", toJSON e) ]


instance FromJSON FCMError where
  parseJSON (Object v) = fmap ( \(n :: Text) ->
                                case n
                                  of "DeviceMessageRate"   -> FCMErrorDeviceMessageRate
                                     "InternalServerError" -> FCMErrorInternalServerError
                                     "InvalidDataKey"      -> FCMErrorInvalidDataKey
                                     "InvalidPackageName"  -> FCMErrorInvalidPackageName
                                     "InvalidRegistration" -> FCMErrorInvalidRegistration
                                     "InvalidTtl"          -> FCMErrorInvalidTtl
                                     "MessageTooBig"       -> FCMErrorMessageTooBig
                                     "MismatchSenderId"    -> FCMErrorMismatchSenderId
                                     "MissingRegistration" -> FCMErrorMissingRegistration
                                     "NotRegistered"       -> FCMErrorNotRegistered
                                     "TopicsMessageRate"   -> FCMErrorTopicsMessageRate
                                     "Unavailable"         -> FCMErrorUnavailable
                                     e                     -> FCMErrorOther e
                              ) (v .: "error")

  parseJSON x = typeMismatch "Object" x



data FCMMessageResponseResultOk =
  FCMMessageResponseResultOkPayload {
    -- | String specifying a unique ID for each successfully processed message.
    _fcmMessageId :: !Text

    -- | Optional string specifying the canonical registration token for the
    -- client app that the message was processed and sent to. Sender should use
    -- this value as the registration token for future requests. Otherwise, the
    -- messages might be rejected.
  , _fcmRegistrationId :: !(Maybe Text)
  } deriving (Eq, Show)

$(makeLenses ''FCMMessageResponseResultOk)
$(deriveJSON (aesonPrefix snakeCase) { omitNothingFields = True } ''FCMMessageResponseResultOk)

instance Default FCMMessageResponseResultOk where
  def = FCMMessageResponseResultOkPayload "" Nothing

data FCMMessageResponseResult =
    FCMMessageResponseResultOk !FCMMessageResponseResultOk
  | FCMMessageResponseResultError !FCMError
  deriving (Eq, Show)

$(makePrisms ''FCMMessageResponseResult)

instance ToJSON FCMMessageResponseResult where
  toJSON (FCMMessageResponseResultOk    b) = toJSON b
  toJSON (FCMMessageResponseResultError e) = toJSON e
  toEncoding (FCMMessageResponseResultOk    b) = toEncoding b
  toEncoding (FCMMessageResponseResultError e) = toEncoding e

instance FromJSON FCMMessageResponseResult where
  parseJSON o =  (FCMMessageResponseResultOk <$> parseJSON o)
             <|> (FCMMessageResponseResultError <$> parseJSON o)


data FCMMessageResponse =
  FCMMessageResponsePayload {
    -- | Required, number  Unique ID (number) identifying the multicast message.
    _fcmMulticastId :: !Integer

    -- | Required, number  Number of messages that were processed without an error.
  , _fcmSuccess :: !Integer

    -- | Required, number  Number of messages that could not be processed.
  , _fcmFailure :: !Integer

    -- | Required, number  Number of results that contain a canonical
    -- registration token. See the registration overview for more discussion of
    -- this topic.
  , _fcmCanonicalIds :: !Integer

    -- | Optional, array object  Array of objects representing the status of
    -- the messages processed. The objects are listed in the same order as the
    -- request (i.e., for each registration ID in the request, its result is
    -- listed in the same index in the response).
  , _fcmResults :: !(Maybe (NonEmpty FCMMessageResponseResult))
  } deriving (Eq, Show)

$(makeLenses ''FCMMessageResponse)
$(deriveJSON (aesonPrefix snakeCase) { omitNothingFields = True } ''FCMMessageResponse)

instance Default FCMMessageResponse where
  def = FCMMessageResponsePayload 0 0 0 0 Nothing

newtype FCMTopicResponseOk =
  FCMTopicResponseOkPayload {
    -- | Optional, number  The topic message ID when FCM has successfully
    -- received the request and will attempt to deliver to all subscribed
    -- devices.
    _fcmTopicMessageId :: Integer
  } deriving (Eq, Show)

$(makeLenses ''FCMTopicResponseOk )
$(deriveJSON (aesonDrop 9 snakeCase) { omitNothingFields = True } ''FCMTopicResponseOk)

instance Default FCMTopicResponseOk where
  def = FCMTopicResponseOkPayload 0

data FCMTopicResponse =
    FCMTopicResponseOk !FCMTopicResponseOk
  | FCMTopicResponseError !FCMError
  deriving (Eq, Show)

$(makePrisms ''FCMTopicResponse)


instance ToJSON FCMTopicResponse where
  toJSON (FCMTopicResponseOk    o) = toJSON o
  toJSON (FCMTopicResponseError e) = toJSON e

  toEncoding (FCMTopicResponseOk    o) = toEncoding o
  toEncoding (FCMTopicResponseError e) = toEncoding e

instance FromJSON FCMTopicResponse where
  parseJSON o =  (FCMTopicResponseOk <$> parseJSON o)
             <|> (FCMTopicResponseError <$> parseJSON o)


data FCMResponseBody = FCMMessageResponse !FCMMessageResponse
                     | FCMTopicResponse !FCMTopicResponse
                     deriving (Eq, Show)

$(makePrisms ''FCMResponseBody)

instance ToJSON FCMResponseBody where
  toJSON (FCMMessageResponse m) = toJSON m
  toJSON (FCMTopicResponse t) = toJSON t

  toEncoding (FCMMessageResponse m) = toEncoding m
  toEncoding (FCMTopicResponse t) = toEncoding t

instance FromJSON FCMResponseBody where
  parseJSON o =  (FCMMessageResponse <$> parseJSON o)
             <|> (FCMTopicResponse <$> parseJSON o)


-- | Types of FCM errors.
data FCMClientError =
    -- | Indicates that the request could not be parsed as JSON, or it
    -- contained invalid fields (for instance, passing a string where a number
    -- was expected). The exact failure reason is described in the response and
    -- the problem should be addressed before the request can be retried.
    FCMErrorResponseInvalidJSON { _fcmErrorMessage :: !Text
                                }

    -- | There was an error authenticating the sender account.
  | FCMErrorResponseInvalidAuth

    -- | Errors in the 500-599 range (such as 500 or 503) indicate that there
    -- was an internal error in the FCM connection server while trying to
    -- process the request, or that the server is temporarily unavailable (for
    -- example, because of timeouts). Sender must retry later, honoring any
    -- Retry-After header included in the response. Application servers must
    -- implement exponential back-off.
  | FCMServerError              { _fcmErrorHttpStatus :: !Status
                                , _fcmErrorMessage :: !Text
                                }

    -- | Client couldn't parse JSON response from server.
  | FCMClientJSONError          { _fcmErrorMessage :: !Text
                                }

    -- | Unexpected HTTP response or some other HTTP error.
  | FCMClientHTTPError          { _fcmErrorMessage :: !Text
                                }
  deriving (Show)

$(makeLenses ''FCMClientError)
$(makePrisms ''FCMClientError)


-- | Result of an RPC call.
--
-- Successful response doesn't imply all the messages were delivered,
-- e.g. some may need to be re-sent if a rate limit was exceeded.
--
-- Error cases enumerate all, client and server error conditions.
--
data FCMResult =
    -- | Successful response (http 200).
    -- Doesn't imply all the messages were delivered,
    -- response body may contain error codes.
    FCMResultSuccess  !FCMResponseBody

    -- | Didn't receive JSON response, there were an error of some kind.
  | FCMResultError !FCMClientError
  deriving (Show)

$(makePrisms ''FCMResult)
