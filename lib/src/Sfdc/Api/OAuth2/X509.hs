module Sfdc.Api.OAuth2.X509
  ( generateX509
  , encodeSignedCertificate
  )
where

import Control.Monad.Except
import Crypto.Hash.Algorithms
import Crypto.PubKey.RSA as RSA
import Crypto.PubKey.RSA.PKCS15 as RSAPKCS15
import Crypto.Random.Types (MonadRandom)
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.OID
import Data.ASN1.Types
import Data.ByteString (ByteString)
import Data.String
import Data.X509
import Time.Types (DateTime)


-- | Generate an X.509 signed certificate and public key
generateX509 :: (MonadRandom m)
             => String -- ^ Issuer Distinguished Name Common Name
             -> DateTime -- ^ Valid beginning
             -> DateTime -- ^ Valid ending
             -> String -- ^ Subject Distinguished Name Common Name
             -> ExceptT Error m (SignedCertificate, PrivateKey)
generateX509 issuer begin_dt end_dt subject= do
  (pub_key, priv_key) <- lift $ RSA.generate 256 0x10001
  let sig_alg = SignatureALG HashSHA256 PubKeyALG_RSA
      cert = Certificate
        { certVersion = 0 -- v1
        , certSerial = 1
        , certSignatureAlg = sig_alg
        , certIssuerDN = DistinguishedName [(getObjectID DnCommonName, fromString issuer)]
        , certValidity = (begin_dt, end_dt)
        , certSubjectDN = DistinguishedName [(getObjectID DnCommonName, fromString subject)]
        , certPubKey = PubKeyRSA pub_key
        , certExtensions = Extensions Nothing
        }

  signed_cert <- objectToSignedExactF
                  (fmap (\x -> (x, sig_alg))
                    . ExceptT
                    . RSAPKCS15.signSafer (Just SHA256) priv_key)
                  cert

  return (signed_cert, priv_key)

-- | Encode a 'SignedCertificate' so it can be uploaded to SFDC
encodeSignedCertificate :: SignedCertificate -> ByteString
encodeSignedCertificate = encodeSignedObject