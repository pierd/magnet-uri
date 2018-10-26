extern crate serde_urlencoded;

use serde_urlencoded::de::Error as UrlEncodeError;
use std::fmt;
use std::str::FromStr;

const SCHEME: &'static str = "magnet:?";

pub(self) mod field_name {
    pub const NAME: &'static str = "dn";
    pub const LENGTH: &'static str = "xl";
    pub const TOPIC: &'static str = "xt";
    pub const ACCEPTABLE_SOURCE: &'static str = "as";
    pub const EXACT_SOURCE: &'static str = "xs";
    pub const KEYWORD: &'static str = "kt";
    pub const MANIFEST: &'static str = "mt";
    pub const ADDRESS_TRACKER: &'static str = "tr";
    pub const EXTENSION_PREFIX: &'static str = "x.";
}

#[derive(Debug)]
pub enum Error {
    Scheme,
    UrlEncode(UrlEncodeError),
    Field(String, String),
    ExactTopic(String),
}

impl Error {
    fn with_field(key: &str, val: &str) -> Self {
        Error::Field(key.to_owned(), val.to_owned())
    }
}

/// A struct holding fields stored in a Magnet URI
#[derive(Debug, Default)]
pub struct MagnetURI {
    fields: Vec<Field>,
}

impl MagnetURI {
    pub fn has_extensions(&self) -> bool {
        self.fields.iter().any(Field::is_extension)
    }

    pub fn has_unknown_fields(&self) -> bool {
        self.fields.iter().any(Field::is_unknown)
    }

    pub fn is_strictly_valid(&self) -> bool {
        // TODO: no unknown fields, one length, no duplicate non matching hashes
        true
    }

    pub fn names(&self) -> Vec<&str> {
        self.iter_field_values(Field::name).collect()
    }

    pub fn name(&self) -> Option<&str> {
        self.iter_field_values(Field::name).next()
    }

    pub fn dn(&self) -> Option<&str> {
        self.name()
    }

    pub fn length(&self) -> Option<u64> {
        self.iter_field_values(Field::length).next()
    }

    pub fn info_hashes(&self) -> Vec<&BTInfoHash> {
        self.iter_field_values(Field::info_hash).collect()
    }

    pub fn info_hash(&self) -> Option<&BTInfoHash> {
        self.iter_field_values(Field::info_hash).next()
    }

    fn iter_field_values<'a, F, T>(&'a self, f: F) -> impl Iterator<Item=T> + 'a
    where
        F: Fn(&'a Field) -> Option<T> + Sized + 'a,
        T: 'a,
    {
        self.fields
            .iter()
            .map(f)
            .filter(Option::is_some)
            .map(Option::unwrap)
    }

    pub fn add_field(&mut self, f: Field) -> &Self {
        self.fields.push(f);
        self
    }

    pub fn add_name(&mut self, name: &str) -> &Self {
        self.add_field(Field::Name(name.to_owned()))
    }

    pub fn add_topic(&mut self, xt: Topic) -> &Self {
        self.add_field(Field::Topic(xt))
    }

    pub fn add_extension(&mut self, ext_name: &str, val: &str) -> &Self {
        self.add_field(Field::Extension(ext_name.to_owned(), val.to_owned()))
    }

    pub fn set_name(&mut self, name: &str) -> &Self {
        self.set_unique_field(|f| f.name().is_none(), Field::Name(name.to_owned()))
    }

    pub fn set_info_hash(&mut self, bith: BTInfoHash) -> &Self {
        self.set_unique_field(|f| {
            match f {
                Field::Topic(Topic::BitTorrentInfoHash(_)) => false,
                _ => true,
            }
        }, Field::Topic(Topic::BitTorrentInfoHash(bith)))
    }

    fn set_unique_field<F>(&mut self, retain_filter: F, field: Field) -> &Self
    where
        F: FnMut(&Field) -> bool
    {
        self.fields.retain(retain_filter);
        self.add_field(field)
    }
}

impl FromStr for MagnetURI {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.starts_with(SCHEME) {
            return Err(Error::Scheme);
        }
        let (_, qs) = s.split_at(SCHEME.len());
        let parse_result: Result<Vec<(String, String)>, _> = serde_urlencoded::from_str(qs);
        match parse_result {
            Err(e) => Err(Error::UrlEncode(e)),
            Ok(parts) => Ok(MagnetURI {
                fields: parts
                    .iter()
                    .map(|(k, v)| Field::from_str(k, v))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
        }
    }
}

impl fmt::Display for MagnetURI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", SCHEME)
    }
}

#[derive(Debug, PartialEq)]
pub enum Field {
    Name(String),
    Length(u64),
    Topic(Topic),
    AcceptableSource(String),
    ExactSource(String),
    Keyword(String),
    Manifest(String),
    AddressTracker(String),
    Extension(String, String),
    Unknown(String, String),
}

impl Field {
    fn from_str(key: &str, val: &str) -> Result<Self, Error> {
        use field_name::*;
        use Field::*;

        match key {
            NAME => Ok(Name(val.to_owned())),
            LENGTH => match u64::from_str(val) {
                Err(_) => Err(Error::with_field(key, val)),
                Ok(l) => Ok(Length(l)),
            },
            TOPIC => Ok(Topic(self::Topic::from_str(val)?)),
            ACCEPTABLE_SOURCE => Ok(AcceptableSource(val.to_owned())),
            EXACT_SOURCE => Ok(ExactSource(val.to_owned())),
            KEYWORD => Ok(Keyword(val.to_owned())),
            MANIFEST => Ok(Manifest(val.to_owned())),
            ADDRESS_TRACKER => Ok(AddressTracker(val.to_owned())),
            _ => if key.starts_with(EXTENSION_PREFIX) {
                let (_, ext_name) = key.split_at(EXTENSION_PREFIX.len());
                Ok(Extension(ext_name.to_owned(), val.to_owned()))
            } else {
                Ok(Unknown(key.to_owned(), val.to_owned()))
            },
        }
    }

    fn is_extension(&self) -> bool {
        match self {
            Field::Extension(_, _) => true,
            _ => false,
        }
    }

    fn is_unknown(&self) -> bool {
        match self {
            Field::Unknown(_, _) => true,
            _ => false,
        }
    }

    fn name(&self) -> Option<&str> {
        match self {
            Field::Name(ref name) => Some(name),
            _ => None,
        }
    }

    fn length(&self) -> Option<u64> {
        match self {
            Field::Length(len) => Some(*len),
            _ => None,
        }
    }

    fn info_hash(&self) -> Option<&BTInfoHash> {
        match self {
            Field::Topic(Topic::BitTorrentInfoHash(ref hash)) => Some(hash),
            _ => None,
        }
    }
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "")
    }
}

type TTHHash = String;
type SHA1Hash = String;
type ED2KHash = String;
type AICHHash = String;
type KazaaHash = String;
type BTInfoHash = String;
type MD5Hash = String;

#[derive(Debug, PartialEq)]
pub enum Topic {
    /// urn:tree:tiger:TTHHash
    TigerTreeHash(TTHHash),
    /// urn:sha1:SHA1Hash
    SHA1(SHA1Hash),
    /// urn:bitprint:SHA1Hash.TTHHash
    BitPrint(SHA1Hash, TTHHash),
    /// urn:ed2k:ED2KHash
    ED2K(ED2KHash),
    /// urn:aich:AICHHash
    AdvancedIntelligentCorruptionHandler(AICHHash),
    /// urn:kzhash:KazaaHash
    Kazaa(KazaaHash),
    /// urn:bith:BTInfoHash
    BitTorrentInfoHash(BTInfoHash),
    /// urn:md5:MD5Hash
    MD5(MD5Hash),
}

impl FromStr for Topic {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Err(Error::ExactTopic(s.to_owned()))
    }
}

impl fmt::Display for Topic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke() {
        // https://en.wikipedia.org/wiki/Magnet_URI_scheme
        MagnetURI::from_str("magnet:?xt=urn:ed2k:31D6CFE0D16AE931B73C59D7E0C089C0&xl=0&dn=zero_len.fil&xt=urn:bitprint:3I42H3S6NNFQ2MSVX7XZKYAYSCX5QBYJ.LWPNACQDBZRYXW3VHJVCJ64QBZNGHOHHHZWCLNQ&xt=urn:md5:D41D8CD98F00B204E9800998ECF8427E").unwrap();
        panic!();
    }
}
