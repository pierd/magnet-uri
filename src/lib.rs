extern crate serde_urlencoded;

use serde_urlencoded::de::Error as UrlEncodeError;
use std::fmt;
use std::str::FromStr;

const SCHEME: &'static str = "magnet:?";

pub(crate) mod field_name {
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
        self.get_field_value(Field::name)
    }

    pub fn name(&self) -> Option<&str> {
        None // FIXME
    }

    pub fn dn(&self) -> Option<&str> {
        self.name()
    }

    pub fn info_hashes(&self) -> Vec<&BTInfoHash> {
        Vec::new() // FIXME
    }

    pub fn info_hash(&self) -> Option<&BTInfoHash> {
        None // FIXME
    }

    fn get_field_value<'a, F, T>(&'a self, f: F) -> Vec<T>
    where
        F: Fn(&'a Field) -> Option<T>,
    {
        self.fields
            .iter()
            .map(f)
            .filter(Option::is_some)
            .map(Option::unwrap)
            .collect()
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
    Length(usize),
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
            LENGTH => match usize::from_str(val) {
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
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "")
    }
}

type TTHHash = String;
type SHA1Hash = String;
type BTInfoHash = String;

#[derive(Debug, PartialEq)]
pub enum Topic {
    TigerTreeHash(TTHHash),
    SHA1(SHA1Hash),
    BitPrint(SHA1Hash, TTHHash),
    ED2K(String),
    MD5(String),
    BitTorrentInfoHash(BTInfoHash),
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

#[derive(Debug, Default)]
pub struct Builder {
    fields: Vec<Field>,
}

impl Builder {
    pub fn with_field(mut self, f: Field) -> Self {
        self.fields.push(f);
        self
    }

    pub fn with_name(self, name: &str) -> Self {
        self.with_field(Field::Name(name.to_owned()))
    }

    pub fn with_topic(self, xt: Topic) -> Self {
        self.with_field(Field::Topic(xt))
    }

    pub fn with_extension(self, ext_name: &str, val: &str) -> Self {
        self.with_field(Field::Extension(ext_name.to_owned(), val.to_owned()))
    }

    pub fn with_info_hash(self, bith: BTInfoHash) -> Self {
        self.with_topic(Topic::BitTorrentInfoHash(bith))
    }

    pub fn build(self) -> MagnetURI {
        MagnetURI {
            fields: self.fields,
        }
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
