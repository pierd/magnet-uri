extern crate serde_urlencoded;

use serde_urlencoded::de::Error as UrlEncodeError;
use std::fmt;
use std::str::FromStr;

const SCHEME: &'static str = "magnet:?";

pub(crate) mod field_name {
    pub const NAME: &'static str = "dn";
    pub const EXACT_LENGTH: &'static str = "xl";
    pub const EXACT_TOPIC: &'static str = "xt";
    pub const ACCEPTABLE_SOURCE: &'static str = "as";
    pub const EXACT_SOURCE: &'static str = "xs";
    pub const KEYWORDS: &'static str = "kt";
    pub const MANIFEST_TOPIC: &'static str = "mt";
    pub const ADDRESS_TRACKER: &'static str = "tr";
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
    pub fn names(&self) -> Vec<&str> {
        self.get_field_value(Field::name)
    }

    fn get_field_value<'a, F, T>(&'a self, f: F) -> Vec<T>
    where F: Fn(&'a Field) -> Option<T>
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
    ExactLength(usize),
    ExactTopic(ExactTopic),
    AcceptableSource(String),
    ExactSource(String),
    Keywords(String),
    ManifestTopic(String),
    AddressTracker(String),
}

impl Field {
    fn from_str(key: &str, val: &str) -> Result<Self, Error> {
        use field_name::*;
        use Field::*;

        if key.starts_with(NAME) {
            Ok(Name(val.to_owned()))
        } else if key.starts_with(EXACT_LENGTH) {
            match usize::from_str(val) {
                Err(_) => Err(Error::with_field(key, val)),
                Ok(l) => Ok(ExactLength(l)),
            }
        } else if key.starts_with(EXACT_TOPIC) {
            Ok(ExactTopic(self::ExactTopic::from_str(val)?))
        } else if key.starts_with(ACCEPTABLE_SOURCE) {
            Ok(AcceptableSource(val.to_owned()))
        } else if key.starts_with(EXACT_SOURCE) {
            Ok(ExactSource(val.to_owned()))
        } else if key.starts_with(KEYWORDS) {
            Ok(Keywords(val.to_owned()))
        } else if key.starts_with(MANIFEST_TOPIC) {
            Ok(ManifestTopic(val.to_owned()))
        } else if key.starts_with(ADDRESS_TRACKER) {
            Ok(AddressTracker(val.to_owned()))
        } else {
            Err(Error::with_field(key, val))
        }
    }

    fn name(&self) -> Option<&str> {
        if let Field::Name(ref name) = self {
            Some(name)
        } else {
            None
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

#[derive(Debug, PartialEq)]
pub enum ExactTopic {
    TigerTreeHash(TTHHash),
    SHA1(SHA1Hash),
    BitPrint(SHA1Hash, TTHHash),
    ED2K(String),
    MD5(String),
}

impl FromStr for ExactTopic {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Err(Error::ExactTopic(s.to_owned()))
    }
}

impl fmt::Display for ExactTopic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn smoke() {
        // https://en.wikipedia.org/wiki/Magnet_URI_scheme
        MagnetURI::from_str("magnet:?xt=urn:ed2k:31D6CFE0D16AE931B73C59D7E0C089C0&xl=0&dn=zero_len.fil&xt=urn:bitprint:3I42H3S6NNFQ2MSVX7XZKYAYSCX5QBYJ.LWPNACQDBZRYXW3VHJVCJ64QBZNGHOHHHZWCLNQ&xt=urn:md5:D41D8CD98F00B204E9800998ECF8427E").unwrap();
        panic!();
    }
}
