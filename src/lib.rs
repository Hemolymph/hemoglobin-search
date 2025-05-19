#[warn(clippy::pedantic)]
#[warn(clippy::nursery)]
pub mod fuzzy;
pub mod query_parser;
use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use fuzzy::weighted_compare;
use hemoglobin::{
    cards::{
        CardId, Keyword, KeywordData,
        kins::KinComparison,
        properties::{Array, Number, Read, Text},
    },
    clean_ascii,
    numbers::{Comparison, ImpreciseOrd, compare::Ternary},
};
use regex::Regex;

/// Errors that might happen during searching
#[derive(Debug)]
pub enum Errors {
    NonRegexable(String),
    InvalidOr,
    InvalidComparisonString,
    UnknownSubQueryParam(String),
    UnknownStringParam(String),
    InvalidOrdering(String),
    InvalidPolarity,
    NotSortable,
    UnclosedSubquery,
    UnclosedString,
    UnclosedRegex,
    RegexErr(regex::Error),
    AttemptedEmptyParamName,
}

/// Represents a search query
#[derive(Debug, Clone)]
pub struct Query {
    pub name: String,
    pub restrictions: Vec<QueryRestriction>,
    pub sort: Sort,
}

impl Query {
    fn from_restrictions(restrictions: Vec<QueryRestriction>) -> Self {
        let mut name = String::new();

        for restriction in &restrictions {
            if let QueryRestriction::Fuzzy(a) = restriction {
                name += a;
                name += " ";
            }
        }

        let sort = if name.is_empty() {
            Sort::Alphabet(Text::Name, Ordering::Ascending)
        } else {
            Sort::Fuzzy
        };

        Self {
            name: name.trim().to_string(),
            restrictions,
            sort,
        }
    }
    fn triviality(&self) -> Triviality {
        if !self.name.is_empty() {
            return Triviality::NonTrivial;
        }

        if self.restrictions.is_empty() {
            return Triviality::Tautology;
        }

        let mut triviality = Triviality::Tautology;

        for restriction in &self.restrictions {
            match restriction.triviality() {
                Triviality::NonTrivial => return Triviality::NonTrivial,
                Triviality::Contradiction => triviality = Triviality::Contradiction,
                Triviality::Tautology => (),
            }
        }

        triviality
    }
    fn keyword_is_match<'card, 'cardvec, 'query, T, I>(
        &'query self,
        keyword: &'card Keyword,
        cards: &'cardvec I,
        cache: &'query mut Cache<&'card T>,
    ) -> bool
    where
        T: Read + 'card + Clone,
        &'card T: Read,
        I: Iterator<Item = &'card T> + Clone,
    {
        if keyword.name == "devours" {
            if let Some(KeywordData::CardId(ref devoured_id)) = keyword.data {
                return matches_query(devoured_id.as_ref(), self, cards, cache).is_true();
            }
        }

        false
    }
}

impl Display for Query {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut text_properties = vec![];
        for restriction in &self.restrictions {
            text_properties.push(format!("{restriction}"));
        }
        match &self.sort {
            Sort::None => (),
            Sort::Fuzzy if !self.name.is_empty() => {
                text_properties.push("sorted by fuzzy match".to_string());
            }
            Sort::Fuzzy => text_properties.push("sorted by Name in Ascending order".to_owned()),
            Sort::Alphabet(property, order) => {
                text_properties.push(format!("sorted by {property} in {order} order"));
            }
            Sort::Numeric(property, order) => {
                text_properties.push(format!("sorted by {property} in {order} order"));
            }
        }

        let text_properties = text_properties.into_iter().reduce(|mut acc, el| {
            write!(&mut acc, ", {el}").unwrap();
            acc
        });
        match text_properties {
            Some(text_properties) => write!(f, "Cards {text_properties}"),
            None => write!(f, "Cards"),
        }
    }
}

#[allow(clippy::match_wildcard_for_single_variants)]
impl Display for QueryRestriction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DevouredBy(devourer) => {
                write!(f, "which are devoured by [{devourer}]")
            }
            Self::Fuzzy(text) => write!(f, "with \"{text}\" written on them"),
            Self::Devours(devourees, _) => write!(f, "that devour [{devourees}]"),
            Self::NumberComparison(property, comparison) => {
                write!(f, "with {property} {comparison}")
            }
            Self::TextComparison(property, text) => match text {
                TextComparison::Contains(text) => write!(f, "whose {property} contains \"{text}\""),
                TextComparison::HasMatch(regex) => write!(f, "whose {property} matches /{regex}/"),
                TextComparison::EqualTo(text) => {
                    write!(f, "whose {property} is equal to \"{text}\"")
                }
            },
            Self::Has(property, text) => match property {
                Array::Functions => match text {
                    TextComparison::Contains(text) => {
                        write!(f, "which can be used to \"{text}\"")
                    }
                    TextComparison::EqualTo(text) => write!(f, "which can be used to \"{text}\""),
                    TextComparison::HasMatch(regex) => write!(f, "which can be used to /{regex}/"),
                },
            },
            Self::HasKw(keyword) => match keyword {
                TextComparison::Contains(text) => {
                    write!(f, "with a \"{text}\" keyword")
                }
                TextComparison::EqualTo(text) => {
                    write!(f, "with exactly a \"{text}\" keyword")
                }
                TextComparison::HasMatch(regex) => {
                    write!(f, "with a /{regex}/ keyword")
                }
            },
            Self::Not(query) => write!(f, "that aren't [{query}]"),
            Self::LenientNot(query) => write!(
                f,
                "that aren't [{query}], counting lacks of a property as a non-match"
            ),
            Self::Group(query) => write!(f, "which are [{query}]"),
            Self::Or(a, b) => write!(f, "which are [{a}] or [{b}]"),
            Self::Xor(a, b) => write!(f, "which are [{a}] xor [{b}] but not both"),
            Self::KinComparison(comparison) => match comparison {
                KinComparison::Equal(kin) => write!(f, "whose kin is exactly {kin}"),
                KinComparison::Similar(kin) => write!(f, "whose kin is {kin}"),
                KinComparison::TextContains(kin) => write!(f, "whose kin is \"{kin}\""),
                KinComparison::TextEqual(kin) => write!(f, "whose kin is exactly \"{kin}\""),
                KinComparison::RegexMatch(regex) => write!(f, "whose kin matches /{regex}/"),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum TextComparison {
    Contains(String),
    EqualTo(String),
    HasMatch(Regex),
}

impl TextComparison {
    pub fn is_match(&self, other: &'_ str) -> bool {
        let clean_other = clean_ascii(other);
        match self {
            TextComparison::Contains(self_text) => clean_other.contains(&clean_ascii(self_text)),
            TextComparison::EqualTo(self_text) => clean_ascii(self_text) == clean_other,
            TextComparison::HasMatch(regex) => regex.is_match(&clean_other),
        }
    }
}

/// Represents a specific restriction that a `Query` will apply to cards.
#[derive(Debug, Clone)]
pub enum QueryRestriction {
    Fuzzy(String),
    Devours(Query, ComparisonKind),
    DevouredBy(Query),
    NumberComparison(Number, Comparison),
    TextComparison(Text, TextComparison),
    Has(Array, TextComparison),
    KinComparison(KinComparison),
    HasKw(TextComparison),
    Not(Query),
    LenientNot(Query),
    Group(Query),
    Or(Query, Query),
    Xor(Query, Query),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Triviality {
    NonTrivial,
    Contradiction,
    Tautology,
}

impl QueryRestriction {
    fn triviality(&self) -> Triviality {
        match self {
            QueryRestriction::NumberComparison(_, Comparison::LowerThan(0)) => {
                Triviality::Contradiction
            }
            QueryRestriction::NumberComparison(Number::Cost, Comparison::GreaterThanOrEqual(0)) => {
                Triviality::Tautology
            }
            QueryRestriction::Not(query) => match query.triviality() {
                Triviality::Tautology => Triviality::Contradiction,
                Triviality::Contradiction => Triviality::Tautology,
                Triviality::NonTrivial => Triviality::NonTrivial,
            },
            QueryRestriction::Group(query) => query.triviality(),
            QueryRestriction::Or(query, query1) => {
                match (query.triviality(), query1.triviality()) {
                    (Triviality::NonTrivial, _) => Triviality::NonTrivial,
                    (_, Triviality::NonTrivial) => Triviality::NonTrivial,
                    (Triviality::Contradiction, Triviality::Contradiction) => {
                        Triviality::Contradiction
                    }
                    (Triviality::Contradiction, Triviality::Tautology) => Triviality::Tautology,
                    (Triviality::Tautology, Triviality::Contradiction) => Triviality::Tautology,
                    (Triviality::Tautology, Triviality::Tautology) => Triviality::Tautology,
                }
            }
            QueryRestriction::Xor(query, query1) => {
                match (query.triviality(), query1.triviality()) {
                    (Triviality::NonTrivial, _) => Triviality::NonTrivial,
                    (_, Triviality::NonTrivial) => Triviality::NonTrivial,
                    (Triviality::Contradiction, Triviality::Contradiction) => {
                        Triviality::Contradiction
                    }
                    (Triviality::Contradiction, Triviality::Tautology) => Triviality::Tautology,
                    (Triviality::Tautology, Triviality::Contradiction) => Triviality::Tautology,
                    (Triviality::Tautology, Triviality::Tautology) => Triviality::Contradiction,
                }
            }
            _ => Triviality::NonTrivial,
        }
    }
    fn is_match<'card, 'cardvec, 'query, C, T, I>(
        &'query self,
        card: &'card C,
        query: &'query Query,
        cards: &'cardvec I,
        cache: &'query mut Cache<&'card T>,
    ) -> Ternary
    where
        C: Read,
        T: Read + 'card + Clone,
        &'card T: Read,
        I: Iterator<Item = &'card T> + Clone,
    {
        match self {
            QueryRestriction::TextComparison(property, comparison) => card
                .get_text_property(property)
                .map_or(Ternary::Void, |property| {
                    comparison.is_match(&property).into()
                }),
            QueryRestriction::Xor(group1, group2) => {
                let res1 = matches_query(card, group1, cards, cache);
                let res2 = matches_query(card, group2, cards, cache);
                res1.xor(res2)
            }
            QueryRestriction::Or(group1, group2) => {
                let res1 = matches_query(card, group1, cards, cache);
                let res2 = matches_query(card, group2, cards, cache);
                res1.or(res2)
            }
            QueryRestriction::Group(group) => matches_query(card, group, cards, cache),
            QueryRestriction::Fuzzy(x) => fuzzy(card, x).into(),

            QueryRestriction::NumberComparison(field, comparison) => {
                comparison.compare(&card.get_num_property(field))
            }
            QueryRestriction::Has(property, comparison) => card
                .get_vec_property(property)
                .map_or(Ternary::Void, |field| {
                    field.iter().any(|x| comparison.is_match(x)).into()
                }),
            QueryRestriction::HasKw(comparison) => {
                card.get_keywords().map_or(Ternary::Void, |keyword| {
                    keyword.iter().any(|x| comparison.is_match(&x.name)).into()
                })
            }
            QueryRestriction::Not(query) => !matches_query(card, query, cards, cache),
            QueryRestriction::LenientNot(query) => {
                matches_query(card, query, cards, cache).is_true().into()
            }
            QueryRestriction::Devours(devours, comparison) => match comparison {
                ComparisonKind::Contains => devours_match(card, cards, devours, cache),
                ComparisonKind::Equals => card.get_keywords().map_or(Ternary::Void, |keyword| {
                    keyword
                        .iter()
                        .any(|keyword| devours.keyword_is_match(keyword, cards, cache))
                        .into()
                }),
            },
            QueryRestriction::DevouredBy(devoured_by) => {
                devouredby_match(card, cards, query, devoured_by, cache)
            }
            QueryRestriction::KinComparison(comparison) => card
                .get_kin()
                .map_or(Ternary::Void, |kin| comparison.is_match(*kin).into()),
        }
    }
}

fn devouredby_match<'card, 'cardvec, 'query, T, I, C>(
    card: &'card C,
    cards: &'cardvec I,
    query: &'query Query,
    devoured_by: &'query Query,
    cache: &'query mut Cache<&'card T>,
) -> Ternary
where
    C: Read,
    T: Read + 'card + Clone,
    &'card T: Read,
    I: Iterator<Item = &'card T> + Clone,
{
    let key = format!("{devoured_by}");
    let devoured_cards;
    let maybe_devourees = cache.devouredby.get(&key).cloned();
    if let Some(value) = maybe_devourees {
        devoured_cards = value;
    } else {
        let devourers = cards
            .clone()
            .filter(|card| matches_query(*card, devoured_by, cards, cache).is_true());

        let mut no_devourers = true;
        let mut queries: Vec<Query> = vec![];

        for devourer in devourers {
            if let Some(Keyword {
                name: _,
                data: Some(KeywordData::CardId(card_id)),
            }) = devourer
                .get_keywords()
                .and_then(|x| x.iter().find(|x| x.name == "devours"))
            {
                no_devourers = false;
                queries.push(Query {
                    name: String::new(),
                    restrictions: card_id.get_as_query(),
                    sort: Sort::None,
                });
            }
        }

        if no_devourers {
            devoured_cards = vec![];
            cache.devouredby.insert(key, vec![]);
        } else {
            let devourees_query = queries
                .into_iter()
                .reduce(|first, second| Query {
                    name: String::new(),
                    restrictions: vec![QueryRestriction::Or(first, second)],
                    sort: Sort::None,
                })
                .unwrap_or(Query {
                    name: String::new(),
                    restrictions: vec![],
                    sort: Sort::None,
                });

            let devourees_query = Query {
                name: query.name.clone(),
                restrictions: devourees_query.restrictions,
                sort: query.sort,
            };

            devoured_cards = search(&devourees_query, cards.clone());
            cache.devouredby.insert(key, devoured_cards.clone());
        }
    }
    devoured_cards
        .iter()
        .any(|x| x.get_name() == card.get_name())
        .into()
}

fn devours_match<'card, 'cardvec, 'query, T, I, C>(
    card: &'card C,
    cards: &'cardvec I,
    devourees: &'query Query,
    cache: &'query mut Cache<&'card T>,
) -> Ternary
where
    C: Read,
    T: Read + 'card + Clone,
    &'card T: Read,
    I: Iterator<Item = &'card T> + Clone,
{
    let devourees = cache
        .devourers
        .entry(devourees.to_string())
        .or_insert(search(devourees, cards.clone()));

    if let Some(Keyword {
        name: _,
        data: Some(KeywordData::CardId(card_id)),
    }) = card
        .get_keywords()
        .and_then(|x| x.iter().find(|x| x.name == "devours"))
    {
        let query = Query::from_restrictions(card_id.get_as_query());
        let specific_devourees = search(&query, cards.clone());
        if specific_devourees
            .into_iter()
            .any(|x| devourees.iter().any(|y| x.get_name() == y.get_name()))
        {
            return Ternary::True;
        }
    }

    Ternary::False
}

/// Represents a specific ordering for sorting.
#[derive(Debug, Clone, Copy)]
pub enum Ordering {
    Ascending,
    Descending,
}

impl Display for Ordering {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ascending => write!(f, "Ascending"),
            Self::Descending => write!(f, "Descending"),
        }
    }
}

/// Specific ways to sort cards.
#[derive(Debug, Clone, Copy)]
pub enum Sort {
    /// Do not sort
    None,
    /// Sort by how much they match a string
    Fuzzy,
    Alphabet(Text, Ordering),
    Numeric(Number, Ordering),
}

/// Restriction that matches only if a card contains some text
#[must_use]
pub fn fuzzy(card: &impl Read, query: &str) -> bool {
    card.get_description()
        .is_some_and(|x| clean_ascii(&x.to_string()).contains(&clean_ascii(query)))
        || card
            .get_name()
            .is_some_and(|x| clean_ascii(x).contains(&clean_ascii(query)))
        || card
            .get_type()
            .is_some_and(|x| clean_ascii(x).contains(&clean_ascii(query)))
        || card
            .get_kin()
            .is_some_and(|x| clean_ascii(x.get_name()).contains(&clean_ascii(query)))
        || card.get_keywords().is_some_and(|x| {
            x.iter()
                .any(|x| clean_ascii(&x.name).contains(&clean_ascii(query)))
        })
}

/// The Cache for `devouredby` queries.
pub type CachePart<T> = HashMap<String, Vec<T>>;

#[derive(Default)]
struct Cache<T> {
    devouredby: CachePart<T>,
    devourers: CachePart<T>,
}

/// Function that takes `cards` and outputs a vector pointing to all the cards that matched the `query`.
#[must_use]
pub fn search<'card, 'query, C, I>(query: &'query Query, cards: I) -> Vec<&'card C>
where
    C: Read + Clone + 'card,
    I: IntoIterator<Item = &'card C> + Clone + 'query,
    I::IntoIter: Clone,
    &'card C: Read,
{
    match query.triviality() {
        Triviality::NonTrivial => (),
        Triviality::Contradiction => return vec![],
        Triviality::Tautology => return cards.into_iter().collect(),
    }

    let iter = cards.into_iter();
    let iter_clone = iter.clone();
    let mut cache = Cache {
        devouredby: HashMap::new(),
        devourers: HashMap::new(),
    };
    let mut results: Vec<&C> = iter
        .filter(|card| matches_query(*card, query, &iter_clone, &mut cache) == Ternary::True)
        .collect();

    match &query.sort {
        Sort::None => (),
        Sort::Fuzzy if !query.name.is_empty() => results.sort_by(|a, b| {
            weighted_compare(b, &query.name)
                .partial_cmp(&weighted_compare(a, &query.name))
                .unwrap_or(std::cmp::Ordering::Equal)
        }),
        Sort::Fuzzy => results.sort_by(|a, b| Ord::cmp(&a.get_name(), &b.get_name())),
        Sort::Alphabet(property, Ordering::Ascending) => results.sort_by(|a, b| {
            Ord::cmp(
                &a.get_text_property(property),
                &b.get_text_property(property),
            )
        }),
        Sort::Numeric(property, Ordering::Ascending) => results.sort_by(|a, b| {
            ImpreciseOrd::imprecise_cmp(
                &a.get_num_property(property),
                &b.get_num_property(property),
            )
        }),
        Sort::Alphabet(property, Ordering::Descending) => {
            results.sort_by(|a, b| {
                Ord::cmp(
                    &a.get_text_property(property),
                    &b.get_text_property(property),
                )
                .reverse()
            });
        }
        Sort::Numeric(property, Ordering::Descending) => results.sort_by(|a, b| {
            ImpreciseOrd::imprecise_cmp(
                &a.get_num_property(property),
                &b.get_num_property(property),
            )
            .reverse()
        }),
    }

    results
}

/// Checks whether a `card` matches a specific `query`'s restrictions.
///
/// Since `devouredby` queries always require two searches, the results of the first search are stored in a `cache` that is internally mutable. This cache is only ever mutated the first time each devouredby query is executed.
/// The sum total of available `cards` is passed in order to perform searches. This function clones these cards, so this value should be an Iterator.
#[allow(clippy::too_many_lines)]
fn matches_query<'card, 'cardvec, 'query, C, T, I>(
    card: &'card C,
    query: &'query Query,
    cards: &'cardvec I,
    cache: &'query mut Cache<&'card T>,
) -> Ternary
where
    C: Read,
    T: Read + 'card + Clone,
    &'card T: Read,
    I: Iterator<Item = &'card T> + Clone,
{
    let mut filtered = Ternary::True;
    for res in &query.restrictions {
        match res.triviality() {
            Triviality::NonTrivial => {
                filtered = filtered.and(res.is_match(card, query, cards, cache))
            }
            Triviality::Contradiction => filtered = filtered.and(Ternary::False),
            Triviality::Tautology => filtered = filtered.and(Ternary::True),
        }
    }
    filtered
}

pub trait QueryFrom {
    // #[must_use]
    // Creates a vector of `QueryRestriction`s.
    fn get_as_query(&self) -> Vec<QueryRestriction>;
}

impl QueryFrom for CardId {
    fn get_as_query(&self) -> Vec<QueryRestriction> {
        let mut restrictions = vec![];

        if let Some(name) = &self.name {
            restrictions.push(QueryRestriction::TextComparison(
                Text::Name,
                TextComparison::EqualTo(name.clone()),
            ));
        }

        if let Some(kin) = &self.kin {
            restrictions.push(QueryRestriction::KinComparison(KinComparison::Equal(*kin)));
        }

        if let Some(keywords) = &self.keywords {
            for keyword in keywords {
                let keyword = TextComparison::EqualTo(keyword.name.clone());
                restrictions.push(QueryRestriction::HasKw(keyword));
            }
        }

        if let Some(cost) = &self.cost {
            restrictions.push(QueryRestriction::NumberComparison(
                Number::Cost,
                cost.as_comparison(),
            ));
        }

        if let Some(health) = &self.health {
            restrictions.push(QueryRestriction::NumberComparison(
                Number::Health,
                health.as_comparison(),
            ));
        }

        if let Some(power) = &self.power {
            restrictions.push(QueryRestriction::NumberComparison(
                Number::Power,
                power.as_comparison(),
            ));
        }

        if let Some(defense) = &self.defense {
            restrictions.push(QueryRestriction::NumberComparison(
                Number::Defense,
                defense.as_comparison(),
            ));
        }

        restrictions
    }
}

#[cfg(test)]
mod test {
    use crate::*;
    use hemoglobin::cards::Card;
    use query_parser::parse_query;

    #[test]
    fn test_equals_search() {
        let cards: Vec<Card> = serde_json::from_str(
            &std::fs::read_to_string("tests/search.json").expect("Couldn't load search.json"),
        )
        .expect("Couldn't convert search.json to a vec of cards");
        for val in 4..=6 {
            let result = search(
                &parse_query(&format!("c={val}")).expect("couldn't parse query"),
                cards.iter(),
            );

            let fail = match val {
                4 => result
                    .iter()
                    .any(|x| x.name == "eq 5" || x.name == "gteq 5" || x.name == "gt 5"),
                5 => result
                    .iter()
                    .any(|x| x.name == "lt 5" || x.name == "gt 5" || x.name == "neq 5"),
                6 => result
                    .iter()
                    .any(|x| x.name == "lt 5" || x.name == "lteq 5" || x.name == "eq 5"),
                _ => unreachable!(),
            };

            assert!(!fail);
        }
    }

    #[test]
    fn test_gt_search() {
        let cards: Vec<Card> = serde_json::from_str(
            &std::fs::read_to_string("tests/search.json").expect("Couldn't load search.json"),
        )
        .expect("Couldn't convert search.json to a vec of cards");
        for val in 4..=6 {
            let result = search(
                &parse_query(&format!("c>{val}")).expect("couldn't parse query"),
                cards.iter(),
            );

            let fail = match val {
                4 => result.iter().any(|x| x.name == "lt 5"),
                5 => result
                    .iter()
                    .any(|x| x.name == "lt 5" || x.name == "lteq 5" || x.name == "eq 5"),
                6 => result
                    .iter()
                    .any(|x| x.name == "lt 5" || x.name == "lteq 5" || x.name == "eq 5"),
                _ => unreachable!(),
            };

            assert!(!fail);
        }
    }

    #[test]
    fn test_gteq_search() {
        let cards: Vec<Card> = serde_json::from_str(
            &std::fs::read_to_string("tests/search.json").expect("Couldn't load search.json"),
        )
        .expect("Couldn't convert search.json to a vec of cards");
        for val in 4..=6 {
            let result = search(
                &parse_query(&format!("c>={val}")).expect("couldn't parse query"),
                cards.iter(),
            );

            let fail = match val {
                4 => false,
                5 => result.iter().any(|x| x.name == "lt 5"),
                6 => result
                    .iter()
                    .any(|x| x.name == "lt 5" || x.name == "lteq 5" || x.name == "eq 5"),
                _ => unreachable!(),
            };

            assert!(!fail);
        }
    }
}

#[derive(Debug, Clone)]
pub enum ComparisonKind {
    Contains,
    Equals,
}
