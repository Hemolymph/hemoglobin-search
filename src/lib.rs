pub mod fuzzy;
pub mod query_parser;
use std::{
    cell::RefCell,
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
            Self::Devours(devourees) => write!(f, "that devour [{devourees}]"),
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
                // property => match text {
                //     TextComparison::Contains(text) => {
                //         write!(f, "whose {property} have \"{text}\" among them")
                //     }
                //     TextComparison::EqualTo(text) => {
                //         write!(f, "whose {property} have exactly \"{text}\" among them")
                //     }
                //     TextComparison::HasMatch(regex) => {
                //         write!(f, "whose {property} have /{regex}/ among them")
                //     }
                // },
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

/// Represents a specific restriction that a `Query` will apply to cards.
#[derive(Debug, Clone)]
pub enum QueryRestriction {
    Fuzzy(String),
    Devours(Query),
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
pub type Cache<T> = RefCell<HashMap<String, Vec<T>>>;

/// Function that takes `cards` and outputs a vector pointing to all the cards that matched the `query`.
#[must_use]
pub fn search<'a, 'b, C, I>(query: &Query, cards: I) -> Vec<&'a C>
where
    C: Read + Clone + 'a,
    I: IntoIterator<Item = &'a C> + Clone + 'b,
    &'a C: Read,
{
    let cards_clone = cards.clone();
    let cache = Cache::new(HashMap::new());
    let mut results: Vec<&C> = cards
        .into_iter()
        .filter(|card| matches_query(card, query, &cards_clone, &cache) == Ternary::True)
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
/// Since `devouredby` queries always require two searches, the results of the first search are stored in a `cache` that is internally mutable. This cache is only ever mutated the first time a devouredby query is executed.
/// The sum total of available `cards` is passed in order to perform searches. This function clones these cards, so this value should be an Iterator.
#[allow(clippy::too_many_lines)]
pub fn matches_query<'a, 'b, C, T, I>(
    card: &C,
    query: &Query,
    cards: &I,
    cache: &Cache<&'a T>,
) -> Ternary
where
    C: Read,
    T: Read + 'a + Clone,
    &'a T: Read,
    I: IntoIterator<Item = &'a T> + Clone,
{
    let mut filtered = Ternary::True;
    for res in &query.restrictions {
        match res {
            QueryRestriction::TextComparison(property, comparison) => match comparison {
                TextComparison::EqualTo(text) => {
                    let matches =
                        card.get_text_property(property)
                            .map_or(Ternary::Void, |property| {
                                if clean_ascii(&property) == clean_ascii(text) {
                                    Ternary::True
                                } else {
                                    Ternary::False
                                }
                            });

                    filtered = filtered.and(matches);
                }
                TextComparison::Contains(contains) => {
                    let matches =
                        card.get_text_property(property)
                            .map_or(Ternary::Void, |property| {
                                if clean_ascii(&property).contains(&clean_ascii(contains)) {
                                    Ternary::True
                                } else {
                                    Ternary::False
                                }
                            });
                    filtered = filtered.and(matches);
                }
                TextComparison::HasMatch(regex) => {
                    let matches = {
                        card.get_text_property(property)
                            .map_or(Ternary::Void, |value| {
                                if regex.is_match(&value.to_lowercase()) {
                                    Ternary::True
                                } else {
                                    Ternary::False
                                }
                            })
                    };
                    filtered = filtered.and(matches);
                }
            },
            QueryRestriction::Xor(group1, group2) => {
                let res1 = matches_query(card, group1, cards, cache);
                let res2 = matches_query(card, group2, cards, cache);
                filtered = filtered.and(res1.xor(res2));
            }
            QueryRestriction::Or(group1, group2) => {
                filtered = filtered.and(
                    matches_query(card, group1, cards, cache)
                        .or(matches_query(card, group2, cards, cache)),
                );
            }
            QueryRestriction::Group(group) => {
                filtered = filtered.and(matches_query(card, group, cards, cache));
            }
            QueryRestriction::Fuzzy(x) => {
                filtered = filtered.and(if fuzzy(card, x) {
                    Ternary::True
                } else {
                    Ternary::False
                });
            }
            QueryRestriction::NumberComparison(field, comparison) => {
                filtered = filtered.and(comparison.compare(&card.get_num_property(field)));
            }
            QueryRestriction::Has(field, thing) => {
                let matches = match thing {
                    TextComparison::Contains(thing) => {
                        match_in_vec(card.get_vec_property(field), |text| {
                            text.to_lowercase().contains(&thing.to_lowercase())
                        })
                    }
                    TextComparison::EqualTo(thing) => {
                        match_in_vec(card.get_vec_property(field), |text| {
                            text.to_lowercase() == thing.to_lowercase()
                        })
                    }
                    TextComparison::HasMatch(regex) => {
                        match_in_vec(card.get_vec_property(field), |text| {
                            regex.is_match(&text.to_lowercase())
                        })
                    }
                };
                filtered = filtered.and(matches);
            }
            QueryRestriction::HasKw(thing) => {
                let matches = match thing {
                    TextComparison::Contains(thing) => {
                        match_in_vec(card.get_keywords(), |keyword| {
                            keyword.name.to_lowercase().contains(&thing.to_lowercase())
                        })
                    }
                    TextComparison::EqualTo(thing) => {
                        match_in_vec(card.get_keywords(), |keyword| {
                            keyword.name.to_lowercase() == thing.to_lowercase()
                        })
                    }
                    TextComparison::HasMatch(regex) => {
                        match_in_vec(card.get_keywords(), |keyword| {
                            regex.is_match(&keyword.name.to_lowercase())
                        })
                    }
                };
                filtered = filtered.and(matches);
            }
            QueryRestriction::Not(queryres) => {
                filtered = filtered.and(!matches_query(card, queryres, cards, cache));
            }
            QueryRestriction::LenientNot(queryres) => {
                filtered = filtered.and(
                    if matches_query(card, queryres, cards, cache) == Ternary::True {
                        Ternary::False
                    } else {
                        Ternary::True
                    },
                );
            }
            QueryRestriction::Devours(query) => {
                let matches = match_in_vec(card.get_keywords(), |keyword| {
                    if keyword.name == "devours" {
                        if let Some(KeywordData::CardId(ref devoured_id)) = keyword.data {
                            matches_query(devoured_id.as_ref(), query, cards, cache)
                                == Ternary::True
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                });
                filtered = filtered.and(matches);
            }
            QueryRestriction::DevouredBy(devoured_by) => {
                let key = format!("{devoured_by}");
                let devoured_cards;
                let maybe_devourees = RefCell::borrow(cache).get(&key).cloned();
                if let Some(value) = maybe_devourees {
                    devoured_cards = value;
                } else {
                    let cloned_cards = cards.clone();
                    let devourers: Vec<&T> = cards
                        .clone()
                        .into_iter()
                        .filter(|card| {
                            matches_query(card, devoured_by, &cloned_cards, cache) == Ternary::True
                        })
                        .collect();

                    let mut queries: Vec<Query> = vec![];

                    for devourer in devourers {
                        if let Some(Keyword {
                            name: _,
                            data: Some(KeywordData::CardId(card_id)),
                        }) = devourer
                            .get_keywords()
                            .and_then(|x| x.iter().find(|x| x.name == "devours"))
                        {
                            queries.push(Query {
                                name: String::new(),
                                restrictions: card_id.get_as_query(),
                                sort: Sort::None,
                            });
                        }
                    }

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

                    devoured_cards = search(&devourees_query, cloned_cards);
                    cache.borrow_mut().insert(key, devoured_cards.clone());
                }
                if devoured_cards
                    .iter()
                    .any(|x| x.get_name() == card.get_name())
                {
                    filtered = filtered.and(Ternary::True);
                } else {
                    filtered = filtered.and(Ternary::False);
                }
            }
            QueryRestriction::KinComparison(comparison) => {
                let filter = card
                    .get_kin()
                    .map_or(Ternary::Void, |kin| comparison.is_match(*kin).into());
                filtered = filtered.and(filter);
            }
        }
    }
    filtered
}

/// Returns whether any part of an optional `vec` fulfills a `cond`ition.
pub fn match_in_vec<T>(vec: Option<&[T]>, cond: impl Fn(&T) -> bool) -> Ternary {
    vec.map_or(Ternary::Void, |vec| {
        if vec.iter().any(cond) {
            Ternary::True
        } else {
            Ternary::False
        }
    })
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
