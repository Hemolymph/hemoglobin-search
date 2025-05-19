use crate::ComparisonKind;
use chumsky::{
    IterParser, Parser,
    error::Rich,
    extra,
    prelude::{any, choice, end, just, recursive},
    text::ident,
};
use hemoglobin::{
    cards::{
        kins::{Kin, KinComparison},
        properties::{Array, Number, Text},
    },
    numbers::Comparison,
};
use regex::Regex;

use super::{Ordering, Query, QueryRestriction, Sort, TextComparison};

#[derive(Clone, Copy)]
pub enum Properties {
    NumProperty(Number),
    StringProperty(Text),
    ArrayProperty(Array),
    Sort(Ordering),
    Kin,
    Keywords,
}

enum TextComparable {
    String(String),
    Regex(Regex),
}

/// Parses a query.
/// # Errors
/// If the parser fails.
pub fn parse_query(string: &str) -> Result<Query, Vec<Rich<'_, char>>> {
    let parser = make_query_parser();
    parser.parse(string).into_result()
}

#[allow(clippy::too_many_lines)]
pub fn make_query_parser<'a>() -> impl Parser<'a, &'a str, Query, extra::Err<Rich<'a, char>>> + 'a {
    let word = any()
        .filter(|c: &char| {
            !c.is_whitespace()
                && *c != '('
                && *c != ')'
                && *c != ':'
                && *c != '='
                && *c != '<'
                && *c != '>'
                && *c != '!'
                && *c != '-'
        })
        .repeated()
        .at_least(1)
        .collect::<String>()
        .labelled("ident")
        .as_context();

    let quoted_word = |delim: char| {
        any()
            .filter(move |c: &char| *c != delim)
            .repeated()
            .collect::<String>()
            .labelled(format!("string wrapped in {delim}"))
            .as_context()
    };

    let keyword = |mat: &'static str| {
        ident()
            .try_map(move |kw, span| {
                if mat == kw {
                    Ok(())
                } else {
                    Err(Rich::custom(span, format!("Expected {kw}")))
                }
            })
            .labelled(format!("`{mat}`"))
            .as_context()
    };

    let name_property_name = choice((keyword("name"), keyword("n"))).to(Text::Name);
    let desc_property_name =
        choice((keyword("description"), keyword("desc"), keyword("d"))).to(Text::Description);
    let flavor_property_name =
        choice((keyword("flavortext"), keyword("flavor"), keyword("ft"))).to(Text::FlavorText);
    let id_property_name = keyword("id").to(Text::Id);
    let type_property_name = choice((keyword("type"), keyword("t"))).to(Text::Type);

    let text_property_name = choice((
        name_property_name,
        desc_property_name,
        flavor_property_name,
        id_property_name,
        type_property_name,
    ))
    .padded();

    let cost_property_name = choice((keyword("cost"), keyword("c"))).to(Number::Cost);
    let flip_cost_property_name = choice((keyword("flip"), keyword("f"))).to(Number::FlipCost);
    let power_property_name = choice((keyword("power"), keyword("p"))).to(Number::Power);
    let def_property_name =
        choice((keyword("defense"), keyword("def"), keyword("d"))).to(Number::Defense);
    let health_property_name =
        choice((keyword("health"), keyword("hp"), keyword("h"))).to(Number::Health);

    let num_property_name = choice((
        cost_property_name,
        flip_cost_property_name,
        power_property_name,
        def_property_name,
        health_property_name,
    ));

    let number = any()
        .filter(|c: &char| c.is_numeric())
        .repeated()
        .at_least(1)
        .collect::<String>()
        .try_map(|x, span| {
            x.parse()
                .map_err(|x| Rich::custom(span, format!("Not a number: {x}")))
        })
        .labelled("number")
        .as_context();

    let regex_text = quoted_word('/')
        .try_map(|x, span| {
            Regex::new(x.as_str())
                .map_err(|x| Rich::custom(span, format!("Not a valid regex: {x}")))
        })
        .delimited_by(just('/'), just('/'))
        .labelled("regex expression")
        .as_context();

    let quoted_text = quoted_word('"')
        .delimited_by(just('"'), just('"'))
        .labelled("quoted text");

    let expr = recursive(|expr| {
        let group = expr
            .clone()
            .repeated()
            .collect()
            .delimited_by(just('(').padded(), just(')').padded())
            .labelled("subquery")
            .as_context();

        let group_restriction = group
            .clone()
            .map(|x| QueryRestriction::Group(Query::from_restrictions(x)));

        let group_query = group.clone().map(Query::from_restrictions);

        // Num Properties
        let num_comparison_symbol = choice((
            just("!=").to(NumberComparisonSymbol::NotEqual),
            just(">=").to(NumberComparisonSymbol::GreaterThanOrEqual),
            just("<=").to(NumberComparisonSymbol::LessThanOrEqual),
            just('>').to(NumberComparisonSymbol::GreaterThan),
            just('<').to(NumberComparisonSymbol::LessThan),
            just('=').to(NumberComparisonSymbol::Equal),
        ))
        .labelled("comparison operator");

        let num_comparison = num_comparison_symbol
            .padded()
            .then(number.padded())
            .map(|(comparison, number)| match comparison {
                NumberComparisonSymbol::GreaterThan => Comparison::GreaterThan(number),
                NumberComparisonSymbol::LessThan => Comparison::LowerThan(number),
                NumberComparisonSymbol::GreaterThanOrEqual => {
                    Comparison::GreaterThanOrEqual(number)
                }
                NumberComparisonSymbol::LessThanOrEqual => Comparison::LowerThanOrEqual(number),
                NumberComparisonSymbol::Equal => Comparison::Equal(number),
                NumberComparisonSymbol::NotEqual => Comparison::NotEqual(number),
            })
            .padded();

        let num_property = num_property_name
            .clone()
            .padded()
            .then(num_comparison)
            .map(|(property, cost)| QueryRestriction::NumberComparison(property, cost));

        // Text Properties
        let text_comparison_symbol = choice((
            just('=').to(ComparisonKind::Equals),
            just(':').to(ComparisonKind::Contains),
        ))
        .padded();

        let text_comparable = choice((
            quoted_text.map(TextComparable::String),
            regex_text.map(TextComparable::Regex),
            word.map(TextComparable::String),
        ))
        .padded();

        let text_comparison = text_comparison_symbol
            .clone()
            .then(text_comparable)
            .map(|(symbol, text)| match text {
                TextComparable::String(string) => match symbol {
                    ComparisonKind::Contains => TextComparison::Contains(string),
                    ComparisonKind::Equals => TextComparison::EqualTo(string),
                },
                TextComparable::Regex(regex) => TextComparison::HasMatch(regex),
            })
            .padded();

        let text_property = text_property_name
            .clone()
            .then(text_comparison.clone())
            .map(|(property, comparison)| QueryRestriction::TextComparison(property, comparison))
            .padded();

        // Kins
        let kin_property_name = choice((keyword("kins"), keyword("k"))).to(Properties::Kin);

        let kin_comparison = text_comparison.clone().map(|x| match x {
            TextComparison::Contains(string) => match Kin::from_string(&string) {
                Some(kin) => KinComparison::Similar(kin),
                None => KinComparison::TextContains(string),
            },
            TextComparison::EqualTo(string) => match Kin::from_string(&string) {
                Some(kin) => KinComparison::Equal(kin),
                None => KinComparison::TextEqual(string),
            },
            TextComparison::HasMatch(regex) => KinComparison::RegexMatch(regex),
        });

        let kin_property = kin_property_name
            .padded()
            .ignore_then(kin_comparison.clone())
            .map(QueryRestriction::KinComparison);

        // Keywords
        let kws_property_name = choice((keyword("keyword"), keyword("kw")));

        let kw_property = kws_property_name
            .ignore_then(text_comparison)
            .map(QueryRestriction::HasKw);

        // Devours
        let devours_property_name = choice((keyword("devours"), keyword("dev"))).to(Text::Name);

        let null_comparison_symbol = choice((just('=').to(()), just(':').to(()))).padded();

        let devours_property = devours_property_name
            .ignore_then(text_comparison_symbol)
            .then(group_query.clone())
            .map(|(comparison, query)| QueryRestriction::Devours(query, comparison))
            .padded();

        // Devoured by
        let devouredby_property_name =
            choice((keyword("devouredby"), keyword("deby"), keyword("dby"))).to(Text::Name);

        let devouredby_property = devouredby_property_name
            .ignore_then(null_comparison_symbol)
            .ignore_then(group_query.clone())
            .map(QueryRestriction::DevouredBy)
            .padded();

        // Fuzzy
        let fuzzy = word
            .filter(|x| x != "XOR" && x != "OR" && x != "SORT")
            .map(QueryRestriction::Fuzzy)
            .labelled("basic query word");

        // Atom
        let atom = choice((
            num_property,
            text_property,
            devours_property,
            devouredby_property,
            kin_property,
            kw_property,
            fuzzy,
        ))
        .padded();

        let atom = group_restriction.or(atom);

        let uniop = choice((
            just('-').to(QueryOp::Not),
            just('!').to(QueryOp::LenientNot),
        ))
        .labelled("negation operator");

        let atom = uniop
            .padded()
            .repeated()
            .foldr(atom, |op, atom| match op {
                QueryOp::Not => QueryRestriction::Not(Query::from_restrictions(vec![atom])),
                QueryOp::LenientNot => {
                    QueryRestriction::LenientNot(Query::from_restrictions(vec![atom]))
                }
            })
            .labelled("search atom");

        let operation = choice((
            keyword("OR").to(QueryBinOp::Or),
            keyword("XOR").to(QueryBinOp::Xor),
        ));

        atom.then(operation.then(expr).or_not())
            .map(
                |(first, op): (QueryRestriction, Option<(QueryBinOp, QueryRestriction)>)| match op {
                    None => first,
                    Some((op, right)) => match op {
                        QueryBinOp::Or => QueryRestriction::Or(
                            Query::from_restrictions(vec![first]),
                            Query::from_restrictions(vec![right]),
                        ),
                        QueryBinOp::Xor => QueryRestriction::Xor(
                            Query::from_restrictions(vec![first]),
                            Query::from_restrictions(vec![right]),
                        ),
                    },
                },
            )
            .labelled("search expression")
    });

    let order = choice((
        keyword("ascending").to(Ordering::Ascending),
        keyword("descending").to(Ordering::Descending),
    ))
    .labelled("ascending or descending");

    let sort_type = choice((
        text_property_name.map(Sortable::Text),
        num_property_name.map(Sortable::Num),
    ))
    .labelled("sortable trait");

    let order =
        sort_type
            .padded()
            .then(order)
            .map(|(sort, order): (Sortable, Ordering)| match sort {
                Sortable::Text(text) => Sort::Alphabet(text, order),
                Sortable::Num(number) => Sort::Numeric(number, order),
            });

    let sort = keyword("SORT")
        .ignore_then(order)
        .labelled("sorting method")
        .or_not()
        .labelled("sorting clause or lack thereof")
        .map(|x| x.map_or(Sort::Fuzzy, |x| x));

    expr.padded()
        .repeated()
        .collect()
        .map(Query::from_restrictions)
        .then(sort.padded())
        .map(|(mut query, sort)| {
            query.sort = sort;
            query
        })
        .then_ignore(end())
}

#[derive(Clone)]
enum NumberComparisonSymbol {
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Equal,
    NotEqual,
}

#[derive(Clone)]
enum QueryOp {
    Not,
    LenientNot,
}

#[derive(Clone)]
enum QueryBinOp {
    Or,
    Xor,
}

enum Sortable {
    Text(Text),
    Num(Number),
}
