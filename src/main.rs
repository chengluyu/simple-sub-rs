use im::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

enum Term {
    Literal(u32),
    Variable(String),
    Lambda(String, Box<Term>),
    Apply(Box<Term>, Box<Term>),
    Record(Vec<(String, Term)>),
    Select(Box<Term>, String),
    Let(bool, String, Box<Term>, Box<Term>),
}

#[derive(Clone, Debug)]
enum SimpleType {
    Variable(VariableState),
    Primitive(String),
    Function(Box<SimpleType>, Box<SimpleType>),
    Record(Vec<(String, SimpleType)>),
}

#[derive(Clone, Debug)]
struct VariableState {
    lower_bounds: VecDeque<SimpleType>,
    upper_bounds: VecDeque<SimpleType>,
}

impl VariableState {
    fn new() -> VariableState {
        VariableState {
            lower_bounds: VecDeque::new(),
            upper_bounds: VecDeque::new(),
        }
    }
}

type Context = HashMap<String, SimpleType>;

fn fresh_variable() -> SimpleType {
    SimpleType::Variable(VariableState::new())
}

fn type_term(term: &Term, context: &Context) -> Result<SimpleType, String> {
    match term {
        Term::Literal(_) => Ok(SimpleType::Primitive("int".into())),
        Term::Variable(name) => match context.get(name) {
            Some(t) => Ok(t.clone()),
            None => Err("".into()),
        },
        Term::Lambda(name, body) => {
            let param = fresh_variable();
            let typed_body = type_term(body, &context.update(name.clone(), param.clone()))?;
            Ok(SimpleType::Function(Box::new(param), Box::new(typed_body)))
        }
        Term::Apply(callee, argument) => {
            let result = fresh_variable();
            constrain(
                &mut type_term(callee, context)?,
                &mut SimpleType::Function(
                    Box::new(type_term(argument, context)?),
                    Box::new(result.clone()),
                ),
            )?;
            Ok(result)
        }
        Term::Record(fields) => {
            let typed_fields: Result<Vec<_>, _> = fields
                .iter()
                .map(|(name, term)| type_term(&term, context).map(|t| (name.clone(), t)))
                .collect();
            Ok(SimpleType::Record(typed_fields?))
        }
        Term::Select(target, field) => {
            let result = fresh_variable();
            constrain(
                &mut type_term(target, context)?,
                &mut SimpleType::Record(vec![(field.clone(), result.clone())]),
            )?;
            Ok(result)
        }
        Term::Let(_recursive, _name, _init, _body) => Err("not implemented".into()),
    }
}

fn constrain(lhs: &mut SimpleType, rhs: &mut SimpleType) -> Result<(), String> {
    let mut cache = HashSet::new();
    constrain_impl(lhs, rhs, &mut cache)
}

fn constrain_impl(
    lhs: &mut SimpleType,
    rhs: &mut SimpleType,
    cache: &mut HashSet<String>,
) -> Result<(), String> {
    let repr = format!("{:?} :: {:?}", lhs, rhs);
    if cache.contains(&repr) {
        return Ok(());
    }
    cache.insert(repr);
    match (lhs, rhs) {
        (SimpleType::Primitive(pl), SimpleType::Primitive(pr)) if pl == pr => Ok(()),
        (SimpleType::Function(l0, l1), SimpleType::Function(r0, r1)) => {
            constrain_impl(l0, r0, cache)?;
            constrain_impl(l1, r1, cache)?;
            Ok(())
        }
        (SimpleType::Record(left_fields), SimpleType::Record(right_fields)) => {
            for (right_name, right_type) in right_fields {
                let found = left_fields
                    .iter_mut()
                    .find(|left_entry| left_entry.0 == *right_name);
                match found {
                    None => return Err(format!("missing field {}", right_name)),
                    Some((_, left_type)) => constrain(left_type, right_type)?,
                }
            }
            Ok(())
        }
        (SimpleType::Variable(lhs), rhs) => {
            lhs.upper_bounds.push_front(rhs.clone());
            for t in lhs.lower_bounds.iter_mut() {
                constrain_impl(t, rhs, cache)?;
            }
            Ok(())
        }
        (lhs, SimpleType::Variable(rhs)) => {
            rhs.lower_bounds.push_front(lhs.clone());
            for t in rhs.upper_bounds.iter_mut() {
                constrain_impl(lhs, t, cache)?;
            }
            Ok(())
        }
        (lhs, rhs) => Err(format!("cannot constrain {:?} <: {:?}", lhs, rhs)),
    }
}

fn main() {
    println!("Hello, world!");
}
