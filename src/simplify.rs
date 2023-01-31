use std::fmt;

use egg::{
    define_language, rewrite, Analysis, AstSize, CostFunction, DidMerge, EGraph, Extractor, Id,
    RecExpr, Rewrite, Runner,
};

use crate::bitvecto::Bitvector;
use crate::expr::Expr;

pub fn simplify<const BITS: u8>(expr: Expr) -> Expr {
    let lang = expr_to_enode(expr);
    let runner = Runner::default()
        .with_expr(&lang)
        .run(&make_rules::<BITS>());
    let root = runner.roots[0];

    let extractor = Extractor::new(&runner.egraph, AstSize);
    enode_to_expr(&extractor, root)
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Name {
    X,
    Y,
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::X => write!(f, "x"),
            Self::Y => write!(f, "y"),
        }
    }
}

impl std::str::FromStr for Name {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "x" => Self::X,
            "y" => Self::Y,
            _ => return Err(()),
        })
    }
}

define_language! {
    enum BitwiseLanguage {
        "not" = Not(Id),
        "and" = And([Id; 2]),
        "or" = Or([Id; 2]),
        "rol" = Rol([Id; 2]),
        "ror" = Ror([Id; 2]),
        "xor" = Xor([Id; 2]),
        "mod" = Mod([Id; 2]),
        "mul" = Mul([Id; 2]),
        "add" = Add([Id; 2]),
        Number(usize),
        Variable(Name),
    }
}

#[derive(Default)]
struct ConstantFolding<const BITS: u8>;
impl<const BITS: u8> Analysis<BitwiseLanguage> for ConstantFolding<BITS> {
    type Data = Option<Bitvector<BITS>>;

    fn make(egraph: &EGraph<BitwiseLanguage, Self>, enode: &BitwiseLanguage) -> Self::Data {
        let c = |id: &Id| egraph[*id].data;

        // Note: we need to be careful to make sure the constant folding behaves
        // the same as in during evaluation.
        match enode {
            BitwiseLanguage::Number(value) => Some((*value).into()),
            BitwiseLanguage::Not(a) => Some(!c(a)?),
            BitwiseLanguage::Mod([a, b]) => Some(c(a)? % c(b)?),

            BitwiseLanguage::Mul([a, b]) => Some(c(a)? * c(b)?),
            BitwiseLanguage::Add([a, b]) => Some(c(a)? + c(b)?),

            BitwiseLanguage::And([a, b]) => Some(c(a)? & c(b)?),
            BitwiseLanguage::Or([a, b]) => Some(c(a)? | c(b)?),
            BitwiseLanguage::Xor([a, b]) => Some(c(a)? ^ c(b)?),

            BitwiseLanguage::Rol([a, b]) => Some(c(a)?.rotate_left(c(b)?)),
            BitwiseLanguage::Ror([a, b]) => Some(c(a)?.rotate_right(c(b)?)),

            BitwiseLanguage::Variable(_) => None,
        }
    }

    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> DidMerge {
        let (data, da, db) = match (&a, b) {
            (None, None) => (None, false, false),
            (Some(a), None) => (Some(*a), false, true),
            (None, Some(b)) => (Some(b), true, false),
            (Some(a), Some(b)) => {
                assert_eq!(*a, b); // if they're merging, they shouldn't be different constants, right?
                (Some(*a), false, false)
            }
        };

        *a = data;
        DidMerge(da, db)
    }

    fn modify(egraph: &mut EGraph<BitwiseLanguage, Self>, id: Id) {
        if let Some(value) = egraph[id].data {
            let added = egraph.add(BitwiseLanguage::Number(value.into()));
            egraph.union(id, added);
        }
    }
}

fn make_rules<const BITS: u8>() -> Vec<Rewrite<BitwiseLanguage, ConstantFolding<BITS>>> {
    vec![
        // Commutative laws
        rewrite!("commute-and";   "(and ?a ?b)" => "(and ?b ?a)"),
        rewrite!("commute-or";    "(or  ?a ?b)" => "(or  ?b ?a)"),
        rewrite!("commute-xor";   "(xor ?a ?b)" => "(xor ?b ?a)"),
        rewrite!("commute-mul";   "(mul ?a ?b)" => "(mul ?b ?a)"),
        rewrite!("commute-add";   "(add ?a ?b)" => "(add ?b ?a)"),
        // Associativity laws
        rewrite!("associate-and"; "(and (and ?a ?b) ?c)" => "(and ?a (and ?b ?c))"),
        rewrite!("associate-or";  "(or  (or  ?a ?b) ?c)" => "(or  ?a (or  ?b ?c))"),
        rewrite!("associate-xor"; "(xor (xor ?a ?b) ?c)" => "(xor ?a (xor ?b ?c))"),
        rewrite!("associate-mul"; "(mul (mul ?a ?b) ?c)" => "(mul ?a (mul ?b ?c))"),
        rewrite!("associate-add"; "(add (add ?a ?b) ?c)" => "(add ?a (add ?b ?c))"),
        // Elimination laws
        rewrite!("self-mod";      "(mod ?a ?a)" => "0"),
        rewrite!("self-add";      "(add ?a ?a)" => "(mul ?a 2)"),
        rewrite!("self-and";      "(and ?a ?a)" => "?a"),
        rewrite!("self-or";       "(or  ?a ?a)" => "?a"),
        rewrite!("self-xor";      "(xor ?a ?a)" => "0"),
        rewrite!("inv-self-and";  "(and (not ?a) ?a)" => "0"),
        rewrite!("inv-self-or";   "(or  (not ?a) ?a)" => "(not 0)"),
        rewrite!("inv-self-xor";  "(xor (not ?a) ?a)" => "(not 0)"),
        rewrite!("double-not";    "(not (not ?a))" => "?a"),
        rewrite!("double-mod";    "(mod (mod ?a ?b) ?b)" => "(mod ?a ?b)"),
        rewrite!("rol-ror";       "(rol (ror ?a ?b) ?b)" => "?a"),
        rewrite!("ror-rol";       "(ror (rol ?a ?b) ?b)" => "?a"),
        // Annihilation laws
        rewrite!("mod-zero";      "(mod ?a 0)" => "0"),
        rewrite!("zero-mod";      "(mod 0 ?a)" => "0"),
        rewrite!("mul-zero";      "(mul ?a 0)" => "0"),
        rewrite!("add-zero";      "(add ?a 0)" => "?a"),
        rewrite!("and-zero";      "(and ?a 0)" => "0"),
        rewrite!("or-zero";       "(or  ?a 0)" => "?a"),
        rewrite!("xor-zero";      "(xor ?a 0)" => "?a"),
        rewrite!("rol-zero";      "(rol ?a 0)" => "?a"),
        rewrite!("ror-zero";      "(ror ?a 0)" => "?a"),
        rewrite!("zero-rol";      "(rol 0 ?a)" => "0"),
        rewrite!("zero-ror";      "(ror 0 ?a)" => "0"),
        // DeMorgan
        rewrite!("not-and";       "(not (and ?a ?b))" => "(or (not ?a) (not ?b))"),
        rewrite!("not-or";        "(not (or  ?a ?b))" => "(and (not ?a) (not ?b))"),
        // Complex simplifications
        rewrite!("and-xor-is-or"; "(xor (and ?a ?b) (xor ?a ?b))" => "(or ?a ?b)"),
        rewrite!("or-and";        "(or (and ?a ?b) ?a)" => "?a"),
    ]
}

fn expr_to_enode(expr: Expr) -> RecExpr<BitwiseLanguage> {
    fn lower_expr(into: &mut RecExpr<BitwiseLanguage>, expr: Expr) -> Id {
        let expr = match expr {
            Expr::X => BitwiseLanguage::Variable(Name::X),
            Expr::Y => BitwiseLanguage::Variable(Name::Y),
            Expr::Constant(value) => BitwiseLanguage::Number(value),

            Expr::Not(inner) => {
                let inner = lower_expr(into, *inner);
                BitwiseLanguage::Not(inner)
            }

            Expr::Mod(a, b) => {
                let a = lower_expr(into, *a);
                let b = lower_expr(into, *b);
                BitwiseLanguage::Mod([a, b])
            }

            Expr::Mul(a, b) => {
                let a = lower_expr(into, *a);
                let b = lower_expr(into, *b);
                BitwiseLanguage::Mul([a, b])
            }

            Expr::Add(a, b) => {
                let a = lower_expr(into, *a);
                let b = lower_expr(into, *b);
                BitwiseLanguage::Add([a, b])
            }

            Expr::And(a, b) => {
                let a = lower_expr(into, *a);
                let b = lower_expr(into, *b);
                BitwiseLanguage::And([a, b])
            }

            Expr::Or(a, b) => {
                let a = lower_expr(into, *a);
                let b = lower_expr(into, *b);
                BitwiseLanguage::Or([a, b])
            }

            Expr::Xor(a, b) => {
                let a = lower_expr(into, *a);
                let b = lower_expr(into, *b);
                BitwiseLanguage::Xor([a, b])
            }

            Expr::Rol(a, b) => {
                let a = lower_expr(into, *a);
                let b = lower_expr(into, *b);
                BitwiseLanguage::Rol([a, b])
            }

            Expr::Ror(a, b) => {
                let a = lower_expr(into, *a);
                let b = lower_expr(into, *b);
                BitwiseLanguage::Ror([a, b])
            }
        };

        into.add(expr)
    }

    let mut result = RecExpr::default();
    let _ = lower_expr(&mut result, expr);

    result
}

fn enode_to_expr<const BITS: u8>(
    extractor: &Extractor<
        impl CostFunction<BitwiseLanguage>,
        BitwiseLanguage,
        ConstantFolding<BITS>,
    >,
    id: Id,
) -> Expr {
    match extractor.find_best_node(id) {
        BitwiseLanguage::Variable(Name::X) => Expr::X,
        BitwiseLanguage::Variable(Name::Y) => Expr::Y,
        BitwiseLanguage::Number(value) => Expr::Constant(*value),

        BitwiseLanguage::Not(inner) => {
            let inner = Box::new(enode_to_expr(extractor, *inner));
            Expr::Not(inner)
        }

        BitwiseLanguage::Mod([a, b]) => {
            let a = Box::new(enode_to_expr(extractor, *a));
            let b = Box::new(enode_to_expr(extractor, *b));
            Expr::Mod(a, b)
        }

        BitwiseLanguage::Mul([a, b]) => {
            let a = Box::new(enode_to_expr(extractor, *a));
            let b = Box::new(enode_to_expr(extractor, *b));
            Expr::Mul(a, b)
        }

        BitwiseLanguage::Add([a, b]) => {
            let a = Box::new(enode_to_expr(extractor, *a));
            let b = Box::new(enode_to_expr(extractor, *b));
            Expr::Add(a, b)
        }

        BitwiseLanguage::And([a, b]) => {
            let a = Box::new(enode_to_expr(extractor, *a));
            let b = Box::new(enode_to_expr(extractor, *b));
            Expr::And(a, b)
        }

        BitwiseLanguage::Or([a, b]) => {
            let a = Box::new(enode_to_expr(extractor, *a));
            let b = Box::new(enode_to_expr(extractor, *b));
            Expr::Or(a, b)
        }

        BitwiseLanguage::Xor([a, b]) => {
            let a = Box::new(enode_to_expr(extractor, *a));
            let b = Box::new(enode_to_expr(extractor, *b));
            Expr::Xor(a, b)
        }

        BitwiseLanguage::Rol([a, b]) => {
            let a = Box::new(enode_to_expr(extractor, *a));
            let b = Box::new(enode_to_expr(extractor, *b));
            Expr::Rol(a, b)
        }

        BitwiseLanguage::Ror([a, b]) => {
            let a = Box::new(enode_to_expr(extractor, *a));
            let b = Box::new(enode_to_expr(extractor, *b));
            Expr::Ror(a, b)
        }
    }
}
