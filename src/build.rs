use std::{
    any::type_name,
    collections::{
        hash_map::Entry::{Occupied, Vacant},
        HashMap,
    },
};

use is_type::Is;
use petgraph::{
    data::Build,
    visit::{Data, GraphBase, GraphProp},
};

use crate::dot;

pub trait BuildableGraph: GraphBase + GraphProp + Data + Build + Default {}

impl<G> BuildableGraph for G where G: GraphBase + GraphProp + Data + Build + Default {}

macro_rules! bail {
    ($tokens:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {
        return Err(::syn::Error::new_spanned($tokens, ::std::format!($fmt, $($arg, )*)))
    };
}

pub fn dot<G: BuildableGraph>(graph: &dot::Graph) -> syn::Result<G>
where
    G::NodeWeight: Is<Type = String>,
    G::EdgeWeight: Default,
{
    use dot::{Graph, GraphDirectedness, NodeId, Stmt, StmtList, StmtNode, ID};
    let Graph {
        strict,
        directedness,
        id: _,
        brace_token: _,
        stmt_list: StmtList { stmts },
    } = graph;

    match (parallel_edges_allowed::<G>(), strict) {
        (true, Some(_)) | (false, None) => {} // matches
        (true, None) => {
            bail!(
                directedness,
                "graph is not `strict`, but graph implementation {} is",
                type_name::<G>()
            )
        }
        (false, Some(strict)) => {
            bail!(
                strict,
                "graph is `strict`, but graph implementation {} is not",
                type_name::<G>()
            )
        }
    }

    let dot_is_directed = match directedness {
        GraphDirectedness::Graph(_) => false,
        GraphDirectedness::Digraph(_) => true,
    };

    if dot_is_directed != G::default().is_directed() {
        bail!(
            directedness,
            "graph direction in `dot` does not match directionality of {}",
            type_name::<G>()
        );
    }

    let mut graph = G::default();
    let mut id2ix = HashMap::new();
    for (stmt, _semi) in stmts {
        match stmt {
            Stmt::Attr(it) => bail!(it, "unsupported statement"),
            Stmt::Assign(it) => bail!(it, "unsupported statement"),
            Stmt::Subgraph(it) => bail!(it, "unsupported statement"),
            Stmt::Node(StmtNode {
                node_id: NodeId { id, port },
                attrs,
            }) => {
                if let Some(port) = port {
                    bail!(port, "ports are not supported")
                }
                if let Some(attrs) = attrs {
                    bail!(attrs, "attrs are not supported")
                }
                let id = match id {
                    ID::AnyIdent(it) => it.to_string(),
                    ID::AnyLit(lit) => match lit {
                        syn::Lit::Str(it) => it.value(),
                        syn::Lit::Char(it) => it.value().to_string(),
                        other => bail!(other, "unsupported id"),
                    },
                    ID::Html(it) => match it.source() {
                        Some(it) => it,
                        None => bail!(it, "unsupported id"),
                    },
                    ID::DotInt(it) => bail!(it, "unsupported id"),
                };
                match id2ix.entry(id.clone()) {
                    Occupied(_) => bail!(id, "duplicate node declaration"),
                    Vacant(it) => {
                        it.insert(graph.add_node(G::NodeWeight::from_val(id)));
                    }
                }
            }
            Stmt::Edge(_) => todo!(),
        }
    }
    todo!()
}

fn parallel_edges_allowed<G: BuildableGraph>() -> bool
where
    G::NodeWeight: Is<Type = String>,
    G::EdgeWeight: Default,
{
    let mut graph = G::default();
    let from = graph.add_node(G::NodeWeight::from_val(String::from("from")));
    let to = graph.add_node(G::NodeWeight::from_val(String::from("to")));
    let first_edge_id = graph.add_edge(from, to, G::EdgeWeight::default()).unwrap();
    let maybe_parallel_edge_id = graph.add_edge(from, to, G::EdgeWeight::default());
    match maybe_parallel_edge_id {
        Some(id) if id == first_edge_id => panic!(
            "`impl petgraph::data::Build for {}` is wrong",
            type_name::<G>()
        ),
        Some(_) => true,
        None => false,
    }
}

#[cfg(test)]
mod tests {
    use petgraph::{
        graph::{DiGraph, UnGraph},
        matrix_graph::{DiMatrix, UnMatrix},
        stable_graph::{StableDiGraph, StableUnGraph},
    };

    use super::*;

    // Basically check the documentation of petgraph
    #[test]
    fn test_parallel_edges_allowed() {
        assert!(parallel_edges_allowed::<
            DiGraph<String, HashMap<String, String>>,
        >());
        assert!(parallel_edges_allowed::<
            UnGraph<String, HashMap<String, String>>,
        >());
        assert!(parallel_edges_allowed::<
            StableDiGraph<String, HashMap<String, String>>,
        >());
        assert!(parallel_edges_allowed::<
            StableUnGraph<String, HashMap<String, String>>,
        >());
        assert!(!parallel_edges_allowed::<
            DiMatrix<String, HashMap<String, String>>,
        >());
        assert!(!parallel_edges_allowed::<
            UnMatrix<String, HashMap<String, String>>,
        >());
    }
}
