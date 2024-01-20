use std::{
    any::type_name,
    collections::{
        hash_map::Entry::{Occupied, Vacant},
        HashMap,
    },
};

use petgraph::{graphmap::GraphMap, EdgeType};

use crate::dot::{
    self, Attrs, EdgeDirectedness, EdgeTarget, Graph, GraphDirectedness, NodeId, Stmt, StmtEdge,
    StmtList, StmtNode, ID,
};

macro_rules! bail {
    ($tokens:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {
        return Err(::syn::Error::new_spanned($tokens, ::std::format!($fmt, $($arg, )*)))
    };
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PortlessNode<'a> {
    pub id: &'a ID,
    pub attrs: Option<&'a Attrs>,
}

pub fn dot<Di: EdgeType>(
    dot_graph: &dot::Graph,
) -> syn::Result<GraphMap<PortlessNode<'_>, Option<&Attrs>, Di>> {
    let Graph {
        strict: _,
        directedness,
        id: _,
        brace_token: _,
        stmt_list: StmtList { stmts },
    } = dot_graph;
    match (directedness, Di::is_directed()) {
        (GraphDirectedness::Digraph(_), true) | (GraphDirectedness::Graph(_), false) => {}
        (GraphDirectedness::Graph(_), true) | (GraphDirectedness::Digraph(_), false) => bail!(
            directedness,
            "graph direction in dot does not match compile-time direction {}",
            type_name::<Di>()
        ),
    }

    let mut id2portless_node = HashMap::new();

    for (stmt, _semi) in stmts {
        if let Stmt::Node(StmtNode {
            node_id: NodeId { id, port },
            attrs,
        }) = stmt
        {
            if port.is_some() {
                bail!(port, "nodes with ports are not supported")
            };
            match id2portless_node.entry(id) {
                Occupied(_) => bail!(id, "concatenation of node attributes is not supported"),
                Vacant(it) => {
                    it.insert(PortlessNode {
                        id,
                        attrs: attrs.as_ref(),
                    });
                }
            }
        }
    }

    let mut petgraph = GraphMap::<_, _, Di>::default();

    for (stmt, _semi) in stmts {
        match stmt {
            Stmt::Attr(_) | Stmt::Assign(_) | Stmt::Subgraph(_) => {
                bail!(stmt, "unsupported statement")
            }
            Stmt::Node(_) => {}
            Stmt::Edge(StmtEdge { from, edges, attrs }) => {
                let mut target_from = from;
                for (directedness, target_to) in edges {
                    match (directedness, Di::is_directed()) {
                        (EdgeDirectedness::Directed(_), true)
                        | (EdgeDirectedness::Undirected(_), false) => {}
                        (EdgeDirectedness::Directed(_), false)
                        | (EdgeDirectedness::Undirected(_), true) => {
                            bail!(directedness, "inconsistent edge direction")
                        }
                    }

                    let portless_node_from = match target_from {
                        EdgeTarget::Subgraph(it) => bail!(it, "unsupported edge target"),
                        EdgeTarget::NodeId(NodeId { id, port }) => match port {
                            Some(port) => bail!(port, "nodes with ports are not supported"),
                            None => id2portless_node
                                .get(id)
                                .copied()
                                .unwrap_or(PortlessNode { id, attrs: None }),
                        },
                    };
                    let portless_node_to = match target_to {
                        EdgeTarget::Subgraph(it) => bail!(it, "unsupported edge target"),
                        EdgeTarget::NodeId(NodeId { id, port }) => match port {
                            Some(port) => bail!(port, "nodes with ports are not supported"),
                            None => id2portless_node
                                .get(id)
                                .copied()
                                .unwrap_or(PortlessNode { id, attrs: None }),
                        },
                    };

                    let clobbered =
                        petgraph.add_edge(portless_node_from, portless_node_to, attrs.as_ref());
                    if clobbered.is_some() {
                        bail!(
                            stmt,
                            "duplicate edges are not supported (graphs are implicitly `strict`)"
                        )
                    }

                    target_from = target_to;
                }
            }
        }
    }

    Ok(petgraph)
}

#[cfg(test)]
mod tests {
    use petgraph::{Directed, Undirected};
    use syn::parse_quote;

    use super::*;
    #[test]
    fn test_dot() {
        let dot_graph: dot::Graph = parse_quote! {
            graph {
                a -- b -- c
            }
        };
        let petgraph = dot::<Undirected>(&dot_graph).unwrap();
        assert_eq!(petgraph.node_count(), 3);
        assert_eq!(petgraph.edge_count(), 2);

        let dot_graph: dot::Graph = parse_quote! {
            digraph {
                a -> b -> c
            }
        };
        let petgraph = dot::<Directed>(&dot_graph).unwrap();
        assert_eq!(petgraph.node_count(), 3);
        assert_eq!(petgraph.edge_count(), 2);
    }
}
