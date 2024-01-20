use std::{
    any::type_name,
    collections::{
        hash_map::Entry::{Occupied, Vacant},
        HashMap,
    },
    iter,
};

use crate::dot::{
    self, Attrs, EdgeDirectedness, EdgeTarget, Graph, GraphDirectedness, NodeId, Stmt, StmtEdge,
    StmtList, StmtNode, ID,
};
use petgraph::{
    data::Build,
    visit::{GetAdjacencyMatrix, GraphProp},
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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct NodeInfo<'a, Ix> {
    portless_node: PortlessNode<'a>,
    node_id: Ix,
}

pub fn from_dot<'a, G>(dot_graph: &'a dot::Graph) -> syn::Result<G>
where
    G: Default
        + Build<NodeWeight = PortlessNode<'a>, EdgeWeight = Option<&'a Attrs>>
        + GraphProp
        + GetAdjacencyMatrix,
{
    let Graph {
        strict,
        directedness,
        id: _,
        brace_token: _,
        stmt_list: StmtList { stmts },
    } = dot_graph;

    let mut petgraph = G::default();

    match (directedness, petgraph.is_directed()) {
        (GraphDirectedness::Digraph(_), true) | (GraphDirectedness::Graph(_), false) => {}
        (GraphDirectedness::Graph(_), true) | (GraphDirectedness::Digraph(_), false) => bail!(
            directedness,
            "graph direction in dot does not match compile-time direction of `{}`",
            type_name::<G>()
        ),
    }

    let mut id2info = HashMap::new();

    for (stmt, _) in stmts {
        if let Stmt::Node(StmtNode {
            node_id: NodeId { id, port },
            attrs,
        }) = stmt
        {
            if port.is_some() {
                bail!(port, "nodes with ports are not supported")
            };
            match id2info.entry(id) {
                Occupied(_) => bail!(id, "duplicate node statements are not supported"),
                Vacant(it) => {
                    let portless_node = PortlessNode {
                        id,
                        attrs: attrs.as_ref(),
                    };
                    let node_id = petgraph.add_node(portless_node);
                    it.insert(NodeInfo {
                        portless_node,
                        node_id,
                    });
                }
            }
        }
    }

    for (stmt, _) in stmts {
        if let Stmt::Edge(StmtEdge {
            from,
            edges,
            attrs: _,
        }) = stmt
        {
            for it in iter::once(from).chain(edges.iter().map(|(_, it)| it)) {
                match it {
                    EdgeTarget::Subgraph(it) => bail!(it, "subgraphs are not supported"),
                    EdgeTarget::NodeId(NodeId { id, port }) => {
                        if port.is_some() {
                            bail!(port, "nodes with ports are not supported");
                        }
                        match id2info.entry(id) {
                            Occupied(_) => {}
                            Vacant(it) => {
                                let portless_node = PortlessNode { id, attrs: None };
                                let node_id = petgraph.add_node(portless_node);
                                it.insert(NodeInfo {
                                    portless_node,
                                    node_id,
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    for (stmt, _semi) in stmts {
        match stmt {
            Stmt::Attr(_) | Stmt::Assign(_) | Stmt::Subgraph(_) => {
                bail!(stmt, "unsupported statement")
            }
            Stmt::Node(_) => {}
            Stmt::Edge(StmtEdge { from, edges, attrs }) => {
                let mut target_from = from;
                for (directedness, target_to) in edges {
                    match (directedness, petgraph.is_directed()) {
                        (EdgeDirectedness::Directed(_), true)
                        | (EdgeDirectedness::Undirected(_), false) => {}
                        (EdgeDirectedness::Directed(_), false)
                        | (EdgeDirectedness::Undirected(_), true) => {
                            bail!(directedness, "inconsistent edge direction")
                        }
                    }

                    let EdgeTarget::NodeId(NodeId { id, port: None }) = target_from else {
                        unreachable!();
                    };
                    let node_id_from = id2info[id].node_id;
                    let EdgeTarget::NodeId(NodeId { id, port: None }) = target_to else {
                        unreachable!();
                    };
                    let node_id_to = id2info[id].node_id;

                    if strict.is_some()
                        && petgraph.is_adjacent(
                            &petgraph.adjacency_matrix(),
                            node_id_from,
                            node_id_to,
                        )
                    {
                        bail!(stmt, "duplicate edge on a strict graph")
                    }

                    let edge_id = petgraph.add_edge(node_id_from, node_id_to, attrs.as_ref());
                    if edge_id.is_none() {
                        bail!(
                            stmt,
                            "could not insert parallel edge: operation not supported by {}",
                            type_name::<G>()
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
    use petgraph::{graphmap::GraphMap, Directed, Undirected};
    use syn::parse_quote;

    use super::*;

    #[test]
    fn test_dot() {
        let dot_graph: dot::Graph = parse_quote! {
            graph {
                a -- b -- c
            }
        };
        let petgraph = from_dot::<GraphMap<_, _, Undirected>>(&dot_graph).unwrap();
        assert_eq!(petgraph.node_count(), 3);
        assert_eq!(petgraph.edge_count(), 2);

        let dot_graph: dot::Graph = parse_quote! {
            digraph {
                a -> b -> c;
            }
        };
        let petgraph = from_dot::<GraphMap<_, _, Directed>>(&dot_graph).unwrap();
        assert_eq!(petgraph.node_count(), 3);
        assert_eq!(petgraph.edge_count(), 2);
    }
}
