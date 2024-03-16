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
                            "could not insert edge: operation not supported by {}",
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
    use std::fmt::Debug;

    use petgraph::graph::{DiGraph, UnGraph};
    use petgraph::graphmap::{DiGraphMap, UnGraphMap};
    use petgraph::matrix_graph::{DiMatrix, UnMatrix};
    use petgraph::stable_graph::{StableDiGraph, StableUnGraph};
    use petgraph::Direction;

    use itertools::Itertools as _;
    use petgraph::visit::{
        EdgeCount, GraphBase, IntoNeighbors, IntoNeighborsDirected, IntoNodeIdentifiers,
        NodeIndexable, Visitable,
    };
    use quote::ToTokens as _;
    use syn::parse_quote;

    use super::*;

    trait Supports {
        fn parallel_edges() -> bool;
        fn self_loops() -> bool;
    }

    macro_rules! supports {
        ($($ident:ident: $parallel:literal, $self_loops:literal);* $(;)?) => {
            $(
                impl<N, E> Supports for $ident<N, E> {
                    fn parallel_edges() -> bool { $parallel }
                    fn self_loops() -> bool { $self_loops }
                }
            )*
        };
    }

    supports! {
        DiGraph: true, true; // tested
        UnGraph: true, true; // tested

        StableDiGraph: true, true; // tested
        StableUnGraph: true, true; // tested

        DiGraphMap: false, true; // documented
        UnGraphMap: false, true; // documented

        DiMatrix: false, true; // tested
        UnMatrix: false, true; // tested
    }

    // no point testing `List` since can't add node weights
    // // documented
    // impl<E> Supports for List<E> {
    //     fn parallel_edges() -> bool { true }
    //     fn self_loops() -> bool { true }
    // }

    #[test]
    fn test_supports() {
        fn test<G, T: Debug + PartialEq>()
        where
            G: Supports,
            G: Default
                + Build<NodeWeight = &'static str, EdgeWeight = (), EdgeId = T>
                + GraphProp
                + GetAdjacencyMatrix
                + EdgeCount,
        {
            print!("testing {}...", type_name::<G>());

            /////////////////
            // Parallel edges
            /////////////////
            let mut graph = G::default();
            let from = graph.add_node("from");
            let to = graph.add_node("to");
            assert_eq!(graph.node_count(), 2);

            let edge = graph.add_edge(from, to, ());
            assert!(edge.is_some());
            assert_eq!(graph.edge_count(), 1);
            match G::parallel_edges() {
                true => {
                    let edge2 = graph.add_edge(from, to, ());
                    assert!(edge2.is_some());
                    assert_ne!(edge, edge2);
                    assert_eq!(graph.edge_count(), 2);
                }
                false => {
                    let edge2 = graph.add_edge(from, to, ());
                    assert!(edge2.is_none());
                    assert_eq!(graph.edge_count(), 1);
                }
            }

            /////////////
            // Self-loops
            /////////////
            let mut graph = G::default();
            let node = graph.add_node("node");
            assert_eq!(graph.node_count(), 1);

            match G::self_loops() {
                true => {
                    let edge = graph.add_edge(node, node, ());
                    assert!(edge.is_some());
                    assert_eq!(graph.edge_count(), 1);
                }
                false => {
                    let edge = graph.add_edge(node, node, ());
                    assert!(edge.is_none());
                    assert_eq!(graph.edge_count(), 0);
                }
            }

            ////////////////////////////////////////////////
            // Adjacency tests should be direction-sensitive
            ////////////////////////////////////////////////
            let mut graph = G::default();
            let from = graph.add_node("from");
            let to = graph.add_node("to");
            assert_eq!(graph.node_count(), 2);

            let edge = graph.add_edge(from, to, ());
            assert!(edge.is_some());
            assert_eq!(graph.edge_count(), 1);
            match graph.is_directed() {
                true => {
                    assert!(graph.is_adjacent(&graph.adjacency_matrix(), from, to));
                    assert!(!graph.is_adjacent(&graph.adjacency_matrix(), to, from));
                    let edge2 = graph.add_edge(to, from, ());
                    assert!(edge2.is_some());
                    assert_ne!(edge, edge2);
                    assert_eq!(graph.edge_count(), 2);
                    assert!(graph.is_adjacent(&graph.adjacency_matrix(), to, from));
                }
                false => {
                    assert!(graph.is_adjacent(&graph.adjacency_matrix(), from, to));
                    assert!(graph.is_adjacent(&graph.adjacency_matrix(), to, from));
                }
            }

            println!("ok.");
        }

        test::<DiGraph<_, _>, _>();
        test::<UnGraph<_, _>, _>();
        test::<StableDiGraph<_, _>, _>();
        test::<StableUnGraph<_, _>, _>();
        test::<DiGraphMap<_, _>, _>();
        test::<UnGraphMap<_, _>, _>();
        test::<DiMatrix<_, _>, _>();
        test::<UnMatrix<_, _>, _>();
    }

    #[test]
    fn test_dot() {
        let dot_graph: dot::Graph = parse_quote! {
            graph {
                a -- b -- c
            }
        };
        let petgraph = from_dot::<UnGraph<_, _>>(&dot_graph).unwrap();
        assert_eq!(petgraph.node_count(), 3);
        assert_eq!(petgraph.edge_count(), 2);

        let dot_graph: dot::Graph = parse_quote! {
            digraph {
                a -> b -> c;
            }
        };
        let petgraph = from_dot::<DiGraph<_, _>>(&dot_graph).unwrap();
        assert_eq!(petgraph.node_count(), 3);
        assert_eq!(petgraph.edge_count(), 2);
    }

    struct TreeNode<'a, I> {
        parent: Option<&'a Self>,
        this: I,
        children: Vec<Self>,
    }

    fn graph2tree<G>(graph: &G)
    where
        G: GraphBase
            + IntoNodeIdentifiers
            + IntoNeighbors
            + NodeIndexable
            + IntoNeighborsDirected
            + Visitable,
    {
        if petgraph::algo::is_cyclic_directed(graph) {
            panic!("graph is cyclic")
        }
        for component in petgraph::algo::tarjan_scc(graph) {
            let mut root = None;
            for id in &component {
                match graph.neighbors_directed(*id, Direction::Incoming).count() {
                    0 => match root {
                        Some(_) => panic!("tree must have exactly one root"),
                        None => root = Some(id),
                    },
                    1 => {}
                    _ => panic!("graph is not a tree"),
                }
            }
            let Some(root) = root else {
                panic!("tree has no root")
            };
            let mut root = TreeNode {
                parent: None,
                this: *root,
                children: vec![],
            };
            fn fill_node<'a, I, G>(parent: &mut TreeNode<'a, I>, graph: G)
            where
                G: IntoNeighborsDirected<NodeId = I>,
                I: Copy,
            {
                let children = vec![];
                for child in graph.neighbors_directed(parent.this, Direction::Outgoing) {
                    let mut child = TreeNode {
                        parent: None,
                        this: child,
                        children: vec![],
                    };
                    fill_node(&mut child, graph);
                    parent.children.push(child);
                    for child in &mut parent.children {
                        child.parent = Some(&parent)
                    }
                }
            }
            fill_node(&mut root, graph);
        }
    }

    #[test]
    fn test() {
        let graph = parse_quote!(digraph {
            a -> b1 -> c1;
            a -> b2 -> c2;
        });
        let graph = from_dot::<DiGraphMap<_, _>>(&graph).unwrap();

        let mut root = None;
        for node in graph.nodes() {
            match graph.neighbors_directed(node, Direction::Incoming).count() {
                0 => {
                    assert!(
                        root.is_none(),
                        "tree must have exactly one root. Duplicate: {}",
                        node.id.into_token_stream()
                    );
                    root = Some(node);
                }
                1 => {}
                _ => panic!("graph is not a tree"),
            }
        }
        let root = root.expect("tree has no roots");

        for (depth, members) in petgraph::algo::dijkstra(&graph, root, None, |_| 1usize)
            .into_iter()
            .sorted_by_key(|(_, depth)| *depth)
            .group_by(|(_, depth)| *depth)
            .into_iter()
            .map(|(depth, members)| (depth, members.map(|(it, _)| it).collect::<Vec<_>>()))
        {
            println!(
                "{depth}: [{}]",
                members
                    .iter()
                    .map(|it| it.id.into_token_stream())
                    .join(", ")
            )
        }
    }
}
