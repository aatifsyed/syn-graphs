pub fn dot(graph: &crate::dot::Graph) -> String {
    let mut p = crate::prettyplease::algorithm::Printer::new();
    p.graph(graph);
    p.eof()
}

mod dot {
    use crate::dot::*;
    use crate::prettyplease::{algorithm, iter::IterDelimited as _};
    use crate::INDENT;

    impl algorithm::Printer {
        pub fn graph(
            &mut self,
            Graph {
                strict,
                direction,
                id,
                brace_token: _,
                statements: Statements { list },
            }: &Graph,
        ) {
            self.cbox(0); // Printer::file
            self.cbox(INDENT); // Printer::item_mod
            if strict.is_some() {
                self.word("strict ");
            }
            match direction {
                Directedness::Graph(_) => self.word("graph "),
                Directedness::Digraph(_) => self.word("digraph "),
            }
            if let Some(id) = id {
                self.id(id)
            }
            self.word(" {");
            self.hardbreak_if_nonempty();
            for (stmt, _semi) in list {
                self.stmt(stmt);
            }
            self.offset(-INDENT); // Printer::item_mod
            self.end();
            self.word("}");
            self.hardbreak()
        }
        fn stmt(&mut self, stmt: &Stmt) {
            match stmt {
                Stmt::Assign(StmtAssign {
                    left,
                    eq_token: _,
                    right,
                }) => {
                    self.id(left);
                    self.word(" = ");
                    self.id(right);
                }
                Stmt::Attr(StmtAttr { on, attributes }) => {
                    match on {
                        StmtAttrOn::Graph(_) => self.word("graph "),
                        StmtAttrOn::Node(_) => self.word("node "),
                        StmtAttrOn::Edge(_) => self.word("edge "),
                    };
                    self.attributes(attributes);
                }
                Stmt::Node(StmtNode {
                    node_id,
                    attributes,
                }) => {
                    self.node_id(node_id);
                    self.nbsp();
                    if let Some(attributes) = attributes {
                        self.attributes(attributes);
                    }
                }
                Stmt::Edge(StmtEdge { from, ops, attrs }) => {
                    self.node_id_or_subgraph(from);
                    for (op, to) in ops {
                        match op {
                            EdgeOp::Directed(_) => self.word(" -> "),
                            EdgeOp::Undirected(_) => self.word(" -- "),
                        }
                        self.node_id_or_subgraph(to)
                    }
                    if let Some(attributes) = attrs {
                        self.nbsp();
                        self.attributes(attributes);
                    }
                }
                Stmt::Subgraph(subgraph) => self.subgraph(subgraph),
            }
            self.word(";");
            self.hardbreak();
        }
        fn node_id_or_subgraph(&mut self, it: &NodeIdOrSubgraph) {
            match it {
                NodeIdOrSubgraph::Subgraph(subgraph) => self.subgraph(subgraph),
                NodeIdOrSubgraph::NodeId(node_id) => self.node_id(node_id),
            }
        }
        fn subgraph(
            &mut self,
            StmtSubgraph {
                prelude,
                brace_token: _,
                statements: Statements { list },
            }: &StmtSubgraph,
        ) {
            self.word("subgraph ");
            if let Some((_subgraph, Some(id))) = prelude {
                self.id(id);
            }
            self.cbox(INDENT); // Printer::expr_block
            self.word("{"); // Printer::small_block
            if !list.is_empty() {
                self.space();
                for (stmt, _semi) in list {
                    self.stmt(stmt)
                }
                self.offset(-INDENT);
            }
            self.word("}"); // Printer::small_block
            self.end(); // Printer::expr_block
        }
        fn id(&mut self, id: &ID) {
            match id {
                ID::AnyIdent(ident) => self.ident(ident),
                ID::AnyLit(lit) => self.lit(lit),
                ID::Html(HtmlString { lt: _, stream }) => self.word(format!("< {}", stream)),
                ID::DotInt(DotInt { dot: _, int }) => {
                    self.word(".");
                    self.word(int.token().to_string())
                }
            }
        }
        fn node_id(&mut self, NodeId { id, port }: &NodeId) {
            self.id(id);
            if let Some(port) = port {
                self.word(":");
                match port {
                    Port::ID { colon: _, id } => self.id(id),
                    Port::Compass { colon: _, compass } => self.word(compass.to_string()),
                    Port::IDAndCompass {
                        colon1: _,
                        id,
                        colon2: _,
                        compass,
                    } => {
                        self.id(id);
                        self.word(":");
                        self.word(compass.to_string())
                    }
                }
            }
        }
        fn attributes(&mut self, Attributes { lists }: &Attributes) {
            for attr_list in lists {
                self.attr_list(attr_list)
            }
        }
        fn attr_list(
            &mut self,
            AttrList {
                bracket_token: _,
                kvs,
            }: &AttrList,
        ) {
            self.word("["); // Printer::expr_array
            self.nbsp();
            self.cbox(INDENT);
            self.zerobreak();
            for element in kvs.iter().delimited() {
                self.attr_kv(&element);
                self.trailing_comma(element.is_last);
            }
            self.offset(-INDENT);
            self.end();
            self.nbsp();
            self.word("]");
        }
        fn attr_kv(
            &mut self,
            AttrKV {
                left,
                eq_token: _,
                right,
                trailing: _,
            }: &AttrKV,
        ) {
            self.id(left);
            self.word(" = ");
            self.id(right);
        }
    }
}
