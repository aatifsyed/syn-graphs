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
                directedness,
                id,
                brace_token: _,
                stmt_list: StmtList { stmts },
            }: &Graph,
        ) {
            self.cbox(0); // Printer::file
            self.cbox(INDENT); // Printer::item_mod
            if strict.is_some() {
                self.word("strict ");
            }
            match directedness {
                GraphDirectedness::Graph(_) => self.word("graph "),
                GraphDirectedness::Digraph(_) => self.word("digraph "),
            }
            if let Some(id) = id {
                self.id(id)
            }
            self.word(" {");
            self.hardbreak_if_nonempty();
            for (stmt, _semi) in stmts {
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
                Stmt::Attr(StmtAttr { target, attrs }) => {
                    match target {
                        AttrTarget::Graph(_) => self.word("graph "),
                        AttrTarget::Node(_) => self.word("node "),
                        AttrTarget::Edge(_) => self.word("edge "),
                    };
                    self.attrs(attrs);
                }
                Stmt::Node(StmtNode { node_id, attrs }) => {
                    self.node_id(node_id);
                    self.nbsp();
                    if let Some(attrs) = attrs {
                        self.attrs(attrs);
                    }
                }
                Stmt::Edge(StmtEdge { from, edges, attrs }) => {
                    self.edge_target(from);
                    for (directedness, to) in edges {
                        match directedness {
                            EdgeDirectedness::Directed(_) => self.word(" -> "),
                            EdgeDirectedness::Undirected(_) => self.word(" -- "),
                        }
                        self.edge_target(to)
                    }
                    if let Some(attrs) = attrs {
                        self.nbsp();
                        self.attrs(attrs);
                    }
                }
                Stmt::Subgraph(subgraph) => self.stmt_subgraph(subgraph),
            }
            self.word(";");
            self.hardbreak();
        }
        fn edge_target(&mut self, it: &EdgeTarget) {
            match it {
                EdgeTarget::Subgraph(subgraph) => self.stmt_subgraph(subgraph),
                EdgeTarget::NodeId(node_id) => self.node_id(node_id),
            }
        }
        fn stmt_subgraph(
            &mut self,
            StmtSubgraph {
                prelude,
                brace_token: _,
                statements: StmtList { stmts },
            }: &StmtSubgraph,
        ) {
            self.word("subgraph ");
            if let Some((_subgraph, Some(id))) = prelude {
                self.id(id);
                self.nbsp()
            }
            self.cbox(INDENT); // Printer::expr_block
            self.word("{"); // Printer::small_block
            if !stmts.is_empty() {
                self.space();
                for (stmt, _semi) in stmts {
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
                ID::Html(
                    html @ HtmlString {
                        lt: _,
                        stream,
                        gt: _,
                    },
                ) => match html.source() {
                    Some(source) => {
                        self.word("<");
                        self.word(source);
                        self.word(">");
                    }
                    None => self.word(format!("< {} >", stream)),
                },
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
        fn attrs(&mut self, Attrs { lists }: &Attrs) {
            for attr_list in lists {
                self.attr_list(attr_list)
            }
        }
        fn attr_list(
            &mut self,
            AttrList {
                bracket_token: _,
                assigns,
            }: &AttrList,
        ) {
            self.word("["); // Printer::expr_array
            self.nbsp();
            self.cbox(INDENT);
            self.zerobreak();
            for element in assigns.iter().delimited() {
                self.attr_assign(&element);
                self.trailing_comma(element.is_last);
            }
            self.offset(-INDENT);
            self.end();
            self.nbsp();
            self.word("]");
        }
        fn attr_assign(
            &mut self,
            AttrAssign {
                left,
                eq_token: _,
                right,
                trailing: _,
            }: &AttrAssign,
        ) {
            self.id(left);
            self.word(" = ");
            self.id(right);
        }
    }
}
