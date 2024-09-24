use super::DocLink;
use std::{cell::RefCell, rc::Rc};

/// A custom tree structure to help constructing the links in the sidebar for documentation.
/// The goal is to end up generating a vector of pre-constructed elements that are simple to handle
/// in the HTML template, but still enforces a visual hierarchy that helps readability of modules.
///
/// So for example the following:
/// - aiken/cbor
/// - aiken/list
/// - aiken/math
/// - aiken/math/rational
/// - aiken/primitive
/// - aiken/primitive/bytearray
/// - aiken/primitive/integer
/// - cardano/asset
/// - cardano/certificate
///
/// is nicely turned into:
///
/// aiken
///   /cbor
///   /list
///   /math
///     /rational
///   /primitive
///     /bytearray
///     /integer
///
/// cardano
///   /asset
///   /certificate
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub(crate) enum LinkTree {
    Empty,

    Leaf {
        value: String,
    },

    Node {
        prefix: String,
        separator: bool,
        children: Vec<Rc<RefCell<LinkTree>>>,
    },
}

#[allow(clippy::derivable_impls)]
impl Default for LinkTree {
    /// The intended way of creating a new empty LinkTree.
    fn default() -> LinkTree {
        LinkTree::Empty
    }
}

impl LinkTree {
    /// Convert a LinkTree into a sequence of DocLinks, ready to be displayed at the right
    /// indentation level.
    pub fn to_vec(&self) -> Vec<DocLink> {
        self.do_to_vec(&[])
    }

    pub fn insert(&mut self, module: &str) {
        /// Strip prefix and ensures to remove any leading slash "/" as well.
        fn strip_prefix(source: &str, prefix: &str) -> String {
            let result = source.strip_prefix(prefix).unwrap();
            if result.starts_with('/') {
                result.strip_prefix('/').unwrap().to_string()
            } else {
                result.to_string()
            }
        }

        match self {
            LinkTree::Empty => {
                *self = LinkTree::Leaf {
                    value: module.to_string(),
                }
            }

            LinkTree::Leaf {
                value: ref mut leaf,
                ..
            } => {
                // In case we try to insert a module that already exists, there's nothing to do.
                if module == leaf {
                    return;
                }

                let (prefix, value) = if let Some(prefix) = common_prefix(module, leaf) {
                    *leaf = strip_prefix(leaf, &prefix);
                    let value = strip_prefix(module, &prefix);
                    (prefix, value)
                } else {
                    (String::new(), module.to_string())
                };

                // When `prefix == module`, we are usually in the case where we try to insert a
                // parent (e.g. `aiken/math`) into a sub-leaf (e.g. `aiken/math/rational`). So
                // `self` must become a child node of our newly created parent.
                if prefix == module {
                    let children = vec![self.clone().into_ref()];
                    *self = LinkTree::Node {
                        // Holds a value, so separator = false
                        separator: false,
                        children,
                        prefix,
                    };

                // If `leaf.is_empty()`, we are in the case where we are inserting a sub-leaf
                // (e.g. `aiken/math/rational`) into a parent (e.g. `aiken/math`); so much that
                // we've run out of path segments to follow down. So `self` can turn into a node
                // that contains that new leaf.
                } else if leaf.is_empty() {
                    let children = vec![LinkTree::Leaf { value }.into_ref()];
                    *self = LinkTree::Node {
                        // Holds a value, so separator = false
                        separator: false,
                        children,
                        prefix,
                    };
                // Otherwise, neither one is a child of the other, so we can nest them under a node
                // with the corresponding (possibly empty) prefix.
                } else {
                    let mut children =
                        vec![self.clone().into_ref(), LinkTree::Leaf { value }.into_ref()];

                    children.sort_by(|a, b| a.borrow().path().cmp(b.borrow().path()));

                    *self = LinkTree::Node {
                        // This node is a 'separator' because it doesn't
                        // hold any value. It is just an intersection point.
                        separator: true,
                        children,
                        prefix,
                    };
                }
            }

            LinkTree::Node {
                ref mut prefix,
                ref mut children,
                ..
            } => {
                // When `module.starts_with(prefix)` is true, it means that the module being
                // inserted belong to our sub-tree. We do not know *where* exactly though, so we
                // have to find whether there's any child that continues the path. If node, we can
                // add it to our children.
                if module.starts_with(prefix.as_str()) {
                    let module = strip_prefix(module, prefix);

                    for child in children.iter_mut() {
                        if common_prefix(child.borrow().path(), module.as_str()).is_some() {
                            return child.borrow_mut().insert(module.as_str());
                        }
                    }

                    children.push(
                        LinkTree::Leaf {
                            value: module.to_string(),
                        }
                        .into_ref(),
                    );

                    children.sort_by(|a, b| a.borrow().path().cmp(b.borrow().path()));
                // Otherwise, we make it a neighbor that shares no common prefix.
                } else {
                    let new_prefix = common_prefix(prefix, module).unwrap_or_default();

                    *prefix = strip_prefix(prefix, &new_prefix);

                    let mut children = vec![
                        self.clone().into_ref(),
                        LinkTree::Leaf {
                            value: strip_prefix(module, &new_prefix),
                        }
                        .into_ref(),
                    ];

                    children.sort_by(|a, b| a.borrow().path().cmp(b.borrow().path()));

                    *self = LinkTree::Node {
                        // This node is a 'separator' because it doesn't
                        // hold any value. It is just an intersection point.
                        separator: true,
                        prefix: new_prefix,
                        children,
                    };
                }
            }
        }
    }

    fn do_to_vec(&self, path: &[&str]) -> Vec<DocLink> {
        let mk_path = |value: &str| {
            [
                path.join("/").as_str(),
                if path.is_empty() { "" } else { "/" },
                value,
                ".html",
            ]
            .concat()
        };

        match self {
            LinkTree::Empty => vec![],

            LinkTree::Leaf { value } => {
                let last_ix = value.split('/').count();
                let module_path = mk_path(value);
                value
                    .split('/')
                    .enumerate()
                    .map(|(offset, segment)| {
                        if offset == last_ix - 1 {
                            DocLink {
                                indent: path.len() + offset,
                                name: segment.to_string(),
                                path: module_path.to_string(),
                            }
                        } else {
                            DocLink {
                                indent: path.len() + offset,
                                name: segment.to_string(),
                                path: String::new(),
                            }
                        }
                    })
                    .collect::<Vec<_>>()
            }

            LinkTree::Node {
                children,
                prefix,
                separator,
            } => {
                let mut links = if prefix.is_empty() {
                    vec![]
                } else {
                    vec![DocLink {
                        indent: path.len(),
                        name: prefix.clone(),
                        path: if *separator {
                            String::new()
                        } else {
                            mk_path(prefix)
                        },
                    }]
                };

                let mut next = vec![];
                for segment in path {
                    next.push(*segment);
                }
                if !prefix.is_empty() {
                    next.push(prefix);
                }

                links.extend(
                    children
                        .iter()
                        .flat_map(|child| child.borrow().do_to_vec(&next[..])),
                );

                links
            }
        }
    }

    fn into_ref(self) -> Rc<RefCell<LinkTree>> {
        Rc::new(RefCell::new(self))
    }

    fn path(&self) -> &str {
        match self {
            LinkTree::Empty => "",
            LinkTree::Leaf { ref value, .. } => value.as_str(),
            LinkTree::Node { ref prefix, .. } => prefix.as_str(),
        }
    }
}

#[test]
fn link_tree_1() {
    let mut tree = LinkTree::default();
    tree.insert("foo");
    assert_eq!(
        tree.to_vec(),
        vec![DocLink {
            indent: 0,
            name: "foo".to_string(),
            path: "foo.html".to_string(),
        }]
    )
}

#[test]
fn link_tree_2() {
    let mut tree = LinkTree::default();
    tree.insert("foo");
    tree.insert("bar");
    assert_eq!(
        tree.to_vec(),
        vec![
            DocLink {
                indent: 0,
                name: "bar".to_string(),
                path: "bar.html".to_string(),
            },
            DocLink {
                indent: 0,
                name: "foo".to_string(),
                path: "foo.html".to_string(),
            }
        ]
    )
}

#[test]
fn link_tree_3() {
    let mut tree = LinkTree::default();
    tree.insert("aiken/list");
    tree.insert("aiken/bytearray");
    assert_eq!(
        tree.to_vec(),
        vec![
            DocLink {
                indent: 0,
                name: "aiken".to_string(),
                path: String::new(),
            },
            DocLink {
                indent: 1,
                name: "bytearray".to_string(),
                path: "aiken/bytearray.html".to_string(),
            },
            DocLink {
                indent: 1,
                name: "list".to_string(),
                path: "aiken/list.html".to_string(),
            },
        ]
    )
}

#[test]
fn link_tree_4() {
    let mut tree = LinkTree::default();
    tree.insert("aiken/cbor");
    tree.insert("aiken/math/rational");
    tree.insert("aiken/math");
    tree.insert("cardano/foo");
    assert_eq!(
        tree.to_vec(),
        vec![
            DocLink {
                indent: 0,
                name: "aiken".to_string(),
                path: String::new(),
            },
            DocLink {
                indent: 1,
                name: "cbor".to_string(),
                path: "aiken/cbor.html".to_string(),
            },
            DocLink {
                indent: 1,
                name: "math".to_string(),
                path: "aiken/math.html".to_string(),
            },
            DocLink {
                indent: 2,
                name: "rational".to_string(),
                path: "aiken/math/rational.html".to_string(),
            },
            DocLink {
                indent: 0,
                name: "cardano".to_string(),
                path: "".to_string(),
            },
            DocLink {
                indent: 1,
                name: "foo".to_string(),
                path: "cardano/foo.html".to_string(),
            }
        ]
    )
}

#[test]
fn link_tree_5() {
    let mut tree = LinkTree::default();
    tree.insert("cardano/foo");
    tree.insert("cardano");
    tree.insert("aiken/cbor");
    tree.insert("aiken/math");
    tree.insert("aiken/math/rational");
    assert_eq!(
        tree.to_vec(),
        vec![
            DocLink {
                indent: 0,
                name: "aiken".to_string(),
                path: String::new(),
            },
            DocLink {
                indent: 1,
                name: "cbor".to_string(),
                path: "aiken/cbor.html".to_string(),
            },
            DocLink {
                indent: 1,
                name: "math".to_string(),
                path: "aiken/math.html".to_string(),
            },
            DocLink {
                indent: 2,
                name: "rational".to_string(),
                path: "aiken/math/rational.html".to_string(),
            },
            DocLink {
                indent: 0,
                name: "cardano".to_string(),
                path: "cardano.html".to_string(),
            },
            DocLink {
                indent: 1,
                name: "foo".to_string(),
                path: "cardano/foo.html".to_string(),
            }
        ]
    )
}

#[test]
fn link_tree_6() {
    let mut tree = LinkTree::default();
    tree.insert("cardano/address");
    tree.insert("cardano/address/credential");
    tree.insert("cardano/address/credential");
    tree.insert("cardano/assets");
    tree.insert("cardano/assets");
    tree.insert("cardano/certificate");
    assert_eq!(
        tree.to_vec(),
        vec![
            DocLink {
                indent: 0,
                name: "cardano".to_string(),
                path: "".to_string(),
            },
            DocLink {
                indent: 1,
                name: "address".to_string(),
                path: "cardano/address.html".to_string(),
            },
            DocLink {
                indent: 2,
                name: "credential".to_string(),
                path: "cardano/address/credential.html".to_string(),
            },
            DocLink {
                indent: 1,
                name: "assets".to_string(),
                path: "cardano/assets.html".to_string(),
            },
            DocLink {
                indent: 1,
                name: "certificate".to_string(),
                path: "cardano/certificate.html".to_string(),
            },
        ]
    )
}

/// Find the common module prefix between two module path, if any.
///
/// ```
/// use aiken_project::docs::link_tree::common_prefix;
///
/// assert_eq!(
///   common_prefix("foo", "foo"),
///   Some("foo".to_string()),
/// );
///
/// assert_eq!(
///   common_prefix("aiken/list", "aiken/bytearray"),
///   Some("aiken".to_string()),
/// );
///
/// assert_eq!(
///   common_prefix("aiken/list", "cardano/asset"),
///   None,
/// );
/// ```
pub fn common_prefix(left: &str, right: &str) -> Option<String> {
    let mut prefix = vec![];

    for (left, right) in left.split('/').zip(right.split('/')) {
        if !left.is_empty() && left == right {
            prefix.push(left);
        } else {
            break;
        }
    }

    if prefix.is_empty() {
        None
    } else {
        Some(prefix.join("/"))
    }
}

#[test]
fn common_prefix_1() {
    assert_eq!(common_prefix("", ""), None)
}

#[test]
fn common_prefix_2() {
    assert_eq!(common_prefix("foo", "bar"), None)
}

#[test]
fn common_prefix_3() {
    assert_eq!(common_prefix("foo", "foo"), Some("foo".to_string()))
}

#[test]
fn common_prefix_4() {
    assert_eq!(common_prefix("foo", ""), None)
}

#[test]
fn common_prefix_5() {
    assert_eq!(
        common_prefix("foo/bar", "foo/bar"),
        Some("foo/bar".to_string())
    )
}

#[test]
fn common_prefix_6() {
    assert_eq!(
        common_prefix("foo/bar", "foo/bar/baz"),
        Some("foo/bar".to_string())
    )
}

#[test]
fn common_prefix_7() {
    assert_eq!(
        common_prefix("foo/bar", "foo/wow/baz"),
        Some("foo".to_string())
    )
}

#[test]
fn common_prefix_8() {
    assert_eq!(
        common_prefix("foo/bar/baz", "foo/wow/baz"),
        Some("foo".to_string())
    )
}
