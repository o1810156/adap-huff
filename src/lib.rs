use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::{Rc, Weak};
use std::str::FromStr;

enum NodeType<T>
where
    T: Display + FromStr + PartialEq + Eq + Hash + Clone,
{
    Leaf { chara: T },
    Internal,
    Zero,
    EOF,
}

impl<T> Display for NodeType<T>
where
    T: Display + FromStr + PartialEq + Eq + Hash + Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Leaf { chara } => write!(f, "{}", chara),
            Self::Internal => write!(f, ""),
            Self::Zero => write!(f, "0-Node"),
            Self::EOF => write!(f, "EOF"),
        }
    }
}

type RRNode<T> = Rc<RefCell<Node<T>>>;
type WRNode<T> = Weak<RefCell<Node<T>>>;

struct Node<T>
where
    T: Display + FromStr + PartialEq + Eq + Hash + Clone,
{
    id: usize,
    item: NodeType<T>,
    count: usize,
    parent: WRNode<T>,
    // 両方存在するか両方Noneであるかの
    // どちらかしかありえないが、今回の実装方法では
    // それを容易に確認することができない
    left: WRNode<T>,
    right: WRNode<T>,
}

impl<T> PartialEq for Node<T>
where
    T: Display + FromStr + PartialEq + Eq + Hash + Clone,
{
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T> Eq for Node<T> where T: Display + FromStr + PartialEq + Eq + Hash + Clone {}

impl<T> Display for Node<T>
where
    T: Display + FromStr + PartialEq + Eq + Hash + Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let left_w = self.left.upgrade();
        let right_w = self.right.upgrade();
        let mut flag = false;
        let l = if let Some(l) = left_w {
            flag = true;
            format!("{}", l.borrow())
        } else {
            "".to_string()
        };
        let r = if let Some(r) = right_w {
            flag = true;
            format!("{}", r.borrow())
        } else {
            "".to_string()
        };
        write!(
            f,
            "id({}) count({}){}{}",
            self.id,
            self.count,
            format!(" {} ", self.item),
            if flag {
                format!("{{ {}, {} }}", l, r)
            } else {
                "".to_string()
            }
        )
    }
}

impl<T> Node<T>
where
    T: Display + FromStr + PartialEq + Eq + Hash + Clone,
{
    fn new_leaf(id: usize, chara: T) -> Self {
        let item = NodeType::Leaf { chara };
        Node {
            id,
            item,
            count: 0,
            parent: Weak::new(),
            left: Weak::new(),
            right: Weak::new(),
        }
    }

    fn new_internal(id: usize) -> Self {
        Node {
            id,
            item: NodeType::Internal,
            count: 0,
            parent: Weak::new(),
            left: Weak::new(),
            right: Weak::new(),
        }
    }

    fn new_eof(id: usize) -> Self {
        Node {
            id, // maybe 1
            item: NodeType::EOF,
            count: 0,
            parent: Weak::new(),
            left: Weak::new(),
            right: Weak::new(),
        }
    }

    fn new_zero(id: usize) -> Self {
        Node {
            id, // maybe 0
            item: NodeType::Zero,
            count: 0,
            parent: Weak::new(),
            left: Weak::new(),
            right: Weak::new(),
        }
    }

    fn is_root(&self) -> bool {
        self.parent.upgrade().is_none()
    }

    /*
    fn is_leaf(&self) -> bool {
        match self.item {
            NodeType::Leaf {_} => true,
            _ => false,
        }
    }
     */
    fn get_chara(&self) -> Option<T> {
        match self.item {
            NodeType::Leaf { ref chara } => Some(chara.clone()),
            _ => None,
        }
    }

    fn is_zero_node(&self) -> bool {
        match self.item {
            NodeType::Zero => true,
            _ => false,
        }
    }

    fn is_eof(&self) -> bool {
        match self.item {
            NodeType::EOF => true,
            _ => false,
        }
    }

    /*
    fn is_internal(&self) -> bool {
        match self.item {
            NodeType::Internal => true,
            _ => false,
        }
    }
     */

    fn get_tree_figure(&self) -> String {
        let mut tree = String::from(".");

        self.tree_rec(&mut tree, "", "");

        tree
    }

    fn tree_rec(&self, tree: &mut String, indent: &str, huff_code: &str) {
        let item = format!("{}", self.item);
        let item = item.replace("\n", "\\n");
        let item = item.replace("\"", "\\\"");
        let item = item.replace("\'", "\\\'");
        let s = format!("{}({}) {}\n", item, self.count, huff_code);
        tree.push_str(s.as_str());

        if let Some(node) = self.left.upgrade() {
            let s = format!("{}├── ", indent);
            tree.push_str(s.as_str());
            node.borrow().tree_rec(
                tree,
                format!("{}|   ", indent).as_str(),
                format!("{}0", huff_code).as_str(),
            );
        }

        if let Some(node) = self.right.upgrade() {
            let s = format!("{}└── ", indent);
            tree.push_str(s.as_str());
            node.borrow().tree_rec(
                tree,
                format!("{}    ", indent).as_str(),
                format!("{}1", huff_code).as_str(),
            );
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum HuffTreeType {
    Encoder,
    Decoder,
}

enum WaitStatus {
    Code,
    Word,
    EOF,
}

pub struct AdapHuffTree<T>
where
    T: Display + FromStr + PartialEq + Eq + Hash + Clone,
{
    tree_type: HuffTreeType,
    top: RRNode<T>,
    zero_node: RRNode<T>,
    eof_node: RRNode<T>,
    node_list: Vec<RRNode<T>>,        // Nodeを保存する実体
    char2node: HashMap<T, RRNode<T>>, // 循環参照が生まれることはないのでRcで
    word_validation: Box<dyn Fn(&Vec<char>) -> bool>,
    wait_status: WaitStatus,
    decode_stack: Vec<char>,
    default_word: T,
}

impl<T> AdapHuffTree<T>
where
    T: Display + FromStr + PartialEq + Eq + Hash + Clone,
{
    pub fn new(tree_type: HuffTreeType, default_word: T) -> Self {
        let char2node = HashMap::new();
        let zero_node = Rc::new(RefCell::new(Node::new_zero(0)));
        let eof_node = Rc::new(RefCell::new(Node::new_eof(1)));
        eof_node.borrow_mut().count = 1;
        let top = Rc::new(RefCell::new(Node::new_internal(2)));
        top.borrow_mut().count = 1;
        top.borrow_mut().left = Rc::downgrade(&zero_node);
        top.borrow_mut().right = Rc::downgrade(&eof_node);
        zero_node.borrow_mut().parent = Rc::downgrade(&top);
        eof_node.borrow_mut().parent = Rc::downgrade(&top);
        let node_list = vec![top.clone(), zero_node.clone(), eof_node.clone()];
        AdapHuffTree {
            tree_type,
            top,
            zero_node,
            eof_node,
            node_list,
            char2node,
            word_validation: Box::new(|v: &Vec<char>| v.len() == 1),
            wait_status: WaitStatus::Code,
            decode_stack: Vec::new(),
            default_word,
        }
    }

    pub fn get_tree_figure(&self) -> String {
        self.top.borrow().get_tree_figure()
    }

    pub fn get_entropy(&self) -> f64 {
        let count = self.top.borrow().count as f64 - 1f64; // remove EOF
        if count == 0f64 {
            return 0f64;
        }

        let s: f64 = self
            .char2node
            .iter()
            .map(|(_, node)| {
                let p = node.borrow().count as f64 / count;
                -p.log2() * p
            })
            .sum();

        s
    }

    pub fn get_avg_len(&self) -> f64 {
        let count = self.top.borrow().count as f64 - 1f64; // remove EOF
        if count == 0f64 {
            return 0f64;
        }
        let s: f64 = self
            .char2node
            .iter()
            .map(|(_, rrnode)| {
                let node = rrnode.borrow();
                (Self::get_code(&node).len() as f64) * (node.count as f64 / count)
            })
            .sum();

        s
    }

    fn reset_stack(&mut self) {
        self.decode_stack = Vec::new();
    }

    // 存在 -> true
    // なかった -> false
    pub fn encode(&mut self, chara: Option<&T>) -> (Vec<u8>, bool) {
        if self.tree_type == HuffTreeType::Decoder {
            panic!("You decided to use this tree only encoding!");
        }

        // NoneをEOFとする
        if chara.is_none() {
            return (Self::get_code(&self.eof_node.borrow()), true);
        }
        let chara = chara.unwrap();

        // 所有権関連でややこしいことになる
        // どのみち落ちるのでRcコピーを使用する
        let (target_node, chara_w) = if let Some(node) = self.char2node.get(chara) {
            (node.clone(), None)
        } else {
            (self.zero_node.clone(), Some(chara.clone()))
        };

        let res = (Self::get_code(&target_node.borrow()), chara_w.is_none());

        self.update(target_node, chara_w);

        res
    }

    fn get_code(node: &Node<T>) -> Vec<u8> {
        let mut res = Vec::new();

        let parent_w = node.parent.upgrade();
        if parent_w.is_none() {
            return res;
        }
        let parent = parent_w.unwrap();

        Self::get_code_rec(node, &parent.borrow(), &mut res);

        res.reverse();
        res
    }

    fn get_code_rec(child: &Node<T>, parent: &Node<T>, res: &mut Vec<u8>) {
        let left_w = parent.left.upgrade();
        let right_w = parent.right.upgrade();
        let b = if left_w.is_some() && &left_w.unwrap().borrow() as &Node<T> == child {
            0
        } else {
            if right_w.is_none() || &right_w.unwrap().borrow() as &Node<T> != child {
                panic!(
                    "Panic @ get_code_rec: Something wrong with tree struct.\n{}",
                    parent
                );
            }
            1
        };

        res.push(b);
        let grandparent_w = parent.parent.upgrade();

        // 親が存在しない時、parentはROOTであった
        // ROOTを調べるメソッドは別で使うので用意はしてある
        if let Some(grandparent) = grandparent_w {
            Self::get_code_rec(parent, &grandparent.borrow(), res);
        }
    }

    pub fn set_word_validation(&mut self, func: Box<dyn Fn(&Vec<char>) -> bool>) {
        self.word_validation = func;
    }

    pub fn decode(&mut self, w: char) -> Option<T> {
        if self.tree_type == HuffTreeType::Encoder {
            panic!("You decided to use this tree only decoding!");
        }

        self.decode_stack.push(w);

        match self.wait_status {
            WaitStatus::Code => {
                if let Some(node) = self.get_node().upgrade() {
                    let chara_w = node.borrow().get_chara();
                    if chara_w.is_some() {
                        self.update(node, chara_w.clone());
                        self.reset_stack();
                        chara_w
                    } else if node.borrow().is_zero_node() {
                        self.wait_status = WaitStatus::Word;
                        self.reset_stack();
                        None
                    } else if node.borrow().is_eof() {
                        // self.update(node, None);
                        self.reset_stack();
                        self.wait_status = WaitStatus::EOF;
                        None
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            WaitStatus::Word => {
                if (self.word_validation)(&self.decode_stack) {
                    let word =
                        T::from_str(self.decode_stack.iter().collect::<String>().as_str()).ok();
                    let z = self.zero_node.clone();
                    self.update(z, word.clone().or(Some(self.default_word.clone())));
                    self.wait_status = WaitStatus::Code;
                    self.reset_stack();
                    word
                } else {
                    None
                }
            }
            WaitStatus::EOF => None,
        }
    }

    fn get_node(&self) -> WRNode<T> {
        let mut v = self.decode_stack.clone();
        v.reverse();

        Self::get_node_rec(self.top.clone(), &mut v)
    }

    fn get_node_rec(node: RRNode<T>, stack: &mut Vec<char>) -> WRNode<T> {
        let next_node = match stack.pop() {
            Some('0') => node.borrow().left.clone(),
            Some('1') => node.borrow().right.clone(),
            Some(c) => panic!("Panic @ get_node_rec: Invalid Code. {}", c),
            None => return Rc::downgrade(&node),
        };

        if let Some(rrnode) = next_node.upgrade() {
            Self::get_node_rec(rrnode, stack)
        } else {
            Rc::downgrade(&node)
        }
    }

    fn rrnode_eq(a: &RRNode<T>, b: &RRNode<T>) -> bool {
        &a.borrow() as &Node<T> == &b.borrow() as &Node<T>
    }

    fn update(&mut self, mut target_node: RRNode<T>, chara_w: Option<T>) {
        // z-nodeでは一度しか実行されないはず
        if !target_node.borrow().is_root() && target_node.borrow().is_zero_node() {
            self.node_list
                .iter()
                .for_each(|node| node.borrow_mut().id += 2);

            let mut new_zero = Node::new_zero(0);
            new_zero.parent = Rc::downgrade(&self.zero_node);
            let new_zero = Rc::new(RefCell::new(new_zero));

            // z-nodeの場合 chara_w はSomeである必要がある
            let chara = chara_w.expect("Panic @ update: target is zero_node but no chara.");
            let mut new_node = Node::new_leaf(1, chara.clone());
            new_node.count = 1;
            new_node.parent = Rc::downgrade(&self.zero_node);
            let new_node = Rc::new(RefCell::new(new_node));
            self.char2node.insert(chara, new_node.clone());

            self.node_list.push(new_node.clone());
            self.node_list.push(new_zero.clone());

            let z = self.zero_node.clone();
            {
                let mut z_b = z.borrow_mut();
                z_b.item = NodeType::Internal;
                z_b.left = Rc::downgrade(&new_zero);
                z_b.right = Rc::downgrade(&new_node);
            }

            target_node = z; // nop
            self.zero_node = new_zero;
        }

        while !target_node.borrow().is_root() {
            let parent = target_node.borrow().parent.upgrade().unwrap();
            // つねに気をつけることにしても良いが、効率化を図るためにこのようにした
            let careful_parent = parent
                .borrow()
                .left
                .upgrade()
                .unwrap()
                .borrow()
                .is_zero_node()
                || parent
                    .borrow()
                    .right
                    .upgrade()
                    .unwrap()
                    .borrow()
                    .is_zero_node();
            let p = parent.clone();
            let judge: Box<dyn Fn(&RRNode<T>, usize, usize) -> bool> = if careful_parent {
                Box::new(|node, count, maxid| {
                    let n_b = node.borrow();
                    n_b.count == count && !Self::rrnode_eq(node, &p) && n_b.id > maxid
                })
            } else {
                Box::new(|node, count, maxid| {
                    let n_b = node.borrow();
                    n_b.count == count && n_b.id > maxid
                })
            };

            let mut max_node = target_node.clone();
            let count = target_node.borrow().count;
            let mut maxid = target_node.borrow().id;
            for node in self.node_list.iter() {
                if judge(&node, count, maxid) {
                    maxid = node.borrow().id;
                    max_node = node.clone();
                }
            }

            if !Self::rrnode_eq(&max_node, &target_node) {
                let p1 = max_node
                    .borrow()
                    .parent
                    .upgrade()
                    .expect("Panic @ update: ROOT node is selected as partner.");
                let p2 = target_node
                    .borrow()
                    .parent
                    .upgrade()
                    .expect("Panic @ update: target_node is root node.");

                let id = target_node.borrow().id;

                target_node.borrow_mut().id = max_node.borrow().id;
                max_node.borrow_mut().id = id;

                target_node.borrow_mut().parent = Rc::downgrade(&p1);
                max_node.borrow_mut().parent = Rc::downgrade(&p2);

                let mut p1_m = p1.borrow_mut();
                if Self::rrnode_eq(&p1_m.left.upgrade().unwrap(), &max_node) {
                    p1_m.left = Rc::downgrade(&target_node);
                } else {
                    p1_m.right = Rc::downgrade(&target_node);
                }

                let mut p2_m = match p2.try_borrow_mut() {
                    Ok(m) => m,
                    Err(_) => p1_m, // 同じ親だった
                };
                if Self::rrnode_eq(&p2_m.left.upgrade().unwrap(), &target_node) {
                    p2_m.left = Rc::downgrade(&max_node);
                } else {
                    p2_m.right = Rc::downgrade(&max_node);
                }
            }

            target_node.borrow_mut().count += 1;
            let tmp = target_node.borrow().parent.upgrade().unwrap();
            target_node = tmp;
        }

        self.top.borrow_mut().count += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let data = "aa_bb_cccc";

        let mut encode_tree = AdapHuffTree::new(HuffTreeType::Encoder, '#');

        println!("{}", encode_tree.get_tree_figure());

        let code = data
            .chars()
            .map(|c| {
                let (v, b) = encode_tree.encode(Some(&c));
                let mut res = v
                    .into_iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join("");
                if !b {
                    res.push_str(format!("{}", c).as_str());
                }
                println!("{}: {}\n", c, res);
                println!("{}", encode_tree.get_tree_figure());
                res
            })
            .collect::<Vec<_>>()
            .join("");

        let (v, _) = encode_tree.encode(None);
        let res = v
            .into_iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join("");

        let code = format!("{}{}", code, res);

        println!("encoded:\n{}", code);

        let code = format!("{}01010101010", code);
        // イタズラ
        println!("mischief:\n{}", code);

        let mut decode_tree = AdapHuffTree::new(HuffTreeType::Decoder, '#');

        let decoded_data = code
            .chars()
            .filter_map(|w| {
                let chara_w = decode_tree.decode(w);
                if chara_w.is_some() {
                    println!("{}", decode_tree.get_tree_figure());
                }
                chara_w
            })
            .collect::<String>();

        println!("decoded:\n{}", decoded_data);

        assert_eq!(decoded_data, data);
    }

    #[test]
    fn test2() {
        /* let data = "\
        The Zen of Python, by Tim Peters

        Beautiful is better than ugly.
        Explicit is better than implicit.
        Simple is better than complex.
        Complex is better than complicated.
        Flat is better than nested.
        Sparse is better than dense.
        Readability counts.
        Special cases aren't special enough to break the rules.
        Although practicality beats purity.
        Errors should never pass silently.
        Unless explicitly silenced.
        In the face of ambiguity, refuse the temptation to guess.
        There should be one-- and preferably only one --obvious way to do it.
        Although that way may not be obvious at first unless you're Dutch.
        Now is better than never.
        Although never is often better than *right* now.
        If the implementation is hard to explain, it's a bad idea.
        If the implementation is easy to explain, it may be a good idea.
        Namespaces are one honking great idea -- let's do more of those!
        ";*/
        let data = "aa_bb_cccc";

        let mut encode_tree = AdapHuffTree::new(HuffTreeType::Encoder, "#".to_string());

        println!("{}", encode_tree.get_tree_figure());

        let code = data
            .chars()
            .map(|c| {
                let c = format!("{:?}", c);
                let (v, b) = encode_tree.encode(Some(&c));
                let mut res = v
                    .into_iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join("");
                if !b {
                    res.push_str(format!("{}", c).as_str());
                }
                println!("{}: {}\n", c, res);
                println!("{}", encode_tree.get_tree_figure());
                res
            })
            .collect::<Vec<_>>()
            .join("");

        let (v, _) = encode_tree.encode(None);
        let res = v
            .into_iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join("");

        let code = format!("{}{}", code, res);

        println!("encoded:\n{}", code);

        let mut decode_tree = AdapHuffTree::new(HuffTreeType::Decoder, "#".to_string());
        decode_tree.set_word_validation(Box::new(|v: &Vec<char>| {
            v.len() >= 3 && v[0] == '\'' && v[v.len() - 2] != '\\' && v[v.len() - 1] == '\''
        }));

        let decoded_data = code
            .chars()
            .filter_map(|w| {
                let chara_w = decode_tree.decode(w);
                if chara_w.is_some() {
                    println!("{}", decode_tree.get_tree_figure());
                }
                chara_w.map(|c| {
                    /*
                    let c = c.replace("\\n", "\n");
                    let c = c.replace("\\\"", "\"");
                    let c = c.replace("\\\'", "\'");
                     */
                    char::from_str(&c[1..c.len() - 1]).unwrap()
                })
            })
            .collect::<String>();

        println!("decoded:\n{}", decoded_data);

        assert_eq!(decoded_data, data);
    }
}
