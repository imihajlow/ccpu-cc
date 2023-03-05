use std::cmp;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;

#[derive(Clone)]
pub struct ObjectGraph<T>
where
    T: Eq + Hash + Clone,
{
    g: Graph,
    obj_to_node: HashMap<T, usize>,
    node_to_obj: Vec<T>,
}

impl<T: Eq + Hash + Clone> ObjectGraph<T> {
    pub fn new() -> Self {
        Self {
            g: Graph::new(),
            obj_to_node: HashMap::new(),
            node_to_obj: Vec::new(),
        }
    }

    pub fn add_node_unique(&mut self, obj: &T) {
        let n = self.add_or_get_node(obj);
        self.g.add_nodes_up_to(n);
    }

    pub fn add_edge(&mut self, from: &T, to: &T) {
        let n_from = self.add_or_get_node(from);
        let n_to = self.add_or_get_node(to);
        self.g.add_edge(n_from, n_to);
    }

    pub fn add_edge_unique(&mut self, from: &T, to: &T) {
        let n_from = self.add_or_get_node(from);
        let n_to = self.add_or_get_node(to);
        self.g.add_edge_unique(n_from, n_to);
    }

    pub fn has_edge(&self, from: &T, to: &T) -> bool {
        if !self.obj_to_node.contains_key(from) || !self.obj_to_node.contains_key(to) {
            false
        } else {
            let n_from = self.obj_to_node[from];
            let n_to = self.obj_to_node[to];
            self.g.has_edge(n_from, n_to)
        }
    }

    pub fn get_node_count(&self) -> usize {
        self.g.get_node_count()
    }

    pub fn get_node_index(&self, node: &T) -> Option<usize> {
        self.obj_to_node.get(node).copied()
    }

    pub fn get_object(&self, index: usize) -> Option<&T> {
        self.node_to_obj.get(index)
    }

    pub fn get_edges_from_index(&self, index: usize) -> impl Iterator<Item = usize> + '_ {
        self.g.edges[index].iter().copied()
    }

    pub fn transposed(self) -> Self {
        Self {
            g: self.g.transposed(),
            ..self
        }
    }

    /// Find strongly connected components.
    ///
    /// Returns a graph of components where each node is a set of original type.
    pub fn find_strongly_connected(&self) -> ObjectGraph<Vec<T>> {
        let (scc, m) = self.g.find_strongly_connected();
        let mut groups: Vec<Vec<T>> = vec![Vec::new(); scc.get_node_count()];
        for (my_node, scc_node) in m.into_iter().enumerate() {
            groups[scc_node].push(T::clone(&self.node_to_obj[my_node]));
        }
        let mut group_to_node: HashMap<Vec<T>, usize> = HashMap::new();
        for (scc_node, scc_val) in groups.iter().enumerate() {
            group_to_node.insert(Vec::clone(scc_val), scc_node);
        }
        ObjectGraph {
            g: scc,
            obj_to_node: group_to_node,
            node_to_obj: groups,
        }
    }

    /// Find an ordering of the vertices,
    /// such that if there's an edge from V to W,
    /// then W comes before V in the ordering.
    /// Returns None if there are loops in the graph.
    pub fn inverse_topsort(self) -> Result<Vec<T>, T> {
        self.g
            .inverse_topsort()
            .map(|v| {
                let mut r: Vec<T> = Vec::new();
                for k in v.iter() {
                    r.push(T::clone(&self.node_to_obj[*k]));
                }
                r
            })
            .map_err(|v| T::clone(&self.node_to_obj[v]))
    }

    fn add_or_get_node(&mut self, obj: &T) -> usize {
        if self.obj_to_node.contains_key(obj) {
            self.obj_to_node[obj]
        } else {
            let new_id = self.obj_to_node.len();
            self.node_to_obj.push(T::clone(obj));
            self.obj_to_node.insert(T::clone(obj), new_id);
            new_id
        }
    }
}

impl<T: Eq + Hash + Clone + fmt::Display> fmt::Display for ObjectGraph<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        for (v, ws) in self.g.edges.iter().enumerate() {
            write!(f, "{}: ", &self.node_to_obj[v])?;
            for s in ws
                .iter()
                .map(|w| format!("{}", &self.node_to_obj[*w]))
                .intersperse(", ".to_string())
            {
                f.write_str(&s)?;
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct Graph {
    edges: Vec<Vec<usize>>,
}

impl Graph {
    pub fn new() -> Self {
        Self { edges: Vec::new() }
    }

    pub fn add_nodes_up_to(&mut self, n: usize) {
        if n >= self.edges.len() {
            self.edges.resize(n + 1, Vec::new());
        }
    }

    pub fn add_edge(&mut self, from: usize, to: usize) {
        self.add_nodes_up_to(from);
        self.add_nodes_up_to(to);

        self.edges[from].push(to);
    }

    pub fn add_edge_unique(&mut self, from: usize, to: usize) {
        self.add_nodes_up_to(from);
        self.add_nodes_up_to(to);
        if !self.edges[from].contains(&to) {
            self.edges[from].push(to);
        }
    }

    pub fn get_node_count(&self) -> usize {
        self.edges.len()
    }

    pub fn has_edge(&self, from: usize, to: usize) -> bool {
        self.edges[from].contains(&to)
    }

    pub fn transposed(self) -> Graph {
        let mut r = Graph::new();
        r.edges.resize(self.edges.len(), Vec::new());
        for (from, tos) in self.edges.into_iter().enumerate() {
            for to in tos {
                r.add_edge(to, from);
            }
        }
        r
    }

    /// Find strongly connected components.
    ///
    /// Returns the graph of components and a mapping from the nodes of the original graph
    /// to the nodes of the SCC graph.
    pub fn find_strongly_connected(&self) -> (Graph, Vec<usize>) {
        // https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
        let mut stack: Vec<usize> = Vec::new();
        let mut indices: Vec<Option<usize>> = vec![None; self.edges.len()];
        let mut lowlink: Vec<usize> = vec![0; self.edges.len()];
        let mut onstack: Vec<bool> = vec![false; self.edges.len()];
        let mut index: usize = 0;
        let mut result: Vec<Vec<usize>> = Vec::new();

        fn strongconnect(
            v: usize,
            edges: &Vec<Vec<usize>>,
            indices: &mut Vec<Option<usize>>,
            lowlink: &mut Vec<usize>,
            stack: &mut Vec<usize>,
            onstack: &mut Vec<bool>,
            index: &mut usize,
            result: &mut Vec<Vec<usize>>,
        ) {
            indices[v] = Some(*index);
            lowlink[v] = *index;
            *index += 1;
            stack.push(v);
            onstack[v] = true;
            for w in edges[v].iter() {
                if indices[*w].is_none() {
                    strongconnect(*w, edges, indices, lowlink, stack, onstack, index, result);
                    lowlink[v] = cmp::min(lowlink[v], lowlink[*w]);
                } else if onstack[*w] {
                    // Successor w is in stack and hence in the current SCC
                    // If w is not on stack, then (v, w) is an edge pointing to an SCC already found and must be ignored
                    lowlink[v] = cmp::min(lowlink[v], indices[*w].unwrap());
                }
            }
            // If v is a root node, pop the stack and generate an SCC
            if lowlink[v] == indices[v].unwrap() {
                let mut r: Vec<usize> = Vec::new();
                loop {
                    let w = stack.pop().unwrap();
                    onstack[w] = false;
                    r.push(w);

                    if w == v {
                        break;
                    }
                }
                result.push(r);
            }
        }

        for i in 0..self.edges.len() {
            if indices[i].is_none() {
                strongconnect(
                    i,
                    &self.edges,
                    &mut indices,
                    &mut lowlink,
                    &mut stack,
                    &mut onstack,
                    &mut index,
                    &mut result,
                );
            }
        }
        let mut v_to_scc: Vec<usize> = vec![0; self.edges.len()];
        for (i, scc) in result.iter().enumerate() {
            for v in scc {
                v_to_scc[*v] = i;
            }
        }
        let mut result_graph = Graph::new();
        if result.len() > 0 {
            result_graph.add_nodes_up_to(result.len() - 1);
        }
        for v in 0..self.edges.len() {
            let v_scc = v_to_scc[v];
            for w in self.edges[v].iter() {
                let w_scc = v_to_scc[*w];
                if v_scc != w_scc {
                    result_graph.add_edge_unique(v_scc, w_scc);
                }
            }
        }
        (result_graph, v_to_scc)
    }

    /// Find an ordering of the vertices,
    /// such that if there's an edge from V to W,
    /// then W comes before V in the ordering.
    /// Returns None if there are loops in the graph.
    pub fn inverse_topsort(&self) -> Result<Vec<usize>, usize> {
        let n = self.edges.len();
        let mut result: Vec<usize> = Vec::new();
        let mut visited = vec![false; n];
        let mut visited_loop = vec![false; n];

        fn dfs(
            v: usize,
            edges: &Vec<Vec<usize>>,
            visited: &mut Vec<bool>,
            visited_loop: &mut Vec<bool>,
            result: &mut Vec<usize>,
        ) -> Result<(), usize> {
            if visited[v] {
                return Ok(());
            }
            if visited_loop[v] {
                return Err(v);
            }
            visited_loop[v] = true;
            for w in edges[v].iter() {
                dfs(*w, edges, visited, visited_loop, result)?;
            }
            visited_loop[v] = false;
            visited[v] = true;
            result.push(v);
            return Ok(());
        }
        for i in 0..n {
            if !visited[i] {
                dfs(i, &self.edges, &mut visited, &mut visited_loop, &mut result)?;
            }
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scc() {
        let mut g = Graph::new();
        g.add_edge(0, 1);
        g.add_edge(1, 2);
        g.add_edge(2, 3);
        g.add_edge(3, 4);
        g.add_edge(4, 0);
        g.add_edge(6, 7);
        g.add_edge(6, 8);
        let (scc, m) = g.find_strongly_connected();
        assert_eq!(scc.get_node_count(), 5);
        assert_eq!(m[0], m[1]);
        assert_eq!(m[0], m[2]);
        assert_eq!(m[0], m[3]);
        assert_eq!(m[0], m[4]);
        assert_ne!(m[5], m[0]);
        assert_ne!(m[5], m[6]);
        assert_ne!(m[5], m[7]);
        assert_ne!(m[5], m[8]);
        assert_ne!(m[6], m[0]);
        assert_ne!(m[6], m[7]);
        assert_ne!(m[6], m[8]);
        assert_ne!(m[7], m[0]);
        assert_ne!(m[7], m[8]);
        assert_ne!(m[8], m[0]);
        assert!(!scc.has_edge(m[0], m[0]));
        assert!(!scc.has_edge(m[5], m[5]));
        assert!(!scc.has_edge(m[7], m[7]));
        assert!(!scc.has_edge(m[6], m[6]));
        assert!(!scc.has_edge(m[8], m[8]));
        assert!(scc.has_edge(m[6], m[7]));
        assert!(scc.has_edge(m[6], m[8]));
    }

    #[test]
    fn test_scc_2() {
        let mut g = Graph::new();
        g.add_edge(1, 2);
        g.add_edge(3, 4);
        g.add_edge(3, 2);
        g.add_edge(4, 0);
        g.add_edge(4, 3);

        let (scc, _m) = g.find_strongly_connected();
        assert_eq!(scc.get_node_count(), 4);
    }

    #[test]
    fn test_scc_3() {
        let mut g = Graph::new();
        g.add_nodes_up_to(0);

        let (scc, _m) = g.find_strongly_connected();
        assert_eq!(scc.get_node_count(), 1);
    }

    #[test]
    fn test_scc_empty() {
        let g = Graph::new();
        let (scc, m) = g.find_strongly_connected();
        assert_eq!(scc.get_node_count(), 0);
        assert_eq!(m.len(), 0);
    }

    #[test]
    fn test_topsort_fail() {
        let mut g = Graph::new();
        g.add_edge(0, 1);
        g.add_edge(1, 0);
        assert!(g.inverse_topsort().is_err());
    }

    fn pos(v: &Vec<usize>, x: usize) -> usize {
        v.iter().position(|y| *y == x).unwrap()
    }

    #[test]
    fn test_topsort() {
        let mut g = Graph::new();
        g.add_edge(0, 4);
        g.add_edge(1, 2);
        g.add_edge(1, 3);
        g.add_edge(4, 3);
        g.add_edge(4, 1);
        let r = g.inverse_topsort().unwrap();
        assert!(r.len() == 5);
        let p0 = pos(&r, 0);
        let p1 = pos(&r, 1);
        let p2 = pos(&r, 2);
        let p3 = pos(&r, 3);
        let p4 = pos(&r, 4);
        assert!(p0 > p4);
        assert!(p1 > p2);
        assert!(p1 > p3);
        assert!(p4 > p3);
        assert!(p4 > p1);
    }
}
