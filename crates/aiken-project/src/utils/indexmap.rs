use indexmap::IndexMap;
use std::{collections::HashMap, hash::Hash};

pub fn as_ref_values<'a, K, V>(iter: &'a IndexMap<K, V>) -> IndexMap<&'a K, &'a V>
where
    K: Eq + Hash + Clone + 'a,
{
    let mut refs = IndexMap::new();
    for (k, v) in iter {
        refs.insert(k, v);
    }
    refs
}

pub fn as_str_ref_values<V>(iter: &'_ HashMap<String, V>) -> IndexMap<&'_ str, &'_ V> {
    let mut refs = IndexMap::new();
    for (k, v) in iter {
        refs.insert(k.as_str(), v);
    }
    refs
}
