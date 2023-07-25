#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct Scope(pub(self) Vec<u64>);

impl From<Vec<u64>> for Scope {
    fn from(value: Vec<u64>) -> Self {
        Self(value)
    }
}

impl Scope {
    pub fn push(&mut self, value: u64) {
        self.0.push(value);
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Find the common ancestor with the replacement,
    /// remove it from `self`, and then prepend the
    /// `replacement` to `self`.
    pub fn replace(&mut self, mut replacement: Scope) {
        let common = self.common_ancestor(&replacement);

        // we know that common will always be in the front of the
        // scope Vec so we can always drain `0..common.len()`.
        self.0.drain(0..common.0.len());

        replacement.0.extend(self.0.iter());
        self.0 = replacement.0;
    }

    pub fn common_ancestor(&self, other: &Self) -> Scope {
        let longest_length = self.0.len().max(other.0.len());

        if *self.0 == *other.0 {
            return self.clone();
        }

        for index in 0..longest_length {
            if self.0.get(index).is_none() {
                return self.clone();
            } else if other.0.get(index).is_none() {
                return other.clone();
            } else if self.0[index] != other.0[index] {
                return Scope(self.0[0..index].to_vec());
            }
        }

        Scope::default()
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use super::Scope;

    #[test]
    fn common_ancestor_equal_vecs() {
        let ancestor = Scope(vec![1, 2, 3, 4, 5, 6]);

        let descendant = Scope(vec![1, 2, 3, 4, 5, 6]);

        let result = ancestor.common_ancestor(&descendant);

        assert_eq!(result, Scope(vec![1, 2, 3, 4, 5, 6]))
    }

    #[test]
    fn common_ancestor_equal_ancestor() {
        let ancestor = Scope(vec![1, 2, 3, 4]);

        let descendant = Scope(vec![1, 2, 3, 4, 5, 6]);

        let result = ancestor.common_ancestor(&descendant);

        assert_eq!(result, Scope(vec![1, 2, 3, 4]));
    }

    #[test]
    fn common_ancestor_not_subset() {
        let ancestor = Scope(vec![1, 2, 3, 4, 5]);

        let descendant = Scope(vec![1, 2, 3, 7, 8]);

        let result = ancestor.common_ancestor(&descendant);

        assert_eq!(result, Scope(vec![1, 2, 3]));
    }

    #[test]
    fn common_ancestor_not_found() {
        let ancestor = Scope(vec![1, 2, 3, 4, 5, 6]);

        let descendant = Scope(vec![4, 5, 6]);

        let result = ancestor.common_ancestor(&descendant);

        assert_eq!(result, Scope::default());
    }

    #[test]
    fn common_ancestor_no_shared_values() {
        let ancestor = Scope(vec![1, 2, 3]);

        let descendant = Scope(vec![4, 5, 6]);

        let result = ancestor.common_ancestor(&descendant);

        assert_eq!(result, Scope::default());
    }

    #[test]
    fn replace_same_value() {
        let mut value = Scope(vec![1, 2, 3, 4, 5, 6]);

        let replacement = Scope(vec![1, 2, 3, 4, 5, 6]);

        value.replace(replacement);

        assert_eq!(value, Scope(vec![1, 2, 3, 4, 5, 6]));
    }

    #[test]
    fn replace_with_pattern() {
        let mut value = Scope(vec![1, 2, 3, 4, 5]);

        let replacement = Scope(vec![1, 2, 8, 9]);

        value.replace(replacement);

        assert_eq!(value, Scope(vec![1, 2, 8, 9, 3, 4, 5]));
    }

    #[test]
    fn replace_with_no_pattern() {
        let mut value = Scope(vec![1, 2, 3, 4, 5]);

        let replacement = Scope(vec![8, 9]);

        value.replace(replacement);

        assert_eq!(value, Scope(vec![8, 9, 1, 2, 3, 4, 5]));
    }
}
