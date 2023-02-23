#[derive(Clone)]
pub struct Enum {
    values: Option<Vec<(String, i128)>>,
}

impl Enum {
    pub fn new(values: Option<Vec<(String, i128)>>) -> Self {
        Self { values }
    }

    pub fn is_complete(&self) -> bool {
        self.values.is_some()
    }
}
