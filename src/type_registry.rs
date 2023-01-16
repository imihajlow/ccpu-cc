use std::collections::HashMap;

use crate::ctype::{QualifiedType, TypeIdentifier};

pub struct TypeRegistry {
    structs: HashMap<TypeIdentifier, Vec<Field>>,
    unions: HashMap<TypeIdentifier, Vec<Field>>,
    aliases: HashMap<String, QualifiedType>,
    last_id: usize,
}

pub struct Field {
    name: String,
    t: QualifiedType,
}

impl TypeRegistry {
    pub fn new() -> Self {
        TypeRegistry {
            structs: HashMap::new(),
            unions: HashMap::new(),
            aliases: HashMap::new(),
            last_id: 0,
        }
    }

    pub fn add_struct(
        &mut self,
        name: Option<&str>,
        fields: Vec<Field>,
    ) -> Result<TypeIdentifier, ()> {
        let id = self.get_id(name);
        if self.structs.contains_key(&id) {
            Err(())
        } else {
            self.structs.insert(id.clone(), fields);
            Ok(id)
        }
    }

    pub fn add_union(
        &mut self,
        name: Option<&str>,
        fields: Vec<Field>,
    ) -> Result<TypeIdentifier, ()> {
        let id = self.get_id(name);
        if self.unions.contains_key(&id) {
            Err(())
        } else {
            self.unions.insert(id.clone(), fields);
            Ok(id)
        }
    }

    pub fn add_alias(&mut self, name: &str, t: QualifiedType) -> Result<(), ()> {
        if self.aliases.contains_key(name) {
            Err(())
        } else {
            self.aliases.insert(name.to_owned(), t);
            Ok(())
        }
    }

    pub fn lookup_alias(&self, name: &str) -> Option<QualifiedType> {
        self.aliases.get(name).cloned()
    }

    fn get_id(&mut self, name: Option<&str>) -> TypeIdentifier {
        if let Some(name) = name {
            TypeIdentifier::Named(name.to_owned())
        } else {
            self.last_id += 1;
            TypeIdentifier::Anon(self.last_id)
        }
    }
}
