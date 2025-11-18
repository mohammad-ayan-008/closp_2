use crate::statements::Statement;

pub type BlockId = usize;
pub type ValueId = usize;

pub struct BasicBlock {
    pub id: BlockId,
    pub label: String,
    pub statements: Vec<Statement>,
}
