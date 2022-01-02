pub struct InterruptManager {
    /// Set to true if non-maskable interrupts are allowed at this time.
    pub allow_nmi: bool,
}

impl Default for InterruptManager {
    fn default() -> Self {
        Self { allow_nmi: true }
    }
}
