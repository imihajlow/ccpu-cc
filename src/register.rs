pub trait Register {
    fn get_register_count() -> usize;

    fn get_temp_register() -> Self;

    /**
     * Get register to store an argument of a function to call.
     */
    fn get_callee_arg(arg_index: usize) -> Option<Self>
    where
        Self: Sized;

    /**
     * Get register to store a variadic argument of a function to call.
     */
    fn get_callee_va_arg(arg_index: usize) -> Option<Self>
    where
        Self: Sized;

    /**
     * Get register with a current function's argument.
     */
    fn get_current_fn_arg(arg_index: usize) -> Option<Self>
    where
        Self: Sized;

    fn get_index(&self) -> usize;

    fn from_index(index: usize) -> Option<Self>
    where
        Self: Sized;
}
