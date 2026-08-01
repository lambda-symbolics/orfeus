//! Native acceleration boundary for Orfeus.

/// Return the C ABI version implemented by this library.
#[unsafe(no_mangle)]
pub extern "C" fn orfeus_bridge_abi_version() -> u32 {
    1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reports_current_abi_version() {
        assert_eq!(orfeus_bridge_abi_version(), 1);
    }
}
