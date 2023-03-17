use super::new_parser::Span;

impl Span {
    pub fn error_message(&self) {
        let (source, filename) = match self {
            Span::Empty => return,
            Span::Eof { source, filename } => (source, filename),
            Span::Char {
                source, filename, ..
            } => (source, filename),
            Span::Range {
                source, filename, ..
            } => (source, filename),
        };
        let bytes = source.as_bytes();
    }
}

/*use super::parse::{Span, SpanInfo};

impl<T> Span<T> {
    pub fn exit_with_message(&self, source: &str, msg: &str) -> ! {
        match &self.info {
            None => {
                eprintln!("Error with generated AST! ({})", source);
                eprintln!("  {}", msg);
                std::process::exit(1);
            }
            Some(info) => info.exit_with_message(source, msg),
        }
    }
}

impl SpanInfo {
    pub fn exit_with_message(&self, source: &str, msg: &str) -> ! {
        eprintln!("Error in {}:", self.filename);
        let total_lines = source.lines().count();
        let source_lines = source.lines();
        let line_number = if self.eof {
            total_lines
        } else {
            self.line.unwrap()
        };
        let start = if line_number >= 2 { line_number - 1 } else { 1 };
        for (i, line) in source_lines.skip(start - 1).take(3).enumerate() {
            let cur_line = start + i;
            eprintln!(
                "{:>3} {} {}",
                cur_line,
                if cur_line == start + 1 { ">>" } else { "  " },
                line
            );
        }
        eprintln!("  {}", msg);
        std::process::exit(1);
    }
}
*/
