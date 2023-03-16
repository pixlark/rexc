use super::parse::Span;

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
        let key_point = match self {
            Span::Empty => unreachable!(),
            Span::Eof { .. } => bytes.len() - 1,
            Span::Char { pos, .. } => *pos,
            Span::Range { start, .. } => *start,
        };
        let key_range_end = match self {
            Span::Range { end, .. } => *end,
            _ => key_point,
        };
        // Iterate backwards from our key point, finding the
        // N lines including/preceding that line.
        let context_lines = 2;
        let mut slice_start = key_point;
        let mut found_lines = 0;
        let mut key_line = 0;
        loop {
            // Make sure we're at a char boundary
            while slice_start > 0 && !source.is_char_boundary(slice_start) {
                slice_start -= 1;
            }
            // If we've reached the beginning, give up on finding more lines of context
            if slice_start == 0 {
                break;
            }
            // If we're at a newline, increase our count
            if bytes[slice_start] == b'\n' {
                found_lines += 1;
                key_line += 1;
            }
            // If this is the last line, retreat and break
            if found_lines == context_lines {
                key_line -= 1;
                slice_start += 1;
                while slice_start < bytes.len() - 1 && !source.is_char_boundary(slice_start) {
                    slice_start += 1;
                }
                break;
            }
            // Otherwise, decrement into the previous character
            slice_start -= 1;
        }
        // Now do the same *forwards* to find the end of our slice
        let mut slice_end = key_point;
        found_lines = 0;
        loop {
            while slice_end < bytes.len() && !source.is_char_boundary(slice_end) {
                slice_end += 1;
            }
            if slice_end == bytes.len() {
                break;
            }
            if bytes[slice_end] == b'\n' {
                found_lines += 1;
            }
            if found_lines == context_lines {
                break;
            }
            slice_end += 1;
        }
        // Find what line our slice beings at
        let mut line_count = {
            let mut line_count = 1;
            let mut u8_position = 0;
            for c in source.chars() {
                if u8_position == slice_start {
                    break;
                }
                if c == '\n' {
                    line_count += 1;
                }
                u8_position += c.len_utf8();
            }
            line_count
        };
        // Coerce that slice into a string and print!
        let context = std::str::from_utf8(&bytes[slice_start..slice_end]).unwrap();
        let mut print_cursor = slice_start;
        eprintln!("...{}", filename);
        for (i, line) in context.split_inclusive('\n').enumerate() {
            // Print source line
            eprint!(
                "{:>3} {} {}",
                line_count,
                if i == key_line { ">>" } else { "  " },
                line
            );
            if !line.ends_with('\n') {
                eprintln!();
            }
            // If this line has the error, print our underline arrows
            if i == key_line {
                // Catch up to the preamble printed before the source line
                eprint!("       ");
                for c in line.chars() {
                    // Where we print the arrow depends on if we're pointing to
                    // one char or a range of chars
                    let print_arrow = if key_point == key_range_end {
                        print_cursor == key_point
                    } else {
                        print_cursor >= key_point && print_cursor < key_range_end
                    };
                    if print_arrow {
                        eprint!("^");
                    } else {
                        eprint!(" ");
                    }
                    print_cursor += c.len_utf8();
                }
                eprintln!();
            } else {
                print_cursor += line.len();
            }
            line_count += 1;
        }
        eprintln!("...");
    }
}
