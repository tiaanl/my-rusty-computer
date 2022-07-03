use crate::ast::Span;

#[repr(u8)]
pub enum DiagnosticKind {
    Info,
    Warning,
    Error,
}

struct Diagnostic {
    kind: DiagnosticKind,
    message: String,
    span: Span,
}

#[derive(Default)]
pub struct Diagnostics {
    diags: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn diag(&mut self, kind: DiagnosticKind, message: impl ToString, span: Span) {
        self.diags.push(Diagnostic {
            kind,
            message: message.to_string(),
            span,
        });
    }

    pub fn info(&mut self, message: impl ToString, span: Span) {
        self.diag(DiagnosticKind::Info, message, span);
    }

    pub fn warn(&mut self, message: impl ToString, span: Span) {
        self.diag(DiagnosticKind::Warning, message, span);
    }

    pub fn error(&mut self, message: impl ToString, span: Span) {
        self.diag(DiagnosticKind::Error, message, span);
    }

    pub fn print<W: std::io::Write>(
        &self,
        source: &str,
        output: &mut W,
    ) -> Result<(), std::io::Error> {
        for diag in &self.diags {
            match diag.kind {
                DiagnosticKind::Info => writeln!(output, "INFO: {}", &diag.message)?,
                DiagnosticKind::Warning => writeln!(output, "WARNING: {}", &diag.message)?,
                DiagnosticKind::Error => writeln!(output, "ERROR: {}", &diag.message)?,
            }

            print_source_line(output, source, &diag.span, None)?;
        }

        Ok(())
    }
}

fn print_source_line<W: std::io::Write>(
    output: &mut W,
    source: &str,
    span: &Span,
    path: Option<&str>,
) -> Result<(), std::io::Error> {
    let prev_new_line = if let Some(found) = source[..span.start].rfind('\n') {
        found + 1
    } else {
        0
    };

    let next_new_line = if let Some(found) = source[span.start..].find('\n') {
        span.start + found
    } else {
        source.len()
    };

    let fragment = &source[prev_new_line..next_new_line];

    let line = source[0..span.start].matches('\n').count() + 1;
    let column = span.start - prev_new_line;

    if let Some(path) = path {
        writeln!(output, "{}:{}:{}", path, line, column + 1)?;
    }

    writeln!(output, "{}", fragment)?;
    for _ in 0..column {
        write!(output, " ")?;
    }
    let end = if span.start == span.end {
        span.end + 1
    } else {
        span.end
    };
    for _ in span.start..end {
        write!(output, "^")?;
    }
    writeln!(output)
}

#[cfg(test)]
mod tests {
    use crate::diagnostics::Diagnostics;

    macro_rules! assert_print_output {
        ($diags:expr, $source:expr, $expected:literal) => {{
            let mut out = Vec::new();
            $diags.print($source, &mut out).unwrap();
            assert_eq!(
                unsafe { String::from_utf8_unchecked(out).as_str() },
                $expected,
            )
        }};
    }

    #[test]
    fn basic() {
        const SOURCE: &str = "This is the source";
        let mut diags = Diagnostics::default();

        diags.info("This is an info", 5..7);
        diags.warn("This is a warning", 8..11);
        diags.error("This is an error", 12..18);

        diags.print(SOURCE, &mut std::io::stdout()).unwrap();

        assert_print_output!(
            diags,
            SOURCE,
            "INFO: This is an info\nThis is the source\n     ^^\nWARNING: This is a warning\nThis is the source\n        ^^^\nERROR: This is an error\nThis is the source\n            ^^^^^^\n"
        );
    }
}
