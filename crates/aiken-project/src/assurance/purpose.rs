use super::{
    Property, Target, TargetKind,
    lean::{Lexer, Token},
};
use aiken_ual::Purpose;
use std::{collections::BTreeMap, iter::Peekable};

/// The V3 purpose selects a calling convention, not a restriction on the context.
pub(super) fn purpose(handler: Option<&str>) -> Purpose {
    match handler {
        None | Some("spend" | "else") => Purpose::Spend,
        Some("mint") => Purpose::Mint,
        Some("withdraw") => Purpose::Withdraw,
        Some("publish") => Purpose::Publish,
        Some("vote") => Purpose::Vote,
        Some("propose") => Purpose::Propose,
        Some(_) => unreachable!("handler scopes were validated during resolution"),
    }
}

fn premise(handler: &str) -> &'static str {
    // The pinned consumer accepts bracketed binders, but rejects Lean's
    // unbracketed existential-binder syntax node. Infer types from ScriptInfo.
    match handler {
        "spend" => "∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d",
        "mint" => "∃ (cs : _), ctx.scriptContextScriptInfo = MintingScript cs",
        "withdraw" => "∃ (cred : _), ctx.scriptContextScriptInfo = RewardingScript cred",
        "publish" => "∃ (i : _) (cert : _), ctx.scriptContextScriptInfo = CertifyingScript i cert",
        "vote" => "∃ (voter : _), ctx.scriptContextScriptInfo = VotingScript voter",
        "propose" => "∃ (i : _) (pp : _), ctx.scriptContextScriptInfo = ProposingScript i pp",
        _ => unreachable!("only the six defined handlers have positive premises"),
    }
}

pub(super) fn lower(
    property: &Property,
    targets: &BTreeMap<String, Target>,
) -> Result<String, String> {
    let mut condition = String::new();
    for scope in &property.scopes {
        let Some(handler) = scope.handler.as_deref() else {
            continue;
        };
        if !condition.is_empty() {
            condition.push_str(" ∧ ");
        }
        condition.push('(');
        if handler == "else" {
            let target = targets
                .get(&scope.target)
                .ok_or_else(|| format!("missing validator target '{}'", scope.target))?;
            let TargetKind::Validator { handlers } = &target.kind else {
                return Err(format!("target '{}' is not a validator", scope.target));
            };
            let mut first = true;
            for handler in handlers.iter().filter(|handler| handler.as_str() != "else") {
                if !first {
                    condition.push_str(" ∧ ");
                }
                first = false;
                condition.push_str("¬ (");
                condition.push_str(premise(handler));
                condition.push(')');
            }
            if first {
                condition.push_str("True");
            }
        } else {
            condition.push_str(premise(handler));
        }
        condition.push(')');
    }
    if condition.is_empty() {
        return Ok(property.body.clone());
    }

    let insertion = context_binder_end(&property.body)?;
    // Keep every original byte, including comments. A newline protects the closing
    // parenthesis from a trailing line comment in the user's proposition.
    Ok(format!(
        "{} ({condition}) → ({}\n)",
        &property.body[..insertion],
        &property.body[insertion..]
    ))
}

const BINDER_ERROR: &str = "handler-scoped properties require an explicit outer ∀/forall binder named 'ctx'; use '∀ (params : Params) (ctx : ScriptContext), ...' or chained forall binders; unsupported binder syntax must be rewritten to this form";

/// A small binder-prefix recognizer, not a Lean parser. Types are opaque balanced
/// token sequences; unparenthesized terms with their own commas are unsupported.
/// No context name is inferred from identifiers in a type or in the proposition.
struct Binders<'a> {
    source: &'a str,
    tokens: Peekable<Lexer<'a>>,
}

impl<'a> Binders<'a> {
    fn peek(&mut self) -> Option<&'a str> {
        self.tokens.peek().map(|token| {
            let (Token::Name(span) | Token::Other(span)) = token;
            &self.source[span.start..span.end]
        })
    }

    fn take(&mut self) -> Option<(&'a str, usize)> {
        self.tokens.next().map(|token| {
            let (Token::Name(span) | Token::Other(span)) = token;
            (&self.source[span.start..span.end], span.end)
        })
    }

    fn names(&mut self, context_count: &mut usize) -> Result<(), String> {
        let mut count = 0;
        while let Some(Token::Name(span)) = self.tokens.peek() {
            let name = &self.source[span.start..span.end];
            if matches!(name, "forall" | "fun" | "let" | "by" | "match")
                || (!name.starts_with('«') && name.contains('.'))
            {
                return Err(BINDER_ERROR.to_string());
            }
            if matches!(name, "ctx" | "«ctx»") {
                *context_count += 1;
            }
            count += 1;
            self.tokens.next();
        }
        if count == 0 {
            return Err(BINDER_ERROR.to_string());
        }
        Ok(())
    }

    fn ty(&mut self, stop: &str) -> Result<(), String> {
        let mut closes = Vec::new();
        let mut nonempty = false;
        while let Some(token) = self.peek() {
            if closes.is_empty() && token == stop {
                return if nonempty {
                    Ok(())
                } else {
                    Err(BINDER_ERROR.to_string())
                };
            }
            match token {
                "=" if !nonempty => return Err(BINDER_ERROR.to_string()),
                "(" => closes.push(")"),
                "{" => closes.push("}"),
                "[" => closes.push("]"),
                "⦃" => closes.push("⦄"),
                "⟨" => closes.push("⟩"),
                ")" | "}" | "]" | "⦄" | "⟩" => {
                    if closes.pop() != Some(token) {
                        return Err(BINDER_ERROR.to_string());
                    }
                }
                "," | ":" | "∀" | "forall" | "fun" | "λ" | "let" | "by" | "match"
                    if closes.is_empty() =>
                {
                    return Err(BINDER_ERROR.to_string());
                }
                _ => {}
            }
            nonempty = true;
            self.take();
        }
        Err(BINDER_ERROR.to_string())
    }

    fn quantifier(&mut self, context_count: &mut usize) -> Result<usize, String> {
        self.take(); // ∀ or forall, checked by the caller.
        let mut nonempty = false;
        loop {
            match self.peek() {
                Some(",") if nonempty => return Ok(self.take().unwrap().1),
                Some("(" | "{" | "⦃") => {
                    let close = match self.take().unwrap().0 {
                        "(" => ")",
                        "{" => "}",
                        _ => "⦄",
                    };
                    self.names(context_count)?;
                    if self.peek() == Some(":") {
                        self.take();
                        self.ty(close)?;
                    }
                    if self.take().map(|token| token.0) != Some(close) {
                        return Err(BINDER_ERROR.to_string());
                    }
                }
                Some(_) => {
                    self.names(context_count)?;
                    if self.peek() == Some(":") {
                        self.take();
                        self.ty(",")?;
                    }
                }
                None => return Err(BINDER_ERROR.to_string()),
            }
            nonempty = true;
        }
    }
}

fn context_binder_end(body: &str) -> Result<usize, String> {
    let mut binders = Binders {
        source: body,
        tokens: Lexer::new(body).peekable(),
    };
    let mut context_count = 0;
    let mut end = None;
    while matches!(binders.peek(), Some("∀" | "forall")) {
        end = Some(binders.quantifier(&mut context_count)?);
    }
    if context_count != 1 || binders.peek().is_none() {
        return Err(BINDER_ERROR.to_string());
    }
    end.ok_or_else(|| BINDER_ERROR.to_string())
}

#[cfg(test)]
mod tests {
    use super::{lower, premise, purpose};
    use crate::assurance::{ExecutionSettings, Property, ResolvedScope, Target, TargetKind};
    use aiken_lang::ast::Span;
    use aiken_ual::Purpose;
    use std::collections::BTreeMap;

    fn property(body: &str, handlers: &[Option<&str>]) -> Property {
        Property {
            id: "owner_only".to_string(),
            module: "vesting".to_string(),
            location: Span { start: 0, end: 0 },
            description: "Only the owner can spend".to_string(),
            body: body.to_string(),
            body_location: Span { start: 0, end: 0 },
            scopes: handlers
                .iter()
                .map(|handler| ResolvedScope {
                    target: "vesting__vesting".to_string(),
                    handler: handler.map(str::to_string),
                    applied_parameters: None,
                    location: Span { start: 0, end: 0 },
                })
                .collect(),
            uses: Vec::new(),
        }
    }

    fn targets(handlers: &[&str]) -> BTreeMap<String, Target> {
        let target = Target {
            id: "vesting__vesting".to_string(),
            module: "vesting".to_string(),
            name: "vesting".to_string(),
            kind: TargetKind::Validator {
                handlers: handlers.iter().map(|handler| handler.to_string()).collect(),
            },
            location: Span { start: 0, end: 0 },
            settings: ExecutionSettings {
                steps: 1000,
                semantics: "c".to_string(),
            },
        };
        BTreeMap::from([(target.id.clone(), target)])
    }

    #[test]
    fn six_literal_premises_and_purpose_selectors() {
        for (handler, selected, expected) in [
            (
                "spend",
                Purpose::Spend,
                "∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d",
            ),
            (
                "mint",
                Purpose::Mint,
                "∃ (cs : _), ctx.scriptContextScriptInfo = MintingScript cs",
            ),
            (
                "withdraw",
                Purpose::Withdraw,
                "∃ (cred : _), ctx.scriptContextScriptInfo = RewardingScript cred",
            ),
            (
                "publish",
                Purpose::Publish,
                "∃ (i : _) (cert : _), ctx.scriptContextScriptInfo = CertifyingScript i cert",
            ),
            (
                "vote",
                Purpose::Vote,
                "∃ (voter : _), ctx.scriptContextScriptInfo = VotingScript voter",
            ),
            (
                "propose",
                Purpose::Propose,
                "∃ (i : _) (pp : _), ctx.scriptContextScriptInfo = ProposingScript i pp",
            ),
        ] {
            assert_eq!(purpose(Some(handler)), selected);
            assert_eq!(premise(handler), expected);
            assert_eq!(
                lower(
                    &property("∀ (ctx : ScriptContext), P ctx", &[Some(handler)]),
                    &targets(&[])
                )
                .unwrap(),
                format!("∀ (ctx : ScriptContext), (({expected})) → ( P ctx\n)")
            );
        }
        assert_eq!(purpose(None), Purpose::Spend);
        assert_eq!(purpose(Some("else")), Purpose::Spend);
    }

    #[test]
    fn fixture_two_keeps_parameter_and_context_binders_outside_premise() {
        let body = "∀ (p : VestingParams) (ctx : ScriptContext),\n  ¬ txSignedBy p.owner ctx.scriptContextTxInfo →\n    isUnsuccessful (vesting__vesting p (toLedgerData ctx))";
        assert_eq!(
            lower(
                &property(body, &[Some("spend")]),
                &targets(&["spend", "else"])
            )
            .unwrap(),
            "∀ (p : VestingParams) (ctx : ScriptContext), ((∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d)) → (\n  ¬ txSignedBy p.owner ctx.scriptContextTxInfo →\n    isUnsuccessful (vesting__vesting p (toLedgerData ctx))\n)"
        );
    }

    #[test]
    fn fallback_negates_only_defined_handlers() {
        let prop = property("∀ ctx : ScriptContext, P ctx", &[Some("else")]);
        assert_eq!(
            lower(&prop, &targets(&["spend", "mint", "else"])).unwrap(),
            "∀ ctx : ScriptContext, ((¬ (∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d) ∧ ¬ (∃ (cs : _), ctx.scriptContextScriptInfo = MintingScript cs))) → ( P ctx\n)"
        );
        assert_eq!(
            lower(&prop, &targets(&["else"])).unwrap(),
            "∀ ctx : ScriptContext, ((True)) → ( P ctx\n)"
        );
    }

    #[test]
    fn unsuffixed_scopes_preserve_opaque_body() {
        let body = "-- opaque Lean\nlet x := 1; P x";
        assert_eq!(
            lower(&property(body, &[None]), &targets(&[])).unwrap(),
            body
        );
    }

    #[test]
    fn multiple_scopes_conjoin_inside_all_chained_binders() {
        let body = "forall (p : Params), ∀ ctx : ScriptContext, ∀ n : Nat, P p ctx n";
        assert_eq!(
            lower(
                &property(body, &[Some("spend"), None, Some("mint")]),
                &targets(&[])
            )
            .unwrap(),
            "forall (p : Params), ∀ ctx : ScriptContext, ∀ n : Nat, ((∃ (ref : _) (d : _), ctx.scriptContextScriptInfo = SpendingScript ref d) ∧ (∃ (cs : _), ctx.scriptContextScriptInfo = MintingScript cs)) → ( P p ctx n\n)"
        );
    }

    #[test]
    fn lexer_ignores_comments_literals_and_nested_commas() {
        let prefix = "-- ∀ ctx, fake\n∀ /- , /- nested ctx -/ -/ (p : Pair Nat (Fin (f \",)\" r#\",)\"# ','))) (h : (∀ n, Q n)) (ctx /- , -/ : ScriptContext),";
        let suffix = "\n P ctx -- trailing comment";
        let body = format!("{prefix}{suffix}");
        assert_eq!(
            lower(&property(&body, &[Some("mint")]), &targets(&[])).unwrap(),
            format!(
                "{prefix} ((∃ (cs : _), ctx.scriptContextScriptInfo = MintingScript cs)) → ({suffix}\n)"
            )
        );
    }

    #[test]
    fn explicit_inferred_and_escaped_context_binders_are_bound() {
        for body in [
            "∀ ctx, P ctx",
            "∀ (p : Params), forall «ctx» : ScriptContext, P ctx",
        ] {
            assert!(lower(&property(body, &[Some("spend")]), &targets(&[])).is_ok());
        }
    }

    #[test]
    fn missing_or_unsupported_context_binding_is_diagnosed() {
        for body in [
            "P ctx",
            "∀ (context : ScriptContext), P context",
            "∀ (p : F ctx), P p",
            "∀ (p : Params), ∃ ctx, P p ctx",
            "∀ (p : Params), (∀ ctx, P p ctx)",
            "∀ (p : Params), let ctx := p.ctx; P ctx",
            "∀ (p : F \"ctx\"), P p",
            "∀ (p : Params) /- (ctx : ScriptContext) -/, P p",
            "∀ (ctx : ScriptContext), ∀ ctx, P ctx",
            "∀ (ctx : ScriptContext], P ctx",
            "∀ ctx : forall n, F n, P ctx",
            "∀ (ctx : ScriptContext), -- no proposition",
            "∀ (ctx := default), P ctx",
            "∀ (ctx : ScriptContext := default), P ctx",
            "∀ [ctx : ScriptContext], P ctx",
            "(∀ (ctx : ScriptContext), P ctx)",
        ] {
            let error = lower(&property(body, &[Some("spend")]), &targets(&[])).unwrap_err();
            assert!(error.contains("binder named 'ctx'"), "{body}: {error}");
        }
    }
}
