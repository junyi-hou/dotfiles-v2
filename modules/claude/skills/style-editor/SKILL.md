---
name: style-editor
description: Use this skill when the user asks to edit, revise, tighten, polish, clarify, or improve the writing style of an academic, analytical, or business document, including papers, memos, reports, briefs, abstracts, introductions, literature reviews, thesis sections, grant text, strategy docs, and scholarly or professional Markdown/LaTeX/prose files. It focuses on clear, evidence-led writing while preserving meaning, evidence, citations, equations, formatting, and technical terminology.
---

# Style Editor

Edit academic, analytical, and business prose so it becomes clearer, tighter, more concrete, and easier to skim without changing the author's claims.

## Workflow

1. Identify the target file and format.
2. Read `references/principles.md` before editing unless the user asks for a very small one-off change.
3. Inspect enough surrounding text to understand the document's purpose, audience, argument, evidence standard, citation style, and markup conventions.
4. Edit in place when the user asks to edit a file. If the user asks for suggestions only, do not modify the file.
5. Preserve facts, claims, citations, links, equations, headings, labels, references, frontmatter, code blocks, tables, and bibliography entries unless the user explicitly asks to change them.
6. After editing, report the main changes and any places where style could not be improved safely without domain input.

## Editing Rules

- Lead with the key takeaway: make abstracts, introductions, executive summaries, memos, and section openings state the central finding, recommendation, result, or decision-relevant point early.
- Prefer concrete claims over process descriptions. Replace "this paper examines" or "this report analyzes" style phrasing with what the work finds, proves, argues, recommends, or changes when the source text supports it.
- Remove throat-clearing, inflated transitions, filler, hedges, and repeated framing.
- Use active voice when the actor matters. Keep passive voice when it is standard in the discipline or usefully shifts focus to the method, result, or object.
- Favor short, direct sentences. Split sentences that carry multiple claims, caveats, and citations.
- Make each paragraph a unit: topic sentence, development, consequence. Move or flag sentences that do not serve the paragraph's point.
- Keep technical terms precise. Do not simplify by making the analysis, scholarship, business context, or evidence less accurate.
- Match the document's register: academic when scholarly, executive when business-facing, formal when institutional. Do not make the prose chatty, promotional, journalistic, or overconfident.
- Avoid cosmetic rewrites that merely swap synonyms. Make edits that improve argument flow, specificity, concision, or reader orientation.

## Safety Checks

- Do not invent findings, mechanisms, recommendations, decisions, citations, datasets, dates, limitations, financial figures, customer claims, or literature relationships.
- Do not remove hedges that encode real uncertainty, statistical limits, scope limits, business risk, legal risk, or disciplinary caution.
- Do not normalize field-specific language if doing so changes meaning.
- Do not alter quoted text except to preserve exact quotation formatting.
- For LaTeX, avoid changing commands, labels, refs, cite keys, math, environments, or bibliography syntax unless the user asks.
- For Markdown, preserve heading hierarchy, links, front matter, fenced code blocks, footnotes, and tables.

## Output

When edits are made, give:

- The edited filepath.
- A concise summary of the style changes.
- Any unresolved questions or risky passages that need author judgment.

When no edits are made, give the suggested revision or review notes directly.
