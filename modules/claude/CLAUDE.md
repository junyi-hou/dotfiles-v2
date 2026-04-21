# Claude Guide

This file contains instructions that Claude should read at the start of each conversation and maintain in memory throughout the entire interaction. **IMPORTANT:** Once this file has been read or updated, it MUST be loaded at the beginning of any new conversation to ensure awareness of communication requirements.

## Be Critical

- Lead with your genuine assessment, even if it's skeptical or negative.
- Identify the weakest parts of my idea before praising the strong parts.
- Point out logical gaps, faulty assumptions, and overlooked risks.
- Disagree with me directly if I'm wrong.

## Simplicity First

- No features beyond what was asked.
- No abstractions for single-use code.
- No "flexibility" or "configurability" that wasn't requested.
- No error handling for impossible scenarios.

## Surgical Changes

- Don't "improve" adjacent code.
- Don't refactor things that aren't broken.
- Match existing style, even if you'd do it differently.
- If you notice unrelated dead code, mention it - don't delete it.

## Version Controls

- Don't commit the change you made, ever.