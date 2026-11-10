# Scouting profile codebook

## Purpose and cohort

The scouting comparison relates draft-era descriptions to later NHL physical play relative to listed size. The annotation file contains one redacted passage for each of 82 eligible players: 59 forwards and 23 defensemen. One human rater initially read the passages with player identity, CSAx, and rankings concealed. The final code reflects a review of each complete passage under the criterion below before the reported comparison.

`validation/scouting_annotations.csv` is the active coding file. Each row contains a stable study ID, player and source details, the redacted passage, the binary code, and optional notes. `sourceTextSha256` identifies the original source text; redaction means it is not a checksum of the displayed passage. Earlier blinded packets and provenance remain historical records.

## Active physical-play profile

`activePhysicalPlayProfile` records whether the passage describes an active physical-play profile. Read the passage in context and apply a human judgment; individual words do not determine the code.

- `1`: The passage describes active physical actions or an attribute tied to such play. Qualifying descriptions can include delivering contact, checking, winning contested-puck battles, forceful net drives, physical aggression or confrontation, using strength through opposition, forceful work in interior ice, or an explicit physical style that conveys active intent. A player's size may help him perform these actions without disqualifying the description.
- `0`: The passage does not describe active physical play. Listed size or strength alone, physical readiness, an ability to absorb contact, blocking a shot, generic competitiveness or effort, and offensive aggression without a physical-play context do not qualify by themselves. This code records the passage's emphasis; it does not characterize the player as soft.

An archetype such as “power forward” requires supporting physical-play language. Phrases about playing bigger than one's size are judged in the context of the full passage. Use `notes` when a brief explanation helps document a borderline interpretation.

## File checks

Retain all 82 study IDs and player IDs, source links, report dates, and complete redacted passages. Enter only `0` or `1` in `activePhysicalPlayProfile`. The analysis validates unique IDs, source details, report timing, binary codes, and the 59-forward/23-defenseman cohort. Player-average CSAx is calculated across eligible seasons after each report's date and analyzed separately by position.
