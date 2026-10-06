# Blinded Central Scouting physicality codebook

## Purpose

This exercise asks whether CSAx agrees with descriptions of active physical engagement recorded before the player entered the study window. One human rater codes official NHL scouting prose while blinded to player identity, CSAx, and rankings. Each row represents one player. The packet contains 43 passages, with one content field to complete for every passage.

Read only the supplied passage. Do not search distinctive phrases, consult the source catalog, infer the player from outside knowledge, or consult CSAx values or rankings until the completed file has been returned and locked.

## Coding order

Read each complete passage and code `activePhysicalEngagement`. Use `notes` only when a brief explanation would document an ambiguity. Save the completed CSV with its original filename, `scouting_expansion_packet.csv`.

## Fields

`activePhysicalEngagement`

- `1`: The passage describes initiating or delivering contact, checking, actively battling for contested pucks, confrontation, aggression, physical competitiveness, fighting, using strength to drive through opposition, or forceful interior work.
- `0`: The passage contains no such description. Merely absorbing contact or blocking a shot does not qualify by itself.

`notes`

- Optional plain text for an ambiguity. Do not enter a guessed player name.

## Completion checks

Use only 0 or 1 in `activePhysicalEngagement`. A zero records absence of the specified description; it does not identify a soft player. General praise for skill, leadership, or competitiveness requires the concrete behavior described above to qualify. Phrases such as "soft hands" describe puck skill and do not establish soft physical play. Do not infer the code from listed size or a generic statement about playing above size.

Do not edit `studyId` or `reportText`, add or remove rows, sort by passage wording, or expose a guessed identity. Names and identifying details appear as bracketed labels where needed. The completed file is hashed and locked before the study key or CSAx values are joined. Every planned validation result is reported regardless of its direction or statistical significance.

The analysis combines these codes with the `activePhysicalEngagement` field from the 40 frozen forward ratings. Original ratings remain immutable. Associations use eligible player-average CSAx separately for forwards and defensemen. Draft-era descriptions, selective profile coverage, and a single rater limit the strength and generalizability of the external evidence.
