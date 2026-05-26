---
title: "NLP Games"
description: "A collection of toys illustrating various NLP tools and concepts. Includes Twenty Questions using WordNet, and an ontological Connections generator."
github: https://github.com/ThePineappleW/NLPgames
---

This is less of a project and more of a small collection of things that interested me.

## 20 Questions

My implementation of 20 questions uses [WordNet](https://wordnet.princeton.edu/front) as an ontology. It starts with the broadest concepts, and asks yes/no questions to attempt to home in on the player's secret.

It works pretty well if your secret concept is something unique. However, the bot is limited to vertical movement in the WordNet hierarchy. Horizontal movement, i.e. guessing between siblings, remains difficult. If your secret word was "Panther", for example, the bot could quickly figure out that you are thinking of a wildcat, but it would have to loop through "Lion", "Tiger", "Cheetah", and so on before guessing "Panther".


## Connections

More fun with ontologies! This program queries [ConceptNet](https://conceptnet.io) to automatically generate games of the New York Times' [Connections](https://www.nytimes.com/games/connections). It works by picking four "seed" terms, one for each category. For each seed, it picks a *relation* from a small list, and obtains four words which share the same relation with the seed. These words make up the category.

The list of relations is as follows:

- PartOf
- IsA
- DerivedFrom
- CapableOf
- HasA
- Causes
- AtLocation
- HasProperty
- Synonym

Some sample categories are `X AtLocation Beach` and `Teacher CapableOf Y`. In English, these are "Things at the beach" or "Things a teacher can do". 

Overall this program is pretty hit-or-miss. There is a lot of noise due to the expansive nature of the ontology, but every so often it generates a genuinely enjoyable puzzle.

I want to expand this program in the future to have more ways of generating categories, such as orthographically-related methods (e.g. "Words containing the name of a species of bird") or perhaps take better advantage of distributional semantics by using embeddings.