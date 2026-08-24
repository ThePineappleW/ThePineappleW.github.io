let searchIndex = null;
let searchMeta = null;

async function loadSearchIndex() {
    const idx = await fetch('/search-index.json');
    searchIndex = await idx.json();

    const meta = await fetch('/search-meta.json');
    searchMeta = await meta.json();
}

function searchFor(query) {
    const terms = query.toLowerCase().match(/[a-z0-9]+/g) || [];
    if (terms.length === 0) return [];

    // For each term in the search, get the set of pages containing that term.
    // Then, intersect them to find the documents that contain all terms.
    const pageSets = terms.map(
        term => new Set((searchIndex[term] || []).map(result => result[0])));
    if (pageSets.length === 0){
        return []; // No matches
    }
    const matches = pageSets.reduce((acc, nextSet) => acc.intersection(nextSet), pageSets[0]);

    return [...matches.keys()];
}

function createChild(parent, content) {
    const child = document.createElement("li");
    child.classList.add("search-result")
    child.innerHTML = content;
    parent.appendChild(child);
}

async function getSearchResults() {
    if (!searchIndex) {
        await loadSearchIndex();
    }

    const params = new URLSearchParams(window.location.search);
    const query = params.get("q") || "";
    const list = document.getElementById("search-results-list");

    const matches = searchFor(query);

    // Update the search query label
    document.getElementById("search-query").textContent = query ? `Search results for "${query}"` : `Enter search term.`;
    
    if (matches.length === 0) {
        createChild(list, "No results.");
    } else {
        for (const match of matches) {
            const link = searchMeta[match]["url"]
            const title = searchMeta[match]["title"]
            createChild(list, `<a href=${link}>${title}</a>`);
        }
    }
    
}