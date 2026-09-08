// The Scryfall API has a strict rate limit of 2 searches per second.
// Since we need to grab images on on mouse-over, we need to debounce, throttle, and cache results.
// This will provide a smooth UX without exceeding the rate limit.

class ImageManager {
  constructor({ minInterval = 500, hoverDelay = 150 } = {}) {
    this.minInterval = minInterval;
    this.hoverDelay = hoverDelay;
    this.queue = Promise.resolve();  // rate limiter chain
    this.cache = new Map();          // url -> promise/result
    this.timers = new WeakMap();     // elem -> pending hover timeout
  }

  // Call on mouseenter
  hover(elem, url) {
    clearTimeout(this.timers.get(elem));
    const timer = setTimeout(() => this._load(elem, url), this.hoverDelay);
    this.timers.set(elem, timer);
  }

  // Call on mouseleave
  leave(elem) {
    clearTimeout(this.timers.get(elem));
  }

  async _load(elem, url) {
    if (!this.cache.has(url)) {
      this.cache.set(url, this._enqueue(url));
    }
    try {
      const result = await this.cache.get(url);
      this._applyResult(elem, url, result);
    } catch (err) {
      this.cache.delete(url); // allow retry on failure
      console.error('Image load failed:', err);
    }
  }

  // Rate-limited fetch, serialized through the queue
  _enqueue(url) {
    const run = async () => {
      const start = Date.now();
      const result = await this._fetchImage(url);
      const wait = this.minInterval - (Date.now() - start);
      if (wait > 0) await new Promise(r => setTimeout(r, wait));
      return result;
    };
    const p = this.queue.then(run);
    this.queue = p.catch(() => {}); // don't let one failure stall the queue
    return p;
  }

  // --- Below: your existing logic, split into fetch vs. apply ---

  async _fetchImage(url) {
    const cards = await fetch(url).then(res => res.json());
    let card = cards["data"][0]
    // Handle DFCs
    if (card.hasOwnProperty("card_faces")){
      card = card["card_faces"][0];
    }

    return card["image_uris"]["normal"]; 
  }

  _applyResult(elem, url, result) {
    elem.children[0].src = result;
  }
}

const imageManager = new ImageManager({ minInterval: 500, hoverDelay: 150 });

const links = document.getElementsByClassName("mtg-link");
for (let link of links) {
    const { name, set, number } = link.dataset;
    let query = `q=${name.replaceAll(" ", "+")}`;
    if (set) {query += `+e:${set}`};
    if (number) {query += `+cn:"${number}"`};
    query = encodeURI(query);
    link.href = `https://scryfall.com/search?${query}`;
    const apiLink = `https://api.scryfall.com/cards/search?${query}`;

    link.addEventListener('mouseenter', () => imageManager.hover(link, apiLink));
    link.addEventListener('mouseleave', () => imageManager.leave(link)); 
}
