/*
 * CroStat — minimalan servisni radnik (strategija: mreza prvo).
 *
 * Sajt je o svjezim podacima, pa se UVIJEK prvo pokusava mreza. Predmemorija je
 * samo zastita da stranica radi kad mreze nema. Kad se navigacija posluzi iz
 * predmemorije (mreza je pala), u posluzeni HTML se ubaci oznaka
 * data-cs-offline="1" na <html>, pa stranica sinkrono zna da podaci mozda nisu
 * najnoviji i prikaze vidljivu oznaku. Dok ima mreze, korisnik uvijek dobiva
 * svjez sadrzaj (bez te oznake).
 */
// v2: adrese su presle na oblik bez nastavka ".html", pa su svi kljucevi u
// predmemoriji v1 zastarjeli (/BDP.html umjesto /BDP). Promjena imena znaci da
// activate obrise staru predmemoriju i pocne od nule. Sigurno je, jer se
// activate dogada tek nakon uspjesnog dohvata novog sw.js, dakle uz mrezu.
var CACHE = 'crostat-runtime-v2';

self.addEventListener('install', function () {
  self.skipWaiting();
});

self.addEventListener('activate', function (event) {
  event.waitUntil((async function () {
    var keys = await caches.keys();
    await Promise.all(keys.filter(function (k) { return k !== CACHE; })
      .map(function (k) { return caches.delete(k); }));
    await self.clients.claim();
  })());
});

// Iz predmemorije: tocan pogodak, pa fallback na pocetnu stranicu (da barem
// nesto smisleno prikazemo za necesiranu podstranicu bez mreze). Naslovnica je
// pod '/', a '/index.html' se jos provjerava zbog predmemorija starijih od v2.
async function izPredmemorije(req) {
  var cache = await caches.open(CACHE);
  return (await cache.match(req)) ||
         (await cache.match('/')) ||
         (await cache.match('/index.html'));
}

// Ubaci oznaku offline prikaza u HTML koji se sluzi iz predmemorije.
async function uzOznakuOffline(resp) {
  try {
    var ct = (resp.headers.get('Content-Type') || '');
    if (ct.indexOf('text/html') === -1) return resp;
    var html = await resp.text();
    html = html.replace(/<html/i, '<html data-cs-offline="1"');
    var h = new Headers(resp.headers);
    h.delete('Content-Length');
    h.delete('Content-Encoding');
    return new Response(html, { status: 200, statusText: 'OK', headers: h });
  } catch (e) {
    return resp;
  }
}

self.addEventListener('fetch', function (event) {
  var req = event.request;
  if (req.method !== 'GET') return;
  var url = new URL(req.url);
  if (url.origin !== self.location.origin) return; // samo vlastiti sadrzaj

  var jeNavigacija = req.mode === 'navigate';

  event.respondWith((async function () {
    try {
      var svjez = await fetch(req);
      // Spremi kopiju uspjesnih odgovora za slucaj nestanka mreze.
      if (svjez && svjez.ok && (svjez.type === 'basic' || svjez.type === 'default')) {
        var kopija = svjez.clone();
        caches.open(CACHE).then(function (c) { c.put(req, kopija); }).catch(function () {});
      }
      return svjez;
    } catch (err) {
      var kesirano = await izPredmemorije(req);
      if (!kesirano) throw err;
      return jeNavigacija ? await uzOznakuOffline(kesirano) : kesirano;
    }
  })());
});
