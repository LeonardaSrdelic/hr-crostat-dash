# HR Crostat Dash

Quarto web s ažuriranim Eurostat grafikonima.
Koraci:
1. U R pokreni `renv::init()` i instaliraj pakete.
2. `quarto preview` za lokalni pregled.
3. `quarto render` i `quarto publish gh-pages` za objavu.
4. GitHub Actions automatski renderira i objavljuje ponedjeljkom u 06:00 CET (05:00 UTC) s branša `main`.
