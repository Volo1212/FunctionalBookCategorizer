# Buchklassifizierungs-Projekt

Dieses Projekt implementiert ein Machine-Learning-Modell in Haskell, um Textdateien als "Kinderbuch" oder "Erwachsenenbuch" zu klassifizieren.

## Architektur

Das Projekt folgt dem **Functional Core, Imperative Shell**-Prinzip:
- **`Main.hs` (Imperative Shell):** Verantwortlich für alle IO-Aktionen wie das Einlesen von Dateien und das Ausgeben der Ergebnisse auf der Konsole.
- **`CoreLogic.hs`:** Orchestriert die pure Logik. Ruft Funktionen zur Merkmalsextraktion und Modelltraining auf.
-   **`Helpers.hs`:** Enthält alle sonstigen Textanalyse- und Statistikfunktionen, die zur Übersicht ausgelagert wurden aus `CoreLogic.hs`.
- **`Types.hs`:** Definiert alle zentralen Datentypen der Anwendung.

## Features & Modell

Das Modell basiert auf einem **logistisch-regressiven Klassifikator**, der mit **Gradient Descent** trainiert wird. Folgende textstatistische Merkmale werden verwendet:
- Durchschnittliche Satzlänge
- Durchschnittliche Wortlänge
- Flesch Reading Ease Score
- Lexikalische Vielfalt (Unique Word Ratio)
- Standardabweichung der Satzlänge
- Durchschnittliche Kommas pro Satz

Zur Vermeidung von Overfitting wird **L2-Regularisierung** eingesetzt. Eine **symmetrische Normalisierung** aller Features stellt einen fairen Vergleich sicher.

## Kompilieren und Ausführen

Voraussetzungen: GHC, Cabal und Python 3.

**1. Vorbereitung: Datensatz herunterladen**

Das Training des Modells erfordert einen Datensatz von Büchern. Ein Python-Skript (`download_books.py`) wird bereitgestellt, um automatisch Bücher vom [Project Gutenberg](https://www.gutenberg.org/) herunterzuladen.

**a) Abhängigkeiten installieren:**
Das Skript benötigt die `requests`-Bibliothek.
```bash
pip install requests
```
**b) Führe das Skript aus:**
Führe das Skript aus, um die Bücher herunterzuladen. Passe bei Bedarf die Variablen SAVE_DIR und SKIP_BOOKSHELVES im Skript an, um verschiedene Kategorien (z.B. Kinderbücher) zu laden.

```
python download_books.py
```

**2. Projekt kompilieren:**
```bash
cabal build
```

**3. Programm ausführen:**

Stelle sicher, dass die Bücher in den Ordnern books/children und books/adults liegen.

```
cabal exec finalProject
```

**4. Tests ausführen:**

Das Projekt enthält eine umfangreiche Test-Suite mit HUnit (Unit Tests) und QuickCheck (Property-Based Tests).

```
cabal test
```


