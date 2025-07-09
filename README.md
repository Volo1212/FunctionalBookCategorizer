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
python downloader.py
```
## Kompilieren und Ausführen

Voraussetzungen: GHC, Cabal und Python 3.
Bei Problemen mit der Kompatibilität liegt es wahrscheinlich an einer falschen Cabal / GHC Version (muss zwischen base >=4.18.2.0 && <4.19 liegen)

**1. Vorbereitung: Datensatz herunterladen**

Das Training des Modells erfordert einen Datensatz von Büchern. Ein Python-Skript (`download_books.py`) wird bereitgestellt, um automatisch Bücher vom [Project Gutenberg](https://www.gutenberg.org/) herunterzuladen.

**a) Abhängigkeiten installieren:**
Das Skript benötigt die `requests`-Bibliothek.
```bash
pip install requests
```
**b) Skript ausführen:**
Führe das Skript aus, um die Bücher herunterzuladen. Passe bei Bedarf die Variablen SAVE_DIR und SKIP_BOOKSHELVES im Skript an, um verschiedene Kategorien (z.B. Kinderbücher) zu laden.

```
python download_books.py
```

**2. Projekt kompilieren:**
```bash
cabal build
```

**3. Programm ausführen:**

Stelle sicher, dass die Bücher in Testdaten ('books/training/[LABEL]' und 'books/categorize[LABEL') liegen.
Es sollten ca. gleich viele Trainingsdaten für Kinder und Erwachsene vorhanden sein um das Ergebnis zu optimieren.
Nach dem Einfügen der Trainings- und Testdaten kann das Programm ausgeführt werden.

```
cabal exec finalProject
```

**4. Tests ausführen:**

Das Projekt enthält über 30 Tests mit HUnit (Unit Tests) und QuickCheck (Property-Based Tests).

```
cabal test
```


