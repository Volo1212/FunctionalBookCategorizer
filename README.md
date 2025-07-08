# Buchklassifizierungs-Projekt

Dieses Projekt implementiert ein Machine-Learning-Modell in Haskell, um Textdateien als "Kinderbuch" oder "Erwachsenenbuch" zu klassifizieren.

## Architektur

Das Projekt folgt dem **Functional Core, Imperative Shell**-Prinzip:
- **`Main.hs` (Imperative Shell):** Verantwortlich für alle IO-Aktionen wie das Einlesen von Dateien und das Ausgeben der Ergebnisse auf der Konsole.
- **`CoreLogic.hs`:** Orchestriert die pure Logik. Ruft Funktionen zur Merkmalsextraktion und Modelltraining auf.
- **`Helpers.hs`:** Enthält alle sonstigen Textanalyse- und Statistikfunktionen, die zur Übersicht ausgelagert wurden aus `CoreLogic.hs`.
- **`Types.hs`:** Definiert alle zentralen Datentypen der Anwendung.

## Features & Modell

Das Modell basiert auf einem **logistisch-regressiven Klassifikator**, der mit **Batch Gradient Descent** trainiert wird. Folgende textstatistische Merkmale werden verwendet:
- Durchschnittliche Satzlänge
- Durchschnittliche Wortlänge
- Flesch Reading Ease Score
- Lexikalische Vielfalt (Unique Word Ratio)
- Standardabweichung der Satzlänge
- Durchschnittliche Kommas pro Satz

Zur Vermeidung von Overfitting wird **L2-Regularisierung** eingesetzt. Eine **symmetrische Normalisierung** aller Features stellt einen fairen Vergleich sicher.

## Kompilieren und Ausführen

Voraussetzungen: GHC und Cabal.

**1. Projekt kompilieren:**
```bash
cabal build

2. Programm ausführen:

Stelle sicher, dass die Bücher in den Ordnern books/children und books/adults liegen.

cabal exec finalProject

3. Tests ausführen:

Das Projekt enthält eine umfangreiche Test-Suite mit HUnit (Unit Tests) und QuickCheck (Property-Based Tests).

cabal test
```

