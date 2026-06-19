# COBOL Minesweeper - Complete Documentation

## 📋 Overview
A fully functional Minesweeper game implemented in COBOL with advanced features including uniform mine distribution, safe first move, chord functionality, and flag validation.

## 🎮 Features

### Core Gameplay
- **Grid-based minefield** with configurable dimensions (up to 50x50)
- **Configurable mine count** 
- **Left-click** (`O` command) to reveal cells
- **Right-click** (`F` command) to place/remove flags
- **Flood fill** automatically reveals adjacent empty cells
- **Chord functionality** - click on a revealed number with correct flags to auto-reveal neighbors

### Safety Features
- **Safe First Move** - First click never hits a mine
- **Flag Validation** - Wrong flags detected and penalized during chord
- **Victory Detection** - Game ends when all non-mine cells are revealed

### Mine Distribution
- **Fisher-Yates Shuffle** algorithm for uniform mine placement
- **No clustering** - mines are evenly distributed across the grid
- **First cell safety** - first click and its neighbors are guaranteed mine-free

## 🏗️ Architecture

### Data Structures

#### Board (variabile up to 50x50 grid)
```cobol
01 BOARD.
    05 ROW OCCURS 50 TIMES.
        10 CELL OCCURS 50 TIMES.
            15 IS-MINE           PIC 9.    *> 1 = mine, 0 = safe
            15 IS-OPEN           PIC 9.    *> 1 = revealed, 0 = hidden
            15 IS-FLAGGED        PIC 9.    *> 1 = flagged, 0 = not flagged
            15 ADJACENT-MINES    PIC 9.    *> Number of neighboring mines
            15 IN-QUEUE          PIC 9.    *> BFS flood fill tracking
