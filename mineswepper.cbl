       >>SOURCE FORMAT FREE
        IDENTIFICATION DIVISION.
        PROGRAM-ID. MINESWEPPER.

        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01 WS-NUMAR_LAT PIC 9(5).
        01 WS-NUMAR_INA PIC 9(5).
        01 WS-MINE-NUMBER PIC 9(3).
        01 WS-OPEND-CELLS PIC 9(6).
        01 WS-VICTORY-FLAG PIC 9(1).

        01 WS-GAME-X PIC 9(5).
        01 WS-GAME-Y PIC 9(5).
        01 WS-TEMP-X PIC 9(5).
        01 WS-TEMP-Y PIC 9(5).
        01 WS-CURRENT-X PIC 9(5).
        01 WS-CURRENT-Y PIC 9(5).

        01 AUX-AUXILIARY PIC 9(9).
        01 RANDOM-SEED PIC 9(9).
        01 RANDOM-VALUE PIC 9(9).

        01 WS-TIME.
            05 WS-HH PIC 9(2).
            05 WS-MM PIC 9(2).
            05 WS-SS PIC 9(2).
            05 WS-HS PIC 9(2).

        01 BOARD.
            05 ROW OCCURS 50 TIMES.
                10 CELL OCCURS 50 TIMES.
                    15 IS-MINE           PIC 9.
                    15 IS-OPEN           PIC 9.
                    15 IS-FLAGGED        PIC 9.
                    15 ADJACENT-MINES    PIC 9.
                    15 IN-QUEUE          PIC 9.

        01 QUEUE-FLOODFILL.
            05 QUEUE-ELEM OCCURS 2500 TIMES.
                10 QUEUE-X PIC 9(5).
                10 QUEUE-Y PIC 9(5).

        01 Q-HEAD PIC 9(5).
        01 Q-TAIL PIC 9(5).

        01 WS-COMANDA PIC X(1).

        01 MINES-PLACED PIC 9(4).
        01 X PIC 9(3).
        01 Y PIC 9(3).
        01 I PIC S9(3).
        01 J PIC S9(3).
        01 ROW-IDX PIC 9(3).
        01 COL-IDX PIC 9(3).
        
        01 WS-WRONG-FLAG PIC 9(1).
        01 WS-FIRST-MOVE PIC 9(1).
        01 WS-FIRST-X PIC 9(5).
        01 WS-FIRST-Y PIC 9(5).
        
        01 WS-TOTAL-CELLS PIC 9(6).
        01 WS-MINE-POS PIC 9(6).
        01 WS-CELL-INDEX PIC 9(6).
        01 WS-ROW PIC 9(3).
        01 WS-COL PIC 9(3).
        01 WS-POS PIC 9(6).
        01 WS-SWAP-POS PIC 9(6).
        01 WS-TEMP-MINE PIC 9(1).
        01 WS-TEMP-OPEN PIC 9(1).
        01 WS-TEMP-FLAG PIC 9(1).
        01 WS-TEMP-ADJ PIC 9(1).
        01 WS-TEMP-QUEUE PIC 9(1).
        01 WS-FOUND PIC 9(1).

        01 MINES-ARRAY.
            05 MINE-POS OCCURS 2500 TIMES PIC 9(6).

        PROCEDURE DIVISION.

        *> INIT
        DISPLAY "INTRODU LATIMEA: ".
        ACCEPT WS-NUMAR_LAT.

        DISPLAY "INTRODU INALTIMEA: ".
        ACCEPT WS-NUMAR_INA.

        DISPLAY "INTRODU NUMARUL DE MINE: ".
        ACCEPT WS-MINE-NUMBER.

        PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > WS-NUMAR_INA
            PERFORM VARYING X FROM 1 BY 1 UNTIL X > WS-NUMAR_LAT
                MOVE 0 TO IS-MINE(Y,X)
                MOVE 0 TO IS-OPEN(Y,X)
                MOVE 0 TO IS-FLAGGED(Y,X)
                MOVE 0 TO ADJACENT-MINES(Y,X)
                MOVE 0 TO IN-QUEUE(Y,X)
            END-PERFORM
        END-PERFORM.

        MOVE 0 TO WS-OPEND-CELLS.
        MOVE 1 TO WS-VICTORY-FLAG.
        MOVE 0 TO WS-FIRST-MOVE.

        PERFORM SHOW-GRID.

        *> MAIN LOOP - First phase: wait for first successful OPEN
        PERFORM UNTIL WS-FIRST-MOVE = 1

            DISPLAY "O = OPEN, F = FLAG: "
            ACCEPT WS-COMANDA

            DISPLAY "INTRODU COLOANA: "
            ACCEPT WS-GAME-X

            DISPLAY "INTRODU RANDUL: "
            ACCEPT WS-GAME-Y

            IF WS-COMANDA = "F"
                IF IS-OPEN(WS-GAME-Y,WS-GAME-X) = 0
                    IF IS-FLAGGED(WS-GAME-Y,WS-GAME-X) = 0
                        MOVE 1 TO IS-FLAGGED(WS-GAME-Y,WS-GAME-X)
                    ELSE
                        MOVE 0 TO IS-FLAGGED(WS-GAME-Y,WS-GAME-X)
                    END-IF
                    PERFORM SHOW-GRID
                END-IF
            END-IF

            IF WS-COMANDA = "O"
                IF IS-OPEN(WS-GAME-Y,WS-GAME-X) = 0
                    MOVE 1 TO WS-FIRST-MOVE
                    MOVE WS-GAME-X TO WS-FIRST-X
                    MOVE WS-GAME-Y TO WS-FIRST-Y
                END-IF
            END-IF
        END-PERFORM.

        *> Generate mines avoiding the first cell and its neighbors
        PERFORM GENERATE-MINES.

        *> Calculate adjacency
        PERFORM CALCULATE-ADJACENCY.

        *> Open the first cell
        MOVE WS-FIRST-X TO WS-GAME-X
        MOVE WS-FIRST-Y TO WS-GAME-Y
        PERFORM REVEAL-CELL-INIT.
        PERFORM SHOW-GRID.

        *> MAIN LOOP - Second phase: normal gameplay
        PERFORM UNTIL WS-OPEND-CELLS =
            (WS-NUMAR_INA * WS-NUMAR_LAT - WS-MINE-NUMBER)
            OR WS-VICTORY-FLAG = 0

            DISPLAY "O = OPEN, F = FLAG: "
            ACCEPT WS-COMANDA

            DISPLAY "INTRODU COLOANA: "
            ACCEPT WS-GAME-X

            DISPLAY "INTRODU RANDUL: "
            ACCEPT WS-GAME-Y

            IF WS-COMANDA = "F"
                IF IS-OPEN(WS-GAME-Y,WS-GAME-X) = 0
                    IF IS-FLAGGED(WS-GAME-Y,WS-GAME-X) = 0
                        MOVE 1 TO IS-FLAGGED(WS-GAME-Y,WS-GAME-X)
                    ELSE
                        MOVE 0 TO IS-FLAGGED(WS-GAME-Y,WS-GAME-X)
                    END-IF
                END-IF
            END-IF

            IF WS-COMANDA = "O"

                IF IS-OPEN(WS-GAME-Y,WS-GAME-X) = 0
                AND IS-FLAGGED(WS-GAME-Y,WS-GAME-X) = 0

                    IF IS-MINE(WS-GAME-Y,WS-GAME-X) = 1
                        MOVE 0 TO WS-VICTORY-FLAG
                        EXIT PERFORM
                    END-IF

                    PERFORM REVEAL-CELL-INIT

                ELSE
                    *> Doar daca celula este deschisa (IS-OPEN = 1)
                    IF IS-OPEN(WS-GAME-Y,WS-GAME-X) = 1
                        PERFORM CHORD-CELL
                    END-IF
                END-IF
            END-IF

            PERFORM SHOW-GRID
        END-PERFORM.

        PERFORM VALIDATE-ENDING.
        STOP RUN.

        *> =========================
        *> GENERATE MINES - Improved version
        *> =========================
        GENERATE-MINES.

        *> Initialize random seed with time and other factors
        ACCEPT WS-TIME FROM TIME
        COMPUTE RANDOM-SEED = 
            (WS-HH * 1000000) +
            (WS-MM * 10000) +
            (WS-SS * 100) +
            WS-HS

        *> Calculate total cells
        COMPUTE WS-TOTAL-CELLS = WS-NUMAR_LAT * WS-NUMAR_INA

        *> Create list of all possible positions
        PERFORM VARYING WS-CELL-INDEX FROM 1 BY 1
                 UNTIL WS-CELL-INDEX > WS-TOTAL-CELLS
            MOVE WS-CELL-INDEX TO MINE-POS(WS-CELL-INDEX)
        END-PERFORM.

        *> Use Fisher-Yates shuffle with better random numbers
        PERFORM VARYING WS-CELL-INDEX FROM WS-TOTAL-CELLS BY -1
                 UNTIL WS-CELL-INDEX < 2

            *> Generate better random number
            COMPUTE RANDOM-VALUE = 
                FUNCTION MOD(
                    (RANDOM-SEED * 1103515245 + 12345),
                    2147483647
                )
            COMPUTE RANDOM-SEED = RANDOM-VALUE
            
            *> Mix with time again for more randomness
            ACCEPT WS-TIME FROM TIME
            COMPUTE RANDOM-VALUE = 
                FUNCTION MOD(
                    (RANDOM-VALUE + 
                     (WS-HH * 1000000) +
                     (WS-MM * 10000) +
                     (WS-SS * 100) +
                     WS-HS),
                    2147483647
                )
            
            COMPUTE WS-POS = 
                FUNCTION MOD(RANDOM-VALUE, WS-CELL-INDEX) + 1
            
            *> Swap positions
            MOVE MINE-POS(WS-POS) TO WS-SWAP-POS
            MOVE MINE-POS(WS-CELL-INDEX) TO MINE-POS(WS-POS)
            MOVE WS-SWAP-POS TO MINE-POS(WS-CELL-INDEX)
        END-PERFORM.

        *> Place mines, avoiding first cell and its neighbors
        MOVE 0 TO MINES-PLACED
        MOVE 1 TO WS-CELL-INDEX

        PERFORM UNTIL MINES-PLACED = WS-MINE-NUMBER

            COMPUTE WS-POS = MINE-POS(WS-CELL-INDEX)
            
            *> Convert position to row and column
            COMPUTE WS-ROW = (WS-POS - 1) / WS-NUMAR_LAT + 1
            COMPUTE WS-COL = 
                FUNCTION MOD(WS-POS - 1, WS-NUMAR_LAT) + 1
            
            *> Check if position is safe (not first cell or neighbor)
            MOVE 0 TO WS-FOUND
            
            IF NOT (WS-COL = WS-FIRST-X AND WS-ROW = WS-FIRST-Y)
                PERFORM VARYING I FROM -1 BY 1 UNTIL I > 1
                PERFORM VARYING J FROM -1 BY 1 UNTIL J > 1
                    IF NOT (I = 0 AND J = 0)
                        COMPUTE WS-TEMP-X = WS-FIRST-X + J
                        COMPUTE WS-TEMP-Y = WS-FIRST-Y + I
                        IF WS-COL = WS-TEMP-X 
                        AND WS-ROW = WS-TEMP-Y
                            MOVE 1 TO WS-FOUND
                            EXIT PERFORM
                        END-IF
                    END-IF
                END-PERFORM
                END-PERFORM
            ELSE
                MOVE 1 TO WS-FOUND
            END-IF

            *> Place mine if position is safe
            IF WS-FOUND = 0
                MOVE 1 TO IS-MINE(WS-ROW, WS-COL)
                ADD 1 TO MINES-PLACED
            END-IF

            ADD 1 TO WS-CELL-INDEX
        END-PERFORM.

        EXIT.

        *> =========================
        *> CALCULATE ADJACENCY
        *> =========================
        CALCULATE-ADJACENCY.

        PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > WS-NUMAR_INA
        PERFORM VARYING X FROM 1 BY 1 UNTIL X > WS-NUMAR_LAT

            PERFORM VARYING I FROM -1 BY 1 UNTIL I > 1
            PERFORM VARYING J FROM -1 BY 1 UNTIL J > 1

                IF NOT (I = 0 AND J = 0)

                    COMPUTE WS-TEMP-X = X + J
                    COMPUTE WS-TEMP-Y = Y + I

                    IF WS-TEMP-X > 0 AND WS-TEMP-X <= WS-NUMAR_LAT
                    AND WS-TEMP-Y > 0 AND WS-TEMP-Y <= WS-NUMAR_INA

                        IF IS-MINE(WS-TEMP-Y,WS-TEMP-X) = 1
                            ADD 1 TO ADJACENT-MINES(Y,X)
                        END-IF

                    END-IF
                END-IF

            END-PERFORM
            END-PERFORM
        END-PERFORM
        END-PERFORM.

        EXIT.

        *> =========================
        *> REVEAL INIT
        *> =========================
        REVEAL-CELL-INIT.

        PERFORM RESET-QUEUE

        MOVE 1 TO Q-HEAD
        MOVE 1 TO Q-TAIL

        MOVE WS-GAME-X TO QUEUE-X(1)
        MOVE WS-GAME-Y TO QUEUE-Y(1)

        PERFORM FLOODFILL.

        EXIT.

        *> =========================
        *> FLOODFILL BFS
        *> =========================
        FLOODFILL.

        PERFORM UNTIL Q-HEAD > Q-TAIL

            MOVE QUEUE-X(Q-HEAD) TO WS-CURRENT-X
            MOVE QUEUE-Y(Q-HEAD) TO WS-CURRENT-Y
            ADD 1 TO Q-HEAD

            IF IS-OPEN(WS-CURRENT-Y,WS-CURRENT-X) = 0
                MOVE 1 TO IS-OPEN(WS-CURRENT-Y,WS-CURRENT-X)
                ADD 1 TO WS-OPEND-CELLS
            END-IF

            IF ADJACENT-MINES(WS-CURRENT-Y,WS-CURRENT-X) = 0

                PERFORM VARYING I FROM -1 BY 1 UNTIL I > 1
                PERFORM VARYING J FROM -1 BY 1 UNTIL J > 1

                    IF NOT (I = 0 AND J = 0)

                        COMPUTE WS-TEMP-X = WS-CURRENT-X + J
                        COMPUTE WS-TEMP-Y = WS-CURRENT-Y + I

                        IF WS-TEMP-X > 0 AND WS-TEMP-X <= WS-NUMAR_LAT
                        AND WS-TEMP-Y > 0 AND WS-TEMP-Y <= WS-NUMAR_INA

                            IF IS-OPEN(WS-TEMP-Y,WS-TEMP-X) = 0
                            AND IN-QUEUE(WS-TEMP-Y,WS-TEMP-X) = 0

                                ADD 1 TO Q-TAIL
                                MOVE WS-TEMP-X TO QUEUE-X(Q-TAIL)
                                MOVE WS-TEMP-Y TO QUEUE-Y(Q-TAIL)

                                MOVE 1 TO IN-QUEUE(WS-TEMP-Y,WS-TEMP-X)

                            END-IF
                        END-IF
                    END-IF
                END-PERFORM
                END-PERFORM
            END-IF

        END-PERFORM.

        EXIT.

        *> =========================
        *> RESET QUEUE
        *> =========================
        RESET-QUEUE.
            MOVE 0 TO Q-HEAD
            MOVE 0 TO Q-TAIL

            PERFORM VARYING ROW-IDX FROM 1 BY 1 
                     UNTIL ROW-IDX > WS-NUMAR_INA
                PERFORM VARYING COL-IDX FROM 1 BY 1 
                         UNTIL COL-IDX > WS-NUMAR_LAT
                    MOVE 0 TO IN-QUEUE(ROW-IDX,COL-IDX)
                END-PERFORM
            END-PERFORM

            EXIT.

        *> =========================
        *> CHORD - Fixed version with wrong flag detection
        *> =========================
        CHORD-CELL.

        MOVE 0 TO AUX-AUXILIARY
        MOVE 0 TO WS-WRONG-FLAG

        *> Count flags around the clicked cell and verify they are correct
        PERFORM VARYING I FROM -1 BY 1 UNTIL I > 1
        PERFORM VARYING J FROM -1 BY 1 UNTIL J > 1

            IF NOT (I = 0 AND J = 0)

                COMPUTE WS-TEMP-X = WS-GAME-X + J
                COMPUTE WS-TEMP-Y = WS-GAME-Y + I

                IF WS-TEMP-X > 0 AND WS-TEMP-X <= WS-NUMAR_LAT
                AND WS-TEMP-Y > 0 AND WS-TEMP-Y <= WS-NUMAR_INA

                    IF IS-FLAGGED(WS-TEMP-Y,WS-TEMP-X) = 1
                        ADD 1 TO AUX-AUXILIARY
                        *> Check if flag is on a mine
                        IF IS-MINE(WS-TEMP-Y,WS-TEMP-X) = 0
                            MOVE 1 TO WS-WRONG-FLAG
                        END-IF
                    END-IF

                END-IF
            END-IF

        END-PERFORM
        END-PERFORM

        *> If wrong flag found, player loses
        IF WS-WRONG-FLAG = 1
            MOVE 0 TO WS-VICTORY-FLAG
            EXIT PARAGRAPH
        END-IF

        *> If flags match the number of adjacent mines, open all neighbors
        IF AUX-AUXILIARY = ADJACENT-MINES(WS-GAME-Y,WS-GAME-X)

            PERFORM VARYING I FROM -1 BY 1 UNTIL I > 1
            PERFORM VARYING J FROM -1 BY 1 UNTIL J > 1

                IF NOT (I = 0 AND J = 0)

                    COMPUTE WS-TEMP-X = WS-GAME-X + J
                    COMPUTE WS-TEMP-Y = WS-GAME-Y + I

                    IF WS-TEMP-X > 0 AND WS-TEMP-X <= WS-NUMAR_LAT
                    AND WS-TEMP-Y > 0 AND WS-TEMP-Y <= WS-NUMAR_INA

                        IF IS-OPEN(WS-TEMP-Y,WS-TEMP-X) = 0
                        AND IS-FLAGGED(WS-TEMP-Y,WS-TEMP-X) = 0

                            IF IS-MINE(WS-TEMP-Y,WS-TEMP-X) = 1
                                MOVE 0 TO WS-VICTORY-FLAG
                                EXIT PARAGRAPH
                            END-IF

                            *> Store the cell coordinates temporarily
                            MOVE WS-TEMP-X TO WS-CURRENT-X
                            MOVE WS-TEMP-Y TO WS-CURRENT-Y

                            *> Open this cell
                            MOVE 1 TO IS-OPEN(WS-CURRENT-Y,WS-CURRENT-X)
                            ADD 1 TO WS-OPEND-CELLS

                            *> If this cell has 0 adjacent mines, flood fill from it
                            IF ADJACENT-MINES(WS-CURRENT-Y,WS-CURRENT-X) = 0
                                PERFORM RESET-QUEUE
                                MOVE 1 TO Q-HEAD
                                MOVE 1 TO Q-TAIL
                                MOVE WS-CURRENT-X TO QUEUE-X(1)
                                MOVE WS-CURRENT-Y TO QUEUE-Y(1)
                                PERFORM FLOODFILL
                            END-IF

                        END-IF
                    END-IF
                END-IF

            END-PERFORM
            END-PERFORM
        END-IF

        EXIT.

        *> =========================
        *> VALIDATE ENDING
        *> =========================
        VALIDATE-ENDING.
            IF WS-VICTORY-FLAG = 0
                DISPLAY "AI EXPLODAT!!!"
            ELSE
                DISPLAY "AI INVINS!!!"
            END-IF

            EXIT.

        *> =========================
        *> SHOW GRID
        *> =========================
        SHOW-GRID.
            PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > WS-NUMAR_INA
                PERFORM VARYING X FROM 1 BY 1 UNTIL X > WS-NUMAR_LAT

                    IF IS-FLAGGED(Y,X) = 1 AND IS-OPEN(Y,X) = 0
                        DISPLAY "P" WITH NO ADVANCING
                    ELSE
                        IF IS-OPEN(Y,X) = 0
                            DISPLAY "_" WITH NO ADVANCING
                        ELSE
                            IF ADJACENT-MINES(Y,X) = 0
                                DISPLAY "." WITH NO ADVANCING
                            ELSE
                                DISPLAY ADJACENT-MINES(Y,X) 
                                        WITH NO ADVANCING
                            END-IF
                        END-IF
                    END-IF

                END-PERFORM
                DISPLAY " "
            END-PERFORM

            EXIT.

        END PROGRAM MINESWEPPER.
        