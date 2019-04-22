DECLARE SUB findbrackets (brackets() AS INTEGER)
DECLARE SUB runprogram (brackets() AS INTEGER, ram() AS INTEGER)

' print "Hello, World!" (from Wikipedia)
CONST PRG = "++++++++ [>++++ [>++>+++>+++>+<<<<-] >+>+>->>+ [<] <-] >>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
CONST RAMSIZE = 30000  ' RAM size
CONST MAXNESTBRA = 100  ' maximum number of nested brackets

DIM ram(RAMSIZE - 1) AS INTEGER
DIM brackets(LEN(PRG) - 1) AS INTEGER  ' positions of matching brackets

CALL findbrackets(brackets())
CLS
CALL runprogram(brackets(), ram())

SUB findbrackets (brackets() AS INTEGER)
' Get positions of matching brackets.

DIM p AS INTEGER
DIM openbracnt AS INTEGER  ' number of brackets currently open
DIM ins AS STRING

' a stack (last in, first out) of positions of opening brackets
DIM openbrackets(MAXNESTBRA - 1) AS INTEGER

FOR p = 0 TO LEN(PRG) - 1
    SELECT CASE MID$(PRG, p + 1, 1)
        CASE "["
            IF openbracnt = MAXNESTBRA THEN
                PRINT "Error: too many nested brackets."
                END
            END IF
            openbrackets(openbracnt) = p
            openbracnt = openbracnt + 1
        CASE "]"
            IF openbracnt = 0 THEN
                PRINT "Error: ']' without matching '['."
                END
            END IF
            brackets(p) = openbrackets(openbracnt - 1)
            brackets(openbrackets(openbracnt - 1)) = p
            openbracnt = openbracnt - 1
    END SELECT
NEXT

IF openbracnt > 0 THEN
    PRINT "Error: '[' without matching ']'."
    END
END IF

END SUB

SUB runprogram (brackets() AS INTEGER, ram() AS INTEGER)
' Run the Brainfuck program.

DIM p AS INTEGER    ' program pointer
DIM r AS INTEGER    ' RAM pointer
DIM inpt AS STRING  ' input

PRINT "The background turns BLUE when the program is expecting input."

WHILE p < LEN(PRG)
    ins$ = MID$(PRG, p + 1, 1)  ' get instruction
    SELECT CASE ins$
        ' change value
        CASE "+": ram(r) = (ram(r) + 1) MOD 256
        CASE "-": ram(r) = (ram(r) + 255) MOD 256
        ' move pointer
        CASE ">": r = (r + 1) MOD RAMSIZE
        CASE "<": IF r > 0 THEN r = r - 1 ELSE r = RAMSIZE - 1
        ' loop
        CASE "[": IF ram(r) = 0 THEN p = brackets(p)
        CASE "]": IF ram(r) THEN p = brackets(p)
        ' input/output
        CASE ","
            PALETTE 0, &H1
            ram(r) = ASC(INPUT$(1))
            PALETTE 0, &H0
        CASE "."
            SELECT CASE ram(r)
                CASE &H20 TO &H7E
                    ' printable ASCII
                    PRINT CHR$(ram(r));
                CASE &HA
                    ' Unix newline
                    PRINT
                CASE ELSE
                    PRINT "?";
            END SELECT
    END SELECT
    p = p + 1
WEND

END SUB

