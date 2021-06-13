DECLARE FUNCTION readprogram$ ()
DECLARE SUB findbrackets (program AS STRING, brackets() AS INTEGER)
DECLARE SUB runprogram (program AS STRING, brackets() AS INTEGER)

CONST FILE = "hello.bf"  ' file to read
CONST MAXPRGLEN = 20000  ' maximum length of program
CONST MAXNESTBRA = 100   ' maximum number of nested brackets
CONST RAMSIZE = 30000    ' RAM size for BF program

DIM program AS STRING                   ' program read from file
DIM brackets(MAXPRGLEN - 1) AS INTEGER  ' positions of matching brackets

CLS

' make sure the file exists (OPEN ... BINARY would create a new file)
ON ERROR GOTO fileerror
OPEN FILE FOR INPUT AS #1: CLOSE
ON ERROR GOTO 0

PRINT "Reading file..."
program = readprogram$
PRINT "Instructions:"; LEN(program)
PRINT "Finding brackets..."
CALL findbrackets(program, brackets())
PRINT "Running. BLUE background means input is expected."
CALL runprogram(program, brackets())
END

fileerror:
PRINT "File not found."
END

SUB findbrackets (program AS STRING, brackets() AS INTEGER)
' Get positions of matching brackets from program.
' (0 means both first byte and no matching bracket, which doesn't matter.)

DIM p AS INTEGER        ' position in program
DIM opencnt AS INTEGER  ' number of brackets currently open

' a stack (last in, first out) of positions of opening brackets
DIM openbrackets(MAXNESTBRA - 1) AS INTEGER

FOR p = 0 TO LEN(program) - 1
    SELECT CASE MID$(program, p + 1, 1)
        CASE "["
            IF opencnt = MAXNESTBRA THEN
                PRINT "Error: too many nested brackets."
                END
            END IF
            openbrackets(opencnt) = p
            opencnt = opencnt + 1
        CASE "]"
            IF opencnt = 0 THEN
                PRINT "Error: ']' without matching '['."
                END
            END IF
            brackets(p) = openbrackets(opencnt - 1)
            brackets(openbrackets(opencnt - 1)) = p
            opencnt = opencnt - 1
    END SELECT
NEXT

IF opencnt > 0 THEN
    PRINT "Error: '[' without matching ']'."
    END
END IF

END SUB

FUNCTION readprogram$
' Read Brainfuck program from FILE. Ignore non-instruction bytes.

DIM byt AS STRING  ' byte read from file
DIM prg AS STRING  ' BF program

byt = " "
prg = ""

' open file, go to start
OPEN FILE FOR BINARY ACCESS READ AS #1
SEEK #1, 1

' read bytes until EOF, store BF instructions
DO
    GET #1, , byt
    IF EOF(1) THEN EXIT DO
    IF INSTR("-+<>[],.", byt) THEN
        IF LEN(prg) = MAXPRGLEN THEN PRINT "Program too long.": END
        prg = prg + byt
    END IF
LOOP

' close file
CLOSE

readprogram$ = prg

END FUNCTION

SUB runprogram (program AS STRING, brackets() AS INTEGER)
' Run the Brainfuck program.

DIM ram(RAMSIZE - 1) AS INTEGER

DIM p AS INTEGER   ' instruction pointer
DIM r AS INTEGER   ' RAM pointer
DIM ins AS STRING  ' instruction

WHILE p < LEN(program)
    ins = MID$(program, p + 1, 1)  ' read instruction

    SELECT CASE ins
        ' change value
        CASE "-": ram(r) = (ram(r) + 255) MOD 256
        CASE "+": ram(r) = (ram(r) + 1) MOD 256
        ' move pointer
        CASE "<": IF r > 0 THEN r = r - 1 ELSE r = RAMSIZE - 1
        CASE ">": r = (r + 1) MOD RAMSIZE
        ' loop
        CASE "[": IF ram(r) = 0 THEN p = brackets(p)
        CASE "]": IF ram(r) THEN p = brackets(p)
        ' input/output
        CASE ","
            PALETTE 0, &H1  ' dark blue
            ram(r) = ASC(INPUT$(1))
            PALETTE 0, &H0  ' black
        CASE "."
            SELECT CASE ram(r)
                ' Unix newline or printable ASCII
                CASE &HA, &H20 TO &H7E: PRINT CHR$(ram(r));
                CASE ELSE: PRINT "?";
            END SELECT
        CASE ELSE
            PRINT "Invalid instruction decoded (should never happen).": END
    END SELECT

    p = p + 1
WEND

END SUB

