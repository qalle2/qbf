DECLARE SUB findbrackets (compiled AS STRING, brackets() AS INTEGER)
DECLARE SUB runprogram (compiled AS STRING, brackets() AS INTEGER)
DECLARE SUB compile (compiled AS STRING)
DECLARE FUNCTION decodeins$ (byt AS STRING)
DECLARE FUNCTION decodecnt% (byt AS STRING)
DECLARE FUNCTION encodeseq$ (ins AS STRING, cnt AS INTEGER)

CONST FILE = "hello.bf"     ' file to read
CONST INSTSET = "-+<>,.[]"  ' instruction set
CONST FLOWINST = "[]"       ' flow control instructions
CONST MAXPRGLEN = 20000     ' maximum length of compiled program
CONST MAXSEQLEN = 32        ' max # of instructions to compile to a byte
CONST MAXNESTBRA = 100      ' maximum number of nested brackets
CONST RAMSIZE = 30000       ' RAM size for BF program
CONST REPLCHAR = "?"        ' replacement character

DIM brackets(MAXPRGLEN - 1) AS INTEGER  ' positions of matching brackets
DIM compiled AS STRING                  ' compiled program

CLS

' make sure the file exists (OPEN ... BINARY would create a new file)
ON ERROR GOTO fileerror
OPEN FILE FOR INPUT AS #1
CLOSE
ON ERROR GOTO 0

PRINT "Compiling..."
CALL compile(compiled)
PRINT "Compiled size:"; LEN(compiled); "byte(s)"
PRINT "Finding brackets..."
CALL findbrackets(compiled, brackets())
PRINT "Running. BLUE background means input is expected."
CALL runprogram(compiled, brackets())
END

fileerror:
PRINT "File not found."
END

SUB compile (compiled AS STRING)
' RLE encode Brainfuck program from file. Ignore non-instruction bytes.
' Do not pack multiple control flow instructions (brackets) to one byte (we
' need to be able to jump to them later).

DIM byt AS STRING   ' byte read from file
DIM ins AS STRING   ' current repeating instruction
DIM cnt AS INTEGER  ' number of repeating instructions

byt = " "
compiled = ""

' open file, go to start
OPEN FILE FOR BINARY ACCESS READ AS #1
SEEK #1, 1

' read bytes until EOF
DO
    GET #1, , byt
    IF EOF(1) THEN EXIT DO

    IF INSTR(INSTSET, byt) = 0 THEN
        ' not an instruction -> ignore byte
    ELSEIF cnt = 0 THEN
        ' at the beginning -> start new sequence
        ins = byt
        cnt = 1
    ELSEIF INSTR(FLOWINST, byt) OR byt <> ins OR cnt = MAXSEQLEN THEN
        ' current sequence cannot continue -> encode it, start new one
        IF LEN(compiled) = MAXPRGLEN THEN
            PRINT "Compiled program too long."
            END
        END IF
        compiled = compiled + encodeseq$(ins, cnt)
        ins = byt
        cnt = 1
    ELSE
        ' continue current sequence
        cnt = cnt + 1
    END IF
LOOP

' close file
CLOSE

IF cnt THEN
    ' encode last sequence
    IF LEN(compiled) = MAXPRGLEN THEN
        PRINT "Compiled program too long."
        END
    END IF
    compiled = compiled + encodeseq$(ins, cnt)
END IF

END SUB

FUNCTION decodecnt% (byt AS STRING)
' Decode instruction count from encoded sequence. See encodeseq$().

decodecnt% = (ASC(byt) AND &H1F) + 1

END FUNCTION

FUNCTION decodeins$ (byt AS STRING)
' Decode instruction from encoded sequence. See encodeseq$().

decodeins$ = MID$(INSTSET, (ASC(byt) \ 32) + 1, 1)

END FUNCTION

FUNCTION encodeseq$ (ins AS STRING, cnt AS INTEGER)
' Encode a sequence of repeating instructions to a byte.
' Bits: iiiccccc (iii = instruction, ccccc = count - 1)

DIM ind AS INTEGER

ind = INSTR(INSTSET, ins) - 1
encodeseq$ = CHR$(ind * 32 OR cnt - 1)

END FUNCTION

SUB findbrackets (compiled AS STRING, brackets() AS INTEGER)
' Get positions of matching brackets from compiled program.
' (0 means both first byte and no matching bracket, which doesn't matter.)

DIM p AS INTEGER        ' position in compiled program
DIM opencnt AS INTEGER  ' number of brackets currently open

' a stack (last in, first out) of positions of opening brackets
DIM openbrackets(MAXNESTBRA - 1) AS INTEGER

FOR p = 0 TO LEN(compiled) - 1
    SELECT CASE decodeins$(MID$(compiled, p + 1, 1))
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

SUB runprogram (compiled AS STRING, brackets() AS INTEGER)
' Run the compiled Brainfuck program.

DIM ram(RAMSIZE - 1) AS INTEGER

DIM p AS INTEGER    ' pointer to compiled program
DIM r AS INTEGER    ' RAM pointer
DIM byt AS STRING   ' encoded byte
DIM ins AS STRING   ' decoded instruction
DIM cnt AS INTEGER  ' decoded instruction count
DIM inpt AS STRING  ' user input

WHILE p < LEN(compiled)
    ' read and decode compiled byte
    byt = MID$(compiled, p + 1, 1)
    ins = decodeins$(byt)
    cnt = decodecnt%(byt)

    SELECT CASE ins
        ' change value
        CASE "+": ram(r) = (ram(r) + cnt) MOD 256
        CASE "-": ram(r) = (ram(r) - cnt + 256) MOD 256
        ' move pointer
        CASE ">": r = (r + cnt) MOD RAMSIZE
        CASE "<": IF r >= cnt THEN r = r - cnt ELSE r = RAMSIZE - cnt + r
        ' loop (cnt = always 1)
        CASE "[": IF ram(r) = 0 THEN p = brackets(p)
        CASE "]": IF ram(r) THEN p = brackets(p)
        ' input/output
        CASE ","
            PALETTE 0, &H1  ' dark blue
            ram(r) = ASC(INPUT$(cnt))
            PALETTE 0, &H0  ' black
        CASE "."
            SELECT CASE ram(r)
                CASE &HA, &H20 TO &H7E
                    ' Unix newline or printable ASCII
                    PRINT STRING$(cnt, ram(r));
                CASE ELSE
                    PRINT STRING$(cnt, REPLCHAR)
            END SELECT
        CASE ELSE
            PRINT "Invalid instruction decoded (should never happen)."
            END
    END SELECT

    p = p + 1
WEND

END SUB

