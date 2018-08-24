( +++// BINARY HUFFMAN TREE COMPRESSOR //+++ )

( Compresses data at the bit level. The effectiveness of this method is highly dependent upon the type of data being compressed. Set SIZE to at least the size your input in bytes, and run COMPRESS in the following way:

    [size of input] COMPRESS [input]

    for example:
    1024 COMPRESS [1024 BYTE STRING]
    
Compressed text is returned in the CIPHERTEXT memory slot, and the size of the compressed string, which is needed to decompress, is returned on the parameter stack. DECOMP takes the length of the compressed string as an argument and looks for the compressed data in the CIPHERTEXT memory register.)

    
10240 CONSTANT SIZE
    ;
: GRANT                                 ( n -- )
  2 BASE !
  0 DO
    00000000 C,
  LOOP
  DECIMAL
  ;
HERE 48 + CONSTANT NODEDICT
1073 SIZE 2 * + GRANT
: NODEPAD 
    NODEDICT 1024 +
  ;
: STR_LEN
    NODEPAD SIZE +
  ;
: DICTSIZE
    STR_LEN 8 +
  ;
: PADSIZE
    DICTSIZE 1 +
  ;
: FLAG0
    PADSIZE 8 +
  ;
: FLAG1
    FLAG0 8 +
  ;
: PAD0
    FLAG1 8 +
  ;
: PAD1
    PAD0 4 +
  ;
: PAD2 
    PAD1 4 +
  ;
: CIPHERTEXT
    PAD2 8 +
  ;
: CLR_BYTE                              ( addr -- )
    2 BASE !
    00000000 SWAP C!
    DECIMAL
  ;
: CLR_BYTES                             ( addr n -- )
  0
  DO
    DUP I + CLR_BYTE
  LOOP
  DROP 
  ;
: CLR                                   ( -- )
  NODEDICT 1073 SIZE 2 * + CLR_BYTES
  ;
: **                                    ( n m -- n^m )
  DUP 0=
  IF
    DROP DROP
    1
  ELSE
    1 SWAP 0 ?DO OVER * LOOP NIP
  THEN
  ;
: SET                                   ( val addr n -- )
  TUCK 0
  DO
    ROT ROT DUP
    I - 1 - 256 SWAP **  
    ROT SWAP /MOD
    -ROT 2SWAP OVER I + C! 
    ROT SWAP
  LOOP
  DROP DROP DROP
  ;
: GET                                   ( addr n -- val )
  0 -ROT 
  1 - TUCK 0 SWAP 
  DO
    DUP I + C@ 
    ROT DUP I - 256 SWAP ** 
    ROT * -ROT 2SWAP + SWAP ROT
    -1 
  +LOOP
  DROP DROP 
  ;
: BIN2HEX                               ( val -- )
  16 /MOD
  DUP 9 >
  IF
    10 - 65 + EMIT
  ELSE
    48 + EMIT
  THEN
    DUP 9 >
    IF 
      10 - 65 + EMIT
    ELSE
      48 + EMIT
    THEN
  ;
: BIT?                                  ( val place -- )
  2 SWAP ** AND 
  ;
: FLAG0?                                ( place -- bool )
  FLAG0 C@ SWAP BIT?
  ;
: FLAG0!                                ( place -- )
  DUP FLAG0? DUP
  IF
    FLAG0 C@ SWAP - FLAG0 C! DROP
  ELSE
    DROP 2 SWAP ** FLAG0 C@ + FLAG0 C! 
  THEN
  ;
: PAD+                                  ( bool -- )
  IF
    PAD1 DUP C@ 1 + SWAP C!     
  ELSE
    PAD0 DUP C@ 1 + SWAP C!
  THEN
  ;
: TEXT                                  ( delimiter -- )      
  PAD ROT BL FILL 
  WORD COUNT 
  PAD SWAP MOVE 
  ; 
: INPUT                                 ( -- )        
  DUP 1 TEXT 
  PAD CIPHERTEXT 
  ROT MOVE 
  ;
: CT_WRITE                              ( n -- )
    DUP TIB 14 + CIPHERTEXT ROT MOVE
    TIB SWAP 14 + BL FILL
    ;
: CT?                                   ( place -- val )
  CIPHERTEXT + C@
  ;
: ND?                                   ( place -- val )
  5 * NODEDICT + C@
  ;
: NF?                                   ( place -- val )
  5 * 1 + NODEDICT + 4 GET
  ;
: PAD_CAT                               ( -- noderep )
  PAD0 C@ 16 * PAD1 C@ + 
  ;
: DSIZE+                                ( -- )
  DICTSIZE DUP C@ 1 + SWAP C!
  ;
: PSIZE+                                ( -- )
  PADSIZE C@ 1 + PADSIZE C!
  ;
: DICT_APPEND                           ( noderep -- )
  NODEDICT DICTSIZE C@ 5 * + DUP -ROT C!
  1 + 1 SWAP 4 SET  
  ;
: DICT_ADD                              ( place -- )
  5 * 1 + NODEDICT + DUP 4 GET 1 + SWAP 4 SET
  ;
: DICT+                                 ( noderep -- )
  DICTSIZE C@ DUP
  IF
    0
    DO
      DUP I ND? =
      IF
        I DICT_ADD
        LEAVE
      ELSE
        I 
        DICTSIZE C@ 1 - =
        IF
          DUP DICT_APPEND
          DSIZE+ 
        THEN
      THEN 
    LOOP
    DROP
  ELSE
    DROP
    DICT_APPEND
    DSIZE+
  THEN
  ;
: DICT_FORM                             ( -- )
  PAD_CAT
  DICT+ 
  0 PAD0 C!
  0 PAD1 C!
  ;
: OF_CHK                                ( -- )
  PAD0 C@ 15 =
  PAD1 C@ 15 =
  +
  IF
    DICT_FORM
    0 FLAG0?
    IF
      0 FLAG0!
    THEN
    1 FLAG0?
    IF
      1 FLAG0!
    THEN
  THEN
  ;
: CT_BIN                                ( bool -- ) 
  OF_CHK
  IF
    1 FLAG0? 
    IF
    ELSE
      0 FLAG0?
      IF
      ELSE
        0 FLAG0!
      THEN
        1 FLAG0!
    THEN
      1 PAD+ 
  ELSE
    0 FLAG0? 
    IF
      1 FLAG0?
      IF
        DICT_FORM
        1 FLAG0!
      ELSE
      THEN
    ELSE
      0 FLAG0!
    THEN
      0 PAD+
  THEN
  ;
: BIN_IT                                ( val -- )
  0 7 
  DO
    DUP I BIT? CT_BIN
  -1 +LOOP
  DROP
  ; 
: CT_IT                                 ( len -- )
  STR_LEN 8 GET 0 
  DO
    I CT? BIN_IT
  LOOP
  PAD_CAT 
  IF
    DICT_FORM
  THEN    
    0 FLAG0 8 SET
    0 PAD0 8 SET
    0 PAD1 8 SET
    DROP
  ;
: BIN_INIT                              ( len -- comp_str )
  CLR 
  DUP STR_LEN 8 SET
  DUP CT_WRITE
  CT_IT
  ;
: P1_CMP                                ( freq -- )
  PAD1 DUP 4 GET DUP
  0 =
  IF
    DROP 4 SET
    PAD2 8 GET FLAG1 8 SET
  ELSE
    ROT DUP ROT <
    IF
      SWAP 
      4 SET
      PAD2 8 GET FLAG1 8 SET
    ELSE
      SWAP DROP DROP
    THEN  
  THEN  
  ;
: P0_CMP                                ( freq -- )
  PAD0 DUP 4 GET DUP
  0 =
  IF
    DROP 4 SET
    PAD2 8 GET FLAG0 8 SET
  ELSE
    ROT DUP ROT <
    IF
      SWAP DUP 4 GET PAD1 4 SET
      FLAG0 8 GET FLAG1 8 SET
      4 SET
      PAD2 8 GET FLAG0 8 SET
    ELSE
      SWAP DROP P1_CMP  
    THEN
  THEN
  ;
: LST_CMP                               ( -- )
  NODEPAD PADSIZE C@ DUP
  IF
    0
    DO
      5 + DUP C@ 1 + +
    LOOP
  ELSE
    DROP
  THEN
  ;
: SM_CLR                                ( -- )
  0 FLAG0 8 SET
  0 PAD0 8 SET
  0 PAD1 8 SET
  ; 
: F>128?                                ( -- bool )
  FLAG0 8 GET C@ 128 /
  ;
: F0>128?                               ( -- bool )
  FLAG1 8 GET C@ 128 /
  ;
: ID_CAT                                ( -- )
  PADSIZE C@ 128 +
  LST_CMP C!
  ;
: F_CAT                                 ( -- )
  PAD0 4 GET PAD1 4 GET +   
  LST_CMP 1 + 4 SET
  ;
: N_CAT                                 ( -- )
  F>128?
  IF
    LST_CMP 6 +  
    FLAG0 8 GET 5 + DUP C@ DUP >R
    0
    DO
      1 + DUP C@
      ROT DUP -ROT C!
      1 +
      SWAP
    LOOP
    DROP
  ELSE
    1 >R
    FLAG0 8 GET NODEDICT - 5 / 
    LST_CMP 6 + DUP -ROT
    C!
    1 +
  THEN  
    F0>128?
    IF
      FLAG1 8 GET 5 + DUP C@ DUP R> + >R
      0
      DO
        1 + DUP C@
        ROT DUP -ROT C!
        1 +
        SWAP
      LOOP
      DROP
      DROP
    ELSE
        1 R> + >R
        FLAG1 8 GET NODEDICT - 5 / 
        SWAP
        C!
    THEN
        R>
        LST_CMP 5 + C!  
  ;
: REP_CLR                               ( addr -- )
  1 + DUP
  1 SWAP 4 SET
  1 SWAP C!
  ;
: REP+                                  ( addr flag -- )
  SWAP 1 + DUP DUP C@ 1 + SWAP C!
  1 + DUP 3 GET 2 * ROT 
  IF
    1 +
  THEN
    SWAP 3 SET
  ;
: ND_CNCAT                              ( -- )  
  ID_CAT
  F_CAT
  N_CAT 
  F>128?
  IF
    FLAG0 8 GET DUP C@ 128 - SWAP C!
    FLAG0 8 GET 5 + DUP C@ 0
    DO
      1 + DUP C@ 5 * NODEDICT + 0 REP+  
    LOOP
    DROP 
  ELSE
    FLAG0 8 GET DUP REP_CLR 0 REP+ 
    FLAG0 8 GET DUP C@ 128 + SWAP C!
  THEN
  F0>128?
  IF
    FLAG1 8 GET DUP C@ 128 - SWAP C!
    FLAG1 8 GET 5 + DUP C@ 0
    DO
      1 + DUP C@ 5 * NODEDICT + 1 REP+
    LOOP
    DROP
  ELSE
    FLAG1 8 GET DUP REP_CLR 1 REP+ 
    FLAG1 8 GET DUP C@ 128 + SWAP C!
  THEN
      PADSIZE DUP C@ 1 + SWAP C!
  ;
: #PL                                   ( rep -- places )
    16 /MOD + 
    ;
: 2_SM                                  ( -- )
  NODEDICT DICTSIZE C@ DUP
  SM_CLR
  IF
    0
    DO
      DUP C@ 
      128 /
      IF
      ELSE
        DUP PAD2 8 SET
        DUP DUP 1 + 4 GET SWAP C@ #PL *
        P0_CMP
      THEN
        5 +  
    LOOP
    DROP
  ELSE
    DROP DROP
  THEN
      PADSIZE C@ DUP 
      IF
        NODEPAD SWAP
        0 
        DO
      DUP C@ 128 /
          IF
            DUP PAD2 8 SET
            DUP  1 + 4 GET P0_CMP
          THEN
            5 + DUP C@ +
            1 + 
        LOOP
        DROP
      ELSE
        DROP
      THEN
        PAD0 4 GET PAD1 4 GET *
        IF
          ND_CNCAT
          0 
        ELSE
          1
        THEN 
    ;
: ID_RST                                ( -- )
  NODEDICT DICTSIZE C@ 0
  DO
    DUP I 5 * +
    DUP C@ 128 - SWAP C!
  LOOP
  DROP
    0 FLAG0 8 SET
    0 FLAG1 8 SET
  0 PAD0 4 SET
    0 PAD1 4 SET
  0 PAD2 8 SET
  0 PADSIZE 8 SET 
  ; 
: ND_GRP                                ( -- )
  BEGIN
    2_SM
  UNTIL
  ID_RST
  ;
: ND_CHK                                ( -- bool )
  0 NODEDICT DICTSIZE C@ 0
  DO
    DUP I 5 * 2 + + 3 GET PAD2 8 GET =
    IF
      NODEDICT I 5 * + C@ PAD1 C!
      SWAP DROP 1 SWAP LEAVE
    THEN
  LOOP
  DROP
  ; 
: RCNT+                                 ( -- )
  PAD0 DUP C@ 1 + SWAP C!
  ; 
: RCNT-                                 ( -- )
  PAD0 DUP C@ 1 - SWAP C!
  ;
: RP0+                                  ( -- )
  PAD2 DUP 8 GET PAD0 C@ 1 + 2 SWAP ** + SWAP 8 SET 
  RCNT+
  ; 
: RP1+                                  ( -- )
  PAD2 DUP 8 GET PAD0 C@ 2 + 2 SWAP ** + SWAP 8 SET 
  RCNT+
  ;
: RP0-                                  ( -- )
  PAD2 DUP 8 GET PAD0 C@ 2 SWAP **  - SWAP 8 SET 
  RCNT-
  ; 
: RP1-                                  ( -- )
  PAD2 DUP 8 GET PAD0 C@ 1 + 2 SWAP ** - SWAP 8 SET
  RCNT-
  ;
: LST_R                                 ( -- bool )
  PAD2 8 GET  
  PAD0 C@
  DUP -ROT 1 + 2 SWAP ** -
  SWAP 2 SWAP ** - 0 >=
  IF
    1
  ELSE
    0
  THEN
  ;
: REP_M                                 ( -- )
  LST_R
  IF
    RP1- 
    1 FLAG0!
  ELSE
    RP0- 
    0 FLAG0!
  THEN
  ;
: REP_APP                               ( bool -- )
  NODEPAD 
  PADSIZE 8 GET 8 /MOD 
  ROT + SWAP 
  2 SWAP 7 SWAP - ** DUP 128 = 
  IF
    OVER 0 SWAP C!
  THEN
    ROT
    IF
      SWAP DUP C@ ROT
      + SWAP C!
    ELSE
      DROP DROP
    THEN
      PADSIZE DUP 8 GET 1 + SWAP 8 SET
      FLAG1 DUP 8 GET 1 + SWAP 8 SET
    ;
: RV_APP                                ( val -- )
    0 7
  DO
    DUP I 
    2 SWAP **
    AND
    IF
      1 REP_APP
    ELSE
      0 REP_APP
    THEN
      -1
  +LOOP
  DROP
  ;
: REP+2                                 ( -- )
  1 FLAG0? 
  IF
    LST_R 
    IF
      RP1-
    ELSE
      RP0- 
      1 FLAG0!
      0 FLAG0!
    THEN
  ELSE 
    0 FLAG0? 
    IF
      RP1+
      0 FLAG0!
    ELSE
      RP0+  
        0 REP_APP
    THEN
  THEN
    0 FLAG0
    DROP DROP
  ;
: REP_IT                                ( -- )
  BEGIN
    ND_CHK
    IF
      REP_M 
      1 REP_APP
      PAD1 C@ RV_APP
    ELSE 
      REP+2 
    THEN
      PAD0 C@
      IF
        0
      ELSE
        1 FLAG0?
        IF
          1
          1 FLAG0!
        ELSE
          0
        THEN
     THEN
  UNTIL
  ;
: REP_BLD                               ( -- )  
  2 PAD2 8 SET 
  REP_IT
  CR
    3 PAD2 8 SET
  REP_IT
  0 FLAG1 8 SET
  0 FLAG0 8 SET
  0 PAD0 4 SET
  0 PAD1 4 SET
  0 PAD2 8 SET
  ;

( END TREE_REP CONSTRUCTION )

: CNST_ADD                              ( -- )
  NODEDICT DICTSIZE C@ 5 * + DUP
  1 + PAD2 8 GET SWAP 4 SET
  0 SWAP C!
  ;
: CNSTID_ADD                            ( bool -- )
  NODEDICT DICTSIZE C@ 5 * +
  DUP C@ 2 * ROT +
  SWAP C!
  ; 
: P2+                                   ( bool -- )
  PAD2 DUP 8 GET 2 * ROT + SWAP 8 SET
  ;
: P2-                                   ( -- )
  PAD2 DUP 8 GET DUP 2 MOD
  IF
    1 -
    1 FLAG0!
  ELSE
    0 FLAG0!
  THEN
    2 /
  SWAP 8 SET
  ;
: REP_CNST                              ( -- )
  2 PAD2 8 SET
  0 DICTSIZE C!
  CIPHERTEXT 
  BEGIN
    DUP C@ 
    0 7
    DO
      I PAD0 C!
      PAD1 C@
      IF
        DUP 2 I **
        AND
        IF  
          1 CNSTID_ADD
        ELSE
          0 CNSTID_ADD
        THEN
          PAD1 C@ 1 =
          IF
            DICTSIZE DUP C@ 1 + SWAP C!
          THEN
            PAD1 DUP C@ 1 - SWAP C!
            -1
      ELSE
        1 FLAG0?
        IF
          1 FLAG0!
          P2-
          0
        ELSE
          0 FLAG0?
          IF
            0 FLAG0!
            1 P2+
            0
          ELSE
            DUP 2 I **
            AND
            IF
              CNST_ADD
              P2-
              8 PAD1 C!
            ELSE  
              0 P2+
            THEN
              -1
          THEN
        THEN      
      THEN
        PAD2 8 GET 0 =
        IF
          LEAVE
        THEN    
    +LOOP
    DROP
    1 +
    PAD2 8 GET 0 =
    IF
      1
    ELSE
      0
    THEN
  UNTIL
  DROP 
  ;

( END TREE_REP REASSEMBLY )

: CT_APP                                ( bool -- )
  NODEPAD
  STR_LEN 8 GET 8 /MOD 
  ROT + SWAP 
  2 SWAP 7 SWAP - ** DUP 128 = 
  IF
    OVER 0 SWAP C!
  THEN
  ROT
  IF
     SWAP DUP C@ ROT
    + SWAP C!
  ELSE
    DROP DROP
  THEN
    STR_LEN DUP 8 GET 1 + SWAP 8 SET
      ;
: R_DC                                  ( rep -- )
  16 /MOD
  2DUP
  DUP
  IF 
    0 
    DO
      0 CT_APP
    LOOP
  ELSE
    DROP
  THEN
    DUP
    IF 
      0 
      DO
        1 CT_APP
      LOOP
    ELSE
      DROP
    THEN    
  ;
: P2+2                                  ( bool -- )
  P2+
  NODEDICT DICTSIZE C@ 0
  DO
    DUP I 5 * 1 + + 4 GET  
    PAD2 8 GET  = 
    IF  
      DUP I 5 * + C@ R_DC 
      1 PAD2 8 SET
      DROP DROP
    THEN
  LOOP
  DROP
  ;   
: TXT_RC                                ( addr -- )
  1 PAD2 8 SET 
  PADSIZE 8 GET 0  
  0 PAD1 C!
  DO
    DUP C@
    2 PAD0 C@ ** AND 
    IF
      1 P2+2
    ELSE
      0 P2+2
    THEN
      PAD0 C@ 0 =
      IF
        7 PAD0 C!
        1 +
      ELSE
        PAD0 DUP C@ 1 - SWAP C!
      THEN
        
  LOOP
  DROP 
  ;

( END TXT REASSEMBLY )

: ND_SRC                                ( ID -- )
  NODEDICT DICTSIZE C@ 0
  DO
    2DUP I 5 * + C@ =
    IF 
      DUP I 5 * + 1 + DUP C@ SWAP 1 + 3 GET 
      SWAP 1 - 0
      DO
        DUP 2 I ** 
        AND
        IF 
          1 REP_APP 
        ELSE
          0 REP_APP
        THEN
      LOOP
      DROP
      LEAVE
    THEN
  LOOP
  DROP DROP
  ;
: CT_CNST                               ( -- )
  CIPHERTEXT 
  BEGIN
    DUP C@ 
    0 7 
    DO
      DUP 2 I ** AND
      IF
        1 FLAG0?
        IF
        ELSE
          1 FLAG0!
        THEN
          PAD0 DUP C@ 1 + SWAP C! 
      ELSE
        1 FLAG0?
        IF
          PAD0 C@ ND_SRC
          0 PAD0 C!
          1 FLAG0!
        THEN
          0 FLAG0?
          IF
          ELSE
            0 FLAG0!
          THEN
            PAD0 DUP C@ 16 + SWAP C!
      THEN 
        -1  
    +LOOP
    DROP
    DUP CIPHERTEXT STR_LEN 8 GET + 1 - =
    IF
      DROP
      PAD0 C@ ND_SRC
      1
    ELSE
      1 +
      0
    THEN
  UNTIL
  NODEDICT 1024 CLR_BYTES
  STR_LEN 9 CLR_BYTES 
  FLAG0 8 CLR_BYTES
  PAD0 16 CLR_BYTES
  ;

( END CMP_STR CONSTRUCTION )

: ARR_CHK                               ( -- )
    CR CR ." + + + + + + + + + + "
    CR ." S_L:" STR_LEN 8 GET .
    CR ." DS:" DICTSIZE 1 GET .
    CR ." PS:" PADSIZE 8 GET .
    CR ." F0:" FLAG0 8 GET .
    CR ." F1:" FLAG1 8 GET .
    CR ." P0:" PAD0 4 GET .
    CR ." P1:" PAD1 4 GET .
    CR ." P2:" PAD2 8 GET .
    CR ." + + + + + + + + + + + "
    CR
    ;
: CT_CHK                                ( -- )
    NODEPAD PADSIZE 8 GET 8 / 0
    DO
        DUP I + C@ BIN2HEX 
    LOOP
    DROP
    ;

: CMP_OUT                               ( -- )
  CR PADSIZE 8 GET . ." ::"
  FLAG1 8 GET . ." ::"
  ." 0x" CT_CHK
  ;
: CMP_CLR     ( -- )
  CIPHERTEXT SIZE CLR_BYTES
  NODEPAD CIPHERTEXT SIZE MOVE
  NODEPAD SIZE CLR_BYTES
  0 FLAG1 8 SET
  PADSIZE 8 CLR_BYTES 
  ;
: COMPRESS                              ( len -- size[bits] )
  BIN_INIT ND_GRP 
  NODEPAD SIZE CLR_BYTES 
  REP_BLD CT_CNST 
  CMP_OUT
  FLAG1 8 GET 
  CMP_CLR
  ;
: CT_DECODE                             ( -- )
  CR 
  NODEPAD STR_LEN 8 GET 8 / 0 
  DO
    DUP I + C@ EMIT
  LOOP
  DROP
  ;
: DECOMP                                ( len -- )
  PADSIZE 8 SET
  REP_CNST
  TXT_RC 
  CT_DECODE 
  ;
: STR_DECOMP                            ( len -- )  
  SIZE INPUT
  DECOMP
  ;
