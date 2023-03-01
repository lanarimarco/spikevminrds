       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TPVM000.
      *------------------------------------------------*
      *     PROVA VM                                   *
      *------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.                         *CSTD
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
           COPY C43401WK SUPPRESS.                                      *C43401
      *
      *Q
      *--------------- LUNGHEZZA COMMAREA STANDARD
       01   LUNGHEZZE.
      * 05   LEN-MENU          PIC S9(4) COMP  VALUE +1060.             *C43401
        05   LEN-MENU          PIC S9(4) COMP-4  VALUE +1060.           *C43401
      * 05   LEN-COMMAREA      PIC S9(4) COMP  VALUE +1360.             *C43401
        05   LEN-COMMAREA      PIC S9(4) COMP-4  VALUE +1360.           *C43401
       01  FILLER        VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".
           02  EL-LETT         PIC X          OCCURS 36
                                              INDEXED   BY  I-LETT.
      *--------------------------------------------------------------*
      *       FLAGS                                                  *
      *--------------------------------------------------------------*
       01  FLAGS.
        05   FLAG-TROVATO                  PIC 9   VALUE ZERO.
             88  TROVATO                           VALUE ZERO.
        05   DEVIA-DATA-ONLY               PIC 9   VALUE ZERO.
             88  DATA-ONLY                         VALUE 0.
             88  NO-DATA-ONLY                      VALUE 1.
        05   FLAG-MAPFILE                  PIC 9   VALUE ZERO.
             88  MAP-FAIL-2                        VALUE 1.
        05   FLAG-ELIMINA                  PIC 9   VALUE ZERO.
             88  NO-ELIMINA                        VALUE 1.
      *--------------------------------------------------------------*
      *       CAMPI VARI                                             *
      *--------------------------------------------------------------*
       01  CAMPI-VARI.
        05   DFH-CANC           PIC S9(03) COMP-3 VALUE +803.
        05   DFH-CANC-R REDEFINES DFH-CANC.
             10 DFHCANC         PIC X(01).
             10 FILLER          PIC X(01).
        05   MESSAGGIO          PIC X(80) VALUE SPACE.
      * 05   WS-TS-ITEM         PIC S9(4) COMP VALUE ZERO.              *C43401
        05   WS-TS-ITEM         PIC S9(4) COMP-4 VALUE ZERO.            *C43401
        05   WS-TS-NAME         PIC X(8) VALUE SPACE.
        05   WS-TS-NAME-N REDEFINES WS-TS-NAME PIC S9(15) COMP-3.
        05   NOME-PROG          PIC X(8) VALUE SPACE.
        05   PROGR-CHIAMANTE.
             10 FILLER          PIC XXX VALUE "TPP".
             10 NOME-CHATO      PIC X(5) VALUE SPACE.
        05 ERRORI               VALUE SPACE.                            *CSTD
             10 ERR-GENERICO    PIC X.
             10 ERR-TASTO       PIC X.
             10 ALTRI-ERRORI.
                20 ERR-INP           PIC X.
      *--------------------------------------------------------------*
      *       CAMPI PER GESTIONE ERRORI                              *
      *--------------------------------------------------------------*
       01  CAMPI-ERRORI.
      * 05 IND              PIC S9(4) COMP VALUE ZERO.                  *C43401
        05 IND              PIC S9(4) COMP-4 VALUE ZERO.                *C43401
        05 TIPO-ERRORE      PIC 99 VALUE ZERO.
        05 TABELLA-ERRORI   PIC X(79)  VALUE SPACE.                     *CSTD
        05 TAB-ERRORI REDEFINES TABELLA-ERRORI.
         10 ELEM-ERRORE     PIC X(7).
         10 EL-ERRORE   OCCURS 2.
           20 FILLER        PIC X.
           20 TIPO-ERR      PIC XX.
         10 FILLER          PIC X(29).
        05  ELEM-TST-ERR    PIC X(79)  VALUE SPACE.                     *CSTD
      *
      *--------------------------------------------------------------*
      *       TASTO FUNZIONE                                         *
      *--------------------------------------------------------------*
       01  TASTO-FUNZIONE   PIC XX VALUE SPACE.
            88 INVIO         VALUE ZERO.
            88 PF1           VALUE "01".
            88 PF2           VALUE "02".
            88 PF3           VALUE "03".
            88 PF4           VALUE "04".
            88 PF5           VALUE "05".
            88 PF6           VALUE "06".
            88 PF7           VALUE "07".
            88 PF8           VALUE "08".
            88 PF9           VALUE "09".
            88 PF10          VALUE "10".
            88 PF11          VALUE "11".
            88 PF12          VALUE "12".
            88 PF13-23       VALUE "13" THRU "23".
            88 PF24          VALUE "24".
            88 PF25          VALUE "25".   
      *
      *===== COMMAREA TOT - LUNGHEZZA :1060+300 =================
        01 DFH-AREA-TOT.
           02  DFH-AREA-CONTAB.
               03  DFH-CHNTE           PIC X(4) VALUE SPACE.
               03  DFH-CHATO           PIC X(4) VALUE SPACE.
               03  DFH-AREA-LEN        PIC S9(4) COMP-4 VALUE ZERO.     *C43401
               03  FILLER              PIC X(10) VALUE SPACE.
               03  DFH-AREA-PAR        PIC X(1000).
               03  DFH-GGMMAA.
                   04  DFH-GG          PIC X(2) VALUE SPACE.
                   04  DFH-MM          PIC X(2) VALUE SPACE.
                   04  DFH-AA          PIC X(2) VALUE SPACE.
               03  DFH-CURRENT-DATE REDEFINES DFH-GGMMAA PIC 9(6).
               03  FILLER              PIC X(20) VALUE SPACE.
               03  DFH-XCTL.
                   04  DFH-FILLER      PIC X(8) VALUE SPACE.
                   04  DFH-OPIDENT     PIC X(4) VALUE SPACE.
               03  FILLER              PIC XX VALUE SPACE.
      *======== COMMAREA =========== LUNGHEZZA :300 ==================
           03  DFH-AREA-DATI.
             05 CINPUT                  PIC X(50) VALUE SPACE.
             05 COUTPUT                 PIC X(50) VALUE SPACE.
             05 TASTO                    PIC XX VALUE SPACE.
             05 TASTO-N REDEFINES TASTO  PIC 99.
             05 DFH-TABELLA-ERRORI.
              10 DFH-EL-TAB-ERR          PIC X OCCURS 79 VALUE SPACE.    .C2
           02 FILLER                   PIC X(119) VALUE SPACE.
      *---------------------------------------------------------------*
      *      =========  C O P Y   E    I N C L U D E  =========       *
      *---------------------------------------------------------------*
      *COPY TMPVM00.   
      *
       01  MPPVM00-TIPO-MAPPA.
         05  MPPVM00-TIPO-M                PIC X VALUE "N".
         05  MPPVM00-TIPO-SIZE             PIC S9(4) COMP-4 VALUE 2480.
         05  MPPVM00-TIPO-RRCC             PIC S9(4) COMP-4 VALUE 0101.
      *
       01  MPPVM00-NUM-CAMPI               PIC S9(4) COMP-4 VALUE    8.
       01  MPPVM00-PC-INI.
         05  MPPVM00-RR-INI                PIC S9(4) COMP-4 VALUE ZERO.
         05  MPPVM00-CC-INI                PIC S9(4) COMP-4 VALUE ZERO.
      *
       01  MPPVM00-ATTR-INI.
001      05  FILLER                        PIC X(2) VALUE X"F0F5".      MASK
002      05  FILLER                        PIC X(2) VALUE X"F0F5".      DFRAME
003      05  FILLER                        PIC X(2) VALUE X"40F4".      CINP
004      05  FILLER                        PIC X(2) VALUE X"F0F5".      COUT
005      05  FILLER                        PIC X(2) VALUE X"7CF5".      MESS
006      05  FILLER                        PIC X(2) VALUE X"F8F7".      M01
007      05  FILLER                        PIC X(2) VALUE X"50F4".      TASFUN
008      05  FILLER                        PIC X(2) VALUE X"F0F5".      DTASFUN
      *
       01  MPPVM00-TAB-CAMPI.
      *
001      05  FILLER                        PIC S9(4) COMP-4 VALUE   13. MASK
001      05  FILLER                        PIC S9(4) COMP-4 VALUE    5. MASK
001      05  FILLER                        PIC S9(4) COMP-4 VALUE 0075. MASK
001      05  FILLER                        PIC S9(4) COMP-4 VALUE   76. MASK
      *
002      05  FILLER                        PIC S9(4) COMP-4 VALUE   21. DFRAME
002      05  FILLER                        PIC S9(4) COMP-4 VALUE   11. DFRAME
002      05  FILLER                        PIC S9(4) COMP-4 VALUE 0317. DFRAME
002      05  FILLER                        PIC S9(4) COMP-4 VALUE  258. DFRAME
      *
003      05  FILLER                        PIC S9(4) COMP-4 VALUE   35. CINP
003      05  FILLER                        PIC S9(4) COMP-4 VALUE   50. CINP
003      05  FILLER                        PIC S9(4) COMP-4 VALUE 0506. CINP
003      05  FILLER                        PIC S9(4) COMP-4 VALUE  407. CINP
      *
004      05  FILLER                        PIC S9(4) COMP-4 VALUE   88. COUT
004      05  FILLER                        PIC S9(4) COMP-4 VALUE   50. COUT
004      05  FILLER                        PIC S9(4) COMP-4 VALUE 0706. COUT
004      05  FILLER                        PIC S9(4) COMP-4 VALUE  567. COUT
      *
005      05  FILLER                        PIC S9(4) COMP-4 VALUE  141. MESS
005      05  FILLER                        PIC S9(4) COMP-4 VALUE   79. MESS
005      05  FILLER                        PIC S9(4) COMP-4 VALUE 2201. MESS
005      05  FILLER                        PIC S9(4) COMP-4 VALUE 1762. MESS
      *
006      05  FILLER                        PIC S9(4) COMP-4 VALUE  223. M01
006      05  FILLER                        PIC S9(4) COMP-4 VALUE    3. M01
006      05  FILLER                        PIC S9(4) COMP-4 VALUE 2301. M01
006      05  FILLER                        PIC S9(4) COMP-4 VALUE 1842. M01
      *
007      05  FILLER                        PIC S9(4) COMP-4 VALUE  229. TASFUN
007      05  FILLER                        PIC S9(4) COMP-4 VALUE    2. TASFUN
007      05  FILLER                        PIC S9(4) COMP-4 VALUE 2305. TASFUN
007      05  FILLER                        PIC S9(4) COMP-4 VALUE 1846. TASFUN
      *
008      05  FILLER                        PIC S9(4) COMP-4 VALUE  234. DTASFUN
008      05  FILLER                        PIC S9(4) COMP-4 VALUE   72. DTASFUN
008      05  FILLER                        PIC S9(4) COMP-4 VALUE 2308. DTASFUN
008      05  FILLER                        PIC S9(4) COMP-4 VALUE 1849. DTASFUN
      *
       01  MPPVM00I.
         05  FILLER                        PIC X(012).
         05  MASKL                         PIC S9(004) COMP-4.
         05  MASKF                         PIC X(0001).
         05  FILLER                        REDEFINES MASKF.
           10  MASKA                       PIC X(0001).
         05  MASKI                         PIC X(005).
         05  DFRAMEL                       PIC S9(004) COMP-4.
         05  DFRAMEF                       PIC X(0001).
         05  FILLER                        REDEFINES DFRAMEF.
           10  DFRAMEA                     PIC X(0001).
         05  DFRAMEI                       PIC X(011).
         05  CINPL                         PIC S9(004) COMP-4.
         05  CINPF                         PIC X(0001).
         05  FILLER                        REDEFINES CINPF.
           10  CINPA                       PIC X(0001).
         05  CINPI                         PIC X(050).
         05  COUTL                         PIC S9(004) COMP-4.
         05  COUTF                         PIC X(0001).
         05  FILLER                        REDEFINES COUTF.
           10  COUTA                       PIC X(0001).
         05  COUTI                         PIC X(050).
         05  MESSL                         PIC S9(004) COMP-4.
         05  MESSF                         PIC X(0001).
         05  FILLER                        REDEFINES MESSF.
           10  MESSA                       PIC X(0001).
         05  MESSI                         PIC X(079).
         05  M01L                          PIC S9(004) COMP-4.
         05  M01F                          PIC X(0001).
         05  FILLER                        REDEFINES M01F.
           10  M01A                        PIC X(0001).
         05  M01I                          PIC X(003).
         05  TASFUNL                       PIC S9(004) COMP-4.
         05  TASFUNF                       PIC X(0001).
         05  FILLER                        REDEFINES TASFUNF.
           10  TASFUNA                     PIC X(0001).
         05  TASFUNI                       PIC X(002).
         05  DTASFUNL                      PIC S9(004) COMP-4.
         05  DTASFUNF                      PIC X(0001).
         05  FILLER                        REDEFINES DTASFUNF.
           10  DTASFUNA                    PIC X(0001).
         05  DTASFUNI                      PIC X(072).
       01  MPPVM00O                        REDEFINES MPPVM00I.
         05  FILLER                        PIC X(012).
         05  FILLER                        PIC X(0002).
         05  FILLER                        PIC X(0001).
         05  MASKO                         PIC X(005).
         05  FILLER                        PIC X(0002).
         05  FILLER                        PIC X(0001).
         05  DFRAMEO                       PIC X(011).
         05  FILLER                        PIC X(0002).
         05  FILLER                        PIC X(0001).
         05  CINPO                         PIC X(050).
         05  FILLER                        PIC X(0002).
         05  FILLER                        PIC X(0001).
         05  COUTO                         PIC X(050).
         05  FILLER                        PIC X(0002).
         05  FILLER                        PIC X(0001).
         05  MESSO                         PIC X(079).
         05  FILLER                        PIC X(0002).
         05  FILLER                        PIC X(0001).
         05  M01O                          PIC X(003).
         05  FILLER                        PIC X(0002).
         05  FILLER                        PIC X(0001).
         05  TASFUNO                       PIC X(002).
         05  FILLER                        PIC X(0002).
         05  FILLER                        PIC X(0001).
         05  DTASFUNO                      PIC X(072).
      *
      *
       COPY CSMBMSCA.
      *
      *
       COPY CSMAID.
      *
      *
       01   DFHCOMMAREA.
         05 DFH1                PIC X(1060).
         05 DFH2                PIC X(300).
      *===============================================================*
      *===============================================================*
       LINKAGE SECTION.                                                 *C43401
       01  KPJBA   PIC X(502).                                          *C43401
      *     COPY C43400KPJB SUPPRESS.                                   *C43401
           
           COPY C43401EIB SUPPRESS.                                     *C43401

           COPY C43401LK SUPPRESS.                                      *C43401
      
           COPY C43401WS SUPPRESS.                                      *C43401
      *
       PROCEDURE DIVISION USING KPJBA                                   *C43401
                                C43401-EIB                              *C43401
                                C43401-LKSTG                            *C43401
                                C43401-WS.                              *C43401
      *---------------------------------------------------------------- *C43401
       C43401-MAIN SECTION.                                             *C43401
       C43401-INIZIO.                                                   *C43401
      *
           MOVE C43401-LK-C  TO C43401-LK-P.                            *C43401
           MOVE C43401-LK-N  TO C43401-LK-C.                            *C43401
           MOVE SPACE        TO C43401-LK-N                             *C43401
                                C43401-LK-X.                            *C43401
           IF EIBCALEN > ZERO                                           *C43401
              MOVE C43401-COMM-DATI TO DFHCOMMAREA                      *C43401
           END-IF.                                                      *C43401
           MOVE EIBCALEN TO C43401-EIBCALEN.                            *C43401
      *---------------------------------------------------------------- *C43401
      *
           IF  EIBCALEN = ZERO
              MOVE "RICHIAMO ERRATO"       TO MESSAGGIO
              PERFORM SEND-TEXT          THRU EX-SEND-TEXT.
      *
           MOVE LOW-VALUE                  TO MPPVM00O.
           MOVE DFH1                       TO DFH-AREA-TOT.
      *
           IF  DFH-CHNTE     =    "VM00"
               IF EIBAID = DFHPF24
                 GO TO RITORNO-MENU.
      *
           IF  EIBCALEN      =    LEN-MENU
               MOVE "VM00"                 TO DFH-CHNTE
               PERFORM PRIMO-GIRO        THRU EX-PRIMO-GIRO
           ELSE
               MOVE DFHCOMMAREA            TO DFH-AREA-TOT
               PERFORM RICEVO-MAPPA      THRU EX-RICEVO-MAPPA
               PERFORM AGGIORNA-COMMAREA THRU EX-AGGIORNA-COMMAREA
               MOVE LOW-VALUE              TO MPPVM00O
               PERFORM SECONDO-GIRO      THRU EX-SECONDO-GIRO.
      *
      *-------------------------------------------------------------*
      *-----------------    PRIMO     GIRO    ----------------------*
      *-------------------------------------------------------------*
       PRIMO-GIRO.
           MOVE "1"                        TO DEVIA-DATA-ONLY.
           PERFORM INIZIALIZZA           THRU EX-INIZIALIZZA.
           PERFORM RIPRISTINA-ATTR       THRU EX-RIPRISTINA-ATTR.
           PERFORM SEND-MAPPA            THRU EX-SEND-MAPPA.
       EX-PRIMO-GIRO.
           EXIT.
      *-------------------------------------------------------------*
      *-------------------- SECONDO   GIRO -------------------------*
      *-------------------------------------------------------------*
       RICEVO-MAPPA.
      *
           MOVE 1 TO C43401-PF3                                         *C43401
           MOVE 1 TO C43401-PF24.                                       *C43401
           MOVE 1 TO C43401-MAPFAIL.                                    *C43401
           SET C43401-WS-RECEIVE TO TRUE                                *C43401
           MOVE "TMPVM00" TO C43401-WS-MAPSET                           *C43401
           MOVE "MPPVM00" TO C43401-WS-MAP                              *C43401
           CALL "C43401IOM" USING KPJBA                                 *C43401
                                  C43401-WS                             *C43401
                                  C43401-EIB                            *C43401
           MOVE "WS" TO C43401-TIPO-ABN                                 *C43401
           MOVE C43401-WS-STATUS TO C43401-ABEND                        *C43401
           EVALUATE TRUE                                                *C43401
              WHEN EIBAID = DFHPF3                                      *C43401
                 GO TO RITORNO-MENU                                     *C43401
              WHEN EIBAID = DFHPF24                                     *C43401
                 GO TO RITORNO-MENU                                     *C43401
           END-EVALUATE                                                 *C43401
           IF C43401-WS-OK                                              *C43401
              MOVE C43401-WS-DATI TO MPPVM00I                           *C43401
           END-IF                                                       *C43401
           IF NOT C43401-WS-OK                                          *C43401
              SET C43401-NO-IGNORE TO TRUE                              *C43401
              IF C43401-WS-MAPFAIL                                      *C43401
                 EVALUATE C43401-MAPFAIL                                *C43401
                    WHEN   1                                            *C43401
                       GO TO EX-RICEVO-MAPPA                            *C43401
                 END-EVALUATE                                           *C43401
              END-IF                                                    *C43401
              IF C43401-NO-IGNORE                                       *C43401
                 CALL "C43401ABRT" USING C43401-WKSTG                   *C43401
                                       C43401-EIB                       *C43401
                                       C43401-LKSTG                     *C43401
              END-IF                                                    *C43401
           END-IF.                                                      *C43401
       EX-RICEVO-MAPPA.
           EXIT.
      *
       SECONDO-GIRO.
      *
           MOVE SPACE                    TO DFH-TABELLA-ERRORI.           
           MOVE 1                        TO IND.
           PERFORM CONTROLLO-PF        THRU EX-CONTROLLO-PF.
           PERFORM CONTROLLI           THRU EX-CONTROLLI.
           PERFORM ATTR-MAPPA          THRU EX-ATTR-MAPPA.
           PERFORM SEND-MAPPA          THRU EX-SEND-MAPPA.
      *
       EX-SECONDO-GIRO.
           EXIT.
      *
       CONTROLLO-PF.
      *
           MOVE SPACE    TO   ERR-GENERICO.                             *CSTD
           IF (NOT     INVIO  AND
               NOT     PF1    AND
               NOT     PF2    AND
               NOT     PF25)
              MOVE "1" TO ERR-GENERICO
              MOVE "TASTO INVALIDO" TO ELEM-TST-ERR.
      *
       EX-CONTROLLO-PF.
           EXIT.
      *-------------------------------------------------------------*
      *------------ CONTROLLI ED ESECUZIONE ------------------------*
      *-------------------------------------------------------------*
       CONTROLLI.
      *
           PERFORM CTR-CAMPO-INPUT THRU EX-CTR-CAMPO-INPUT.
      *
           IF ERRORI = SPACE  AND INVIO                                 *CSTD
              MOVE CINPUT       TO CINPO
                                   COUTPUT.
      *
       EX-CONTROLLI.
           EXIT.
      *
       CTR-CAMPO-INPUT.
      *              
           MOVE SPACE             TO ERR-INP.
           MOVE SPACE             TO MESSO.
      *
           IF CINPUT = SPACE OR LOW-VALUE
              PERFORM ERRO-CAMPO-INPUT THRU EX-ERRO-CAMPO-INPUT
           END-IF.
      *
       EX-CTR-CAMPO-INPUT.
          EXIT.
      *
       ERRO-CAMPO-INPUT.
      *
           MOVE "1" TO ERR-INP.
           PERFORM CARICA-ERRORI THRU EX-CARICA-ERRORI.
      *
       EX-ERRO-CAMPO-INPUT.
           EXIT.
      *
      *-----------------------------------------------------------
      *         CONTROLLI VARI
      *-----------------------------------------------------------
       CTR-TASTO.
      *
           PERFORM CTR-CON-EIB      THRU  EX-CTR-CON-EIB.
      *
       EX-CTR-TASTO.
           EXIT.
      *
       CTR-CON-EIB.
           IF ERR-TASTO > SPACE                                         *CSTD
              MOVE ZERO TO TASTO-FUNZIONE
              GO TO EX-CTR-CON-EIB.
           IF TASFUNL  > ZERO   OR
              TASFUNA  = DFHCANC
              GO TO EX-CTR-CON-EIB.
           IF EIBAID = DFHCLEAR
              MOVE "25" TO TASTO-FUNZIONE.
           IF EIBAID = DFHENTER
              MOVE "00" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF1
              MOVE "01" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF2
              MOVE "02" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF3
              MOVE "03" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF4
              MOVE "04" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF5
              MOVE "05" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF6
              MOVE "06" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF7
              MOVE "07" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF8
              MOVE "08" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF9
              MOVE "09" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF10
              MOVE "10" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF11
              MOVE "11" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF12
              MOVE "12" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF13
              MOVE "13" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF14
              MOVE "14" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF15
              MOVE "15" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF16
              MOVE "16" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF17
              MOVE "17" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF18
              MOVE "18" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF19
              MOVE "19" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF20
              MOVE "20" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF21
              MOVE "21" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF22
              MOVE "22" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF23
              MOVE "23" TO TASTO-FUNZIONE.
           IF EIBAID = DFHPF24  OR  DFHPA1  OR  DFHPA2
              MOVE "24" TO TASTO-FUNZIONE.
       EX-CTR-CON-EIB.
           EXIT.
      *
      *--------------------------------------------------------------
      *                  AGGIORNAMENTI VARI
      *--------------------------------------------------------------
      *
       AGGIORNA-COMMAREA.
      *
           MOVE "1" TO FLAG-MAPFILE.
      *
           PERFORM CTR-TASTO        THRU EX-CTR-TASTO.
      *
           IF PF25
              MOVE "1" TO DEVIA-DATA-ONLY
              GO TO EX-AGGIORNA-COMMAREA.
      *
           MOVE SPACE                TO COUTPUT.
      *
           IF   CINPL          >        ZERO OR                         *CSTD
                CINPA          =  DFHCANC
              MOVE ZERO TO FLAG-MAPFILE
              MOVE CINPI           TO CINPUT
              INSPECT CINPUT REPLACING ALL LOW-VALUE BY SPACE           *CSTD
              INSPECT CINPUT REPLACING ALL "_" BY SPACE.                *CSTD
      *
           IF CINPUT NOT = SPACE
              MOVE CINPUT                TO COUTPUT.
      *
       EX-AGGIORNA-COMMAREA.
           EXIT.
      *
       INIZIALIZZA.
           MOVE SPACE    TO   ERRORI                                    *CSTD
                              DFH-TABELLA-ERRORI
                              CINPUT
                              COUTPUT.
           MOVE ZERO     TO   TASTO.
       EX-INIZIALIZZA.
           EXIT.
      *
      *
       SEND-MAPPA.
      *
           IF ERR-TASTO > SPACE                                         *CSTD
              MOVE "TASTO INVALIDO" TO ELEM-TST-ERR.  
      *
           MOVE         TABELLA-ERRORI      TO      MESSO.
           MOVE         ELEM-TST-ERR        TO      MESSO.
           MOVE "F3=USCITA INVIO=DUPLICA CAMPO"    TO DTASFUNO.
           PERFORM RIEMPI-MAPPA   THRU  EX-RIEMPI-MAPPA.
           IF ERRORI = SPACE  OR                                        *CSTD
              (ALTRI-ERRORI = SPACES AND ERR-GENERICO > SPACES)
              MOVE -1 TO CINPL.
      *
           IF ERR-INP = "1"
              MOVE "CAMPO IN INPUT A SPAZIO"    TO MESSO
           END-IF.
      *
           IF  DATA-ONLY
               MOVE "TMPVM00" TO C43401-WS-MAPSET                       *C43401
               MOVE "MPPVM00" TO C43401-WS-MAP                          *C43401
               MOVE MPPVM00O TO C43401-WS-DATI                          *C43401
               MOVE MPPVM00-TIPO-MAPPA TO C43401-TIPO-MAPPA             *C43401
               MOVE MPPVM00-NUM-CAMPI TO C43401-NUM-CAMPI               *C43401
               MOVE MPPVM00-TAB-CAMPI TO C43401-TAB-CAMPI               *C43401
               MOVE MPPVM00-ATTR-INI TO C43401-ATTR-INI                 *C43401
               MOVE MPPVM00-PC-INI TO C43401-PC-INI                     *C43401
               SET C43401-WS-DATAONLY TO TRUE                           *C43401
               SET C43401-WS-SEND TO TRUE                               *C43401
               CALL "C43401IOM" USING KPJBA                             *C43401
                                      C43401-WS                         *C43401
                                      C43401-EIB                        *C43401
               MOVE "WS" TO C43401-TIPO-ABN                             *C43401
               MOVE C43401-WS-STATUS TO C43401-ABEND                    *C43401
               IF NOT C43401-WS-OK                                      *C43401
                  SET C43401-NO-IGNORE TO TRUE                          *C43401
                  IF C43401-NO-IGNORE                                   *C43401
                     CALL "C43401ABRT" USING C43401-WKSTG               *C43401
                                           C43401-EIB                   *C43401
                                           C43401-LKSTG                 *C43401
                  END-IF                                                *C43401
               END-IF                                                   *C43401
             ELSE
              MOVE "TMPVM00" TO C43401-WS-MAPSET                        *C43401
              MOVE "MPPVM00" TO C43401-WS-MAP                           *C43401
              MOVE MPPVM00O TO C43401-WS-DATI                           *C43401
              MOVE MPPVM00-TIPO-MAPPA TO C43401-TIPO-MAPPA              *C43401
              MOVE MPPVM00-NUM-CAMPI TO C43401-NUM-CAMPI                *C43401
              MOVE MPPVM00-TAB-CAMPI TO C43401-TAB-CAMPI                *C43401
              MOVE MPPVM00-ATTR-INI TO C43401-ATTR-INI                  *C43401
              MOVE MPPVM00-PC-INI TO C43401-PC-INI                      *C43401
              SET C43401-WS-ERASE TO TRUE                               *C43401
              SET C43401-WS-SEND TO TRUE                                *C43401
              CALL "C43401IOM" USING KPJBA                              *C43401
                                     C43401-WS                          *C43401
                                     C43401-EIB                         *C43401
              MOVE "WS" TO C43401-TIPO-ABN                              *C43401
              MOVE C43401-WS-STATUS TO C43401-ABEND                     *C43401
              IF NOT C43401-WS-OK                                       *C43401
                 SET C43401-NO-IGNORE TO TRUE                           *C43401
                 IF C43401-NO-IGNORE                                    *C43401
                    CALL "C43401ABRT" USING C43401-WKSTG                *C43401
                                          C43401-EIB                    *C43401
                                          C43401-LKSTG                  *C43401
                 END-IF                                                 *C43401
              END-IF.                                                   *C43401
       SEND-MAPPA-1.
      *===================================================== START ==== *C43401
      *    EXEC   CICS RETURN TRANSID    ("VM00")                       *C43401
      *                       COMMAREA   (DFH-AREA-TOT)                 *C43401
      *                       LENGTH     (LEN-COMMAREA)                 *C43401
      *           END-EXEC.                                             *C43401
      *---------------------------------------------------------------- *C43401
           MOVE "U " TO C43401-STARTCODE                                *C43401
           MOVE "VM00" TO EIBTRNID                                      *C43401
           MOVE LEN-COMMAREA TO EIBCALEN                                *C43401
           MOVE DFH-AREA-TOT TO C43401-COMM-DATI                        *C43401
           MOVE SPACE TO C43401-LK-N                                    *C43401
                         C43401-LK-X                                    *C43401
           GO TO C43401-RETURN.                                         *C43401
      *===================================================== END   ==== *C43401
      *
       EX-SEND-MAPPA.
           EXIT.
      *
       RIEMPI-MAPPA.
      *
           MOVE CINPUT       TO CINPO.
      *
           IF CINPUT   = SPACE
              MOVE ALL "_"   TO CINPO.
      *
           MOVE COUTPUT      TO COUTO.
      *
           IF COUTPUT   = SPACE
              MOVE ALL "_"   TO COUTO.
      *
           MOVE TASTO      TO TASFUNI.
      *
           IF ERR-TASTO = SPACE
              MOVE TASTO-N TO TASFUNO.
      *
           INSPECT TASFUNI REPLACING ALL SPACE BY "_".                  .C2
      *
       EX-RIEMPI-MAPPA.
           EXIT.
      *
       ATTR-MAPPA.
      *
           PERFORM RIPRISTINA-ATTR THRU EX-RIPRISTINA-ATTR.
      *
           IF ERR-INP > SPACE                                           *CSTD
              MOVE DFHBMBRY TO CINPA
              MOVE -1 TO CINPL.
      *
           IF ERR-TASTO > SPACE                                         *CSTD
              MOVE DFHBMUNB TO TASFUNA
              MOVE -1 TO TASFUNL.
      *
       EX-ATTR-MAPPA.
           EXIT.
      *
       RIPRISTINA-ATTR.
      *
           MOVE DFHBMASB       TO MESSA.
           MOVE DFHBMASD       TO M01A
           MOVE DFHBMASK       TO DTASFUNA.
           MOVE DFHBMASD       TO TASFUNA.
           MOVE DFHBMUNP       TO CINPA.
      *
       EX-RIPRISTINA-ATTR.
           EXIT.
      *
       CARICA-ERRORI.
      *
           MOVE "ERRORI:" TO ELEM-ERRORE.
      *
           IF IND NOT > 14
              MOVE TIPO-ERRORE TO TIPO-ERR (IND)
              ADD 1 TO IND
           END-IF.
      *
           MOVE "1"         TO DFH-EL-TAB-ERR (TIPO-ERRORE).
      *
       EX-CARICA-ERRORI.
           EXIT.
      *
      *
       SEND-TEXT.
           SET C43401-PR-ERASE TO TRUE                                  *C43401
           MOVE 80 TO C43401-LENGTH                                     *C43401
           CALL "C43401PRTF" USING KPJBA                                *C43401
                                   C43401-EIB                           *C43401
                                   C43401-PR                            *C43401
                                   C43401-LENGTH MESSAGGIO.             *C43401
      *===================================================== START ==== *C43401
      *    EXEC       CICS  RETURN                                      *C43401
      *               END-EXEC.                                         *C43401
      *---------------------------------------------------------------- *C43401
           MOVE "CIXS" TO EIBTRNID                                      *C43401
           MOVE C43401-EIBCALEN TO EIBCALEN                             *C43401
           MOVE DFHCOMMAREA TO C43401-COMM-DATI                         *C43401
           MOVE SPACE TO C43401-LK-N                                    *C43401
                         C43401-LK-X                                    *C43401
           GO TO C43401-RETURN.                                         *C43401
      *===================================================== END   ==== *C43401
       EX-SEND-TEXT.

      *
       RITORNO-MENU.
      *
           MOVE DFH-CHATO TO NOME-CHATO.
           MOVE PROGR-CHIAMANTE TO C43401-CO-PGM                        *C43401
           CALL "C43401CHKF" USING C43401-CO-PGM                        *C43401
                                   C43401-CO-STATUS                     *C43401
           END-CALL                                                     *C43401
           MOVE "CO" TO C43401-TIPO-ABN                                 *C43401
           MOVE C43401-CO-STATUS TO C43401-ABEND                        *C43401
           IF NOT C43401-CO-OK                                          *C43401
              SET C43401-NO-IGNORE TO TRUE                              *C43401
              IF C43401-NO-IGNORE                                       *C43401
                 CALL "C43401ABRT" USING C43401-WKSTG                   *C43401
                                       C43401-EIB                       *C43401
                                       C43401-LKSTG                     *C43401
              END-IF                                                    *C43401
           END-IF                                                       *C43401
           MOVE LEN-MENU TO EIBCALEN                                    *C43401
           MOVE DFH-AREA-CONTAB TO C43401-COMM-DATI                     *C43401
           MOVE PROGR-CHIAMANTE TO C43401-LK-X                          *C43401
           MOVE SPACE TO C43401-LK-N                                    *C43401
           GO TO C43401-RETURN.                                         *C43401
      *
           GOBACK.
      *
      *
       C43401-RETURN SECTION.                                           *C43401
       C43401-RETURN-01.                                                *C43401
       C43401-FINE SECTION.                                             *C43401
       C43401-FINE-01.                                                  *C43401
           GOBACK.                                                      *C43401
