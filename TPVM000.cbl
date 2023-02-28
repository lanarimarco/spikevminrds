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
      *    COPY C43401WK SUPPRESS.                                      *C43401
      *
      * COPY  C43401WK ESPLOSA ALL'INTERNO DEL PROGRAMMA
      *
       01  C43401-WKSTG.
           05 C43401-NOME-PGM            PIC  X(10).
           05 C43401-SORT-RETURN         PIC S9(4) COMP-4 VALUE ZERO.
           05 C43401-ITEM                PIC S9(4) COMP-4.
           05 C43401-LENGTH              PIC S9(4) COMP-4.
           05 C43401-EIBCALEN            PIC S9(9) COMP-4.
           05 C43401-BLANK               PIC X            VALUE SPACE.
           05 C43401-DLY                 PIC 9(6).
           05 C43401-DAY                 PIC 9(5).
           05 C43401-RSMTIME             PIC 9(6).
           05 C43401-DATE.
              10 C43401-AS-AA            PIC  99.
              10 C43401-AS-MM            PIC  99.
              10 C43401-AS-GG            PIC  99.
           05 C43401-TIME.
              10 C43401-AS-HMS-R.
                 15 C43401-AS-HH         PIC  99.
                 15 C43401-AS-MT         PIC  99.
                 15 C43401-AS-SS         PIC  99.
              10 C43401-AS-HMS           REDEFINES C43401-AS-HMS-R
                                         PIC  9(6).
              10 C43401-AS-CT            PIC  99.
           05 C43401-DATA.
              10 C43401-GG               PIC  99.
              10 C43401-S1               PIC  X VALUE "/".
              10 C43401-MM               PIC  99.
              10 C43401-S2               PIC  X VALUE "/".
              10 C43401-AA               PIC  99.
           05 C43401-DB.
              10 C43401-DB-STATUS        PIC  XX VALUE "00".
               88 C43401-DB-OK           VALUE "00".
               88 C43401-DB-OK-DUP       VALUE "02".
               88 C43401-DB-ENDFILE      VALUE "10" THRU "14".
               88 C43401-DB-NOTFND       VALUE "23".
               88 C43401-DB-DUPREC       VALUE "22".
               88 C43401-DB-DUPKEY       VALUE "22".
               88 C43401-DB-NOSPACE      VALUE "24".
               88 C43401-DB-IOERR        VALUE "30" THRU "34"
                                               "36" THRU "43".
               88 C43401-DB-FILENOTFOUND VALUE "35".
               88 C43401-DB-DSIDERR      VALUE "35".
               88 C43401-DB-LENGERR      VALUE "44".
               88 C43401-DB-NOTOPEN      VALUE "46" THRU "49".
               88 C43401-DB-INVREQ       VALUE "4F" THRU "99".
               88 C43401-DB-ERROR        VALUE "01" THRU "99".
              10 C43401-OPEN-STATUS      PIC  X.
               88 C43401-OPEN-OK         VALUE "O".
               88 C43401-OPEN-KO         VALUE "K".
           05 C43401-IGNORE              PIC  X.
               88 C43401-NO-IGNORE       VALUE " ".
               88 C43401-SI-IGNORE       VALUE "S".
           05 C43401-ABEND               PIC  XX          VALUE "00".
           05 C43401-TIPO-ABN            PIC  XX          VALUE SPACE.
           05 C43401-HANDLE-ABEND        PIC  9(4) BINARY VALUE ZERO.
           05 C43401-HNDL-COND.
              10 C43401-TERMIDERR        PIC  9(4) BINARY VALUE ZERO.
              10 C43401-FILENOTFOUND     PIC  9(4) BINARY VALUE ZERO.
              10 C43401-DSIDERR          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-DUPKEY           PIC  9(4) BINARY VALUE ZERO.
              10 C43401-ENDFILE          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-ILLOGIC          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-INVREQ           PIC  9(4) BINARY VALUE ZERO.
              10 C43401-IOERR            PIC  9(4) BINARY VALUE ZERO.
              10 C43401-ISCINVREQ        PIC  9(4) BINARY VALUE ZERO.
              10 C43401-LENGERR          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-NOTAUTH          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-NOTFND           PIC  9(4) BINARY VALUE ZERO.
              10 C43401-NOTOPEN          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-SYSIDERR         PIC  9(4) BINARY VALUE ZERO.
              10 C43401-DUPREC           PIC  9(4) BINARY VALUE ZERO.
              10 C43401-NOSPACE          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-EOC              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-EODS             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-INVMPSZ          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-INVPARTN         PIC  9(4) BINARY VALUE ZERO.
              10 C43401-MAPFAIL          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PARTNFAIL        PIC  9(4) BINARY VALUE ZERO.
              10 C43401-RDATT            PIC  9(4) BINARY VALUE ZERO.
              10 C43401-UNEXPIN          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-IGREQCD          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-IGREQID          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-INVLDC           PIC  9(4) BINARY VALUE ZERO.
              10 C43401-OVERFLOW         PIC  9(4) BINARY VALUE ZERO.
              10 C43401-RETPAGE          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-TSIOERR          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-WKBRK            PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PGMIDERR         PIC  9(4) BINARY VALUE ZERO.
              10 C43401-QIDERR           PIC  9(4) BINARY VALUE ZERO.
              10 C43401-ITEMERR          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-QZERO            PIC  9(4) BINARY VALUE ZERO.
              10 C43401-ERROR            PIC  9(4) BINARY VALUE ZERO.
              10 C43401-ENDDATA          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-TRANSIDERR       PIC  9(4) BINARY VALUE ZERO.
           05 C43401-HNDL-AID.
              10 C43401-PA1              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PA2              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PA3              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF1              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF2              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF3              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF4              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF5              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF6              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF7              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF8              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF9              PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF10             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF11             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF12             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF13             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF14             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF15             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF16             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF17             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF18             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF19             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF20             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF21             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF22             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF23             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-PF24             PIC  9(4) BINARY VALUE ZERO.
              10 C43401-CLEAR            PIC  9(4) BINARY VALUE ZERO.
              10 C43401-CLRPARTN         PIC  9(4) BINARY VALUE ZERO.
              10 C43401-ENTER            PIC  9(4) BINARY VALUE ZERO.
              10 C43401-LIGHTPEN         PIC  9(4) BINARY VALUE ZERO.
              10 C43401-OPERID           PIC  9(4) BINARY VALUE ZERO.
              10 C43401-TRIGGER          PIC  9(4) BINARY VALUE ZERO.
              10 C43401-ANYKEY           PIC  9(4) BINARY VALUE ZERO.
           05 C43401-FLOW-CTL.
              10 C43401-WK-PCNX.
                 15 C43401-WK-P          PIC  X(10).
                 15 C43401-WK-C          PIC  X(10).
                 15 C43401-WK-N          PIC  X(10).
                 15 C43401-WK-X          PIC  X(10).
              10 C43401-CALL-LEVEL       PIC  9(2).
           05 C43401-PR.
              10 C43401-PR-STAMPAIMMED   PIC  X.
               88 C43401-PR-PRINT        VALUE "P".
               88 C43401-PR-NOPRINT      VALUE " ".
              10 C43401-PR-ERASEBUFFER   PIC  X.
               88 C43401-PR-ERASE        VALUE "E".
               88 C43401-PR-NOERASE      VALUE " ".
              10 C43401-PR-NOEDIT        PIC  X.
               88 C43401-PR-NOEDT        VALUE " ".
               88 C43401-PR-SIEDT        VALUE "E".
              10 C43401-PR-HONEOM        PIC  X.
               88 C43401-PR-NONEOM       VALUE " ".
               88 C43401-PR-SINEOM       VALUE "H".
      *
           05 C43401-DTAQ.
              10 C43401-DQ-STATUS        PIC  XX.
               88 C43401-DQ-OK           VALUE "00".
               88 C43401-DQ-QIDERR       VALUE "01".
               88 C43401-DQ-ITEMERR      VALUE "02".
               88 C43401-DQ-LENGERR      VALUE "03".
               88 C43401-DQ-INVREQ       VALUE "04".
               88 C43401-DQ-ERROR        VALUE "01" THRU "05".
              10 C43401-TS-OPE           PIC  XX.
      *
           05 C43401-START.
              10  C43401-CO-INT          PIC  X(001).
                  88  C43401-CO-INTERVAL    VALUE "1".
                  88  C43401-CO-NOTINTERVAL VALUE " ".
              10 C43401-DEV-SW           PIC  X(4).
NT    *       10 C43401-DEV-HW           PIC  X(15).
NT            10 C43401-DEV-HW           PIC  X(60).
              10 C43401-CO-TRANID        PIC  X(4).
              10 C43401-CO-PGM           PIC  X(10).
              10 C43401-CO-STATUS        PIC  XX.
               88 C43401-CO-OK           VALUE "00" "01".
994287         88 C43401-CO-PROSEGUI     VALUE "01", "96", "97".
               88 C43401-CO-TERMIDERR    VALUE "96".
               88 C43401-CO-TRANSIDERR   VALUE "97".
               88 C43401-CO-NOTAUTH      VALUE "98".
               88 C43401-CO-PGMIDERR     VALUE "99".
               88 C43401-CO-ERROR        VALUE "02" THRU "99".
      *
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
               03  DFH-AREA-PAR.
      *COPY TPCPA01.  
      *
      * COPY  TPCPA01 ESPLOSA
      *
      ****************** PA-REC (1000) *********************************
                   05  PA-TIPO       PIC XX VALUE SPACE.
      *            05  PA-LONG       PIC S9(4) COMP VALUE ZERO.         *C43400
                   05  PA-LONG       PIC S9(4) COMP-4 VALUE ZERO.       *C43400
                   05  PA-KEY.
                     10 PA-SIGLA     PIC XX VALUE SPACE.
                     10 PA-CTRL1     PIC X(5) VALUE SPACE.
                     10 PA-COD-FIL   PIC XX VALUE SPACE.
                   05  PA-VARIAZ     PIC 9(7)  COMP-3 VALUE ZERO.
                   05  PA-USO        PIC 9(7)  COMP-3 VALUE ZERO.
                   05  FILLER        PIC X VALUE SPACE.
                   05  PA-GES-COSTI  PIC X VALUE SPACE.
                   05  PA-COD-COSTO  PIC XXX VALUE SPACE.
                   05  PA-VARIO-RA   PIC X VALUE SPACE.
                   05  PA-VARIO-RA9  REDEFINES PA-VARIO-RA PIC 9.
                   05  PA-CONT.
                     10 PA-TEST-C     PIC X VALUE SPACE.
                     10 PA-PART-D     PIC X VALUE SPACE.
                     10 PA-RIEP       PIC X VALUE SPACE.
                     10 PA-EFFETTI    PIC X VALUE SPACE.
                   05  PA-CLI.
                     10 PA-GRUCLI.
      *                12 PA-GRUCLIR PIC X OCCURS 3 INDEXED BY PA-IND20. .CB
                       12 PA-GRUCLIR PIC X OCCURS 3 INDEXED BY PA-IND20  .C2
                                        VALUE SPACE.                     .C2
                     10 PA-SGRCLI.
      *                12 PA-SGRCLIR PIC X OCCURS 3 INDEXED BY PA-IND22. .CB
                       12 PA-SGRCLIR PIC X OCCURS 3 INDEXED BY PA-IND22  .C2
                                        VALUE SPACE.                     .C2
                   05  PA-FOR.
                     10 PA-GRUFOR.
      *                12 PA-GRUFORR PIC X OCCURS 3 INDEXED BY PA-IND21. .CB
                       12 PA-GRUFORR PIC X OCCURS 3 INDEXED BY PA-IND21  .C2
                                        VALUE SPACE.                     .C2
                     10 PA-SGRFOR.
      *                12 PA-SGRFORR PIC X OCCURS 3 INDEXED BY PA-IND23. .CB
                       12 PA-SGRFORR PIC X OCCURS 3 INDEXED BY PA-IND23  .C2
                                        VALUE SPACE.                     .C2
                   05  PA-ES-CO1X     PIC X(4) VALUE SPACE.
                   05  PA-ES-CO1 REDEFINES PA-ES-CO1X.
                     06 PA-ES-CO1G    PIC XX.
                     06 PA-ES-CO1M    PIC XX.
                   05  PA-ES9-CO1 REDEFINES PA-ES-CO1X.
                     06 PA-ES9-CO1G   PIC 99.
                     06 PA-ES9-CO1M   PIC 99.
                   05  PA-ES-CO2X     PIC X(4) VALUE SPACE.
                   05  PA-ES-CO2 REDEFINES PA-ES-CO2X.
                     06 PA-ES-CO2G    PIC XX.
                     06 PA-ES-CO2M    PIC XX.
                   05  PA-ES9-CO2 REDEFINES PA-ES-CO2X.
                     06 PA-ES9-CO2G   PIC 99.
                     06 PA-ES9-CO2M   PIC 99.
                   05  FILLER         PIC X(9) VALUE SPACE.
                   05  PA-CO-ESPL.
      *                               OCCURS 4 INDEXED BY PA-IND4.       .CB
                     10 PA-EL-ESPL    PIC X(10)
                                      OCCURS 4 INDEXED BY PA-IND4        .C2
                                        VALUE SPACE.                     .C2
                   05  PA-IVA.
                     10 PA-IVA-TEST  PIC X VALUE SPACE.
                     10 PA-ACQ       PIC X VALUE SPACE.
                     10 PA-VEN       PIC X VALUE SPACE.
                     10 PA-COR       PIC X VALUE SPACE.
                  05  PA-DA-NUMCC    PIC X(2) VALUE SPACE.
                  05  PA-DA-NUMCC9 REDEFINES PA-DA-NUMCC  PIC 9(2).
                  05  PA-CCOSTO      PIC X VALUE SPACE.
                  05  PA-VALUTE      PIC X VALUE SPACE.
                  05  PA-VARIO-SCAD  PIC X VALUE SPACE.
                  05  PA-VARIO-SCAD9 REDEFINES PA-VARIO-SCAD PIC 9.
                  05  PA-SCADENZE    PIC X VALUE SPACE.
                  05  PA-DA-C        PIC X VALUE SPACE.
                  05  PA-DA-M        PIC X VALUE SPACE.
                  05  PA-VARIO-VAL   PIC X VALUE SPACE.
                  05  PA-VARIO-VAL9 REDEFINES PA-VARIO-VAL PIC 9.
                  05  PA-VARIO-BA    PIC X VALUE SPACE.
                  05  PA-VARIO-BA9 REDEFINES PA-VARIO-BA PIC 9.
                  05  PA-DESC-FATT   PIC X VALUE SPACE.
                  05  PA-TEST-F      PIC X VALUE SPACE.
                  05  PA-VARIO-AG    PIC X VALUE SPACE.
                  05  PA-VARIO-AG9 REDEFINES PA-VARIO-AG PIC 9.
                  05  PA-VARIO-PV    PIC X VALUE SPACE.
                  05  PA-VARIO-PV9 REDEFINES PA-VARIO-PV PIC 9.
                  05  PA-VARIO-ES    PIC X VALUE SPACE.
                  05  PA-VARIO-ES9 REDEFINES PA-VARIO-ES PIC 9.
                  05  PA-DESC-SOLL   PIC X VALUE SPACE.
                  05  PA-DATE-BOLL   PIC X(6) VALUE SPACE.
                  05  PA-DATE-BOLL9 REDEFINES PA-DATE-BOLL PIC 9(6).
                  05  PA-RP-GRUPPO-CONT   PIC X VALUE SPACE.
                  05  PA-SEND-VTAM        PIC X VALUE SPACE.
                  05  PA-CONT-FISC        PIC X VALUE SPACE.
                  05  PA-SIGLA-PROG-CC    PIC XX VALUE SPACE.
                  05  PA-LEN-STAMPA       PIC 999 VALUE ZERO.
                  05  PA-LEN-STAMPAX REDEFINES PA-LEN-STAMPA PIC XXX.
                  05  PA-DA-NUMSA    PIC X(2) VALUE SPACE.
                  05  PA-DA-NUMSA9 REDEFINES PA-DA-NUMSA  PIC 9(2).
                  05  PA-A-NUMSA    PIC X(2) VALUE SPACE.
                  05  PA-A-NUMSA9 REDEFINES PA-A-NUMSA  PIC 9(2).
                  05  PA-BEN-BANC         PIC X VALUE SPACE.
                  05  PA-POS-CONTO        PIC X(2) VALUE SPACE.
                  05  PA-POS-CONTO9 REDEFINES PA-POS-CONTO  PIC 9(2).
      *           05  FILLER              PIC X.
                  05  PA-SOTTOGRU-LIS     PIC X VALUE SPACE.
                  05  PA-GRPP.
      *                           OCCURS 5 INDEXED BY PA-IND7 PA-IND8.   .CB
                    10  PA-ELGR-PP PIC X(3)
                                  OCCURS 5 INDEXED BY PA-IND7 PA-IND8    .C2
                                        VALUE SPACE.                     .C2
                  05  PA-CVPP.
                    10  PA-VARPP     PIC X VALUE SPACE.
                    10  PA-VARPP9 REDEFINES PA-VARPP PIC 9.
                    10  PA-CODVPP    PIC XXX VALUE SPACE.
                  05  PA-EL-CT.
      *                              OCCURS 10 INDEXED BY PA-IND-CT.     .CB
                    10  PA-EL-TP-CO  PIC XX
                                     OCCURS 10 INDEXED BY PA-IND-CT      .C2
                                        VALUE SPACE.                     .C2
                  05  PA-SGL-PRE     PIC X(2) VALUE SPACE.
                  05  PA-MAGG-LIST   PIC S9(4)V9(3) COMP-3 VALUE ZERO.
                  05  PA-CONSEGNA    PIC XXX VALUE SPACE.
                  05  PA-IM-CT.
      *                              OCCURS 10 INDEXED BY PA-IND2.       .CB
                    10  PA-EL-IM     PIC X
                                     OCCURS 10 INDEXED BY PA-IND2        .C2
                                        VALUE SPACE.                     .C2
                  05  PA-VARI.
      *                              OCCURS 3 INDEXED BY PA-IND6.        .CB
                    10  PA-EL-VARI   PIC X
                                     OCCURS 3 INDEXED BY PA-IND6         .C2
                                        VALUE SPACE.                     .C2
                  05  PA-CPAR-NOTE   PIC X VALUE SPACE.
                  05  FILLER         PIC X(6) VALUE SPACE.
                  05  PA-TAB-AGG-CONT.
      *                              OCCURS 5 INDEXED BY PA-IND18.       .CB
                    10  PA-AGG-CONT  PIC X
                                     OCCURS 5 INDEXED BY PA-IND18        .C2
                                        VALUE SPACE.                     .C2
                  05  FILLER         PIC X(10) VALUE SPACE.
                  05  PA-DESCV1.
                   08  PA-DES-CV OCCURS 10 INDEXED BY PA-IND3.
      *             10  PA-EL-CL     PIC X.                              .CB
                    10  PA-EL-CL     PIC X VALUE SPACE.                  .C2
      *             10  PA-EL-FO     PIC X.                              .CB
                    10  PA-EL-FO     PIC X VALUE SPACE.                  .C2
      *             10  PA-EL-CV     PIC X(10).                          .CB
                    10  PA-EL-CV     PIC X(10) VALUE SPACE.              .C2
                  05  PA-PAGREG      PIC 9(5) VALUE ZERO.
                  05  PA-PAGREGX REDEFINES PA-PAGREG PIC X(5).
                  05  FILLER         PIC X(15) VALUE SPACE.
                  05  PA-DESCA       PIC X(10) VALUE SPACE.
                  05  PA-DESCB       PIC X(10) VALUE SPACE.
                  05  FILLER         PIC X(12) VALUE SPACE.
                  05  PA-NUM-NAS     PIC XX VALUE SPACE.
                  05  PA-NUM-NAS9 REDEFINES PA-NUM-NAS   PIC 99.
                  05  PA-TAB-DATE.
                    10  PA-ELEM-DATA OCCURS 4 INDEXED BY PA-IND19.
      *               15 PA-EL-DATA  PIC X(6).                           .CB
                      15 PA-EL-DATA  PIC X(6) VALUE SPACE.               .C2
                      15 PA-EL-DATA9 REDEFINES PA-EL-DATA PIC 9(6).
                      15 FILLER      REDEFINES PA-EL-DATA.
                        20  PA-EL-GG PIC 99.
                        20  PA-EL-MM PIC 99.
                        20  PA-EL-AA PIC 99.
                  05  PA-SIGSEC.
      *                              OCCURS 10 INDEXED BY PA-IND5.       .CB
                    10  PA-EL-SIGSEC PIC XX
                                     OCCURS 10 INDEXED BY PA-IND5        .C2
                                        VALUE SPACE.                     .C2
                  05  PA-PERC-VAR-PREZZI PIC 999V99 COMP-3 VALUE ZERO.
                  05  PA-CONTR-GIAC  PIC X VALUE SPACE.
                  05  PA-CONTR-FIDO  PIC X VALUE SPACE.
                  05  PA-ALLEGATO-MAGA    PIC X(6) VALUE SPACE.
                  05  PA-ALLEGATO-MAGA9 REDEFINES
                                        PA-ALLEGATO-MAGA PIC 9(6).
                  05  PA-BOLLATO-MAGA    PIC X(6) VALUE SPACE.
                  05  PA-BOLLATO-MAGA9 REDEFINES
                                       PA-BOLLATO-MAGA PIC 9(6).
      *           05  FILLER         PIC X(10).
                  05  PA-VA-TIPOLOGIA  PIC X VALUE SPACE.
                  05  PA-GES-DATSCA    PIC X VALUE SPACE.
                  05  PA-GES-SCO       PIC X VALUE SPACE.
                  05  PA-GES-LISV      PIC X VALUE SPACE.
                  05  PA-GES-LISA      PIC X VALUE SPACE.
                  05  PA-GES-LISS      PIC X VALUE SPACE.
                  05  PA-COD-LISS      PIC XXX VALUE SPACE.
                  05  PA-VAR-LIS       PIC X VALUE SPACE.
                  05  PA-OPE-CO.
      *                              OCCURS 10 INDEXED BY PA-IND10.      .CB
                     10 PA-EL-OPE-CO PIC XXX
                                     OCCURS 10 INDEXED BY PA-IND10       .C2
                                        VALUE SPACE.                     .C2
                   05  PA-CONTI-CO.
      *                              OCCURS 10 INDEXED BY PA-IND11.      .CB
                     10 PA-EL-CO-CO  PIC X(10)
                                     OCCURS 10 INDEXED BY PA-IND11       .C2
                                        VALUE SPACE.                     .C2
                   05  PA-MAGA.
                     10 PA-TEST-M    PIC X VALUE SPACE.
                     10 PA-TEST-DB   PIC X VALUE SPACE.
                     10 PA-KEYALFA   PIC X VALUE SPACE.
                     10 PA-SCAFFALE  PIC X VALUE SPACE.
                     10 PA-PROVVIG   PIC X VALUE SPACE.
                  05  PA-GRUPPO-LIFO PIC X VALUE SPACE.
                  05  PA-PREZ-MAT    PIC X VALUE SPACE.
                  05  PA-VIA-LIFO    PIC X VALUE SPACE.
                  05  PA-VIA-LIFO9 REDEFINES PA-VIA-LIFO PIC 9.
                  05  PA-LIV-FISCALE PIC 9 VALUE ZERO.
                  05  PA-LIV-FISC REDEFINES PA-LIV-FISCALE PIC X.
                  05  PA-LISTINI     PIC X VALUE SPACE.
                  05  PA-CONVERSIONE PIC X VALUE SPACE.
                  05  PA-GEST-MAGA   PIC X VALUE SPACE.
                  05  PA-GEST-SCAF   PIC X VALUE SPACE.
                  05  PA-GEST-SCAF-FISSO PIC X VALUE SPACE.
                  05  PA-GEST-MAGA-FISC PIC X VALUE SPACE.
                  05  PA-VARIO-SC    PIC X VALUE SPACE.
                  05  PA-NUM-SC      PIC X VALUE SPACE.
                  05  PA-NUM-SCR REDEFINES PA-NUM-SC PIC 9.
                  05  PA-NOTE-PROD   PIC X VALUE SPACE.
                  05  PA-DISPONIB    PIC X VALUE SPACE.
                  05  PA-GRUPPO-PROV PIC X VALUE SPACE.
                  05  PA-CONFEZIONE  PIC X VALUE SPACE.
                  05  PA-VARIO-LAVOR PIC X VALUE SPACE.
                  05  PA-VARIO-LIST  PIC X VALUE SPACE.
                  05  PA-VARIO-LISTR REDEFINES PA-VARIO-LIST PIC S9.
                  05  PA-IM-MG.
      *                              OCCURS 20 INDEXED BY PA-IND1.       .CB
                    10  PA-EL-IMMG   PIC X
                                     OCCURS 20 INDEXED BY PA-IND1        .C2
                                        VALUE SPACE.                     .C2
                  05  PA-EL-MT.
      *                              OCCURS 10 INDEXED BY PA-IND-MT.     .CB
                    10  PA-EL-TP-MG  PIC XX
                                     OCCURS 10 INDEXED BY PA-IND-MT      .C2
                                        VALUE SPACE.                     .C2
                  05  PA-POS-PREZZO-TRA                   PIC X(2)
                                        VALUE SPACE.
                  05  PA-POS-PREZZO-TRA9
                             REDEFINES PA-POS-PREZZO-TRA  PIC 9(2).
                  05  PA-MAGA-FISC   PIC XX VALUE SPACE.
                  05  PA-GEST-LIFO   PIC X VALUE SPACE.
                  05  PA-ANNO-LIFO   PIC 9(2) VALUE ZERO.
                  05  PA-ANNO-LIFOX REDEFINES PA-ANNO-LIFO PIC X(2).
                  05  PA-OPE-MG.
      *                              OCCURS 10 INDEXED BY PA-IND12.      .CB
                     10 PA-EL-OPE-MG PIC XXX
                                     OCCURS 10 INDEXED BY PA-IND12       .C2
                                        VALUE SPACE.                     .C2
                   05  PA-CONTI-MG.
      *                              OCCURS 10 INDEXED BY PA-IND13.      .CB
                     10 PA-EL-CO-MG  PIC X(4)
                                     OCCURS 10 INDEXED BY PA-IND13       .C2
                                        VALUE SPACE.                     .C2
                  05  PA-OPE-MG2.
      *                              OCCURS 10 INDEXED BY PA-IND17.      .CB
                     10 PA-EL-OPE-MG2 PIC XXX
                                     OCCURS 10 INDEXED BY PA-IND17       .C2
                                        VALUE SPACE.                     .C2
                  05  PA-OPE-CO2.
      *                              OCCURS 10 INDEXED BY PA-IND15.      .CB
                    10 PA-EL-OPE-CO2 PIC XXX
                                     OCCURS 10 INDEXED BY PA-IND15       .C2
                                        VALUE SPACE.                     .C2
                  05  PA-DES-MG.
      *                              OCCURS 10 INDEXED BY PA-IND9.       .CB
                    10 PA-EL-MG      PIC X(10)
                                     OCCURS 10 INDEXED BY PA-IND9        .C2
                                        VALUE SPACE.                     .C2
                  05  PA-VARIE-DES-MG.
                    10 PA-ELEM-DES-MG OCCURS 6 INDEXED BY PA-IND14.
      *                15 PA-EL-DES-MG PIC X(10).                        .CB
                       15 PA-EL-DES-MG PIC X(10) VALUE SPACE.            .C2
      *                15 PA-EL-NUM-MG PIC X.                            .CB
                       15 PA-EL-NUM-MG PIC X VALUE SPACE.                .C2
                  05  PA-PROG-CC       PIC X(2) VALUE SPACE.
                  05  PA-GEST-COMMESSE PIC X VALUE SPACE.
                  05  PA-GEST-CC       PIC X VALUE SPACE.
                  05  PA-COD-IVA.
      *                              OCCURS 10 INDEXED BY PA-IND16.      .CB
                    10 PA-EL-COD-IVA PIC XXX
                                     OCCURS 10 INDEXED BY PA-IND16       .C2
                                        VALUE SPACE.                     .C2
      * FINE COPY TPCPA01
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
      *COPY CSMBMSCA.
      *
      * COPY CSMBM SCA ESPLOSA
      *
       01      CSMBMSCA.
           02    DFHBMIFI  PIC     X   VALUE  IS  X"1D".
           02    DFHBMEOF  PIC     X   VALUE  IS  X"80".
           02    DFHBMPEM  PIC     X   VALUE  IS  X"19".
           02    DFHBMPNL  PIC     X   VALUE  IS  X"15".
           02    DFHBMPFF  PIC     X   VALUE  IS  X"0C".
           02    DFHBMPCR  PIC     X   VALUE  IS  X"0D".
           02    DFHBMASK  PIC     X   VALUE  IS  X"F0".
           02    DFHBMUNP  PIC     X   VALUE  IS  X"40".
           02    DFHBMUNN  PIC     X   VALUE  IS  X"50".
           02    DFHBMPRO  PIC     X   VALUE  IS  X"60".
           02    DFHBMBRY  PIC     X   VALUE  IS  X"C8".
           02    DFHBMDAR  PIC     X   VALUE  IS  X"4C".
           02    DFHBMFSE  PIC     X   VALUE  IS  X"C1".
           02    DFHBMPRF  PIC     X   VALUE  IS  X"61".
           02    DFHBMASF  PIC     X   VALUE  IS  X"F1".
           02    DFHBMASB  PIC     X   VALUE  IS  X"F8".
           02    DFHBMASD  PIC     X   VALUE  IS  X"7C".
           02    DFHBMUNB  PIC     X   VALUE  IS  X"D8".
           02    DFHSA     PIC     X   VALUE  IS  X"28".
           02    DFHCOLOR  PIC     X   VALUE  IS  X"42".
           02    DFHPS     PIC     X   VALUE  IS  X"43".
           02    DFHHLT    PIC     X   VALUE  IS  X"41".
           02    DFH3270   PIC     X   VALUE  IS  X"C0".
           02    DFHVAL    PIC     X   VALUE  IS  X"C1".
           02    DFHALL    PIC     X   VALUE  IS  X"00".
           02    DFHERROR  PIC     X   VALUE  IS  X"3F".
           02    DFHDFT    PIC     X   VALUE  IS  X"FF".
           02    DFHDFCOL  PIC     X   VALUE  IS  X"FF".
           02    DFHBLUE   PIC     X   VALUE  IS  X"F1".
           02    DFHRED    PIC     X   VALUE  IS  X"F2".
           02    DFHPINK   PIC     X   VALUE  IS  X"F3".
           02    DFHGREEN  PIC     X   VALUE  IS  X"F4".
           02    DFHTURQ   PIC     X   VALUE  IS  X"F5".
           02    DFHYELLO  PIC     X   VALUE  IS  X"F6".
           02    DFHNEUTR  PIC     X   VALUE  IS  X"F7".
           02    DFHBASE   PIC     X   VALUE  IS  X"FF".
           02    DFHDFHI   PIC     X   VALUE  IS  X"FF".
           02    DFHBLINK  PIC     X   VALUE  IS  X"F1".
           02    DFHREVRS  PIC     X   VALUE  IS  X"F2".
           02    DFHUNDLN  PIC     X   VALUE  IS  X"F4".
           02    DFHMFIL   PIC     X   VALUE  IS  X"04".
           02    DFHMENT   PIC     X   VALUE  IS  X"02".
           02    DFHMFE    PIC     X   VALUE  IS  X"06".
           02    DFHBMABF  PIC     X   VALUE  IS  X"C9".
           02    DFHBMFSN  PIC     X   VALUE  IS  X"D1".
           02    DFHBMNBF  PIC     X   VALUE  IS  X"D9".
      *
      *COPY CSMAID.
      *
      * COPY CSMAID SCA ESPLOSA
      *
      *===== COMMAREA TOT - LUNGHEZZA :1060+300 =================
       01    CSMAID.
           02  DFHNULL   PIC  X  VALUE IS X"00".
           02  DFHENTER  PIC  X  VALUE IS X"7D".
           02  DFHCLEAR  PIC  X  VALUE IS X"6D".
           02  DFHPEN    PIC  X  VALUE IS X"7E".
           02  DFHOPID   PIC  X  VALUE IS X"E6".
           02  DFHMSRE   PIC  X  VALUE IS X"E7".
           02  DFHSTRF   PIC  X  VALUE IS X"88".
           02  DFHTRIG   PIC  X  VALUE IS X"7D".
           02  DFHPA1    PIC  X  VALUE IS X"6C".
           02  DFHPA2    PIC  X  VALUE IS X"6E".
           02  DFHPA3    PIC  X  VALUE IS X"6B".
           02  DFHPF1    PIC  X  VALUE IS X"F1".
           02  DFHPF2    PIC  X  VALUE IS X"F2".
           02  DFHPF3    PIC  X  VALUE IS X"F3".
           02  DFHPF4    PIC  X  VALUE IS X"F4".
           02  DFHPF5    PIC  X  VALUE IS X"F5".
           02  DFHPF6    PIC  X  VALUE IS X"F6".
           02  DFHPF7    PIC  X  VALUE IS X"F7".
           02  DFHPF8    PIC  X  VALUE IS X"F8".
           02  DFHPF9    PIC  X  VALUE IS X"F9".
           02  DFHPF10   PIC  X  VALUE IS X"7A".
           02  DFHPF11   PIC  X  VALUE IS X"7B".
           02  DFHPF12   PIC  X  VALUE IS X"7C".
           02  DFHPF13   PIC  X  VALUE IS X"C1".
           02  DFHPF14   PIC  X  VALUE IS X"C2".
           02  DFHPF15   PIC  X  VALUE IS X"C3".
           02  DFHPF16   PIC  X  VALUE IS X"C4".
           02  DFHPF17   PIC  X  VALUE IS X"C5".
           02  DFHPF18   PIC  X  VALUE IS X"C6".
           02  DFHPF19   PIC  X  VALUE IS X"C7".
           02  DFHPF20   PIC  X  VALUE IS X"C8".
           02  DFHPF21   PIC  X  VALUE IS X"C9".
           02  DFHPF22   PIC  X  VALUE IS X"4A".
           02  DFHPF23   PIC  X  VALUE IS X"4B".
           02  DFHPF24   PIC  X  VALUE IS X"4C".

       01  TPF-REC.
      *COPY TPRFNUM.     
      *
      * ESPLOSIONE COPY TPRFNUM
      *
      * **********        COMMAREA TPFNUM (66)   ***********
      * ------------------------------------------------------------
      *                   LEGENDA                                  *
      *    TPF-TPFNUM       :  SIGLA PGR CHIAMATO                  *
      *    TPF-SIGLA-PGR    :  SIGLA PGR CHIAMANTE                 *
      *    TPF-LEN          :  LUNGHEZZA COMMAREA (66)             *
      *    TPF-CAMPO        :  CAMPO DA CONTROLLARE/CONTROLLATO    *
      *    TPF-NRO-DECIMALI :  NUMERO DECIMALI EFFETTIVI           *
      *    TPF-NRO-INTERI   :  NUMERO INTERI EFFETTIVI             *
      *    TPF-SEGNO        :  SEGNO DIGITATO                      *
      *    TPF-SWERR        :  ERRORI DIGITAZIONE                  *
      * ------------------------------------------------------------
           02  TPF-TPFNUM      PIC X(4) VALUE SPACE.
           02  TPF-SIGLA-PGR   PIC X(4) VALUE SPACE.
      *    02  TPF-LEN         PIC S9(4)   COMP  VALUE +66.             *C43400
           02  TPF-LEN         PIC S9(4)   COMP-4  VALUE +66.           *C43400
           02  FILLER          PIC X(10) VALUE SPACE.
           02  TPF-CAMPO       PIC X(24) VALUE SPACE.
           02  FILLER REDEFINES TPF-CAMPO.
              03 TPF-ELEMENTO  OCCURS 24 INDEXED BY TPF-IND1 TPF-IND2.
                 04 TPF-EL     PIC X.
           02 TPF-CAMP-1     REDEFINES TPF-CAMPO.
               03 FILLER       PIC X(6).
               03 TPF-CAMPO-R  PIC S9(18).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X.
                  04 TPF-CAMPO-L17 PIC S9(17).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC XX.
                  04 TPF-CAMPO-L16 PIC S9(16).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC XXX.
                  04 TPF-CAMPO-L15 PIC S9(15).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(4).
                  04 TPF-CAMPO-L14 PIC S9(14).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(5).
                  04 TPF-CAMPO-L13 PIC S9(13).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(6).
                  04 TPF-CAMPO-L12 PIC S9(12).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(7).
                  04 TPF-CAMPO-L11 PIC S9(11).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(8).
                  04 TPF-CAMPO-L10 PIC S9(10).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(9).
                  04 TPF-CAMPO-L9 PIC S9(9).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(10).
                  04 TPF-CAMPO-L8 PIC S9(8).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(11).
                  04 TPF-CAMPO-L7 PIC S9(7).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(12).
                  04 TPF-CAMPO-L6 PIC S9(6).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(13).
                  04 TPF-CAMPO-L5 PIC S9(5).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(14).
                  04 TPF-CAMPO-L4 PIC S9(4).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(15).
                  04 TPF-CAMPO-L3 PIC S9(3).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(16).
                  04 TPF-CAMPO-L2 PIC S9(2).
               03 FILLER  REDEFINES TPF-CAMPO-R.
                  04 FILLER    PIC X(17).
                  04 TPF-CAMPO-L1 PIC S9.
           02 TPF-NRO-DECIMALI PIC 99 VALUE ZERO.
           02 TPF-NRO-INTERI   PIC 99 VALUE ZERO.
           02 TPF-SEGNO        PIC X VALUE SPACE.
           02 TPF-SWERR        PIC X VALUE SPACE.
           02 TPF-DECPOINT     PIC X VALUE SPACE.
           02 FILLER           PIC X(15) VALUE SPACE.

      *
      *01  LEN-TF-AREA  PIC S9(4) COMP VALUE +680.                      *C43401
       01  LEN-TF-AREA  PIC S9(4) COMP-4 VALUE +680.                    *C43401
      *01  TF-AREA.
      *COPY TPRFI01.
      *   02 TF-REC     PIC X(330) VALUE SPACE.
      *
       01   DFHCOMMAREA.
         05 DFH1                PIC X(1060).
         05 DFH2                PIC X(300).
      *===============================================================*
      *===============================================================*
       LINKAGE SECTION.                                                 *C43401
       01  KPJBA.                                                       *C43401
      *     COPY C43400KPJB SUPPRESS.                                   *C43401
      * COPY ESPLOSA
         05  KPJBA-RECORD PIC X(502).
         05  FMKPJBA       REDEFINES KPJBA-RECORD.
           06 CSM-UTENTE            PIC X(15).
           06 CSM-TERM              PIC X(15).
           06 CSM-NRJOB             PIC 9(6).
           06 CSM-NOMEJOB           PIC X(10).
           06 CSM-DATAJOB           PIC X(6).
           06 CSM-I-B               PIC X(1).
           06 CSM-GEST-GRAF         PIC X(1).
           06 CSM-CAR-GRAF          PIC X(1).
           06 CSM-UX                PIC X(15).
           06 CSM-TMP               PIC X(50).
           06 CSM-SPL               PIC X(50).
           06 CSM-PGM-DA-ESEG       PIC X(10).
           06 CSM-PGM-INI           PIC X(10).
994194     06 CSM-AGG-RPCO          PIC X(1).
994194*    06 FILLER                PIC X(55).
NT         06 CSM-HANDLE-PRINTER    PIC X(4) COMP-5.
NT         06 CSM-FONT-NAME         PIC X(20).
NT         06 CSM-FONT-SIZE         PIC X(2) COMP-5.
NT         06 CSM-ORIEN             PIC X.
           06 CSM-LINGUA            PIC X(3).
NEW        06 CSM-UTENTE-RDS        PIC X(10).
NEW        06 CSM-CHIUDI-GRAF       PIC X.
FORMDT     06 CSM-FMTDEC            PIC X.
FORMDT     06 CSM-FMTDATE           PIC X.
NT         06 CSM-MEMOR             PIC X.
09I197     06 CSM-ALTRA-START       PIC X.
15I006*13I003     06 FILLER                PIC X(09).
15I006     06 FILLER                PIC X(08).
15I006     06 RDS-CALL              PIC X(01).
13I003     06 RDS-WTRC              PIC X(01).
13I003*09I197     06 FILLER                PIC X(10).
09I197*NT         06 FILLER                PIC X(11).
NT    *    06 FILLER                PIC X(25).
NT    *    06 FILLER                PIC X(14).
           06 CSM-DATI              PIC X(256).
           06 CSTD-KPJBU-RED REDEFINES CSM-DATI.
             08  CSTD-RINP          PIC  X.
             08  CSTD-DAF           PIC  X.
             08  CSTD-DIA           PIC  X(80).
             08  CSTD-PROGRAMMA     PIC  X(10).
             08  CSTD-RETURN-CODE   PIC  XX.
             08  FILLER             PIC  X(162).
           
      *    COPY C43401EIB SUPPRESS.                                     *C43401
      * COPY ESPLOSA
       01 C43401-EIB.
              10 EIBAID                  PIC  X.
              10 EIBATT                  PIC  X.
              10 EIBCALEN                PIC S9(9) COMP-4.
              10 EIBCOMPL                PIC  X.
              10 EIBCONF                 PIC  X.
              10 EIBCPOSN                PIC S9(9) COMP-4.
              10 EIBDATE                 PIC S9(7) COMP-3.
              10 EIBDS                   PIC  X(8).
              10 EIBEOC                  PIC  X.
              10 EIBERR                  PIC  X.
              10 EIBERRCD                PIC  X(4).
              10 EIBFMH                  PIC  X.
              10 EIBFN                   PIC  XX.
              10 EIBFREE                 PIC  X.
              10 EIBNODAT                PIC  X.
              10 EIBRCODE                PIC  X(6).
              10 REDEFINES EIBRCODE.
                 20 EIBRCODE-1           PIC X.
                 20 EIBRCODE-5           PIC X(5).
              10 EIBRECV                 PIC  X.
              10 EIBREQID                PIC  X(8).
              10 EIBRESP                 PIC S9(9) COMP-4.
              10 EIBRESP2                PIC S9(9) COMP-4.
              10 EIBRLDBK                PIC  X.
              10 EIBRSRCE                PIC  X(8).
              10 EIBSIG                  PIC  X.
              10 EIBSYNC                 PIC  X.
              10 EIBSYNRB                PIC  X.
              10 EIBTASKN                PIC S9(7) COMP-3.
              10 EIBTIME                 PIC S9(7) COMP-3.
              10 EIBTRMID                PIC  X(4).
              10 EIBTRNID                PIC  X(4).

      *    COPY C43401LK SUPPRESS.                                      *C43401
      * COPY ESPLOSA
       01  C43401-LKSTG.
           05 C43401-LK-PCNX.
              10 C43401-LK-P               PIC  X(10).
              10 C43401-LK-C               PIC  X(10).
              10 C43401-LK-N               PIC  X(10).
              10 C43401-LK-X               PIC  X(10).
           05 C43401-RETURN-TRANID         PIC  X(10).
           05 C43401-ABCODE                PIC  X(10).

      *
      * Parametri di ASSIGN
      *
           05 C43XASSIGN.
              10 C43401-STARTCODE              PIC  X(2).
              10 C43401-OPID                   PIC  X(4).
              10 C43401-USRID                  PIC  X(10).
      *       10 C43401-PASSWD                 PIC  X(10).
              10 C43401-SCRNWD                 PIC S9(4) COMP-4.
      *
      * Communication area
      *
           05  C43401-COMM-DATI                PIC  X(32700).
      
      *    COPY C43401WS SUPPRESS.                                      *C43401
      * COPY ESPLOSA
      *
      * Parametri di IO video    6700 BY.
      *
       01  C43401-WS.
         05 C43401-WS-OPE                 PIC  XX.
           88 C43401-WS-SEND               VALUE "SN".
           88 C43401-WS-RECEIVE            VALUE "RC".
         05 C43401-WS-STATUS              PIC  XX.
           88 C43401-WS-OK                 VALUE "00".
           88 C43401-WS-MAPFAIL            VALUE "10".
           88 C43401-WS-OVERFLOW           VALUE "OW".
           88 C43401-WS-ERROR              VALUE "ER", "10".
           88 C43401-WS-INVREQ             VALUE "ER".
         05 C43401-WS-MAP                 PIC  X(10).
         05 C43401-WS-MAPSET              PIC  X(10).
         05 C43401-WS-KWERASE             PIC  X.
           88 C43401-WS-ERASE              VALUE "1".
           88 C43401-WS-NOERASE            VALUE " ".
         05 C43401-WS-KWMAPONLY           PIC  X.
           88 C43401-WS-MAPONLY            VALUE "1".
           88 C43401-WS-NOMAPONLY          VALUE " ".
         05 C43401-WS-KWDATAONLY          PIC  X.
           88 C43401-WS-DATAONLY           VALUE "1".
           88 C43401-WS-NODATAONLY         VALUE " ".
         05 C43401-WS-KWALARM             PIC  X.
           88 C43401-WS-ALARM              VALUE "1".
           88 C43401-WS-NOALARM            VALUE " ".
         05 C43401-WS-KWWAIT              PIC  X.
           88 C43401-WS-WAIT               VALUE "1".
           88 C43401-WS-NOWAIT             VALUE " ".
         05 C43401-WS-DATI                PIC X(4000).
         05 C43401-ATTR-INI.
           10 C43401-ELEM-INI  OCCURS 256.
             15 C43401-A-INI              PIC X.
             15 C43401-C-INI              PIC X.
         05 C43401-TIPO-MAPPA.
           10 C43401-TIPO-M               PIC X.
           10 C43401-TIPO-SIZE            PIC S9(4) COMP-4.
           10 C43401-TIPO-RRCC            PIC S9(4) COMP-4.
         05 C43401-NUM-CAMPI              PIC S9(4) COMP-4.
         05 C43401-PC-INI.
           10 C43401-RR-INI               PIC S9(4) COMP-4.
           10 C43401-CC-INI               PIC S9(4) COMP-4.
         05 C43401-PC-CURSOR              PIC S9(4) COMP-4.
         05 C43401-TAB-CAMPI.
           10 C43401-CAMPO    OCCURS 256.
             15 C43401-C-POSC             PIC S9(4) COMP-4.
             15 C43401-C-LUNG             PIC S9(4) COMP-4.
             15 C43401-C-POSM             PIC S9(4) COMP-4.
             15 C43401-C-POSV             PIC S9(4) COMP-4.
         05 FILLER                        PIC X(98).
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
      *===================================================== START ==== *C43401
      *    EXEC CICS   HANDLE AID   PF3       (RITORNO-MENU)            *C43401
      *                             PF24      (RITORNO-MENU)            *C43401
      *                             END-EXEC.                           *C43401
      *---------------------------------------------------------------- *C43401
           MOVE 1 TO C43401-PF3                                         *C43401
           MOVE 1 TO C43401-PF24.                                       *C43401
      *===================================================== END   ==== *C43401
      *===================================================== START ==== *C43401
      *    EXEC CICS   HANDLE CONDITION MAPFAIL (EX-RICEVO-MAPPA)       *C43401
      *                             END-EXEC.                           *C43401
      *---------------------------------------------------------------- *C43401
           MOVE 1 TO C43401-MAPFAIL.                                    *C43401
      *===================================================== END   ==== *C43401
      *===================================================== START ==== *C43401
      *    EXEC CICS   RECEIVE      MAP       ("MPPVM00")               *C43401
      *                             MAPSET    ("TMPVM00")               *C43401
      *                             END-EXEC.                           *C43401
      *---------------------------------------------------------------- *C43401
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
      *===================================================== END   ==== *C43401
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
           MOVE SPACE    TO   ERR-TASTO.                                *CSTD
           MOVE TASTO TO TPF-CAMPO.
           PERFORM LINK-TPFNUM      THRU  EX-LINK-TPFNUM.
      *
           IF TPF-SWERR   >  SPACE   OR
              TPF-CAMPO-R NEGATIVE   OR
              TPF-CAMPO-R >   25     OR
              TPF-NRO-DECIMALI >  ZERO
              MOVE "1" TO ERR-TASTO
           ELSE
              MOVE TPF-CAMPO-R TO TASTO-N
              MOVE TASTO TO TASTO-FUNZIONE
              MOVE ZERO TO TASTO
           END-IF.
      *
           PERFORM CTR-CON-EIB      THRU  EX-CTR-CON-EIB.
      *
       EX-CTR-TASTO.
           EXIT.
      *
       LINK-TPFNUM.
      *===================================================== START ==== *C43401
      *    EXEC CICS LINK PROGRAM("TPFNUM")                             *C43401
      *                   COMMAREA(TPF-REC)                             *C43401
      *                   LENGTH (TPF-LEN)                              *C43401
      *                   END-EXEC.                                     *C43401
      *---------------------------------------------------------------- *C43401
           MOVE "TPFNUM" TO C43401-LK-N                                 *C43401
           MOVE SPACE TO C43401-LK-X                                    *C43401
           MOVE C43401-LK-PCNX TO C43401-WK-PCNX                        *C43401
           PERFORM TEST AFTER UNTIL C43401-LK-N = SPACE                 *C43401
              MOVE TPF-LEN TO EIBCALEN                                  *C43401
              MOVE TPF-REC TO C43401-COMM-DATI                          *C43401
              CALL C43401-LK-N USING KPJBA                              *C43401
                                     C43401-EIB                         *C43401
                                     C43401-LKSTG                       *C43401
                                     C43401-WS                          *C43401
              ON EXCEPTION                                              *C43401
                 MOVE C43401-EIBCALEN TO EIBCALEN                       *C43401
                 MOVE "LK" TO C43401-TIPO-ABN                           *C43401
                 MOVE "99" TO C43401-ABEND                              *C43401
                 SET C43401-NO-IGNORE TO TRUE                           *C43401
                 IF C43401-NO-IGNORE                                    *C43401
                    CALL "C43401ABRT" USING C43401-WKSTG                *C43401
                                          C43401-EIB                    *C43401
                                          C43401-LKSTG                  *C43401
                 END-IF                                                 *C43401
              END-CALL                                                  *C43401
              MOVE C43401-COMM-DATI (1:TPF-LEN) TO TPF-REC (1:TPF-LEN)  *C43401
              MOVE C43401-EIBCALEN TO EIBCALEN                          *C43401
              IF C43401-LK-X = SPACE                                    *C43401
                 MOVE C43401-WK-PCNX TO C43401-LK-PCNX                  *C43401
                 MOVE SPACE TO C43401-LK-N C43401-LK-X                  *C43401
              ELSE                                                      *C43401
                 MOVE C43401-LK-X TO C43401-WK-N                        *C43401
                 MOVE SPACE TO C43401-WK-X                              *C43401
                 MOVE C43401-WK-PCNX TO C43401-LK-PCNX                  *C43401
              END-IF                                                    *C43401
           END-PERFORM.                                                 *C43401
      *===================================================== END   ==== *C43401
       EX-LINK-TPFNUM.
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
           IF   TASFUNL         >        ZERO OR                        *CSTD
                TASFUNA         =  DFHCANC
              MOVE TASFUNI TO TASTO
              INSPECT TASTO REPLACING ALL "_" BY SPACE.                 *CSTD
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
      *       TRANSFORM CINPUT    FROM LOW-VALUE TO SPACES               .CB
              INSPECT CINPUT REPLACING ALL "_" BY SPACE.                *CSTD
      *       TRANSFORM CINPUT    FROM "_" TO SPACES.                    .CB
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
      *    MOVE         DFH-CURRENT-DATE    TO      SYSDT00O.
      *    MOVE         EIBTIME             TO      SYSOR00O.
      *    MOVE         PA-SIGLA            TO      SIGCD00O.
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
      *===================================================== START ==== *C43401
      *        EXEC CICS SEND MAP ("MPPVM00")                           *C43401
      *                        MAPSET   ("TMPVM00")                     *C43401
      *                        DATAONLY                                 *C43401
      *                        FRSET                                    *C43401
      *                        FREEKB                                   *C43401
      *                        CURSOR                                   *C43401
      *                        END-EXEC                                 *C43401
      *---------------------------------------------------------------- *C43401
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
      *===================================================== END   ==== *C43401
             ELSE
      *===================================================== START ==== *C43401
      *       EXEC CICS SEND MAP ("MPPVM00")                            *C43401
      *                       MAPSET    ("TMPVM00")                     *C43401
      *                       FREEKB                                    *C43401
      *                       CURSOR                                    *C43401
      *                       ERASE                                     *C43401
      *                   END-EXEC.                                     *C43401
      *---------------------------------------------------------------- *C43401
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
      *===================================================== END   ==== *C43401
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
      *    TRANSFORM TASFUNI FROM SPACE TO "_".                         .CB
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
      *===================================================== START ==== *C43401
      *    EXEC CICS SEND TEXT FROM   (MESSAGGIO)                       *C43401
      *                        LENGTH (80)                              *C43401
      *                        ERASE                                    *C43401
      *               END-EXEC.                                         *C43401
      *---------------------------------------------------------------- *C43401
           SET C43401-PR-ERASE TO TRUE                                  *C43401
           MOVE 80 TO C43401-LENGTH                                     *C43401
           CALL "C43401PRTF" USING KPJBA                                *C43401
                                   C43401-EIB                           *C43401
                                   C43401-PR                            *C43401
                                   C43401-LENGTH MESSAGGIO.             *C43401
      *===================================================== END   ==== *C43401
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
      *===================================================== START ==== *C43401
      *    EXEC       CICS  XCTL  PROGRAM   (PROGR-CHIAMANTE)           *C43401
      *                           COMMAREA  (DFH-AREA-CONTAB)           *C43401
      *                           LENGTH    (LEN-MENU)                  *C43401
      *               END-EXEC.                                         *C43401
      *---------------------------------------------------------------- *C43401
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
      *===================================================== END   ==== *C43401
           GOBACK.
      *
      *
       C43401-RETURN SECTION.                                           *C43401
       C43401-RETURN-01.                                                *C43401
       C43401-FINE SECTION.                                             *C43401
       C43401-FINE-01.                                                  *C43401
           GOBACK.                                                      *C43401
