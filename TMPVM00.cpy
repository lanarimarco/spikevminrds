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
