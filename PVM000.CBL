000000$set OSVS
000000$set REF
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PVM000.
      *
       DATE-COMPILED. 14-02-00 17:14.
      *COMPILATO_ACU  25-01-13 12:21
      *
      *RELEASE    
      *
------*-----------------------------------------------------------
      *
      *		Programma di test per RDS
      *
------*-----------------------------------------------------------
      *
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
      *
------*-----------------------------------------------------------
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
------*-----------------------------------------------------------
      *
      $set rtncode-size(4)
      *
       WORKING-STORAGE      SECTION.
      *
      *
      *------------------- Area Lavoro ----------------------------
      *
      *
      *.... Eventi a video
      *
       77  KEY-STATUS IS SPECIAL-NAMES CRT STATUS PIC 9(5) VALUE 0.
      *
       01  CURRENT-CONTROL		PIC 99 VALUE ZEROES.
      * 
      *.... Handles
      *
       01  FORM1-HANDLE			HANDLE.
       01  WM-EF-HND-EF-1		HANDLE.
       01  WM-EF-HND-EF-2		HANDLE.
       77  KEY-ENTERED  		pic 9(4).

      *
      *
       01  WW-ENTRY-FIELD-1		PIC X(20) VALUE SPACES.
      *
      *
      *------------------------------------------------------------
      *
       LINKAGE SECTION.
      *
      *------------------------------------------------------------
      *
      *
       SCREEN SECTION.
      *
       01  FORM1.
      *
      *.... Bottoni Preferiti
      *
           03 SCREEN-BTN-1, PUSH-BUTTON, 
              COL 04,80, LINE 20,00, LINES 3,00 CELLS, SIZE 18,00 CELLS, 
              SELF-ACT, ENABLED 1, 
      *	      FONT IS MS-SANS-SERIF8, 
	      ID IS 15, 
	      EXCEPTION-VALUE 201,
              TITLE "Copia Entry Field", VISIBLE 1.
      *
      *.... Frames
      *
           03 SCREEN1-FR-1, FRAME, 
              COL 01,20, LINE 02,00, LINES 8,00 CELLS, 
	      SIZE 32,20 CELLS, ENGRAVED,
      *	      FONT MS-SANS-SERIF8,
              TITLE "Prova frame", TITLE-POSITION 3.
      *
      *.... Label
      *
           02 SCREEN1-LB-1, LABEL,
                 COL 002,0, LINE 012,5,
                 PERMANENT,
                 ENABLED 1,
                 VISIBLE 1,
      *******               LINES WMC-LA-HEIGHT,
      *******                 FONT WMC-LA-FONT,
      *******                 LABEL-OFFSET WMC-LA-OFFSET,
      *******                 COLOR WMC-LA-COLOR,
                    TRANSPARENT,
                 LEFT,
                 TITLE "Test label"
                 .
      *
      *.... Entry Field
      *
              02 SCREEN1-EF-1 ,
                 ENTRY-FIELD,
                 COL 002,0, LINE 004,0,
                 LINES 1,5,
                 MAX-TEXT 0030,
                 PERMANENT,
      *******                 FONT WMC-EF-FONT,
      *******                 COLOR WMC-EF-COLOR-LOW,
                 ENABLED 1,
                 VISIBLE 1,
                 WIDTH-IN-CELLS,
                 HEIGHT-IN-CELLS,
                 SIZE 030,0,
                 LEFT,
                 AUTO,
                 NO-BOX,
                 ID 0001,
                 .
      *
      *
      *.... Entry Field
      *
              02 SCREEN1-EF-2 ,
                 ENTRY-FIELD,
                 COL 002,0, LINE 007,0,
      *******                 LINES WMC-EF-HEIGHT,
                 MAX-TEXT 0030,
                 PERMANENT,
      *******                 FONT WMC-EF-FONT,
      *******                 COLOR WMC-EF-COLOR-LOW,
                 ENABLED 0,
                 VISIBLE 1,
                 WIDTH-IN-CELLS,
                 HEIGHT-IN-CELLS,
                 SIZE 030,0,
                 LEFT,
                 AUTO,
                 NO-BOX,
                 ID 0002,
                 .
      *
      *
------*-----------------------------------------------------------
      *
       PROCEDURE DIVISION.
      *
      * 
           PERFORM CREATE-WIN.
      *
	   MOVE 1 TO CURRENT-CONTROL.
      *
      *.... Ciclo richiesta	   
      *	   
       ACCEPT-CONTROL.
      *
      *.... Lettura controlli
      *
           EVALUATE CURRENT-CONTROL
      	   WHEN 1  	ACCEPT WM-EF-HND-EF-1
      				VALUE WW-ENTRY-FIELD-1
				key KEY-ENTERED
	   WHEN 2 	ACCEPT SCREEN-BTN-1 ON EXCEPTION 
			CONTINUE END-ACCEPT
	   END-EVALUATE.
      *
      *
       NEXT-CONTROL.
      *
      *.... Va al controllo successivo
      *
           EVALUATE KEY-STATUS
      *
	   WHEN 27
	   WHEN 107
		GO TO STOP-ROUTINE
      *
	   WHEN 201
	   WHEN 13
		MODIFY WM-EF-HND-EF-2 VALUE WW-ENTRY-FIELD-1
		MODIFY SCREEN1-LB-1 TITLE "Campo copiato"
	   END-EVALUATE.
      *
	   GO TO ACCEPT-CONTROL.
      *
      *
       STOP-ROUTINE.
      *
	   GO TO USCITA.
      *
      *
------*-------------- Creazione finestra ----------------------------- 
      *
       CREATE-WIN.
      *
      *.... Creazione Finestra
      *
           DISPLAY INITIAL GRAPHICAL WINDOW
	      SCREEN COL 1, SCREEN LINE 1, LINES 30,50, SIZE 50,10,
              CELL HEIGHT 10, CELL WIDTH 10, 
      ******	      COLOR 65793,
              LINK TO THREAD, MODELESS, NO SCROLL, TITLE-BAR,
              AUTO-MINIMIZE, 
	      NO WRAP,
              HANDLE IS FORM1-HANDLE.
      *
           DISPLAY FORM1 UPON FORM1-HANDLE.
           SET INPUT WINDOW TO FORM1-HANDLE.

	   SET WM-EF-HND-EF-1 TO HANDLE SCREEN1-EF-1.
	   SET WM-EF-HND-EF-2 TO HANDLE SCREEN1-EF-2.
      *
      *
------*--------------------- Lancio funzioni -------------------------
      *
      *
      *
      *------------------------------------------------------------
      *
       ATTESA.
      *....  Wait per 1 secondo
                CALL "attesa".
      *
      *
      *
      *
      *------------------------------------------------------------
      *
      *
       USCITA.
      *	   
           EXIT PROGRAM.
      *	   
           STOP RUN.
