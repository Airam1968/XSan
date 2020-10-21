*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa	: 
* Data        : 01/01/99
* Objetivo    : VISUALIZACAO DO ARQUIVO DE COTACOES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
function VIS_COTA()
SAVE SCREEN TO TELA03
PTIT( "VISUALIZACAO DO ARQUIVO DE COTACOES")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   SELE 1
   USE COTACAO INDEX COTACAO
   GOTO TOP
   PLIMPA()
   SET COLOR TO W/N,+W/R
   DECLARE CAMPO[180],PIC[180],CAB[180],LENS[180]
   AFIELDS(CAMPO,.T.,LENS)
   @ 5,00 TO 21,79 DOUBLE
   PLQUA("01")
   SET COLOR TO
   SET COLOR TO W/N+*
   @ 23,00 SAY "[ESC]"
   SET COLOR TO
   SET COLOR TO W/N,+W/R
   @ 23,07 SAY "Para Sair"
   DBEDIT(06,01,21,78,CAMPO,"F_VIS_COTA",PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
**************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_VIS_COTA
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         IF DB_COLUMN = 2 
	    IF F_BLOQR()
               SAVE SCREEN TO T_PROP1
               eCOD_FORN   = FORNECED_1
               SELE 9
               USE FORNECED INDEX FORNECED
               GOTO TOP
               SEEK eCOD_FORN  
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Fornecedor",12)
               @ 13,15 GET NOME
               CLEAR GETS
               SELE 9
               USE
               SELE 1
               PPARA()
               RESTORE SCREEN FROM T_PROP1
	       UNLOCK
	    ENDIF
	 ENDIF
         IF DB_COLUMN = 3
	    IF F_BLOQR()
               SAVE SCREEN TO T_PROP1
               eCOD_FORN   = FORNECED_2
               SELE 9
               USE FORNECED INDEX FORNECED
               GOTO TOP
               SEEK eCOD_FORN  
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Fornecedor",12)
               @ 13,15 GET NOME
               CLEAR GETS
               SELE 9
               USE
               SELE 1
               PPARA()
               RESTORE SCREEN FROM T_PROP1
	       UNLOCK
	    ENDIF
	 ENDIF
         IF DB_COLUMN = 4
	    IF F_BLOQR()
               SAVE SCREEN TO T_PROP1
               eCOD_FORN   = FORNECED_3
               SELE 9
               USE FORNECED INDEX FORNECED
               GOTO TOP
               SEEK eCOD_FORN  
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Fornecedor",12)
               @ 13,15 GET NOME
               CLEAR GETS
               SELE 9
               USE
               SELE 1
               PPARA()
               RESTORE SCREEN FROM T_PROP1
	       UNLOCK
	    ENDIF
	 ENDIF
         IF DB_COLUMN = 5 
	    IF F_BLOQR()
               SAVE SCREEN TO T_PROP1
               eCOD_FORN   = FORNECED_4
               SELE 9
               USE FORNECED INDEX FORNECED
               GOTO TOP
               SEEK eCOD_FORN  
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Fornecedor",12)
               @ 13,15 GET NOME
               CLEAR GETS
               SELE 9
               USE
               SELE 1
               PPARA()
               RESTORE SCREEN FROM T_PROP1
	       UNLOCK
	    ENDIF
	 ENDIF
         IF DB_COLUMN = 6 
	    IF F_BLOQR()
               SAVE SCREEN TO T_PROP1
               eCOD_FORN   = FORNECED_5
               SELE 9
               USE FORNECED INDEX FORNECED
               GOTO TOP
               SEEK eCOD_FORN  
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Fornecedor",12)
               @ 13,15 GET NOME
               CLEAR GETS
               SELE 9
               USE
               SELE 1
               PPARA()
               RESTORE SCREEN FROM T_PROP1
	       UNLOCK
	    ENDIF
	 ENDIF

      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
