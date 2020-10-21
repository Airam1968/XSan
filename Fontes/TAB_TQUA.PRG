*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_TQUA.PRG
* Data        : 01/01/99
* Objetivo    : TABELA DE GRUPO TIPO 1
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_TQUA()
SAVE SCREEN TO TELA03
PTIT( "TABELA DE GRUPO TIPO 1")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   SELE 1
   USE CLAS_PRO INDEX CLAS_PRO
   SET FILTER TO TIPO = 1
   GOTO TOP
   SELE 2
   USE TIPO_PRO INDEX TIPO_PRO
   SET FILTER TO CODIGO = 1
   GOTO TOP
   SELE 3
   USE CAD_QUAD INDEX CAD_QUAD
   GOTO TOP
   PLIMPA()
   SET COLOR TO W/N,+W/R
   DECLARE CAMPO[4],PIC[4],CAB[4],LENS[4]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1] ="@R99999"
   PIC[2] ="@R99999"
   PIC[3] ="@R99999"
   PIC[4] ="@R99999"
   CAB[1] ="No.PRODUTO"
   CAB[2] ="No.GRUPO"
   CAB[3] ="No.INICIAL"
   CAB[4] ="No.FINAL"
   @ 5,00 TO 21,79 DOUBLE
   PLQUA("01")
   SET COLOR TO
   SET COLOR TO W/N+*
   @ 23,00 SAY "[ENTER]"
   @ 23,21 SAY "[ESC]"
   @ 23,37 SAY "[ESPA€O]"
   @ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
   SET COLOR TO
   SET COLOR TO W/N,+W/R
   @ 23,07 SAY "Para Alterar"
   @ 23,26 SAY "Para Sair"
   @ 23,45 SAY "Para Incluir"
   @ 23,65 SAY "Movimenta‡Æo"
   DBEDIT(06,01,21,78,CAMPO,"F_CAD_TQUA",PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
*************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_CAD_TQUA
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 32
	 SAVE SCREEN
	 SET CURSOR ON
	 PLQUA("01")
	 PPERG ( "Confirma Inclusao (S/N) ? ")
	 IF sisRESP = "S"
	    DO WHILE .T.
	       @ 10,10 CLEAR TO 16,70
	       @ 10,10 TO 17,70 DOUBLE
		   PCENTRA( "Dados da Quadra",11)
	       eLOTEAMENTO= 0
	       eQUADRA    = 0
	       eINICIAL   = 0
	       eFINAL     = 0
               @ 13,17 SAY "Produto      : " GET eLOTEAMENTO PICT "99999"
	       READ
	       IF eLOTEAMENTO = 0
		  EXIT
	       ENDIF
	       SELE 1
	       SEEK eLOTEAMENTO
	       IF .NOT. FOUND()
		  PLQUA("01")
                  PMSGR( "Grupo nao Cadastrado !",3)
		  LOOP
	       ELSE
		  @ 13,38 SAY " - "+ALLTRIM(DESCRICAO)
	       ENDIF
	       SELE 3
	       DO WHILE .T.
                  @ 14,17 SAY "Grupo        : " GET eQUADRA    PICT "99999"
		  READ
		  SEEK STR(eLOTEAMENTO,5)+STR(eQUADRA,5)
		  IF FOUND()
		     PLQUA("01") 
                     PMSGR( "Grupo ja cadastrado !",3)
		     LOOP
		  ELSE
		     EXIT
		  ENDIF
	       ENDDO
	       @ 15,17 SAY "Num.Inicial  : " GET eINICIAL     PICT "99999"
	       @ 16,17 SAY "Num.Final    : " GET eFINAL       PICT "99999"
	       READ
	       PLQUA("01")
	       PPERG ( "Confirma (S/N) ? ")
	       IF sisRESP = "S"
		  SELE 3
		  APPEND BLANK
		  IF F_BLOQR()
		     REPLACE NUM_LOTEAM WITH STR(eLOTEAMENTO,5),;
			     NUM_QUADRA WITH STR(eQUADRA,5),;
			     NUM_INICIO WITH eINICIAL
		     REPLACE NUM_FINAL  WITH eFINAL
		     UNLOCK
		  ENDIF
		  EXIT
	       ELSE
		  EXIT
	       ENDIF
	    ENDDO
	 ENDIF
	 SET CURSOR OFF
	 RESTORE SCREEN
      CASE KEY == 13
	 IF DB_COLUMN > 1
	    IF F_BLOQR()
	       SAVE SCREEN
	       SET CURSOR ON
	       FLD_NAME = CAMPO[DB_COLUMN]
	       FLD_PIC  = PIC[DB_COLUMN]
	       @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
	       READ
	       SET CURSOR OFF
	       RESTORE SCREEN
	    ENDIF
	 ENDIF
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
		   
