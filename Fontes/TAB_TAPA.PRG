*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_TAPA.PRG
* Data        : 01/01/99
* Objetivo    : TABELA DE GRUPO TIPO 2
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   : 
************************* inicio ************************************
FUNCTION TAB_TAPA()
SAVE SCREEN TO TELA03
PTIT( "TABELA DE GRUPO TIPO 2")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   SELE 1
   USE CLAS_PRO INDEX CLAS_PRO
   SET FILTER TO TIPO = 2 .OR. TIPO = 3
   GOTO TOP
   SELE 2
   USE TIPO_PRO INDEX TIPO_PRO
   SET FILTER TO CODIGO = 2 .OR. CODIGO = 3
   GOTO TOP
   SELE 3
   USE CAD_APAR INDEX CAD_APAR
   GOTO TOP
   PLIMPA()
   SET COLOR TO W/N,+W/R
   DECLARE CAMPO[8],PIC[8],CAB[8],LENS[8]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1] ="@R99999"
   PIC[2] ="@R99999"
   PIC[3] ="@S30"
   PIC[4] ="@R99999"
   PIC[5] ="@S1"
   PIC[6] ="@R999,999,999.99"
   PIC[7] ="@S20"
   PIC[8] ="@S20"
   CAB[1] ="No.PRODUTO"
   CAB[2] ="No.GRUPO"
   CAB[3] ="NOME"
   CAB[4] ="QUANTIDADE"
   CAB[5] ="ATIVO"
   CAB[6] ="VALOR MEDIO"
   CAB[7] ="OBSERVA_1"
   CAB[8] ="OBSERVA_2"
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
   DBEDIT(06,01,21,78,CAMPO,"F_CAD_TAPA2",PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
*************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_CAD_TAPA2
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
	       @ 10,10 CLEAR TO 17,70
	       @ 10,10 TO 17,70 DOUBLE
			   PCENTRA( "Dados Produto",11)
	       eLOTEAMENTO= 0
	       eBLOCO     = 0
	       eNOME      = SPACE(30)
	       eQUANTIDADE= 0
               @ 13,12 SAY "Produto      : " GET eLOTEAMENTO PICT "99999"
	       READ
	       IF eLOTEAMENTO = 0
		  EXIT
	       ENDIF
	       SELE 1
	       SEEK eLOTEAMENTO
	       IF .NOT. FOUND()
		  PLQUA("01")
                  PMSGR( "Produto nao Cadastrado !",3)
		  LOOP
	       ELSE
		  @ 13,33 SAY " - "+ALLTRIM(DESCRICAO)
	       ENDIF
	       SELE 3
	       DO WHILE .T.
                  @ 14,12 SAY "Grupo        : " GET eBLOCO     PICT "99999"
		  READ
		  SEEK STR(eLOTEAMENTO,5)+STR(eBLOCO,5)
		  IF FOUND()
		     PLQUA("01") 
                     PMSGR( "Grupo ja Cadastrado !",3)
		     LOOP
		  ELSE
		     EXIT
		  ENDIF
	       ENDDO
	       @ 15,12 SAY "Nome         : " GET eNOME        PICT "@S30"
	       @ 16,12 SAY "Quantidade   : " GET eQUANTIDADE  PICT "99999"
	       READ
	       PLQUA("01")
	       PPERG ( "Confirma (S/N) ? ")
	       IF sisRESP = "S"
		  SELE 3
		  APPEND BLANK
		  IF F_BLOQR()
		     REPLACE NUM_LOTEAM WITH STR(eLOTEAMENTO,5),;
			     NUM_BLOCO  WITH STR(eBLOCO,5),;
			     QUANTIDADE WITH eQUANTIDADE
		     REPLACE DESCRICAO  WITH eNOME
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
         IF DB_COLUMN > 1 .AND. DB_COLUMN <> 5
	    IF F_BLOQR()
	       SAVE SCREEN 
	       SET CURSOR ON
	       FLD_NAME = CAMPO[DB_COLUMN]
	       FLD_PIC  = PIC[DB_COLUMN]
	       @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
	       READ
	       SET CURSOR OFF
	       UNLOCK
	       RESTORE SCREEN
	    ENDIF
	 ENDIF
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
		   
