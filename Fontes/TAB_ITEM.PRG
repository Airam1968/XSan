*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_ITEM.PRG
* Data        : 01/01/99
* Objetivo    : Consulta a banco de Itens
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_ITEM()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA ‹¨«TENS DE PRODUTOS")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   SELE 2
   USE ARQ_TIPO INDEX ARQ_TIPO
   GOTO TOP
   PLQUA("01")
   PPERG ( "Deseja consultar em ordem Alfab‹¨«tica (S/N) ? ")
   IF sisRESP = "S"
      eFAZ = .T.
      USE ARQ_ITEM INDEX ITEM_DES,ARQ_ITEM
      GOTO TOP
   ELSE
      eFAZ = .F.
      SELE 1
      USE ARQ_ITEM INDEX ARQ_ITEM,ITEM_DES 
      GOTO TOP
   ENDIF
   PLIMPA()
   SET COLOR TO W/N,+W/R
   DECLARE CAMPO[25],PIC[25],CAB[25],LENS[25]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1] ="@R 9999999"
   PIC[2] ="@R 99999"
   PIC[3] ="@S2"
   PIC[4] ="@S40"
   PIC[5] ="@S15"
   PIC[6] ="@S15"
   PIC[7] ="@R 999,999.99"
   PIC[8] ="@R 99,999,999.99"
   PIC[9] ="@R 99,999,999.99"
   PIC[10]="@R 99,999.999"
   PIC[11]="@R 99,999,999.99"
   PIC[12]="@L"
   PIC[13]="  /  /    "
   PIC[14]="  /  /    "
   PIC[15]="  /  /    "
   PIC[16]="@S60"
   PIC[17]="@S60"
   PIC[18]="@S60"
   PIC[19]="@S60"
   PIC[20]="@S60"
   PIC[21]="@S60"
   PIC[22]="@S60"
   PIC[23]="@S60"
   PIC[24]="@S60"
   PIC[25]="@S60"
   CAB[01]="C‹¨«DIGO"
   CAB[02]="TIPO"
   CAB[03]="UNID."
   CAB[04]="DESCRI‹¨«‹¨«O"
   CAB[05]="MARCA"
   CAB[06]="MODELO"
   CAB[07]="ESTOQ. MIN."
   CAB[08]="CUSTO"
   CAB[09]="PRECO 1"
   CAB[10]="QUANTIDADE"
   CAB[11]="VALOR"
   CAB[12]="ATIVA"
   CAB[13]="ULT_COMPRA"
   CAB[14]="ULT_ENTRA"
   CAB[15]="ULT_SAIDA"
   CAB[16]="OBSERVA_01"
   CAB[17]="OBSERVA_02"
   CAB[18]="OBSERVA_03"
   CAB[19]="OBSERVA_04"
   CAB[20]="OBSERVA_05"
   CAB[21]="OBSERVA_06"
   CAB[22]="OBSERVA_07"
   CAB[23]="OBSERVA_08"
   CAB[24]="OBSERVA_09"
   CAB[25]="OBSERVA_10"
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
   DBEDIT(06,01,21,78,CAMPO,"F_TAB_ITEM",PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
**************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_TAB_ITEM
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
               @ 05,00 CLEAR TO 18,79
               @ 05,00 TO 18,79 DOUBLE
			   PCENTRA( "Dados do ‹¨«tem",06)
               eNUM_ITEM   = 0
               eUNID       = SPACE(2)
               eITEM       = SPACE(40)
               eMARC       = SPACE(15)
               eMODE       = SPACE(15)
               eMINI       = 0
               eCUSTO      = 0
               ePRECO1     = 0
               eATIVA      = ATIVA
               eULT_COMPRA = CTOD("")
               eULT_ENTRA  = CTOD("")
               eULT_SAIDA  = CTOD("")
               eOBSER_1  = SPACE(50)
               eOBSER_2  = SPACE(50)
               eOBSER_3  = SPACE(50)
               eOBSER_4  = SPACE(50)
               eOBSER_5  = SPACE(50)
               eOBSER_6  = SPACE(50)
               eOBSER_7  = SPACE(50)
               eOBSER_8  = SPACE(50)
               eOBSER_9  = SPACE(50)
               eOBSER_10 = SPACE(50)
               @ 08,02 SAY "C‹¨«digo No.    : " GET eNUM_ITEM  PICT "@R 9999999"
	       READ
	       IF eNUM_ITEM = 0
		  EXIT
	       ENDIF
	       SELE 1
	       IF eFAZ
		  USE ARQ_ITEM INDEX ARQ_ITEM,ITEM_DES 
		  GOTO TOP
	       ENDIF
	       GOTO TOP
	       SEEK eNUM_ITEM
	       IF FOUND()
		  PLQUA("01")
		  PMSGR( "‹¨«tem j‹¨« Cadastrado",3)
		  EXIT
	       ELSE
		  DO WHILE .T.
		     eTIPO      = 0
                     @ 09,02 SAY "Tipo Produto  : " GET eTIPO  PICT "@R 99999"
		     READ
		     IF eTIPO = 0
			LOOP
		     ELSE
			SELE 2
			SEEK eTIPO
			IF FOUND()
                           @ 09,24 SAY " - "+ALLTRIM(DESCRICAO)
			   SELE 1
			   EXIT
			ELSE
			   PLQUA("01")
			   PMSGR( "‹¨«tem n‹¨«o Cadastrado",3)
			ENDIF
		     ENDIF
		  ENDDO
                  @ 10,02 SAY "Unidade       : " GET eUNID    PICT "@S2"
                  @ 11,02 SAY "Descri‹¨«‹¨«o     : " GET eITEM    PICT "@S40"
                  @ 12,02 SAY "Marca         : " GET eMARC    PICT "@S15"
                  @ 13,02 SAY "Modelo        : " GET eMODE    PICT "@S15"
                  @ 14,02 SAY "Estoque Minimo: " GET eMINI    PICT  "@R 999,999"
                  READ
                  @ 15,02 SAY "Custo         : " GET eCUSTO   PICT  "@R 999,999.99"
		  READ
                  @ 16,02 SAY "Preco 1       : " GET ePRECO1  PICT  "@R 999,999.99"
                  READ
                  @ 10,01 CLEAR TO 17,69
                  @ 10,02 SAY "Observa‹¨«oes   : " GET eOBSER_1
                  @ 11,02 SAY "              : " GET eOBSER_2
                  @ 12,02 SAY "              : " GET eOBSER_3
                  @ 13,02 SAY "              : " GET eOBSER_4
                  @ 14,02 SAY "              : " GET eOBSER_5
                  @ 15,02 SAY "              : " GET eOBSER_6
                  @ 16,02 SAY "              : " GET eOBSER_7
                  @ 17,02 SAY "              : " GET eOBSER_8
                  READ
		  PLQUA("01")
                  PPERG ( "Confirma Inclus‹¨«o (S/N) ? ")
		  IF sisRESP = "S"
		     SELE 1
		     IF eFAZ
			USE ARQ_ITEM INDEX ITEM_DES,ARQ_ITEM
			GOTO TOP
		     ENDIF
		     APPEND BLANK
		     IF F_BLOQR()
			REPLACE CODIGO     WITH eNUM_ITEM,;
				DESCRICAO  WITH eITEM,;
                                CUSTO      WITH eCUSTO
			REPLACE TIPO       WITH eTIPO,;
				UNIDADE    WITH eUNID,;
				MARCA      WITH eMARC
                        REPLACE MODELO     WITH eMODE,;
                                MINIMO     WITH eMINI,;
                                PRECO_1    WITH ePRECO1
                        REPLACE ATIVA      WITH .T.,;
                                OBSERVA_01 WITH eOBSER_1,;
                                OBSERVA_02 WITH eOBSER_2  
                        REPLACE OBSERVA_03 WITH eOBSER_3,;
                                OBSERVA_04 WITH eOBSER_4,;
                                OBSERVA_05 WITH eOBSER_5
                        REPLACE OBSERVA_06 WITH eOBSER_6,;
                                OBSERVA_07 WITH eOBSER_7,;
                                OBSERVA_08 WITH eOBSER_8
			UNLOCK
		     ENDIF
		     EXIT
		  ELSE
		     EXIT
		  ENDIF
	       ENDIF
	    ENDDO
	 ENDIF
	 SET CURSOR OFF
	 RESTORE SCREEN
      CASE KEY == 13
         DO CASE
            CASE DB_COLUMN = 1 .AND. .NOT. eFAZ
               SET CURSOR ON
               PLQUA("01")
               eITEM = 0
               @ 23,00 SAY "Digite o Codigo do Item : " GET eITEM PICT "@R 99999999"
               READ
               SEEK eITEM
               PLQUA("01")
               SET CURSOR OFF
            CASE DB_COLUMN = 1 .AND. eFAZ
               SET CURSOR ON
               PLQUA("01")
               ePESQUISA = SPACE(49)
               @ 23,00 SAY "Nome :" GET ePESQUISA PICT "@! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               READ
               ePESQUISA = ALLTRIM(ePESQUISA)
               SEEK ePESQUISA
               PLQUA("01")
               SET CURSOR OFF
            OTHERWISE
               SAVE SCREEN
               SET CURSOR ON
               PLQUA("01")
               PPERG ( "Confirma Altera‹¨«‹¨«o (S/N) ? ")
               IF sisRESP = "S"
                  DO WHILE .T.
                     @ 05,00 CLEAR TO 18,79
                     @ 05,00 TO 18,79 DOUBLE
					 PCENTRA( "Dados do ‹¨«tem",06)
                     eNUM_ITEM  = CODIGO
                     eTIPO      = TIPO
                     eUNID      = UNIDADE
                     eITEM      = DESCRICAO
                     eMARC      = MARCA
                     eMODE      = MODELO
                     eMINI      = MINIMO
                     eCUSTO     = CUSTO
                     ePRECO1    = PRECO_1
                     eATIVA     = ATIVA
                     eULT_COMPRA = ULT_COMPRA
                     eULT_ENTRA  = ULT_ENTR
                     eULT_SAIDA  = ULT_SAID
                     eOBSER_01 = OBSERVA_01
                     eOBSER_02 = OBSERVA_02
                     eOBSER_03 = OBSERVA_03
                     eOBSER_04 = OBSERVA_04
                     eOBSER_05 = OBSERVA_05
                     eOBSER_06 = OBSERVA_06
                     eOBSER_07 = OBSERVA_07
                     eOBSER_08 = OBSERVA_08
                     @ 08,02 SAY "C‹¨«digo No.    : " GET eNUM_ITEM  PICT "@R 9999999"
                     CLEAR GETS
                     @ 09,02 SAY "Tipo Produto  : " GET eTIPO  PICT "@R 99999"
                     READ
                     SELE 2
                     SEEK eTIPO
                     IF FOUND()
                        @ 09,24 SAY " - "+ALLTRIM(DESCRICAO)
                        SELE 1
                     ELSE
                        PLQUA("01")
						PMSGR( "‹¨«tem n‹¨«o Cadastrado",3)
                     ENDIF
                     @ 10,02 SAY "Unidade       : " GET eUNID    PICT "@S2"
                     @ 11,02 SAY "Descri‹¨«‹¨«o     : " GET eITEM    PICT "@S40"
                     @ 12,02 SAY "Marca         : " GET eMARC    PICT "@S15"
                     @ 13,02 SAY "Modelo        : " GET eMODE    PICT "@S15"
                     @ 14,02 SAY "Estoque Minimo: " GET eMINI    PICT  "@R 999,999"
                     READ
                     @ 15,02 SAY "Custo         : " GET eCUSTO   PICT  "@R 999,999.99"
                     READ
                     @ 16,02 SAY "Preco 1       : " GET ePRECO1  PICT  "@R 999,999.99"
                     READ
                     @ 17,02 SAY "Codigo Ativo (F/T) : " GET eATIVA  PICT  "@L"
                     READ
                     @ 10,01 CLEAR TO 17,69
                     @ 10,02 SAY "Observa‹¨«oes   : " GET eOBSER_01
                     @ 11,02 SAY "              : " GET eOBSER_02
                     @ 12,02 SAY "              : " GET eOBSER_03
                     @ 13,02 SAY "              : " GET eOBSER_04
                     @ 14,02 SAY "              : " GET eOBSER_05
                     @ 15,02 SAY "              : " GET eOBSER_06
                     @ 16,02 SAY "              : " GET eOBSER_07
                     @ 17,02 SAY "              : " GET eOBSER_08
                     READ
                     PLQUA("01")
                     PPERG ( "Confirma Altera‹¨«‹¨«o (S/N) ? ")
                     IF sisRESP = "S"
                        SELE 1
                        SEEK eNUM_ITEM
                        IF F_BLOQR()
                           REPLACE DESCRICAO  WITH eITEM,;
                                   CUSTO      WITH eCUSTO
                           REPLACE TIPO       WITH eTIPO,;
                                   UNIDADE    WITH eUNID,;
                                   MARCA      WITH eMARC
                           REPLACE MODELO     WITH eMODE,;
                                   MINIMO     WITH eMINI,;
                                   PRECO_1    WITH ePRECO1
                           REPLACE ATIVA      WITH eATIVA
                           REPLACE ATIVA      WITH .T.,;
                                   OBSERVA_01 WITH eOBSER_01,;
                                   OBSERVA_02 WITH eOBSER_02  
                           REPLACE OBSERVA_03 WITH eOBSER_03,;
                                   OBSERVA_04 WITH eOBSER_04,;
                                   OBSERVA_05 WITH eOBSER_05
                           REPLACE OBSERVA_06 WITH eOBSER_06,;
                                   OBSERVA_07 WITH eOBSER_07,;
                                   OBSERVA_08 WITH eOBSER_08
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
         ENDCASE
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
