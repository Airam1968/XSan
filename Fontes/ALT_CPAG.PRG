*********************************************************************
FUNCTION ALT_CPAG()
SAVE SCREEN TO TELA03
PTIT( "ALTERA€ÇO DE CONTAS A PAGAR"
SELE 4
USE CLASSE INDEX CLASSE
SET FILTER TO R_D = "D"
GOTO TOP
SELE 3
USE STATUS
GOTO TOP
SELE 2
USE FORNECED INDEX FORNECED
GOTO TOP
SELE 1
USE CPAGAR INDEX CPAGAR,DATA_CPG
GOTO TOP
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   DO WHILE .T.
      PLQUA("11")
      DO WHILE .T.
         eNUM_LANC   = 0
         @ 06,05 SAY "Lan‡amento    : " GET eNUM_LANC PICT "@R 9999999999"
         READ
         IF eNUM_LANC = 0
            CLOSE ALL
            RESTORE SCREEN FROM TELA03
            RETURN nil
         ELSE
            SEEK eNUM_LANC
            IF .NOT. FOUND()
               PLQUA("01")
               PMSGR( "Lan‡amento Inexistente !",3)
               LOOP
            ELSE
               EXIT
            ENDIF
         ENDIF
      ENDDO
      DO WHILE .T.
         IF F_BLOQR()
            @ 08,05 SAY "Data Pagamento: " GET DATA
            READ
	    UNLOCK
	 ENDIF
         IF DATA = CTOD("")
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
      DO WHILE .T.
         IF F_BLOQR()
            @ 10,05 SAY "Cliente       : " GET CODIGO_FOR PICT "@R 9999999"
            READ
	    UNLOCK
	 ENDIF
         eCODIGO_FOR = CODIGO_FOR
         SELE 2
         SEEK eCODIGO_FOR
         IF .NOT. FOUND()
            PLQUA("01")
            PMSGR( "C¢digo Inexistente",3
            SELE 1
            LOOP
         ELSE
            @ 10,29 SAY " - "+ALLTRIM(NOME)
            SELE 1
            EXIT
         ENDIF
      ENDDO
      IF F_BLOQR()
         @ 12,05 SAY "Descri‡Æo     : " GET DESCRICAO
	 READ
	 UNLOCK
      ENDIF
      DO WHILE .T.
         IF F_BLOQR()
            @ 14,05 SAY "Valor a Pagar : " GET VALOR PICT "@R 999,999,999,999.99"
            READ
	    UNLOCK
	 ENDIF
         IF VALOR = 0
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
      IF F_BLOQR()
         @ 16,05 SAY "Previs‡o\Real : " GET SITUACAO PICT "X" VALID SITUACAO $("PRpr")
         READ
	 UNLOCK
      ENDIF
      @ 18,05 SAY "Tipo Receita  : " GET TIPO
      CLEAR GETS
      PLQUA("01")
      PPERG ( "Deseja Alterar o Tipo de Receita (S/N) ? ")
      IF sisRESP = "S"
         SELE 4
         DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1] ="@R 99999999"
         PIC[2] ="@S30"
         CAB[1] ="        "
         CAB[2] ="                               "
         DBEDIT(18,20,18,63,CAMPO,"F_CONS",PIC,CAB,CHR(196),CHR(16),CHR(205))
         eTIPO = CODIGO
         SELE 1
	 IF F_BLOQR()
            REPLACE TIPO WITH eTIPO
	    UNLOCK
	 ENDIF
      ENDIF
      PLQUA("01")
      PPERG ( "Continua (S/N) ? ")
      IF sisRESP = "S"
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
