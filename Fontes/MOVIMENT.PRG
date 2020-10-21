*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MOVIMENT.PRG
* Data        : 01/01/99
* Objetivo    : Movimentacoes de estoque
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MOVIMENT()
SAVE SCREEN TO TELA03
PTIT( "MOVIMENTACOES DE ESTOQUE")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   SELE 3
   USE TIPO INDEX TIPO
   GOTO TOP
   SELE 4
   USE ITENS INDEX ITENS ALIAS S4
   GOTO TOP
   SELE 1
   USE ESTOQUE INDEX ESTOQUE
   SET RELATION TO CODIGO INTO S4
   GOTO TOP
   PLIMPA()
   eFAZ  = .F.
   eITEM = 0
   DO WHILE .T.
      @ 06,10 SAY "Item de Estoque : " GET eITEM PICT "@R 999,999"
      READ
      IF eITEM <> 0
	 SELE 4
	 GOTO TOP
	 SEEK eITEM
	 IF FOUND()
	    SELE 1
	    GOTO TOP
	    SEEK eITEM
	    IF FOUND()
	       EXIT
	    ELSE
               PLQUA("01")
               PMSGR( "Item nao Possui Estoque",3
	       PLQUA("01")
	       PPERG ( "Deseja Incluir (S/N) ? ")
	       IF sisRESP = "S"
	          eFAZ = .T.
	          EXIT
	       ELSE
                  LOOP
	       ENDIF
	    ENDIF
	 ELSE
	    PLQUA("01")
	    PMSGR( "Item nao Cadastrado",3
	    LOOP
	 ENDIF
      ELSE
         RESTORE SCREEN FROM TELA03
         CLOSE DATABASES
	 RETURN nil
      ENDIF
   ENDDO
   SELE 4
   SEEK eITEM
   @ 06,37 SAY " - "+ALLTRIM(DESCRICAO)
   *** Tipo de movimentacao
   IF eFAZ
      eTIPO = "E"
      @ 08,10 SAY "Movimento       :  Entradas "
   ELSE
      @ 08,10 SAY "Movimento       : "
      SET INTE ON
      SET COLOR TO
      SET COLOR TO W/N,+W/R
      @ 08,29 PROMPT " Entradas          "
      @ 08,29 PROMPT " Saidas            "
      READ
      sisOP=1
      MENU TO sisOP
      SAVE SCREEN TO TELA01
      DO CASE
         CASE sisOP = 1
            eTIPO = "E"
         CASE sisOP = 2
            eTIPO = "S"
      ENDCASE
   ENDIF
   * Data de Referencia
   eDATA = CTOD("")
   @ 10,10 SAY "Data            : " GET eDATA
   READ
   * Quantidade envolvida
   eQUANT = 0
   DO WHILE .T.
      @ 12,10 SAY "Quantidade      : " GET eQUANT PICT "@R 999,999"
      READ
      IF eQUANT = 0
	 LOOP
      ELSE
         IF eTIPO = "S"
	    SELE 1
	    SEEK eITEM
	    IF SALDO_FINA <= eQUANT
	       PLQUA("01")
	       PMSGR( "N∆o H† quantidade suficiente.",3
	       LOOP
	    ELSE
               EXIT
	    ENDIF
         ELSE
            EXIT
         ENDIF
      ENDIF
   ENDDO
   CLEAR GETS
   PLQUA("01")
   PPERG ( "Confirma (S/N) ? ")
   IF sisRESP = "S"
      SELE 1
      SEEK eITEM
      IF .NOT. FOUND()
         APPEND BLANK
         REPLACE CODIGO WITH eITEM
      ENDIF
      IF eTIPO = "S"
         REPLACE SAIDA      WITH SAIDA+eQUANT,;
                 SALDO_FINA WITH ((SALDO_INIC+ENTRADA)-(SAIDA)),;
                 MOVIMENTO  WITH eDATA
      ELSE
         REPLACE ENTRADA    WITH ENTRADA+eQUANT,;
                 SALDO_FINA WITH ((SALDO_INIC+ENTRADA)-(SAIDA)),;
                 MOVIMENTO  WITH eDATA
      ENDIF
      SELE 4
      IF eTIPO = "S"
         REPLACE ULT_ESTR WITH eDATA
      ELSE
         REPLACE ULT_SAID WITH eDATA
      ENDIF
      SELE 1
   ENDIF
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
**************************  fim  *********************************
