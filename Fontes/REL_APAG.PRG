*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_APAG.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE CONTAS A PAGAR - EMISS‡O DE A. PAGAMENTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_APAG()
SAVE SCREEN TO TELA06
PTIT( "EMISSÇOO DE AUTORIZA€ÇO DE PAGAMENTO")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   LIN = 0
   PLIMPA()
   SELE 1
   USE CPAGAR INDEX CPAGAR,DATA_CPV,DATA_CPG
   GOTO TOP
   SELE 2
   USE FORNECED INDEX FORNECED
   GOTO TOP
   SELE 3
   USE CONTAS INDEX CONTAS
   GOTO TOP
   SELE 4
   USE STATUS
   GOTO TOP
   eCIDADE = CIDADE
   PLQUA("01")
   eOPCAO = SPACE(1)
   @ 23,00 SAY "Deseja Sele‡Æo por N£mero de (L)an‡amento ou por (D)ata ? " GET eOPCAO PICT "@!X" VALID eOPCAO $("LD")
   READ
   IF eOPCAO = "D"
      SELE 1
      USE CPAGAR INDEX DATA_CPG,CPAGAR,DATA_CPV
      GOTO TOP
      PLQUA("01")
      eDATA_INI   = CTOD("")
      eDATA_FIM   = CTOD("")
      DO WHILE .T.
         @ 10,02 SAY "Periodo     : " GET eDATA_INI
         @ 11,02 SAY "Ate         : " GET eDATA_FIM
         READ
         IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
            PLQUA("01")
            PMSGR( "Datas Inv lidas !",3)
            LOOP
         ELSE
            IF eDATA_INI > eDATA_FIM
               PLQUA("01")
               PMSGR( "Data Inicio > Que Final !",3)
               LOOP
            ELSE
               EXIT
            ENDIF  
         ENDIF
      ENDDO
      DO WHILE .T.
         eCOD_CONTA = 0
         @ 12,02 SAY "Num. Conta  : " GET eCOD_CONTA PICT "@R 999999999"
         READ
         IF eCOD_CONTA <> 0
            SELE 3
            SEEK eCOD_CONTA
            IF FOUND()
               @ 12,30 SAY " AG. : "+ALLTRIM(COD_AGENCI)+" C/C : "+ALLTRIM(COD_CC)
               EXIT
            ELSE
               PLQUA("01")
               PMSGR( "Aten‡Æo !",3)
               PMENSAG("C¢digo de Conta Inv lido !")
               PPARA()
               LOOP
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
      SELE 1
      IF eCOD_CONTA <> 0
         SET FILTER TO COD_CONTA = eCOD_CONTA .AND. DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. DATA_REAL = CTOD("") .AND. REALIZADO = 0
         GOTO TOP
      ELSE
         SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. DATA_REAL = CTOD("") .AND. REALIZADO = 0
         GOTO TOP
      ENDIF
   ELSE
      SELE 1
      USE CPAGAR INDEX CPAGAR,DATA_CPV,DATA_CPG
      GOTO TOP
      DO WHILE .T.
         eNUM_LANC = 0
         @ 12,02 SAY "Num. Lan‡amento : " GET eNUM_LANC PICT "@R 999999999"
         READ
         IF eNUM_LANC <> 0
            SELE 1
            SEEK eNUM_LANC
            IF FOUND()
               EXIT
            ELSE
               PLQUA("01")
               PMSGR( "Aten‡Æo !",3)
               PMENSAG("Lan‡amento Inv‡lido !")
               PPARA()
               LOOP
            ENDIF
         ELSE
            CLOSE ALL
            RESTORE SCREEN FROM TELA06
            RETURN nil
         ENDIF
      ENDDO
      SELE 1
      SET FILTER TO LANCAMENTO = eNUM_LANC
      GOTO TOP
   ENDIF
   PLQUA("01")
   eTIPO = SPACE(1)
   @ 23,00 SAY "Deseja ImpressÆo de AP (G)rande ou (P)equena ? " GET eTIPO PICT "@!X" VALID eTIPO $("GP")
   READ
   eARQUIVO = SPACE(60)
   eARQUIVO = sisDRIVE_REL+":\"+sisPASTA_REL+"CTAPAG.TXT"+SPACE(30)
   IF (ALLTRIM(sisDRIVE_REL) = "").and.(ALLTRIM(sisPASTA_REL) = "") 
      eARQUIVO = "C:\RELATORIOS\RELATORIO.TXT          "
   ENDIF        
   PLQUA("01")
   PMENSAG("Posicione o Formulario e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SET DEVICE TO PRINT
   @ LIN,00  SAY " "
   DO WHILE .NOT. EOF()
      eNUM_LANC = LANCAMENTO
      IF eTIPO = "P"
         * Modelo Pequeno
         @ LIN,20  SAY "AUTORIZACAO DE PAGAMENTO No. "+STR(eNUM_LANC,5)
         @ LIN,20  SAY "AUTORIZACAO DE PAGAMENTO No. "+STR(eNUM_LANC,5)
         LIN = LIN + 2
         @ LIN,00  SAY "Empresa : "+sisTIT1
         LIN = LIN + 1
         eCODIGO_FOR = CODIGO_FOR
         SELE 2
         SEEK eCODIGO_FOR
         eFORNECED = NOME
         SELE 1
         @ LIN,00  SAY "Fornecedor ...: "+STR(eCODIGO_FOR,5)+"-"+SUBSTR(eFORNECED,1,27)
         @ LIN,50  SAY "Parcela....: "
         LIN = LIN + 1
         @ LIN,00  SAY "No. Documento : "+NUM_DOCUME
         @ LIN,50  SAY "Data Compra: "+DTOC(DATA_NOTA)
         LIN = LIN + 1
         @ LIN,00  SAY "Vencimento ...: "+DTOC(DATA_PREV)
         @ LIN,50  SAY "Valor......: "
         @ LIN,63  SAY PREVISTO   PICT "@R 999,999,999.99"
         LIN = LIN + 1
         @ LIN,00  SAY "Descricao.....: "+SUBSTR(DESCRICAO,1,32)
         LIN = LIN + 1
         @ LIN,00  SAY REPLICATE("_",79)
         LIN = LIN + 1      
         @ LIN,00  SAY REPLICATE("_",79)
         LIN = LIN + 1      
         @ LIN,00  SAY REPLICATE("_",79)
         LIN = LIN + 1      
         @ LIN,00  SAY REPLICATE("_",79)
         LIN = LIN + 1
         @ LIN,00  SAY "|"
         @ LIN,01  SAY "EMITENTE"
         @ LIN,20  SAY "|"
         @ LIN,21  SAY "RESPONSAVEL"
         @ LIN,40  SAY "|"
         @ LIN,41  SAY "CONTAS A PAGAR"
         @ LIN,60  SAY "|"
         @ LIN,61  SAY "CONTABILIDADE"
         @ LIN,79  SAY "|"
         LIN = LIN + 1      
         @ LIN,00  SAY "|"
         @ LIN,20  SAY "|"
         @ LIN,40  SAY "|"
         @ LIN,60  SAY "|"
         @ LIN,79  SAY "|"
         LIN = LIN + 1
         @ LIN,01  SAY REPLICATE(" _",38)
         LIN = LIN + 2
         IF LIN >= 45
            LIN = 0
            @ LIN,00  SAY " "
         ENDIF
      ELSE
         * Modelo Grande
         @ LIN,30  SAY "AUTORIZACAO DE PAGAMENTO"
         @ LIN,30  SAY "AUTORIZACAO DE PAGAMENTO"
         LIN = LIN + 4
         eCODIGO_FOR = CODIGO_FOR
         SELE 2
         SEEK eCODIGO_FOR
         eFORNECED = NOME
         SELE 1
         @ LIN,00  SAY "Cedente .. ...: "+STR(eCODIGO_FOR,5)+"-"+SUBSTR(eFORNECED,1,27)
         @ LIN,50  SAY "Obra.......: "+STR(EMPREENDIM,5)
         LIN = LIN + 2
         @ LIN,00  SAY "Nota Fiscal...: "+NUM_DOCUME
         @ LIN,30  SAY "Valor.: "
         @ LIN,40  SAY PREVISTO PICT "@R 9,999,999.99"
         @ LIN,55  SAY "Vencimento.: "+DTOC(DATA_PREV)
         LIN = LIN + 2
         @ LIN,00  SAY "Discrimina‡Æo do Material ou Servico ...: "
         LIN = LIN + 2
         @ LIN,00  SAY DESCRICAO
         LIN = LIN + 2
         @ LIN,00  SAY "Cheque No.....: "+REPLICATE("_",12)
         @ LIN,30  SAY "Banco.........: "+REPLICATE("_",8)
         @ LIN,55  SAY "Lancamento.: "+STR(eNUM_LANC,5)
         LIN = LIN + 2
         @ LIN,00  SAY "Classificacao.: "+STR(CLASSE_D)
         LIN = LIN + 2
         @ LIN,00  SAY "Observacao....: "+REPLICATE("_",52)
         LIN = LIN + 4
         @ LIN,00  SAY "Aprovacao.....: "+REPLICATE("_",52)
         LIN = LIN + 2
         @ LIN,01  SAY REPLICATE(" _",38)
         LIN = LIN + 2
         IF LIN >= 45
            LIN = 0
            @ LIN,00  SAY " "
         ENDIF
      ENDIF
      SELE 1
      SKIP
      DO PTECLA
      IF sisINTERR
         SET PRINT TO
         SET DEVICE TO SCREEN
         CLOSE ALL
         RETURN nil
      ENDIF
   ENDDO
   SET PRINT TO
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA06
RETURN nil
*************************  fim  *********************************
