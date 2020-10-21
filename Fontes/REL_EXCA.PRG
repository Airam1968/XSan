*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_EXCA.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE ESTOQUE DE MATERIAIS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_EXCA()
SAVE SCREEN TO TELA04
PTIT( "RELATORIO DE EXTRATO DE MOVIMENTACOES")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "N"
   RESTORE SCREEN FROM TELA04
   RETURN nil
ENDIF
SELE 8
USE FORNECED INDEX FORNECED
GOTO TOP
SELE 7
USE BANCOS INDEX BANCOS
GOTO TOP
SELE 4
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 5
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ? " GET eCONTA PICT "@R 999"
   READ
   SELE 2
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA <> 0
         PLQUA("01")
         PMSGR( "Aten‡Æo !",3)
         PMENSAG("Conta NÆo Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
PLQUA("01")
PPERG ( "Deseja Relat‡rio com Informa‡äes de Descricao (S/N) ? ")
IF sisRESP = "S"
   eFAZ = .T.
ELSE
   eFAZ = .F.
ENDIF
PLQUA("01")
PPERG ( "Deseja Relat‡rio com Informa‡äes de Cheques/Depositos (S/N) ? ")
IF sisRESP = "S"
   eFAZ2 = .T.
ELSE
   eFAZ2 = .F.
ENDIF
eCOD_BANCO  = COD_BANCO
eNOME_CONTA = ALLTRIM(COD_AGENCI)+" - "+ALLTRIM(COD_CC)
eLIMITE     = LIMITE
SELE 7
SEEK eCOD_BANCO
eNOME_BANCO = ALLTRIM(NOME_BANCO)
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   PLQUA("01")
   @ 23,02 SAY "Periodo : " GET eDATA_INI
   @ 23,25 SAY "Ate : "     GET eDATA_FIM
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
eOPCAO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Deseja Cheque/Depositos (T)otais ou do (P)eriodo selecionado (T/P) ? " GET eOPCAO PICT "@! X" VALID eOPCAO $("TP")
READ
PLQUA("01")
PPERG ( "Deseja Consolida‡Æo de Saldos (S/N) ? ")
IF sisRESP = "N"
   SET EXCLUSIVE ON
   SELE 6
   eCONT  = STR(eCONTA,5)
   eZERO = "00"
   eARQ  = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONT))))+ALLTRIM(eCONT) 
   eIND1 = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONT))))+ALLTRIM(eCONT) 
   USE &eARQ INDEX &eIND1
   GOTO TOP
   SET EXCLUSIVE OFF
   SELE 6
   use
   USE &eARQ INDEX &eIND1
   GOTO TOP
   SELE 1
   USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
   SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE <> CTOD("")
   GOTO TOP
   SELE 3
   USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
   SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA
   GOTO TOP
ELSE
   PLQUA("01")
   PPERG ( "Deseja Fornecedores no Hist¢rico de Despesas (S/N) ? ")
   SET EXCLUSIVE ON
   SELE 6
   eCONT  = STR(eCONTA,5)
   eZERO = "00"
   eARQ  = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONT))))+ALLTRIM(eCONT) 
   eIND1 = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONT))))+ALLTRIM(eCONT) 
   USE &eARQ INDEX &eIND1
   GOTO TOP
   DELE ALL FOR DATA >= eDATA_INI .AND. DATA <= eDATA_FIM
   PACK
   GOTO TOP
   SET EXCLUSIVE OFF
   SELE 6
   use
   USE &eARQ INDEX &eIND1
   GOTO TOP
   SELE 1
   USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
   SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE <> CTOD("")
   GOTO TOP
   SELE 3
   USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
   SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA
   GOTO TOP
   eDATA_AUX = eDATA_INI
   eSALDO_I  = 0
   eAPLIC_D  = 0
   eAPLIC_B  = 0
   DO WHILE .T.
      SELE 6
      SEEK eDATA_AUX
      IF FOUND()
         DO WHILE DATA = eDATA_AUX
            SKIP
         ENDDO
         SKIP -1
         IF SALDO_F = 0 .AND. ENTRADAS = 0 .AND. SAIDAS = 0
            eSALDO_I = SALDO_I
            eAPLIC_D = APLICACAOD
            eAPLIC_B = APLICACAOB
         ELSE
            eSALDO_I = SALDO_F
            eAPLIC_D = APLICACAOD
            eAPLIC_B = APLICACAOB
         ENDIF
         EXIT
      ELSE
         eDATA_AUX = eDATA_AUX - 1
         LOOP
      ENDIF
   ENDDO
   eDATA_AUX = eDATA_INI
   SELE 6
   APPEND BLANK
   IF F_BLOQR()
      REPLACE DATA       WITH eDATA_AUX,;
              CONTA      WITH eCONTA,;
              DESCRICAO  WITH "SALDO INICIAL "
      REPLACE SALDO_I    WITH eSALDO_I,;
              ENTRADAS   WITH 0,;
              SAIDAS     WITH 0
      REPLACE SALDO_F    WITH eSALDO_I,;
              APLICACAOD WITH eAPLIC_D,;
              APLICACAOB WITH eAPLIC_B
      UNLOCK
      eTRANSP  = eSALDO_I
      eAPLIC_D = APLICACAOD
      eAPLIC_B = APLICACAOB
   ENDIF
   DO WHILE eDATA_AUX <= eDATA_FIM
      PLQUA("01")
	  PMENSAG("Aguarde o Processamento, Dia "+DTOC(eDATA_AUX))
      SELE 3
      GOTO TOP
      SET FILTER TO DATA_REAL = eDATA_AUX .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA
      GOTO TOP
      DO WHILE .NOT. EOF()
         eVLR_REAL   = 0
         eCLASSE_R   = 0
         eDESCRICAO  = SPACE(40)
         eVLR_REAL   = REALIZADO
         eCLASSE_R   = CLASSE_R
         eDESCRICA2  = DESCRICAO
         eLANCAMENTO = LANCAMENTO
         eEMPREENDIM = EMPREENDIM
         IF eCLASSE_R < 0
            IF eCLASSE_R = -1
               eAPLIC_D = eAPLIC_D - eVLR_REAL
            ENDIF
            IF eCLASSE_R = -2
               eAPLIC_B = eAPLIC_B - eVLR_REAL
            ENDIF
         ENDIF
         SELE 5
         SEEK eCLASSE_R
         eDESCRICAO = DESCRICAO
         SELE 6
         APPEND BLANK
         IF F_BLOQR()
            REPLACE LANCAMENTO WITH eLANCAMENTO
            REPLACE DATA       WITH eDATA_AUX,;
                    CONTA      WITH eCONTA,;
                    DESCRICAO  WITH eDESCRICAO
            REPLACE SALDO_I    WITH eTRANSP,;
                    ENTRADAS   WITH eVLR_REAL,;
                    SAIDAS     WITH 0
            REPLACE SALDO_F    WITH (SALDO_I+ENTRADAS)-SAIDAS,;
                    APLICACAOD WITH eAPLIC_D,;
                    APLICACAOB WITH eAPLIC_B
            REPLACE DESCRICA2  WITH eDESCRICA2,;
                    CLASSE     WITH eCLASSE_R,;
                    EMPREENDIM WITH eEMPREENDIM
            UNLOCK
            eTRANSP = SALDO_F
            eAPLIC_D = APLICACAOD
            eAPLIC_B = APLICACAOB
         ENDIF
         SELE 3
         SKIP
      ENDDO
      SELE 1
      GOTO TOP
      SET FILTER TO DATA_REAL = eDATA_AUX .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA
      GOTO TOP
      DO WHILE .NOT. EOF()
         eCOMPENSADO = SPACE(1)
         eVLR_REAL   = 0
         eCLASSE_D   = 0
         eDESCRICAO  = SPACE(40)
         eNUM_CHEQUE = 0
         eCODIGO_FOR = 0
         eNOME_FOR   = SPACE(40)
         eVLR_REAL   = REALIZADO
         eCLASSE_D   = CLASSE_D
         eNUM_CHEQUE = NUM_CHEQUE
         eDESCRICA2  = DESCRICAO
         eCODIGO_FOR = CODIGO_FOR
         eLANCAMENTO = LANCAMENTO
         eNUM_COMPRA = NUM_COMPRA
         eEMPREENDIM = EMPREENDIM
         IF eCLASSE_D < 0
            IF eCLASSE_D = -1
               eAPLIC_D = eAPLIC_D + eVLR_REAL
            ENDIF
            IF eCLASSE_D = -2
               eAPLIC_B = eAPLIC_B + eVLR_REAL
            ENDIF
         ENDIF
         SELE 4
         SEEK eCLASSE_D
         eDESCRICAO = DESCRICAO
         SELE 8
         SEEK eCODIGO_FOR
         eNOME_FOR = NOME
         SELE 6
         APPEND BLANK
         IF F_BLOQR()
            REPLACE LANCAMENTO WITH eLANCAMENTO
            REPLACE DATA       WITH eDATA_AUX,;
                    CONTA      WITH eCONTA,;
                    DESCRICAO  WITH IIF(sisRESP = "S",eNOME_FOR,eDESCRICAO)
            REPLACE SALDO_I    WITH eTRANSP,;
                    ENTRADAS   WITH 0,;
                    SAIDAS     WITH eVLR_REAL
            REPLACE SALDO_F    WITH (SALDO_I+ENTRADAS)-SAIDAS,;
                    NUM_CHEQUE WITH eNUM_CHEQUE
            REPLACE APLICACAOD WITH eAPLIC_D,;
                    APLICACAOB WITH eAPLIC_B,;
                    DESCRICA2  WITH eDESCRICA2
            REPLACE NUM_COMPRA WITH eNUM_COMPRA,;
                    CLASSE     WITH eCLASSE_D,;
                    EMPREENDIM WITH eEMPREENDIM
            UNLOCK
            eTRANSP = SALDO_F
            eAPLIC_D = APLICACAOD
            eAPLIC_B = APLICACAOB
         ENDIF
         SELE 1
         SKIP
      ENDDO
      eDATA_AUX = eDATA_AUX + 1
   ENDDO
ENDIF
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"EXTRATO.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "EXTRATO.TXT"+SPACE(10) 
ENDIF   
eSAIDA   = SPACE(10)
DO WHILE .T.
   PLQUA("01")
   PPERG ( "Deseja extrato em arquivo texto no Drive (X:) (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
      @ 23,00 SAY "CONFIRME O DESTINO  : " GET eARQUIVO
      READ
      eARQUIVO = ALLTRIM(eARQUIVO)
      PLQUA("01")
	  PMENSAG("Insira um disco no drive (X:), e tecle ENTER.")
      PPARA()
      eSAIDA = "ARQUIVO"
      EXIT
   ELSE
      PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      DO PIMPR
      eSAIDA = "IMPRESSORA"
      EXIT
   ENDIF
ENDDO
SELE 6
GOTO TOP
SET FILTER TO DATA >= eDATA_INI .AND. DATA <= eDATA_FIM
GOTO TOP
IF eSAIDA = "ARQUIVO"
   SET PRINT ON
   SET DEVICE TO PRINT
   SET PRINTER TO &eARQUIVO
ELSE
   PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SET DEVICE TO PRINT
ENDIF
eDATA_AUX = DATA
eSUB_ENTR = 0
eSUB_SAID = 0
eSUB_SALD = 0
eTOT_ENTR = 0
eTOT_SAID = 0
eTOT_SALD = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,120 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,121 SAY "Relat: SAN004"
   @ 03,10  SAY sisLETRAE+"EXTRATO DE MOVIMENTACAO DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 04,00  SAY "Banco\Conta : "+eNOME_BANCO+" - "+eNOME_CONTA
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "DATA"
   @ 06,10  SAY "LANC."
   @ 06,20  SAY "DESCRICAO"
   @ 06,50  SAY "CHEQUE"
   @ 06,60  SAY "SALDO INICIAL"
   @ 06,80  SAY "ENTRADAS"
   @ 06,100 SAY "SAIDAS"
   @ 06,120 SAY "SALDO FINAL"
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 56
      IF eDATA_AUX = DATA
         @ LIN,00  SAY DATA                         
         @ LIN,10  SAY LANCAMENTO PICT "9999999"
         @ LIN,20  SAY SUBSTR(DESCRICAO,1,28)
         @ LIN,50  SAY NUM_CHEQUE PICT "@R 999999999"
         @ LIN,60  SAY SALDO_I    PICT "@R 999,999,999.99"
         @ LIN,80  SAY ENTRADAS   PICT "@R 999,999,999.99"
         @ LIN,100 SAY SAIDAS     PICT "@R 999,999,999.99"
         @ LIN,119 SAY SALDO_F    PICT "@R 999,999,999.99"
         LIN = LIN + 1
         IF eFAZ
            @ LIN,20  SAY SUBSTR(DESCRICA2,1,28)
            LIN = LIN + 1
         ENDIF
         eTOT_ENTR = eTOT_ENTR + ENTRADAS
         eTOT_SAID = eTOT_SAID + SAIDAS
         eSUB_ENTR = eSUB_ENTR + ENTRADAS
         eSUB_SAID = eSUB_SAID + SAIDAS
         eSALDO_F  = SALDO_F
         SKIP
         DO PTECLA
         IF sisINTERR
            IF eSAIDA = "ARQUIVO"
               SET PRINT OFF
               SET DEVICE TO SCREEN
            ELSE
               SET DEVICE TO SCREEN
            ENDIF
            CLOSE ALL
            RETURN nil
         ENDIF
      ELSE
         @ LIN,80  SAY REPLICATE("_",50)
         LIN = LIN + 1
         @ LIN,02  SAY "SubTotal: "
         @ LIN,80  SAY eSUB_ENTR  PICT "@R 999,999,999.99"
         @ LIN,100 SAY eSUB_SAID  PICT "@R 999,999,999.99"
         @ LIN,119 SAY eSALDO_F   PICT "@R 999,999,999.99"
         LIN = LIN + 2
         eSALDO_F  = SALDO_F
         eSUB_ENTR = 0
         eSUB_SAID = 0
         eSUB_SALD = 0
         eDATA_AUX = DATA
      ENDIF
   ENDDO
   IF EOF()
      @ LIN,80  SAY REPLICATE("_",50)
      LIN = LIN + 1
      @ LIN,02  SAY "SubTotal: "
      @ LIN,80  SAY eSUB_ENTR  PICT "@R 999,999,999.99"
      @ LIN,100 SAY eSUB_SAID  PICT "@R 999,999,999.99"
      @ LIN,119 SAY eSALDO_F   PICT "@R 999,999,999.99"
      LIN = LIN + 2
      @ LIN,80  SAY REPLICATE("_",50)
      LIN = LIN + 1
      @ LIN,02  SAY "Totais : "
      @ LIN,80  SAY eTOT_ENTR  PICT "@R 999,999,999.99"
      @ LIN,100 SAY eTOT_SAID  PICT "@R 999,999,999.99"
      @ LIN,119 SAY eSALDO_F   PICT "@R 999,999,999.99"
      LIN = LIN + 2
      IF LIN >= 53
         @ 58,00 SAY REPLICATE("=",135)
         @ 59,104 SAY sisTIT3
         @ 01,00 SAY " "+sisLETRAN
         @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
         @ 01,119 SAY "Data : "+dtoc(DATE())
         @ 02,00  SAY "Sistema : "+sisTIT2
         @ 02,121 SAY "Relat: SAN004"
         @ 03,10  SAY sisLETRAE+"EXTRATO DE MOVIMENTACAO DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
         @ 04,00  SAY "Banco\Conta : "+eNOME_BANCO+" - "+eNOME_CONTA
         @ 05,00  SAY REPLICATE("=",135)
         @ 06,02  SAY "DATA"
         @ 06,10  SAY "CONTA"
         @ 06,20  SAY "DESCRICAO"
         @ 06,50  SAY "CHEQUE"
         @ 06,60  SAY "SALDO INICIAL"
         @ 06,80  SAY "ENTRADAS"
         @ 06,100 SAY "SAIDAS"
         @ 06,120 SAY "SALDO FINAL"
         @ 08,00 SAY REPLICATE("=",135)
         LIN = 9
      ENDIF
      SKIP -1
      @ LIN,15 SAY "Saldo Final          : "
      @ LIN,40 SAY SALDO_F     PICT "@R 999,999,999,999.99"
      eSALDO_L = SALDO_F + eLIMITE
      LIN = LIN + 1
      @ LIN,15 SAY "Saldo c\Limite Cred. : "
      @ LIN,40 SAY eSALDO_L    PICT "@R 999,999,999,999.99"
      LIN = LIN + 1
      @ LIN,15 SAY "Aplica‡‡es Desbloque.: "
      @ LIN,40 SAY APLICACAOD  PICT "@R 999,999,999,999.99"
      LIN = LIN + 1
      @ LIN,15 SAY "Aplica‡‡es Bloqueadas: "
      @ LIN,40 SAY APLICACAOB  PICT "@R 999,999,999,999.99"
      eGERAL = 0
      eGERAL = SALDO_F + APLICACAOD + APLICACAOB + eLIMITE
      LIN = LIN + 1
      @ LIN,15 SAY "Saldo Final Geral    : "
      @ LIN,40 SAY eGERAL      PICT "@R 999,999,999,999.99"
      LIN = LIN + 1
      SKIP      
      IF eFAZ2
         SELE 1
         USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG
         GOTO TOP
         IF eOPCAO = "T"
            SET FILTER TO REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE = CTOD("") .AND. NUM_CHEQUE <> 0
            GOTO TOP
         ELSE
            SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND.REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE = CTOD("") .AND. NUM_CHEQUE <> 0
            GOTO TOP
         ENDIF
         eCHE_COMP = 0
         DO WHILE .NOT. EOF()
            eCHE_COMP = eCHE_COMP + REALIZADO
            SKIP
         ENDDO
         @ LIN,15 SAY "Cheques a Compensar  : "
         @ LIN,40 SAY eCHE_COMP   PICT "@R 999,999,999,999.99"
         LIN = LIN + 1
         SELE 3
         USE CRECEBER INDEX DATA_CRE,CRECEBER
         GOTO TOP
         IF eOPCAO = "T"
            SET FILTER TO REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE = CTOD("")
            GOTO TOP
         ELSE
            SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND.REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE = CTOD("")
            GOTO TOP
         ENDIF
         eDEP_COMP = 0
         DO WHILE .NOT. EOF()
            eDEP_COMP = eDEP_COMP + REALIZADO
            SKIP
         ENDDO
         @ LIN,15 SAY "Depositos a Compensar: "
         @ LIN,40 SAY eDEP_COMP   PICT "@R 999,999,999,999.99"
         SELE 6
         SKIP
         LIN = 59 
      ELSE
         LIN = 59       
      ENDIF
   ENDIF
   @ 58,00 SAY REPLICATE("=",135)
   @ 59,104 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
IF eSAIDA = "ARQUIVO"
   SET PRINT TO
   SET PRINT OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINT TO
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
